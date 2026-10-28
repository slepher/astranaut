%%%-------------------------------------------------------------------
%%% @doc State and planning for local macros.
%%%
%%% This module intentionally does not own the forms scan nor macro invocation.
%%% This module owns declaration snapshots, closure analysis, expansion
%%% scheduling. astranaut_macro_expander performs generic expansion;
%%% astranaut_macro_local_compile owns BEAM preparation and generation state.
%%%-------------------------------------------------------------------
-module(astranaut_macro_local).

-include("do.hrl").
-export([
    init/1,
    format_error/1,
    handle_form/3,
    prepare_target/3,
    validate_generated/2,
    remove_declarations/2,
    finish_attribute_pass/3,
    new/0, new/1,
    register/5,
    need_callable/3,
    compile_plan/2,
    finalize/2,
    finalize/3,
    frozen_ids/1,
    local_macros/1,
    retained_form_ids/1,
    nonclosure_retain_roots/2,
    source_view/2,
    reject_locked_mutation/2,
    finalize_plan/1,
    form_id/1,
    execute_plan/3
]).

-type fa() :: {atom(), non_neg_integer()}.
-type form_id() :: {function | spec, atom(), non_neg_integer()}.
-type formatter_protocol() :: present | missing.
-type macro_environment() ::
    astranaut_macro_registry:macro_environment().
-type workflow_context() :: #{
    source_view := [term()],
    compile_opts := [compile:option()]
}.
-type compilation_boundary() :: #{members := [fa()], requests := [map()]}.
-type state() :: #{compiler := astranaut_macro_local_compile:state(), atom() => term()}.

format_error({invalid_closure_roots, Functions}) ->
    io_lib:format(
        "closure_roots contains undefined functions: ~p", [Functions]
    );
format_error({undefined_local_macro_retain, Functions}) ->
    io_lib:format(
        "local_macro_retain contains undefined functions: ~p", [Functions]
    );
format_error({ineffective_local_macro_retain, Functions}) ->
    io_lib:format(
        "local_macro_retain has no effect for functions outside every local macro closure: ~p",
        [Functions]
    );
format_error({duplicate_local_macro_declaration, Function}) ->
    io_lib:format(
        "duplicate local macro declaration for ~p", [Function]
    );
format_error({illegal_locked_form_mutation, Form}) ->
    io_lib:format(
        "local macro expansion modified frozen form: ~p", [Form]
    ).

%%%===================================================================
%%% Optional capability callbacks
%%%===================================================================

init(#{module := Module}) ->
    (new(astranaut_macro_local_compile:module_name(Module)))#{declarations => #{}}.

handle_form(
    {attribute, Pos, local_macro, Attr} = Form,
    #{
        module := SourceModule,
        source_view := SourceView,
        macro_environment := MacroEnvironment
    } = Context,
    State
) ->
    FormatterDetected = maps:is_key(formatter_info, State),
    ClauseMap = function_clauses_map(SourceView, #{}),
    do([
        return
     || {FAs, Options} <-
            validate_local_macro_attribute(ClauseMap, Attr, Pos),
        State1 <- register_return(
            FAs,
            Options,
            SourceView,
            MacroEnvironment,
            State,
            Pos
        ),
        FormatterInfo = maps:get(formatter_info, State1),
        maybe_missing_formatter_warning(
            FormatterDetected, FormatterInfo, SourceModule
        ),
        Definitions = local_macro_definitions(
            FAs, Options, FormatterInfo, State1, Context
        ),
        Declarations = maps:get(declarations, State1, #{}),
        return(
            {keep, Definitions, State1#{
                declarations =>
                    maps:put(
                        Form,
                        {Pos, FAs},
                        Declarations
                    )
            }}
        )
    ]).

maybe_missing_formatter_warning(true, _FormatterInfo, _SourceModule) ->
    astranaut_return:return(ok);
maybe_missing_formatter_warning(false, #{protocol := present}, _SourceModule) ->
    astranaut_return:return(ok);
maybe_missing_formatter_warning(false, #{protocol := missing}, SourceModule) ->
    astranaut_return:warning({missing_macro_formatter, SourceModule}).

register_return(FAs, Options, SourceView, MacroEnvironment, State, Pos) ->
    case register(FAs, Options, SourceView, MacroEnvironment, State) of
        {ok, State1} -> astranaut_return:return(State1);
        {error, Error} -> local_error(Pos, Error)
    end.

%% The scanner supplies context lazily: external and already-callable targets
%% do not need a source snapshot or declaration environment.
prepare_target(
    #{
        macro := #{
            macro_source := local_macro,
            function := Function,
            arity := Arity
        }
    },
    GetWorkflowContext,
    State
) ->
    FA = {Function, Arity},
    case
        maps:is_key(FA, local_macros(State)) andalso
            not astranaut_macro_local_compile:is_callable(FA, maps:get(compiler, State))
    of
        true -> need_callable(FA, GetWorkflowContext(), State);
        false -> astranaut_return:return(State)
    end;
prepare_target(_ExternalTarget, _GetWorkflowContext, State) ->
    astranaut_return:return(State).

validate_generated(Forms, State) ->
    case reject_locked_mutation(Forms, State) of
        ok ->
            astranaut_return:return(ok);
        {error, {illegal_locked_form_mutation, Form} = Error} ->
            astranaut_return:with_error(
                fun(ErrorStruct) ->
                    astranaut_error:update_pos(
                        form_pos(Form), ?MODULE, ErrorStruct
                    )
                end,
                astranaut_return:error_fail(Error)
            )
    end.

declarations(State) ->
    maps:get(declarations, State, #{}).

validate_local_macro_attribute(ClauseMap, Attr, Pos) ->
    do([
        return
     || {FAs, Options} <-
            astranaut_macro_registry:validate_definition_attribute(
                local_macro, Attr, local_macro_validator()
            ),
        FAs1 <- astranaut_macro_registry:remove_undefined_macros(
            FAs, ClauseMap
        ),
        Options1 <- validate_closure_roots_defined(
            Options, ClauseMap, Pos
        ),
        return({FAs1, Options1})
    ]).

local_macro_validator() ->
    maps:merge(
        astranaut_macro_registry:macro_definition_validator(),
        #{
            closure_roots =>
                {list_of, fun validate_function_with_arity/1}
        }
    ).

validate_function_with_arity({Function, Arity} = FA) when
    is_atom(Function), is_integer(Arity), Arity >= 0
->
    {ok, FA};
validate_function_with_arity(FA) ->
    {error, {invalid_function_with_arity, FA}}.

validate_closure_roots_defined(Options, ClauseMap, Pos) ->
    Missing = [
        FA
     || FA <- maps:get(closure_roots, Options, []),
        not maps:is_key(FA, ClauseMap)
    ],
    case Missing of
        [] ->
            astranaut_return:return(Options);
        _ ->
            local_error(Pos, {invalid_closure_roots, Missing})
    end.

local_macro_definitions(
    FAs,
    Options,
    FormatterInfo,
    State,
    #{
        module := Module,
        global_macro_opts := GlobalMacroOpts0
    }
) ->
    LocalMacroModule = astranaut_macro_local_compile:module(maps:get(compiler, State)),
    GlobalMacroOpts =
        formatter_options(
            FormatterInfo,
            LocalMacroModule,
            GlobalMacroOpts0
        ),
    maps:from_list(
        [
            {FA,
                local_macro_options(
                    Module,
                    LocalMacroModule,
                    GlobalMacroOpts,
                    FA,
                    Options
                )}
         || FA <- FAs
        ]
    ).

local_macro_options(
    Module,
    LocalMacroModule,
    GlobalMacroOpts,
    {Function, Arity},
    MacroOptions
) ->
    Options = maps:merge(
        GlobalMacroOpts,
        MacroOptions#{macro_source => local_macro}
    ),
    Options#{
        module => LocalMacroModule,
        macro_module => Module,
        display_module => Module,
        diagnostic_mfa => #{function => Function, local => true},
        macro => Function,
        function => Function,
        arity => Arity
    }.

remove_declarations(Forms, State) ->
    Declarations = declarations(State),
    [
        Form
     || Form <- Forms,
        not maps:is_key(Form, Declarations)
    ].

finish_attribute_pass(Forms, CompileOpts, State) ->
    FormatterInfo = maps:get(formatter_info, State),
    do([
        return
     || WorkflowContext = #{
            source_view => Forms,
            compile_opts => CompileOpts,
            formatter_info => FormatterInfo
        },
        RetainRoots = retain_roots(Forms),
        {_FinalLocalEnv, FinalSkipIds, State1} <-
            finalize(RetainRoots, WorkflowContext, State),
        RetainWarnings = retain_warnings(Forms, State1),
        Forms1 = astranaut_macro_local_compile:remove_forms(Forms, FinalSkipIds),
        Canonical = maps:get(canonical_forms, State1),
        Forms2 = [maps:get(form_id(Form), Canonical, Form) || Form <- Forms1],
        return({Forms2, frozen_ids(State1), State1, RetainWarnings})
    ]).

retain_roots(Forms) ->
    lists:foldl(
        fun
            ({attribute, _Pos, local_macro_retain, Attr}, Acc) ->
                retain_fas(Attr, Acc);
            ({attribute, _Pos, export_macro, Attr}, Acc) ->
                retain_fas(Attr, Acc);
            ({attribute, _Pos, export, Attr}, Acc) ->
                retain_fas(Attr, Acc);
            (_Form, Acc) ->
                Acc
        end,
        [],
        Forms
    ).

retain_warnings(Forms, State) ->
    DefinedFAs = ordsets:from_list(
        maps:keys(function_clauses_map(Forms, #{}))
    ),
    lists:flatmap(
        fun
            ({attribute, Pos, local_macro_retain, Attr}) ->
                Roots = ordsets:from_list(retain_fas(Attr, [])),
                Undefined = ordsets:subtract(Roots, DefinedFAs),
                Existing = ordsets:subtract(Roots, Undefined),
                Nonclosure = nonclosure_retain_roots(Existing, State),
                retain_root_warnings(Pos, Undefined, Nonclosure);
            (_Form) ->
                []
        end,
        Forms
    ).

retain_root_warnings(Pos, Undefined, Nonclosure) ->
    [
        {Pos, ?MODULE, {undefined_local_macro_retain, Undefined}}
     || Undefined =/= []
    ] ++
        [
            {Pos, ?MODULE, {ineffective_local_macro_retain, Nonclosure}}
         || Nonclosure =/= []
        ].

retain_fas({FAs, _Options}, Acc) when is_list(FAs) ->
    retain_fas(FAs, Acc);
retain_fas(FAs, Acc) when is_list(FAs) ->
    [
        FA
     || FA = {Name, Arity} <- FAs,
        is_atom(Name),
        is_integer(Arity)
    ] ++ Acc;
retain_fas({Name, Arity} = FA, Acc) when
    is_atom(Name), is_integer(Arity)
->
    [FA | Acc];
retain_fas(_Other, Acc) ->
    Acc.

-spec new() -> state().
new() ->
    new(astranaut_macro_local_compile:module_name(astranaut_local_macro_test)).

-spec new(module()) -> state().
new(LocalMacroModule) ->
    #{
        local_macros => #{},
        compiler => astranaut_macro_local_compile:new(LocalMacroModule),
        frozen_forms => #{},
        canonical_forms => #{}
    }.

function_clauses_map(
    [{function, _Pos, Name, Arity, Clauses} | T], Acc
) ->
    function_clauses_map(
        T, maps:put({Name, Arity}, Clauses, Acc)
    );
function_clauses_map([_H | T], Acc) ->
    function_clauses_map(T, Acc);
function_clauses_map([], Acc) ->
    Acc.

-spec register([fa()], map(), [term()], macro_environment(), state()) ->
    {ok, state()} | {error, term()}.
register(
    FAs,
    Options,
    SourceView,
    #{
        macro_map := MacroMap,
        macro_options := _
    },
    State
) when
    is_list(FAs), is_map(Options)
->
    State1 = declaration_formatter_state(SourceView, State),
    Macros = maps:get(local_macros, State),
    case duplicate_or_existing(FAs, Macros) of
        none ->
            FormMap = forms_id_map(SourceView),
            case
                closures(
                    FAs,
                    Options,
                    FormMap,
                    MacroMap
                )
            of
                {ok, Closures, FunctionCallAnalysis} ->
                    Order = next_order(Macros),
                    FrozenForms = freeze_closures(Closures, FormMap),
                    RecordForms = record_forms(SourceView),
                    Bindings = maps:map(
                        fun(Id, Form) ->
                            Analysis = maps:get(Id, FunctionCallAnalysis, #{}),
                            #{
                                original_form => Form,
                                macro_map => MacroMap,
                                order => Order,
                                record_forms => RecordForms,
                                has_macro_call => maps:get(has_macro_call, Analysis, false)
                            }
                        end,
                        FrozenForms
                    ),
                    %% Existing bindings win, including helpers later declared as macros.
                    Frozen = maps:merge(Bindings, maps:get(frozen_forms, State)),
                    NewMacros =
                        lists:foldl(
                            fun(FA, Acc) ->
                                Closure = maps:get(FA, Closures),
                                maps:put(
                                    FA,
                                    #{
                                        order => Order,
                                        closure_ids =>
                                            closure_ids(Closure, FormMap),
                                        closure_fas => Closure,
                                        source_view => SourceView
                                    },
                                    Acc
                                )
                            end,
                            Macros,
                            FAs
                        ),
                    {ok, State1#{
                        local_macros => NewMacros,
                        frozen_forms => Frozen
                    }};
                {error, _} = Error ->
                    Error
            end;
        FA ->
            {error, {duplicate_local_macro_declaration, FA}}
    end.

-spec local_macro_fas(map()) -> ordsets:ordset(fa()).
local_macro_fas(MacroMap) ->
    ordsets:from_list(
        [
            {Function, Arity}
         || {_Key, #{
                macro_source := local_macro,
                function := Function,
                arity := Arity
            }} <-
                maps:to_list(MacroMap)
        ]
    ).

-spec need_callable(fa(), workflow_context(), state()) ->
    astranaut_return:struct(state()).
need_callable(FA, WorkflowContext, State) ->
    case compile_plan(FA, State) of
        {ok, Plan} -> execute_plan(Plan, WorkflowContext, State);
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

%% A plan is deliberately pure. execute_plan/3 expands each requested frozen
%% form in its first-bound environment before compiling a generation.
-spec compile_plan(fa() | all, state()) ->
    {ok, [compilation_boundary()]} | {error, term()}.
compile_plan(Needed, #{local_macros := Macros} = State) ->
    case needed_entries(Needed, Macros) of
        {error, _} = Error ->
            Error;
        Entries ->
            Ordered = lists:keysort(1, [
                {maps:get(order, Entry), FA, Entry}
             || {FA, Entry} <- Entries
            ]),
            {ok,
                compilation_boundaries(
                    Ordered, maps:get(frozen_forms, State)
                )}
    end.

%% The final boundary includes every declaration; canonical results are reused.
-spec finalize_plan(state()) ->
    {ok, [compilation_boundary()]} | {error, term()}.
finalize_plan(State) -> compile_plan(all, State).

%% Retain roots are supplied at the end because export/export_macro/retain can
%% occur after a declaration.  The scanner remains responsible for finding
%% those attributes in its complete output stream.
-spec finalize([fa()], state()) -> {map(), ordsets:ordset(form_id()), state()}.
finalize(RetainRoots, #{local_macros := Macros} = State) ->
    Retained = retained_ids(ordsets:from_list(RetainRoots), Macros),
    Skip = ordsets:subtract(
        astranaut_macro_local_compile:compiled_ids(maps:get(compiler, State)), Retained
    ),
    LocalEnv = maps:from_list([
        {FA, Entry}
     || {FA, Entry} <- maps:to_list(Macros),
        astranaut_macro_local_compile:is_callable(FA, maps:get(compiler, State))
    ]),
    {LocalEnv, Skip, State#{retained_form_ids => Retained}}.

-spec finalize([fa()], workflow_context(), state()) ->
    astranaut_return:struct({map(), ordsets:ordset(form_id()), state()}).
finalize(RetainRoots, WorkflowContext, State) ->
    case finalize_plan(State) of
        {error, Error} ->
            astranaut_return:error_fail(Error);
        {ok, Plan} ->
            do([
                return
             || State1 <- execute_plan(Plan, WorkflowContext, State),
                {FinalLocalEnv, FinalSkipIds, State2} = finalize(RetainRoots, State1),
                return({FinalLocalEnv, FinalSkipIds, State2})
            ])
    end.

-spec frozen_ids(state()) -> ordsets:ordset(form_id()).
frozen_ids(State) -> ordsets:from_list(maps:keys(maps:get(frozen_forms, State))).

-spec local_macros(state()) -> map().
local_macros(State) -> maps:get(local_macros, State).

-spec retained_form_ids(state()) -> ordsets:ordset(form_id()).
retained_form_ids(State) ->
    maps:get(retained_form_ids, State, ordsets:new()).

-spec nonclosure_retain_roots([fa()], state()) -> ordsets:ordset(fa()).
nonclosure_retain_roots(Roots, #{local_macros := Macros}) ->
    ClosureFAs = maps:fold(
        fun(_FA, Entry, Acc) ->
            ordsets:union(
                maps:get(closure_fas, Entry), Acc
            )
        end,
        ordsets:new(),
        Macros
    ),
    ordsets:subtract(ordsets:from_list(Roots), ClosureFAs).

%% The declaration source view is deliberately a two-part concatenation.  A
%% generated form becomes visible only after it has entered Queue; no future
%% splice result can leak into a declaration's closure analysis.
-spec source_view([term()], [term()]) -> [term()].
source_view(Passed, Queue) -> Passed ++ Queue.

-spec reject_locked_mutation([term()], state()) -> ok | {error, term()}.
reject_locked_mutation(Forms, State) ->
    Frozen = maps:get(frozen_forms, State),
    case
        [
            Form
         || Form <- Forms,
            begin
                Id = form_id(Form),
                Id =/= undefined andalso maps:is_key(Id, Frozen)
            end
        ]
    of
        [] -> ok;
        [Form | _] -> {error, {illegal_locked_form_mutation, Form}}
    end.

formatter_options(#{protocol := present}, LocalMacroModule, GlobalMacroOpts) ->
    GlobalMacroOpts#{formatter => LocalMacroModule};
formatter_options(
    #{protocol := missing},
    _LocalMacroModule,
    GlobalMacroOpts
) ->
    GlobalMacroOpts#{formatter => astranaut_macro}.

declaration_formatter_state(_SourceView, #{formatter_info := _} = State) ->
    State;
declaration_formatter_state(SourceView, State) ->
    ClauseMap = function_clauses_map(SourceView, #{}),
    State#{
        formatter_info =>
            formatter_info_for_source(ClauseMap, SourceView)
    }.

formatter_info_for_source(ClauseMap, SourceView) ->
    formatter_info_with_closure(local_formatter_info(ClauseMap), SourceView).

local_formatter_info(ClauseMap) ->
    Protocol = formatter_protocol(ClauseMap),
    FormatterFunctions =
        case Protocol of
            present -> [{format_error, 1}];
            missing -> []
        end,
    FormatterFAs = ordsets:from_list(FormatterFunctions),
    #{
        protocol => Protocol,
        roots => FormatterFAs,
        exports => FormatterFAs,
        related_fas => ordsets:new()
    }.

-spec formatter_protocol(#{term() => term()}) -> formatter_protocol().
formatter_protocol(ClauseMap) ->
    case maps:is_key({format_error, 1}, ClauseMap) of
        true -> present;
        false -> missing
    end.

formatter_info_with_closure(
    #{roots := FormatterRoots} = FormatterInfo, SourceView
) ->
    FormMap = forms_id_map(SourceView),
    {ok, FormatterRelated, _FunctionCallAnalysis} =
        closure(FormatterRoots, FormMap, #{}, #{}, ordsets:new()),
    FormatterInfo#{related_fas => FormatterRelated}.

%% Strategy A prepares every prefix before a later macro-bearing binding.
%% Committed generations are reused; canonical forms are never re-expanded.
-spec execute_plan([compilation_boundary()], workflow_context(), state()) ->
    astranaut_return:struct(state()).
execute_plan(Plan, Context, State) ->
    astranaut_return:foldl_m(
        fun(Boundary, Acc) ->
            execute_boundary(Boundary, Context, Acc)
        end,
        State,
        Plan
    ).

execute_boundary(#{members := Members, requests := Requests}, Context, State) ->
    Compiler = maps:get(compiler, State),
    case
        lists:all(fun(FA) -> astranaut_macro_local_compile:is_callable(FA, Compiler) end, Members)
    of
        true ->
            astranaut_return:return(State);
        false ->
            Ids = ordsets:union([maps:get(closure_ids, R) || R <- Requests]),
            Frozen = maps:get(frozen_forms, State),
            Ordered = lists:sort([{maps:get(order, maps:get(Id, Frozen)), Id} || Id <- Ids]),
            do([
                return
             || Prepared <- astranaut_return:foldl_m(
                    fun({_Order, Id}, Acc) -> prepare_form(Id, Acc) end, State, Ordered
                ),
                Compiler1 <- astranaut_macro_local_compile:ensure(
                    Members,
                    Requests,
                    maps:get(canonical_forms, Prepared),
                    #{
                        compile_opts => maps:get(compile_opts, Context),
                        formatter_info => maps:get(formatter_info, Prepared)
                    },
                    maps:get(compiler, Prepared)
                ),
                return(Prepared#{compiler => Compiler1})
            ])
    end.

prepare_form(Id, #{canonical_forms := Canonical, frozen_forms := Frozen} = State) ->
    case maps:is_key(Id, Canonical) of
        true ->
            astranaut_return:return(State);
        false ->
            #{
                original_form := Form,
                macro_map := MacroMap,
                has_macro_call := HasMacroCall,
                record_forms := Records
            } = maps:get(Id, Frozen),
            Expansion =
                case HasMacroCall of
                    false ->
                        astranaut_return:return(Form);
                    true ->
                        astranaut_return:lift_m(
                            fun(#{task_results := Results}) -> maps:get(Id, Results) end,
                            astranaut_macro_expander:expand_functions(
                                [Form | Records], #{
                                    Id => #{
                                        form => Form,
                                        macro_map => MacroMap,
                                        has_macro_call => true
                                    }
                                }
                            )
                        )
                end,
            astranaut_return:lift_m(
                fun(Expanded) ->
                    State#{canonical_forms => maps:put(Id, Expanded, Canonical)}
                end,
                Expansion
            )
    end.

record_forms(Forms) ->
    [Form || {attribute, _Pos, record, {_Name, _Fields}} = Form <- Forms].

duplicate_or_existing(FAs, Macros) ->
    case [FA || FA <- FAs, maps:is_key(FA, Macros)] ++ duplicate_fas(FAs) of
        [FA | _] -> FA;
        [] -> none
    end.

duplicate_fas(FAs) ->
    FAs -- ordsets:from_list(FAs).

next_order(Macros) ->
    lists:foldl(fun(#{order := Order}, Acc) -> max(Order, Acc) end, 0, maps:values(Macros)) + 1.

-spec forms_id_map([term()]) -> #{form_id() => term()}.
forms_id_map(Forms) ->
    lists:foldl(
        fun(Form, Acc) ->
            case form_id(Form) of
                undefined -> Acc;
                Id -> maps:put(Id, Form, Acc)
            end
        end,
        #{},
        Forms
    ).

form_id({function, _Pos, Name, Arity, _Clauses}) -> {function, Name, Arity};
form_id({attribute, _Pos, spec, {{Name, Arity}, _Body}}) -> {spec, Name, Arity};
form_id(_) -> undefined.

form_pos({attribute, Pos, _Name, _Value}) -> Pos;
form_pos({function, Pos, _Name, _Arity, _Clauses}) -> Pos;
form_pos(_Form) -> 0.

closures(FAs, Options, FormMap, MacroMap) ->
    ClosureRoots = maps:get(closure_roots, Options, []),
    case
        [
            FA
         || FA <- ClosureRoots,
            not maps:is_key(
                {function, element(1, FA), element(2, FA)}, FormMap
            )
        ]
    of
        [] ->
            closures_1(
                FAs, ClosureRoots, FormMap, MacroMap, #{}, #{}
            );
        Missing ->
            {error, {invalid_closure_roots, Missing}}
    end.

closures_1(
    [FA | T],
    Extra,
    FormMap,
    MacroMap,
    FunctionCallAnalysis,
    Acc
) ->
    {ok, Closure, FunctionCallAnalysis1} =
        closure(
            ordsets:from_list([FA | Extra]),
            FormMap,
            MacroMap,
            FunctionCallAnalysis,
            ordsets:new()
        ),
    closures_1(
        T,
        Extra,
        FormMap,
        MacroMap,
        FunctionCallAnalysis1,
        maps:put(FA, Closure, Acc)
    );
closures_1(
    [],
    _Extra,
    _FormMap,
    _MacroMap,
    FunctionCallAnalysis,
    Acc
) ->
    {ok, Acc, FunctionCallAnalysis}.

closure([], _FormMap, _MacroMap, FunctionCallAnalysis, Seen) ->
    {ok, Seen, FunctionCallAnalysis};
closure([FA | T], FormMap, MacroMap, FunctionCallAnalysis, Seen) ->
    case ordsets:is_element(FA, Seen) of
        true ->
            closure(T, FormMap, MacroMap, FunctionCallAnalysis, Seen);
        false ->
            FormId = {function, element(1, FA), element(2, FA)},
            case maps:find(FormId, FormMap) of
                error ->
                    closure(
                        T,
                        FormMap,
                        MacroMap,
                        FunctionCallAnalysis,
                        Seen
                    );
                {ok, Function} ->
                    {Analysis, FunctionCallAnalysis1} =
                        analyze_closure_function(
                            FormId,
                            Function,
                            MacroMap,
                            FunctionCallAnalysis
                        ),
                    LocalCalls = maps:get(local_calls, Analysis),
                    closure(
                        ordsets:union(LocalCalls, T),
                        FormMap,
                        MacroMap,
                        FunctionCallAnalysis1,
                        ordsets:add_element(FA, Seen)
                    )
            end
    end.

analyze_closure_function(
    FormId, Function, MacroMap, FunctionCallAnalysis
) ->
    case maps:find(FormId, FunctionCallAnalysis) of
        {ok, Analysis} ->
            {Analysis, FunctionCallAnalysis};
        error ->
            {function, _, _, _, Clauses} = Function,
            Analysis = astranaut:sreduce(
                fun(Node, Acc) ->
                    Calls =
                        case Node of
                            {call, _, {atom, _, Name}, Args} ->
                                ordsets:add_element(
                                    {Name, length(Args)}, maps:get(local_calls, Acc)
                                );
                            _ ->
                                maps:get(local_calls, Acc)
                        end,
                    HasMacro =
                        maps:get(has_macro_call, Acc) orelse
                            astranaut_macro_expander:match_macro_call(Node, MacroMap) =/= error,
                    Acc#{local_calls => Calls, has_macro_call => HasMacro}
                end,
                #{local_calls => [], has_macro_call => false},
                Clauses,
                #{traverse => pre}
            ),
            {Analysis, maps:put(FormId, Analysis, FunctionCallAnalysis)}
    end.

freeze_closures(Closures, FormMap) ->
    lists:foldl(
        fun(Closure, Acc) ->
            lists:foldl(fun(FA, A) -> freeze_fa(FA, FormMap, A) end, Acc, Closure)
        end,
        #{},
        maps:values(Closures)
    ).

freeze_fa({Name, Arity}, FormMap, Acc) ->
    FunctionId = {function, Name, Arity},
    SpecId = {spec, Name, Arity},
    Acc1 =
        case maps:find(FunctionId, FormMap) of
            {ok, FunctionForm} -> maps:put(FunctionId, FunctionForm, Acc);
            error -> Acc
        end,
    case maps:find(SpecId, FormMap) of
        {ok, SpecForm} -> maps:put(SpecId, SpecForm, Acc1);
        error -> Acc1
    end.

closure_ids(Closure, FormMap) ->
    lists:foldl(
        fun(FA, Acc) ->
            Ids = [
                {function, element(1, FA), element(2, FA)}, {spec, element(1, FA), element(2, FA)}
            ],
            ordsets:union([Id || Id <- Ids, maps:is_key(Id, FormMap)], Acc)
        end,
        ordsets:new(),
        Closure
    ).

needed_entries(all, Macros) ->
    maps:to_list(Macros);
needed_entries(FA, Macros) ->
    case maps:find(FA, Macros) of
        error ->
            {error, {undefined_local_macro, FA}};
        {ok, #{order := RequestedOrder}} ->
            %% A requested declaration is compiled in the cumulative prefix;
            %% dependency analysis decides only whether that prefix needs
            %% intermediate generations, never which declarations belong to
            %% the final generation.
            [
                {F, Entry}
             || {F, #{order := Order} = Entry} <- maps:to_list(Macros),
                Order =< RequestedOrder
            ]
    end.

compilation_boundaries(Ordered, Frozen) ->
    RequiredIds = ordsets:union([maps:get(closure_ids, Entry) || {_, _, Entry} <- Ordered]),
    Bindings = maps:values(maps:with(RequiredIds, Frozen)),
    PrefixOrders = [
        Order - 1
     || #{order := Order, has_macro_call := true, macro_map := Map} <- Bindings,
        local_macro_fas(Map) =/= []
    ],
    FinalOrder =
        case Ordered of
            [] -> 0;
            _ -> element(1, lists:last(Ordered))
        end,
    [
        #{
            members => [FA || {Order, FA, _} <- Ordered, Order =< Boundary],
            requests => [
                maps:with([closure_ids, closure_fas, source_view], Entry)
             || {Order, _, Entry} <- Ordered, Order =< Boundary
            ]
        }
     || Boundary <- ordsets:from_list(PrefixOrders ++ [FinalOrder]), Boundary > 0
    ].

retained_ids(Roots, Macros) ->
    lists:foldl(
        fun(Root, Acc) ->
            lists:foldl(
                fun({_FA, Entry}, A) ->
                    Closure = maps:get(closure_fas, Entry),
                    case ordsets:is_element(Root, Closure) of
                        true -> ordsets:union(maps:get(closure_ids, Entry), A);
                        false -> A
                    end
                end,
                Acc,
                maps:to_list(Macros)
            )
        end,
        ordsets:new(),
        Roots
    ).

local_error(Pos, Error) ->
    astranaut_return:with_error(
        fun(Errors) -> astranaut_error:update_pos(Pos, ?MODULE, Errors) end,
        astranaut_return:error_fail(Error)
    ).
