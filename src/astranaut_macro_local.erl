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
    handle_form/3,
    prepare_target/3,
    validate_generated/2,
    remove_declarations/2,
    finish_attribute_pass/3,
    prepare_function_pass/3,
    finish_function_pass/3,
    new/0, new/1,
    register/5,
    prepare_declaration/3,
    need_callable/3,
    compile_plan/2,
    cache_expanded/5,
    finalize/2,
    finalize/3,
    frozen_ids/1,
    local_macros/1,
    retained_form_ids/1,
    nonclosure_retain_roots/2,
    source_view/2,
    env_key/3,
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
-type local_macro_whitelist_control() ::
    astranaut_macro_expander:observation_control().
-type expansion_record() ::
    #{
        canonical_whitelist := disabled | ordsets:ordset(fa()),
        canonical_result := term(),
        results_by_input :=
            #{
                term() => #{
                    whitelist := disabled | ordsets:ordset(fa()),
                    result := term()
                }
            }
    }.
-type workflow_context() :: #{
    source_view := [term()],
    compile_opts := [compile:option()]
}.
-type expansion_request() ::
    #{
        closure_ids := ordsets:ordset(form_id()),
        closure_fas := ordsets:ordset(fa()),
        candidate_local_macros := ordsets:ordset(fa()),
        function_call_analysis := map(),
        macro_environment_snapshot := macro_environment(),
        source_view := [term()],
        forms := #{form_id() => term()}
    }.
-type compilation_boundary() ::
    #{
        members := [fa()],
        requests := [expansion_request()]
    }.
-type state() ::
    #{
        compiler := astranaut_macro_local_compile:state(),
        expansion_records := #{form_id() => expansion_record()},
        atom() => term()
    }.

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
        compile_opts := CompileOpts,
        macro_environment := MacroEnvironment
    } = Context,
    State
) ->
    FormatterDetected = maps:is_key(formatter_info, State),
    ClauseMap = function_clauses_map(SourceView, #{}),
    do([
        return
     || {FAs, Options} <-
            validate_local_macro_attribute(ClauseMap, Attr),
        State1 <- register_return(
            FAs,
            Options,
            SourceView,
            MacroEnvironment,
            State
        ),
        FormatterInfo = maps:get(formatter_info, State1),
        WorkflowContext = #{
            source_view => SourceView,
            compile_opts => CompileOpts,
            formatter_info => FormatterInfo
        },
        maybe_missing_formatter_warning(
            FormatterDetected, FormatterInfo, SourceModule
        ),
        State2 <- prepare_declaration(
            FAs,
            WorkflowContext,
            State1
        ),
        Definitions = local_macro_definitions(
            FAs, Options, FormatterInfo, State2, Context
        ),
        Declarations = maps:get(declarations, State2, #{}),
        return(
            {keep, Definitions, State2#{
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

register_return(FAs, Options, SourceView, MacroEnvironment, State) ->
    case register(FAs, Options, SourceView, MacroEnvironment, State) of
        {ok, State1} -> astranaut_return:return(State1);
        {error, Error} -> astranaut_return:error_fail(Error)
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
                        form_pos(Form), astranaut_macro, ErrorStruct
                    )
                end,
                astranaut_return:error_fail(Error)
            )
    end.

declarations(State) ->
    maps:get(declarations, State, #{}).

validate_local_macro_attribute(ClauseMap, Attr) ->
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
            Options, ClauseMap
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

validate_closure_roots_defined(Options, ClauseMap) ->
    Missing = [
        FA
     || FA <- maps:get(closure_roots, Options, []),
        not maps:is_key(FA, ClauseMap)
    ],
    case Missing of
        [] ->
            astranaut_return:return(Options);
        _ ->
            astranaut_return:error_fail(
                {invalid_closure_roots, Missing}
            )
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
        macro => Function,
        function => Function,
        arity => Arity,
        observation_id => {Function, Arity}
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
        RetainedFunctionIds = ordsets:from_list(
            [
                Id
             || Id = {function, _, _} <-
                    retained_form_ids(State1)
            ]
        ),
        return({Forms1, RetainedFunctionIds, State1, RetainWarnings})
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
        {Pos, astranaut_macro, {undefined_local_macro_retain, Undefined}}
     || Undefined =/= []
    ] ++
        [
            {Pos, astranaut_macro, {ineffective_local_macro_retain, Nonclosure}}
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
        expansion_records => #{},
        canonical_expanded_forms => #{}
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
    } = MacroEnvironment,
    State
) when
    is_list(FAs), is_map(Options)
->
    State1 = declaration_formatter_state(SourceView, State),
    Macros = maps:get(local_macros, State),
    case duplicate_or_existing(FAs, Macros) of
        none ->
            FormMap = forms_id_map(SourceView),
            CandidateFAs = local_macro_fas(MacroMap),
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
                    Frozen = maps:merge(
                        maps:get(frozen_forms, State),
                        freeze_closures(Closures, FormMap)
                    ),
                    NewMacros =
                        lists:foldl(
                            fun(FA, Acc) ->
                                Closure = maps:get(FA, Closures),
                                ClosureAnalysis =
                                    closure_function_analysis(
                                        Closure, FunctionCallAnalysis
                                    ),
                                Refs = referenced_local_macros(
                                    ClosureAnalysis
                                ),
                                maps:put(
                                    FA,
                                    #{
                                        order => Order,
                                        macro_environment_snapshot =>
                                            MacroEnvironment,
                                        closure_ids =>
                                            closure_ids(Closure, FormMap),
                                        closure_fas => Closure,
                                        candidate_local_macros => CandidateFAs,
                                        function_call_analysis =>
                                            ClosureAnalysis,
                                        referenced_local_macros => Refs,
                                        source_view => SourceView,
                                        options => Options
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

-spec keep_allowed_local_fas(map(), ordsets:ordset(fa())) -> map().
keep_allowed_local_fas(MacroMap, Allowed) ->
    maps:filter(
        fun
            (
                _Key,
                #{
                    macro_source := local_macro,
                    function := Function,
                    arity := Arity
                }
            ) ->
                ordsets:is_element({Function, Arity}, Allowed);
            (_Key, _Macro) ->
                true
        end,
        MacroMap
    ).

%% Registration freezes each declaration member with the same order and
%% runtime snapshot. Preparation eagerly validates
%% every form that is currently expandable, compiling only earlier local
%% dependencies that are genuinely needed to perform that expansion.
-spec prepare_declaration([fa()], workflow_context(), state()) ->
    astranaut_return:struct(state()).
prepare_declaration(FAs, WorkflowContext, State) ->
    Macros = maps:get(local_macros, State),
    Dependencies = lists:foldl(
        fun(FA, Acc) ->
            Entry = maps:get(FA, Macros),
            ordsets:union(
                maps:get(referenced_local_macros, Entry), Acc
            )
        end,
        ordsets:new(),
        FAs
    ),
    do([
        return
     || State1 <- astranaut_return:foldl_m(
            fun(Dependency, StateAcc) ->
                need_callable(
                    Dependency, WorkflowContext, StateAcc
                )
            end,
            State,
            Dependencies
        ),
        Requests = requests_for_fas(FAs, State1),
        State2 <- prepare_requests(
            Requests, WorkflowContext, State1
        ),
        return(State2)
    ]).

-spec need_callable(fa(), workflow_context(), state()) ->
    astranaut_return:struct(state()).
need_callable(FA, WorkflowContext, State) ->
    case compile_plan(FA, State) of
        {ok, Plan} -> execute_plan(Plan, WorkflowContext, State);
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

%% A plan is deliberately pure. execute_plan/3 expands each requested frozen
%% form, validates the cache, then commits it atomically.
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
                    Needed, Ordered, maps:get(frozen_forms, State)
                )}
    end.

%% The final generation is intentionally rebuilt from every declaration, not
%% merely pending ones.  Cached expansion results let the caller avoid work.
-spec finalize_plan(state()) ->
    {ok, [compilation_boundary()]} | {error, term()}.
finalize_plan(State) -> compile_plan(all, State).

-spec cache_expanded(
    form_id(),
    term(),
    disabled | ordsets:ordset(fa()),
    term(),
    state()
) -> {ok, state()} | {error, term()}.
cache_expanded(FormId, EnvKey, Whitelist, ExpandedForm, State) ->
    Records = maps:get(expansion_records, State, #{}),
    Canonical = maps:get(canonical_expanded_forms, State, #{}),
    Record0 = maps:get(FormId, Records, #{}),
    case validate_expansion(FormId, Whitelist, ExpandedForm, Record0) of
        ok ->
            Results0 = maps:get(results_by_input, Record0, #{}),
            Result = #{whitelist => Whitelist, result => ExpandedForm},
            Record = #{
                canonical_whitelist => Whitelist,
                canonical_result => ExpandedForm,
                results_by_input => maps:put(EnvKey, Result, Results0)
            },
            {ok, State#{
                expansion_records => maps:put(FormId, Record, Records),
                canonical_expanded_forms => maps:put(FormId, ExpandedForm, Canonical)
            }};
        {error, _} = Error ->
            Error
    end.

validate_expansion(
    FormId,
    Whitelist,
    _ExpandedForm,
    #{canonical_whitelist := Expected}
) when
    Expected =/= Whitelist
->
    {error, whitelist_conflict(FormId, Expected, Whitelist)};
validate_expansion(FormId, _Whitelist, ExpandedForm, #{canonical_result := Expected}) when
    Expected =/= ExpandedForm
->
    {error, {conflicting_local_macro_closure_environment, FormId}};
validate_expansion(_FormId, _Whitelist, _ExpandedForm, _Record) ->
    ok.

whitelist_conflict(FormId, Expected, Observed) ->
    {conflicting_local_macro_whitelist, FormId, #{
        expected => Expected,
        observed => Observed,
        unexpected => ordsets:subtract(Observed, Expected),
        missing => ordsets:subtract(Expected, Observed)
    }}.

-spec expanded_form(form_id(), term(), state()) ->
    {ok, #{
        whitelist := disabled | ordsets:ordset(fa()),
        result := term()
    }}
    | error.
expanded_form(FormId, EnvKey, State) ->
    case maps:find(FormId, maps:get(expansion_records, State, #{})) of
        {ok, Record} ->
            maps:find(EnvKey, maps:get(results_by_input, Record, #{}));
        error ->
            error
    end.

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
                %% Retain is now only a lifecycle/selection policy.  Retain
                %% and ordinary functions are expanded together later with
                %% the single final macro environment.
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

%% Only frozen targets need provider policy. Ordinary tasks remain core-owned.
prepare_function_pass(Tasks, FunctionEnv, State) ->
    Frozen = maps:get(frozen_forms, State),
    TargetIds = lists:sort([
        Id
     || Id = {function, _, _} <- maps:keys(maps:with(maps:keys(Frozen), Tasks))
    ]),
    {Overrides, PendingIds} = lists:foldl(
        fun(FormId, Acc) ->
            prepare_final_function_task(
                FormId, maps:get(FormId, Frozen), FunctionEnv, State, Acc
            )
        end,
        {#{}, []},
        TargetIds
    ),
    {Overrides, lists:reverse(PendingIds)}.

%% Each pending FormId occurs once. Final results need validation only;
%% the expander owns output forms and no later phase consumes a new cache.
finish_function_pass(Results, PendingIds, State) ->
    Records = maps:get(expansion_records, State, #{}),
    astranaut_return:foldl_m(
        fun(FormId, ok) ->
            case maps:find(FormId, Results) of
                error ->
                    astranaut_return:return(ok);
                {ok, #{form := Form, observed_macro_ids := Whitelist}} ->
                    case
                        validate_expansion(
                            FormId, Whitelist, Form, maps:get(FormId, Records, #{})
                        )
                    of
                        ok -> astranaut_return:return(ok);
                        {error, Error} -> astranaut_return:error_fail(Error)
                    end
            end
        end,
        ok,
        PendingIds
    ).

prepare_final_function_task(
    FormId,
    OriginalForm,
    #{
        macro_environment := #{macro_map := MacroMap, macro_options := MacroOptions},
        function_call_analysis := Analysis
    },
    State,
    {Tasks, PendingIds}
) ->
    WhitelistControl = whitelist_control(FormId, State),
    AllowedLocalFAs = allowed_local_fas(
        WhitelistControl, local_macro_fas(MacroMap)
    ),
    EffectiveMacroMap = keep_allowed_local_fas(MacroMap, AllowedLocalFAs),
    EnvKey = env_key(
        EffectiveMacroMap,
        astranaut_macro_local_compile:versions(AllowedLocalFAs, maps:get(compiler, State)),
        MacroOptions
    ),
    case expanded_form(FormId, EnvKey, State) of
        {ok, #{result := ExpandedForm}} ->
            Task = #{
                form => ExpandedForm,
                macro_map => #{},
                observation_control => disabled,
                has_macro_call => false
            },
            {maps:put(FormId, Task, Tasks), PendingIds};
        error ->
            Task = add_macro_call_hint(
                FormId,
                OriginalForm,
                Analysis,
                #{
                    form => OriginalForm,
                    macro_map => EffectiveMacroMap,
                    observation_control => WhitelistControl
                }
            ),
            {maps:put(FormId, Task, Tasks), [FormId | PendingIds]}
    end.

allowed_local_fas(#{mode := verify, expected := Expected}, _Candidates) ->
    Expected;
allowed_local_fas(_Control, Candidates) ->
    Candidates.

%% The declaration source view is deliberately a two-part concatenation.  A
%% generated form becomes visible only after it has entered Queue; no future
%% splice result can leak into a declaration's closure analysis.
-spec source_view([term()], [term()]) -> [term()].
source_view(Passed, Queue) -> Passed ++ Queue.

%% The resolved macro map already contains injected attribute values, so all
%% observable environment inputs enter the structural key without serializing
%% the environment or retaining source Forms.
-spec env_key(map(), term(), term()) -> {map(), term(), term()}.
env_key(MacroMap, LocalVersions, Options) ->
    {MacroMap, LocalVersions, Options}.

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

%% Execute pure plan data in two explicit stages. ExpansionValidator prepares
%% canonical forms first; GenerationCompiler then consumes only those forms.
-spec execute_plan([compilation_boundary()], workflow_context(), state()) ->
    astranaut_return:struct(state()).
execute_plan([], _WorkflowContext, State) ->
    astranaut_return:return(State);
execute_plan([Boundary | Rest], WorkflowContext, State) ->
    Members = maps:get(members, Boundary),
    case Members of
        [] ->
            execute_plan(Rest, WorkflowContext, State);
        _ ->
            do([
                return
             || PreparedState <- prepare_requests(
                    maps:get(requests, Boundary),
                    WorkflowContext,
                    State
                ),
                Compiler1 <- astranaut_macro_local_compile:ensure(
                    Members,
                    [
                        maps:with([closure_ids, closure_fas, source_view], Request)
                     || Request <- maps:get(requests, Boundary)
                    ],
                    maps:get(canonical_expanded_forms, PreparedState),
                    #{
                        compile_opts => maps:get(compile_opts, WorkflowContext),
                        formatter_info => maps:get(formatter_info, PreparedState)
                    },
                    maps:get(compiler, PreparedState)
                ),
                execute_plan(Rest, WorkflowContext, PreparedState#{compiler => Compiler1})
            ])
    end.

-spec prepare_requests([expansion_request()], workflow_context(), state()) ->
    astranaut_return:struct(state()).
prepare_requests(Requests, WorkflowContext, State) ->
    astranaut_return:foldl_m(
        fun(Request, StateAcc) ->
            prepare_request(Request, WorkflowContext, StateAcc)
        end,
        State,
        Requests
    ).

-spec prepare_request(expansion_request(), workflow_context(), state()) ->
    astranaut_return:struct(state()).
prepare_request(#{forms := FrozenForms} = Request, WorkflowContext, State) ->
    RecordForms = record_forms(maps:get(source_view, Request)),
    astranaut_return:foldl_m(
        fun(FormId, StateAcc) ->
            do([
                return
             || {_ExpandedForm, State1} <-
                    prepare_request_form(
                        FormId,
                        RecordForms,
                        Request,
                        WorkflowContext,
                        StateAcc
                    ),
                return(State1)
            ])
        end,
        State,
        maps:keys(FrozenForms)
    ).

-spec prepare_request_form(
    form_id(),
    [term()],
    expansion_request(),
    workflow_context(),
    state()
) ->
    astranaut_return:struct({term(), state()}).
prepare_request_form(
    {spec, _, _} = FormId, _RecordForms, #{forms := FrozenForms}, _WorkflowContext, State
) ->
    cache_form_result(FormId, spec, disabled, maps:get(FormId, FrozenForms), State);
prepare_request_form(
    FormId,
    RecordForms,
    #{
        candidate_local_macros := Candidates,
        macro_environment_snapshot :=
            #{
                macro_map := SnapshotMacroMap,
                macro_options := MacroOptions
            }
    } = Request,
    WorkflowContext,
    State
) ->
    WhitelistControl = whitelist_control(FormId, State),
    LocalVersions = astranaut_macro_local_compile:versions(
        allowed_local_fas(
            WhitelistControl, Candidates
        ),
        maps:get(compiler, State)
    ),
    EffectiveMacroMap0 = keep_allowed_local_fas(
        SnapshotMacroMap, Candidates
    ),
    EffectiveMacroMap = mark_local_macro_callable(
        EffectiveMacroMap0, State
    ),
    EnvKey = env_key(
        EffectiveMacroMap0,
        LocalVersions,
        MacroOptions
    ),
    case expanded_form(FormId, EnvKey, State) of
        {ok, #{result := Form}} ->
            astranaut_return:return({Form, State});
        error ->
            expand_and_cache_form(
                FormId,
                EffectiveMacroMap,
                EnvKey,
                WhitelistControl,
                RecordForms,
                Request,
                WorkflowContext,
                State
            )
    end.

whitelist_control(FormId, State) ->
    case maps:find(FormId, maps:get(expansion_records, State, #{})) of
        {ok, #{canonical_whitelist := Expected}} when Expected =/= disabled ->
            #{
                mode => verify,
                form_id => FormId,
                expected => Expected,
                conflict_tag => conflicting_local_macro_whitelist
            };
        _ ->
            #{
                mode => collect,
                form_id => FormId,
                conflict_tag => conflicting_local_macro_whitelist
            }
    end.

-spec expand_and_cache_form(
    form_id(),
    map(),
    {map(), term(), term()},
    local_macro_whitelist_control(),
    [term()],
    expansion_request(),
    workflow_context(),
    state()
) ->
    astranaut_return:struct({term(), state()}).
expand_and_cache_form(
    FormId,
    EffectiveMacroMap,
    EnvKey,
    WhitelistControl,
    RecordForms,
    #{
        forms := FrozenForms,
        function_call_analysis := FunctionCallAnalysis
    } = Request,
    WorkflowContext,
    State
) ->
    OriginalForm = maps:get(FormId, FrozenForms),
    Task0 = #{
        form => OriginalForm,
        macro_map => EffectiveMacroMap,
        observation_control => WhitelistControl
    },
    Task = add_macro_call_hint(
        FormId,
        OriginalForm,
        FunctionCallAnalysis,
        Task0
    ),
    Tasks = #{FormId => Task},
    do([
        return
     || #{task_results := #{FormId := TaskResult}} <-
            astranaut_macro_expander:expand_functions(
                [OriginalForm | RecordForms], Tasks
            ),
        finish_or_schedule_expansion(
            TaskResult,
            FormId,
            EnvKey,
            RecordForms,
            Request,
            WorkflowContext,
            State
        )
    ]).

add_macro_call_hint(FormId, OriginalForm, FunctionCallAnalysis, Task) ->
    case maps:find(FormId, FunctionCallAnalysis) of
        {ok, #{
            form := OriginalForm,
            has_macro_call := HasMacroCall
        }} ->
            Task#{has_macro_call => HasMacroCall};
        _ ->
            Task
    end.

finish_or_schedule_expansion(
    #{needed_macro_ids := [_ | _] = Needed},
    FormId,
    _EnvKey,
    RecordForms,
    Request,
    WorkflowContext,
    State
) ->
    do([
        return
     || State1 <- astranaut_return:foldl_m(
            fun(Dependency, StateAcc) ->
                need_callable(
                    Dependency,
                    WorkflowContext,
                    StateAcc
                )
            end,
            State,
            Needed
        ),
        prepare_request_form(
            FormId, RecordForms, Request, WorkflowContext, State1
        )
    ]);
finish_or_schedule_expansion(
    #{
        form := ExpandedForm,
        observed_macro_ids := Whitelist
    },
    FormId,
    EnvKey,
    _RecordForms,
    _Request,
    _WorkflowContext,
    State
) ->
    cache_form_result(
        FormId, EnvKey, Whitelist, ExpandedForm, State
    ).

record_forms(Forms) ->
    [Form || {attribute, _Pos, record, {_Name, _Fields}} = Form <- Forms].

mark_local_macro_callable(MacroMap, State) ->
    Compiler = maps:get(compiler, State),
    maps:map(
        fun
            (
                _Key,
                #{
                    macro_source := local_macro,
                    function := Function,
                    arity := Arity
                } = Macro
            ) ->
                Callable = astranaut_macro_local_compile:is_callable(
                    {Function, Arity}, Compiler
                ),
                Macro#{callable => Callable};
            (_Key, Macro) ->
                Macro
        end,
        MacroMap
    ).

-spec cache_form_result(
    form_id(),
    term(),
    disabled | ordsets:ordset(fa()),
    term(),
    state()
) ->
    astranaut_return:struct({term(), state()}).
cache_form_result(FormId, EnvKey, Whitelist, Form, State) ->
    case cache_expanded(FormId, EnvKey, Whitelist, Form, State) of
        {ok, State1} -> astranaut_return:return({Form, State1});
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

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
            AnalysisMap =
                astranaut_macro_expander:function_call_analysis(
                    [Function], MacroMap
                ),
            Analysis = maps:get(FormId, AnalysisMap),
            {Analysis, maps:put(FormId, Analysis, FunctionCallAnalysis)}
    end.

closure_function_analysis(Closure, FunctionCallAnalysis) ->
    FunctionIds = [
        {function, Name, Arity}
     || {Name, Arity} <- Closure
    ],
    maps:with(FunctionIds, FunctionCallAnalysis).

referenced_local_macros(FunctionCallAnalysis) ->
    maps:fold(
        fun(_FormId, Analysis, Acc) ->
            ordsets:union(
                maps:get(observed_macro_calls, Analysis), Acc
            )
        end,
        ordsets:new(),
        FunctionCallAnalysis
    ).

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

-spec compilation_boundaries(
    fa() | all,
    [{non_neg_integer(), fa(), map()}],
    #{form_id() => term()}
) ->
    [compilation_boundary()].
compilation_boundaries(Needed, Ordered, Frozen) ->
    Index = compilation_plan_index(Ordered),
    Prefix = [FA || {_Order, FA, _Entry} <- Ordered],
    DependencyRoots =
        case Needed of
            all -> Prefix;
            FA -> [FA]
        end,
    {Dependencies, _Memo} = dependency_boundaries_for_fas(
        DependencyRoots, Index, #{}
    ),
    Boundaries = ordsets:to_list(ordsets:from_list(Dependencies ++ [Prefix])),
    %% ordsets sorts lists lexically, not declaration order; restore the plan
    %% order by the final member's declaration index.
    Sorted = lists:sort(
        fun(A, B) ->
            boundary_order(A, Index) =< boundary_order(B, Index)
        end,
        Boundaries
    ),
    [plan_boundary(Members, Index, Frozen) || Members <- Sorted].

compilation_plan_index(Ordered) ->
    #{
        entries => maps:from_list(
            [{FA, Entry} || {_Order, FA, Entry} <- Ordered]
        ),
        orders => maps:from_list(
            [{FA, Order} || {Order, FA, _Entry} <- Ordered]
        ),
        prefixes => maps:from_list(
            [
                {FA, [
                    PrefixFA
                 || {PrefixOrder, PrefixFA, _PrefixEntry} <- Ordered,
                    PrefixOrder =< Order
                ]}
             || {Order, FA, _OuterEntry} <- Ordered
            ]
        )
    }.

dependency_boundaries_for_fas(FAs, Index, Memo) ->
    {Reversed, Memo1} =
        lists:foldl(
            fun(FA, {BoundariesAcc, MemoAcc}) ->
                {Boundaries, MemoNext} = dependency_boundaries(
                    FA, Index, MemoAcc
                ),
                {[Boundaries | BoundariesAcc], MemoNext}
            end,
            {[], Memo},
            FAs
        ),
    {lists:append(lists:reverse(Reversed)), Memo1}.

dependency_boundaries(
    FA,
    #{
        entries := Entries,
        prefixes := Prefixes
    } = Index,
    Memo
) ->
    case maps:find(FA, Memo) of
        {ok, Boundaries} ->
            {Boundaries, Memo};
        error ->
            Entry = maps:get(FA, Entries),
            Dependencies = maps:get(referenced_local_macros, Entry, []),
            {ReversedChunks, Memo1} =
                lists:foldl(
                    fun(Dep, {ChunksAcc, MemoAcc}) ->
                        {Nested, MemoNext} = dependency_boundaries(
                            Dep, Index, MemoAcc
                        ),
                        Chunk = Nested ++ [maps:get(Dep, Prefixes)],
                        {[Chunk | ChunksAcc], MemoNext}
                    end,
                    {[], Memo},
                    Dependencies
                ),
            Boundaries = lists:append(lists:reverse(ReversedChunks)),
            {Boundaries, maps:put(FA, Boundaries, Memo1)}
    end.

boundary_order(Members, #{orders := Orders}) ->
    maps:get(lists:last(Members), Orders).

-spec plan_boundary(
    [fa()],
    map(),
    #{form_id() => term()}
) -> compilation_boundary().
plan_boundary(Members, #{entries := EntryIndex}, Frozen) ->
    Entries = [{FA, maps:get(FA, EntryIndex)} || FA <- Members],
    #{
        members => Members,
        requests => [
            request_for_entry(Entry, Frozen)
         || {_FA, Entry} <- Entries
        ]
    }.

-spec requests_for_fas([fa()], state()) -> [expansion_request()].
requests_for_fas(FAs, State) ->
    Macros = maps:get(local_macros, State),
    Frozen = maps:get(frozen_forms, State),
    [request_for_entry(maps:get(FA, Macros), Frozen) || FA <- FAs].

-spec request_for_entry(map(), #{form_id() => term()}) ->
    expansion_request().
request_for_entry(Entry, Frozen) ->
    #{
        closure_ids => maps:get(closure_ids, Entry),
        closure_fas => maps:get(closure_fas, Entry),
        candidate_local_macros => maps:get(candidate_local_macros, Entry),
        function_call_analysis =>
            maps:get(function_call_analysis, Entry),
        macro_environment_snapshot =>
            maps:get(macro_environment_snapshot, Entry),
        source_view => maps:get(source_view, Entry),
        forms => maps:with(maps:get(closure_ids, Entry), Frozen)
    }.

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
