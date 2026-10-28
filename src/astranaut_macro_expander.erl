%%%-------------------------------------------------------------------
%%% @doc Shared macro matching, invocation, and recursive function expansion.
%%%
%%% This module owns expansion-local traversal state. It does not own the
%%% source-ordered attribute queue, macro-environment updates, or local-macro
%%% generation lifecycle.
%%%-------------------------------------------------------------------
-module(astranaut_macro_expander).

-include("do.hrl").

-export([
    expand_functions/2,
    function_call_analysis/2,
    match_macro_call/2,
    attribute_macro_index/1,
    resolve_attribute_target/3,
    expand_attribute_target/1,
    function_macro_callers/1,
    function_macro_callers/2,
    format_mfa/1
]).
-type form_id() :: {function, atom(), non_neg_integer()}.
-type macro_map() :: map().
-type function_expansion_task() :: #{
    form := term(), macro_map := macro_map(), has_macro_call => boolean()
}.
-type function_call_analysis() :: #{form := term(), has_macro_call := boolean()}.
-type function_task_expansion() :: #{
    forms := [term()], task_results := #{form_id() => term()}
}.

%%%===================================================================
%%% Public shared expansion operations
%%%===================================================================

%% Expand every target during one source-ordered Forms traversal.  Each task
%% carries its own source form and execution environment; traversal state is
%% scoped per function so rename counters cannot leak between
%% targets.
-spec expand_functions(
    [term()], #{form_id() => function_expansion_task()}
) ->
    astranaut_return:struct(function_task_expansion()).
expand_functions(Forms, Tasks) ->
    RecordForms = record_forms(Forms),
    FunctionClausesUniplate = function_clauses_uniplate(),
    Monad =
        astranaut:map_m(
            fun
                ({function, _Pos, Name, Arity, _Clauses} = Function) ->
                    FormId = {function, Name, Arity},
                    case maps:find(FormId, Tasks) of
                        error ->
                            astranaut_traverse:return(Function);
                        {ok, Task} ->
                            expand_function_task(
                                FormId,
                                Task,
                                RecordForms,
                                FunctionClausesUniplate
                            )
                    end;
                (Form) ->
                    astranaut_traverse:return(Form)
            end,
            Forms,
            #{traverse => none}
        ),
    astranaut_return:lift_m(
        fun({Forms1, TaskResults}) ->
            #{forms => Forms1, task_results => TaskResults}
        end,
        astranaut_traverse:run(
            astranaut_traverse:fail_on_error(Monad),
            astranaut_macro,
            #{},
            #{}
        )
    ).

-spec attribute_macro_index(macro_map()) -> map().
attribute_macro_index(MacroMap) ->
    maps:fold(
        fun
            (
                _MacroKey,
                #{
                    as_attr := Attribute,
                    call_arity := Arity
                } = Macro,
                Acc
            ) ->
                AttributeMacros = maps:get(Attribute, Acc, #{}),
                maps:put(
                    Attribute,
                    maps:put({Attribute, Arity}, Macro, AttributeMacros),
                    Acc
                );
            (_MacroKey, _Macro, Acc) ->
                Acc
        end,
        #{},
        MacroMap
    ).

-spec function_call_analysis([term()], macro_map()) ->
    #{form_id() => function_call_analysis()}.
function_call_analysis(Forms, MacroMap) ->
    maps:from_list([
        {{function, Name, Arity}, #{
            form => Form, has_macro_call => has_macro_call(Clauses, MacroMap)
        }}
     || {function, _, Name, Arity, Clauses} = Form <- Forms
    ]).

-spec resolve_attribute_target(term(), macro_map(), map()) ->
    {ok, map()} | error | not_macro.
resolve_attribute_target(
    {attribute, Pos, exec_macro, {Function, Arguments}},
    Macros,
    _AttributeIndex
) ->
    resolve_macro_target(
        Function,
        Arguments,
        Pos,
        Macros,
        {attribute, Pos, exec_macro, {Function, Arguments}}
    );
resolve_attribute_target(
    {attribute, Pos, exec_macro, {Module, Function, Arguments}},
    Macros,
    _AttributeIndex
) ->
    resolve_macro_target(
        {Module, Function},
        Arguments,
        Pos,
        Macros,
        {attribute, Pos, exec_macro, {Module, Function, Arguments}}
    );
resolve_attribute_target(
    {attribute, Pos, Attribute, Arguments},
    _Macros,
    AttributeIndex
) ->
    resolve_attribute_target_by_name(
        Attribute,
        Arguments,
        Pos,
        AttributeIndex,
        {attribute, Pos, Attribute, Arguments}
    );
resolve_attribute_target(_Node, _MacroMap, _AttributeIndex) ->
    not_macro.

-spec expand_attribute_target(map()) ->
    astranaut_traverse:struct(term(), term()).
expand_attribute_target(Target) ->
    Invocation = build_macro_invocation(Target),
    expand_macro(Invocation, #{expected_role => form}).

-spec function_macro_callers([term()], macro_map()) ->
    ordsets:ordset({function, atom(), non_neg_integer()}).
function_macro_callers(Forms, MacroMap) ->
    function_macro_callers(function_call_analysis(Forms, MacroMap)).

-spec function_macro_callers(#{form_id() => function_call_analysis()}) ->
    ordsets:ordset({function, atom(), non_neg_integer()}).
function_macro_callers(FunctionAnalysis) ->
    maps:fold(
        fun
            (FormId, #{has_macro_call := true}, Acc) ->
                ordsets:add_element(FormId, Acc);
            (_FormId, _Analysis, Acc) ->
                Acc
        end,
        ordsets:new(),
        FunctionAnalysis
    ).

-spec format_mfa(map()) -> iolist().
format_mfa(#{function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(#{module := Module, function := Function, arity := Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).

%%%===================================================================
%%% Function expansion
%%%===================================================================

expand_function_task(
    FormId,
    #{form := Function, macro_map := MacroMap} = Task,
    RecordForms,
    FunctionClausesUniplate
) ->
    Expand = do([
        traverse
     || Function1 <- astranaut_traverse:scoped_state(
            0,
            transform_task_function_if_needed(
                Function,
                MacroMap,
                RecordForms,
                FunctionClausesUniplate,
                maps:find(has_macro_call, Task)
            )
        ),
        astranaut_traverse:modify(fun(Results) -> maps:put(FormId, Function1, Results) end),
        return(Function1)
    ]),
    astranaut_traverse:catch_on_error(
        Expand, fun() -> astranaut_traverse:return(Function) end
    ).

transform_task_function_if_needed(
    {function, _Pos, _Name, _Arity, Clauses} = Function,
    MacroMap,
    RecordForms,
    FunctionClausesUniplate,
    MacroCallHint
) ->
    case function_has_macro_call(Clauses, MacroMap, MacroCallHint) of
        false ->
            astranaut_traverse:return(Function);
        true ->
            astranaut:map_m(
                fun(Clause) ->
                    transform_clause(
                        MacroMap,
                        Clause,
                        RecordForms
                    )
                end,
                Function,
                #{
                    traverse => subtree,
                    uniplate => FunctionClausesUniplate
                }
            )
    end.

function_has_macro_call(_Clauses, MacroMap, _Hint) when
    map_size(MacroMap) =:= 0
->
    false;
function_has_macro_call(_Clauses, _MacroMap, {ok, HasMacroCall}) ->
    HasMacroCall;
function_has_macro_call(Clauses, MacroMap, error) ->
    has_macro_call(Clauses, MacroMap).

function_clauses_uniplate() ->
    fun
        ({function, Pos, Name, Arity, Clauses}) ->
            {[Clauses], fun([NewClauses]) ->
                {function, Pos, Name, Arity, NewClauses}
            end};
        (Node) ->
            {[[]], fun(_) -> Node end}
    end.

transform_clause(
    MacroMap,
    {clause, Pos, Patterns, Guards, Exprs},
    RecordForms
) ->
    do([
        traverse
     || astranaut_traverse:put(1),
        Guards1 <- transform_guard_sequences(
            MacroMap,
            Guards,
            #{
                depth => 0,
                expected_role => guard,
                forms => RecordForms
            }
        ),
        Exprs1 <- transform_exprs(
            MacroMap,
            Exprs,
            #{
                depth => 0,
                expected_role => expression,
                forms => RecordForms
            }
        ),
        return({clause, Pos, Patterns, Guards1, Exprs1})
    ]).

transform_guard_sequences(MacroMap, Guards, DepthOpts) ->
    %% Erlang clauses store guards as a list of guard sequences.  Transform
    %% each sequence independently so conjunctions and disjunctions retain
    %% their original nesting and every element is visited as an AST node.
    astranaut_traverse:map_m(
        fun(GuardSequence) ->
            transform_exprs(
                MacroMap, GuardSequence, DepthOpts
            )
        end,
        Guards
    ).

record_forms(Forms) ->
    [Form || {attribute, _Anno, record, {_Name, _Fields}} = Form <- Forms].

transform_exprs(MacroMap, Exprs, DepthOpts) ->
    ExpectedRole = maps:get(expected_role, DepthOpts, expression),
    InitAttr = #{node => ExpectedRole, validator => {role, ExpectedRole}},
    Monad = astranaut:map_m(
        fun(Node) ->
            do([
                traverse
             || Attr = #{step := Step} <- astranaut_traverse:ask(),
                DepthOpts1 = DepthOpts#{
                    rename_quoted_variables => true, step => Step, attr => Attr
                },
                case match_macro_call(Node, MacroMap, Step) of
                    {ok, Macro} -> expand_macro_recursive(MacroMap, Macro, DepthOpts1);
                    error -> return(Node)
                end
            ])
        end,
        Exprs,
        #{traverse => all, normalize => false}
    ),
    astranaut_traverse:local(fun(_) -> InitAttr end, Monad).

%%%===================================================================
%%% Macro matching, invocation, and returned AST processing
%%%===================================================================

match_macro_call(Node, Macros, Step) ->
    case match_macro_call(Node, Macros) of
        {ok, Macro} ->
            case match_macro_order(Macro, Step) of
                true -> {ok, Macro};
                false -> error
            end;
        error ->
            error
    end.

match_macro_call(
    {call, Pos1, {atom, _Pos2, Function}, Arguments} = Node,
    Macros
) ->
    find_macro_with_arguments(Function, Arguments, Pos1, Macros, Node);
match_macro_call(
    {call, Pos1, {remote, _Pos2, {atom, _Pos3, RemoteModule}, {atom, _Pos4, Function}}, Arguments} =
        Node,
    Macros
) ->
    find_macro_with_arguments(
        {RemoteModule, Function}, Arguments, Pos1, Macros, Node
    );
match_macro_call(_Node, _Macros) ->
    error.

match_macro_order(Macro, Step) ->
    Order = maps:get(order, Macro, inner),
    ((Order =:= inner) and (Step =:= post)) or
        ((Order =:= outer) and (Step =:= pre)).

expand_macro_recursive(
    _MacroMap,
    #{max_depth := MaxDepth} = Macro,
    #{depth := Depth} = DepthOpts
) when
    Depth >= MaxDepth
->
    CurrentMacro = maps:get(origin_macro, DepthOpts, Macro),
    recover_macro_call(
        Macro,
        astranaut_traverse:fail(
            {max_macro_expansion_depth_exceeded, maps:get(macro, CurrentMacro),
                maps:get(arguments, CurrentMacro, [])}
        )
    );
expand_macro_recursive(
    MacroMap,
    Macro,
    #{step := post} = DepthOpts
) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    expand_macro_with(
        Macro,
        DepthOpts1#{macro_map => MacroMap},
        fun(Node1) ->
            transform_exprs(MacroMap, Node1, DepthOpts1)
        end
    );
expand_macro_recursive(
    MacroMap,
    Macro,
    #{step := pre} = DepthOpts
) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    expand_macro(
        Macro, DepthOpts1#{macro_map => MacroMap}
    ).

update_depth_opts(Macro, #{depth := Depth} = Opts) ->
    Opts1 = update_macro_context(Macro, Opts),
    Opts1#{depth => Depth + 1}.

update_macro_context(Macro, #{depth := 0} = Opts) ->
    Opts#{origin_macro => Macro, current_macro => Macro};
update_macro_context(Macro, Opts) ->
    Opts#{current_macro => Macro}.

expand_macro(Macro, Opts) ->
    expand_macro_with(Macro, Opts, fun astranaut_traverse:return/1).

expand_macro_with(
    #{pos := Pos, formatter := Formatter} = Macro, Opts, Success
) ->
    recover_macro_call(
        Macro,
        do([
            traverse
         || Node <- astranaut_traverse:update_pos(
                Pos, Formatter, invoke_macro_function(Macro)
            ),
            {Node1, ReturnAnalysis} <-
                process_macro_return(Node, Macro, Opts),
            format_node(Node1, Macro),
            case ReturnAnalysis of
                true -> Success(Node1);
                false -> return(Node1)
            end
        ])
    ).

recover_macro_call(Macro, Monad) ->
    astranaut_traverse:catch_on_error(
        Monad,
        fun() ->
            astranaut_traverse:return(maps:get(call_ast, Macro))
        end
    ).

invoke_macro_function(
    #{
        pos := Pos,
        module := Module,
        function := Function,
        arguments := Arguments
    } = Macro
) ->
    try erlang:apply(Module, Function, Arguments) of
        Return ->
            astranaut_traverse:scoped_state(
                ok, astranaut:traverse_return(Return)
            )
    catch
        Class:Exception:Stacktrace ->
            StackTraces1 =
                lists:takewhile(
                    fun
                        ({M, F, A, _Pos}) ->
                            {M, F, A} =/=
                                {?MODULE, invoke_macro_function, 1};
                        (_Stack) ->
                            false
                    end,
                    Stacktrace
                ),
            Error = macro_exception(
                Arguments, Class, Exception, StackTraces1, Macro
            ),
            astranaut_traverse:update_pos(
                Pos, astranaut_macro, astranaut_traverse:fail(Error)
            )
    end.

process_macro_return(Return, Macro, Opts) ->
    ValidateOpts =
        #{record_defs => maps:get(forms, Opts, []), fail => collect},
    MacroMap = maps:get(macro_map, Opts, #{}),
    do([
        traverse
     || Attr <- astranaut_traverse:ask(),
        RenameCounter <- macro_return_rename_counter(Opts),
        ProcessReturn =
            astranaut_traverse:with_all_error(
                fun
                    ({invalid_transform_normalization, Detail}) ->
                        {invalid_macro_return, macro_return_detail(Macro, Opts, Detail)};
                    (Error) ->
                        Error
                end,
                astranaut:map_m(
                    fun(Node) ->
                        Node1 = update_macro_return_node(
                            Node,
                            RenameCounter,
                            maps:get(pos, Macro)
                        ),
                        collect_return_macro_calls(
                            Node1,
                            MacroMap
                        )
                    end,
                    Return,
                    #{
                        traverse => post,
                        validate => input,
                        attr => Attr,
                        validate_opts => ValidateOpts
                    }
                )
            ),
        {Return1, ReturnAnalysis} <-
            astranaut_traverse:scoped_state_run(
                false,
                astranaut_traverse:fail_on_error(ProcessReturn)
            ),
        commit_macro_return_counter(Opts),
        return({Return1, ReturnAnalysis})
    ]).

collect_return_macro_calls(Node, MacroMap) ->
    case match_macro_call(Node, MacroMap) of
        {ok, _Macro} ->
            astranaut_traverse:then(
                astranaut_traverse:put(true), astranaut_traverse:return(Node)
            );
        error ->
            astranaut_traverse:return(Node)
    end.

macro_return_detail(Macro, Opts, Detail) ->
    Current = macro_call_ref(Macro),
    Origin = macro_call_ref(maps:get(origin_macro, Opts, Macro)),
    case same_macro_call_ref(Current, Origin) of
        true -> Detail#{macro => Current};
        false -> Detail#{origin_macro => Origin, current_macro => Current}
    end.

same_macro_call_ref(
    #{mfa := MFA, arguments := Arguments, ast := Ast},
    #{mfa := MFA, arguments := Arguments, ast := Ast}
) ->
    true;
same_macro_call_ref(_Current, _Origin) ->
    false.

macro_call_ref(Macro) ->
    #{
        mfa => macro_mfa(Macro),
        arguments => maps:get(arguments, Macro, []),
        ast => maps:get(call_ast, Macro, undefined)
    }.

macro_mfa(#{diagnostic_mfa := MFA, arguments := Arguments}) ->
    MFA#{arity => length(Arguments)};
macro_mfa(#{macro := {Module, Function}, arguments := Arguments}) ->
    #{module => Module, function => Function, arity => length(Arguments)};
macro_mfa(#{module := Module, function := Function, arguments := Arguments}) ->
    #{module => Module, function => Function, arity => length(Arguments)}.

macro_exception(Arguments, Class, Exception, StackTraces, #{module := Module} = Macro) ->
    DisplayModule = maps:get(display_module, Macro, Module),
    DisplayStack = lists:map(
        fun
            ({M, F, A, Pos}) when M =:= Module -> {DisplayModule, F, A, Pos};
            (Frame) -> Frame
        end,
        StackTraces
    ),
    {macro_exception, macro_mfa(Macro), Arguments, {Class, Exception, DisplayStack}}.

%%%===================================================================
%%% Attribute target resolution and invocation construction
%%%===================================================================

resolve_attribute_target_by_name(
    Attribute, Arguments, Pos, AttributeIndex, CallAst
) ->
    case maps:find(Attribute, AttributeIndex) of
        error ->
            not_macro;
        {ok, AttributeMacros} ->
            RawArguments = to_list(Arguments),
            case
                find_macro(
                    Attribute, length(RawArguments), AttributeMacros
                )
            of
                {ok, Macro} ->
                    {ok, #{
                        macro => Macro,
                        raw_arguments => RawArguments,
                        pos => Pos,
                        call_ast => CallAst
                    }};
                error ->
                    error
            end
    end.

resolve_macro_target(MacroName, Arguments, Pos, Macros, CallAst) ->
    RawArguments = to_list(Arguments),
    case find_macro(MacroName, length(RawArguments), Macros) of
        {ok, Macro} ->
            {ok, #{
                macro => Macro,
                raw_arguments => RawArguments,
                pos => Pos,
                call_ast => CallAst
            }};
        error ->
            error
    end.

build_macro_invocation(
    #{
        macro := Macro0,
        raw_arguments := RawArguments,
        pos := Pos,
        call_ast := CallAst
    }
) ->
    Macro = Macro0#{pos => Pos, call_ast => CallAst},
    GroupedArguments = group_arguments(RawArguments, Macro),
    Arguments = append_attrs(GroupedArguments, Macro),
    Macro#{arguments => Arguments}.

find_macro_with_arguments(MacroName, Arguments, Pos, Macros, CallAst) ->
    case resolve_macro_target(MacroName, Arguments, Pos, Macros, CallAst) of
        {ok, Target} -> {ok, build_macro_invocation(Target)};
        error -> error
    end.

find_macro(MacroName, Arity, Macros) ->
    case maps:find({MacroName, Arity}, Macros) of
        {ok, Macro} ->
            {ok, Macro};
        error ->
            case maps:find({MacroName, 1}, Macros) of
                {ok, Macro} ->
                    case maps:get(group_args, Macro, false) of
                        false -> error;
                        true -> {ok, Macro}
                    end;
                error ->
                    error
            end
    end.

group_arguments(Arguments, #{group_args := true}) ->
    [Arguments];
group_arguments(Arguments, #{}) ->
    Arguments.

append_attrs(Arguments, #{attributes := Attrs, pos := Pos}) ->
    Arguments ++ [Attrs#{pos => Pos}];
append_attrs(Arguments, #{}) ->
    Arguments.

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].

%%%===================================================================
%%% Caller detection and returned-node normalization
%%%===================================================================

has_macro_call(_Nodes, MacroMap) when map_size(MacroMap) =:= 0 ->
    false;
has_macro_call(Nodes, MacroMap) ->
    astranaut:search(
        fun(Node) ->
            case match_macro_call(Node, MacroMap) of
                {ok, _Macro} -> true;
                error -> false
            end
        end,
        Nodes,
        #{traverse => pre}
    ).

macro_return_rename_counter(#{rename_quoted_variables := true}) ->
    astranaut_traverse:get();
macro_return_rename_counter(_Opts) ->
    astranaut_traverse:return(undefined).

commit_macro_return_counter(#{rename_quoted_variables := true}) ->
    astranaut_traverse:modify(fun(Counter) -> Counter + 1 end);
commit_macro_return_counter(#{}) ->
    astranaut_traverse:return(ok).

update_macro_return_node(Node, RenameCounter, Pos) ->
    Node1 = rename_quoted_variable_node(Node, RenameCounter),
    Node2 = replace_pos_zero_node(Node1, Pos),
    astranaut_syntax:revert(Node2).

rename_quoted_variable_node(
    {var, Pos, VarName} = Var, Counter
) when is_integer(Counter) ->
    case rename_quote_variable(VarName, Counter) of
        VarName ->
            Var;
        VarName1 ->
            {var, Pos, VarName1}
    end;
rename_quoted_variable_node(
    {named_fun, Pos, Name, Clauses} = NamedFun, Counter
) when
    is_integer(Counter)
->
    case rename_quote_variable(Name, Counter) of
        Name ->
            NamedFun;
        Name1 ->
            {named_fun, Pos, Name1, Clauses}
    end;
rename_quoted_variable_node(Node, _RenameCounter) ->
    Node.

rename_quote_variable(Name, Counter) ->
    case astranaut_quote:decode_quote_variable(Name) of
        {template, OriginalName, Context} ->
            astranaut_quote:encode_quote_variable(
                OriginalName, Context, Counter
            );
        {expanded, _OriginalName, _Context, _ExistingCounter} ->
            Name;
        not_quote_variable ->
            Name
    end.

replace_pos_zero_node(Node, 0) ->
    Node;
replace_pos_zero_node(Node, Pos) ->
    case astranaut_syntax:get_pos(Node) of
        0 -> astranaut_syntax:set_pos(Node, Pos);
        _ -> Node
    end.

%%%===================================================================
%%% Per-call debug output
%%%===================================================================

format_node(Nodes, Options) when is_list(Nodes) ->
    lists:foreach(fun(Node) -> format_node(Node, Options) end, Nodes);
format_node(Node, #{file := File, pos := Pos} = Options) ->
    case maps:get(debug, Options, false) of
        true ->
            io:format(
                "from ~s:~p ~s~n",
                [filename:basename(File), Pos, format_mfa(Options)]
            ),
            io:format("~s~n", [astranaut_lib:ast_safe_to_string(Node)]);
        false ->
            ok
    end,
    case maps:get(debug_ast, Options, false) of
        true ->
            io:format(
                "from ~s:~p ~s~n",
                [filename:basename(File), Pos, format_mfa(Options)]
            ),
            io:format("~p~n", [Node]);
        false ->
            ok
    end;
format_node(_Node, _Options) ->
    ok.
