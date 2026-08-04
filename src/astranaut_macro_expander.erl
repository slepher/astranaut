%%%-------------------------------------------------------------------
%%% @doc Shared macro matching, invocation, and recursive function expansion.
%%%
%%% This module owns expansion-local traversal state. It does not own the
%%% source-ordered attribute queue, macro-environment updates, or local-macro
%%% generation lifecycle.
%%%-------------------------------------------------------------------
-module(astranaut_macro_expander).

-include("do.hrl").

-export([expand_functions/2,
         function_call_analysis/2,
         function_call_analysis/3,
         attribute_macro_index/1,
         resolve_attribute_target/3,
         expand_attribute_target/1,
         function_macro_callers/1,
         function_macro_callers/2,
         format_mfa/1]).
-export_type([observation_control/0]).

-type fa() :: {atom(), non_neg_integer()}.
-type form_id() :: {function, atom(), non_neg_integer()}.
-type macro_map() :: map().
-type observation_control() ::
        disabled |
        #{mode := collect,
          form_id := {function, atom(), non_neg_integer()},
          conflict_tag := atom()} |
        #{mode := verify,
          form_id := {function, atom(), non_neg_integer()},
          conflict_tag := atom(),
          expected := ordsets:ordset(fa())}.
-type function_expansion() ::
        #{form := term(),
          observed_macro_ids := disabled | ordsets:ordset(term()),
          needed_macro_ids := ordsets:ordset(term())}.
-type function_expansion_task() ::
        #{form := term(),
          macro_map := macro_map(),
          observation_control := observation_control(),
          has_macro_call => boolean()}.
-type function_call_analysis() ::
        #{form := term(),
          has_macro_call := boolean(),
          local_calls => ordsets:ordset(fa()),
          observed_macro_calls => ordsets:ordset(term())}.
-type function_task_expansion() ::
        #{forms := [term()],
          task_results := #{form_id() => function_expansion()}}.

%%%===================================================================
%%% Public shared expansion operations
%%%===================================================================

%% Expand every target during one source-ordered Forms traversal.  Each task
%% carries its own source form and execution environment; traversal state is
%% scoped per function so whitelist and depth observations cannot leak between
%% targets.
-spec expand_functions(
        [term()], #{form_id() => function_expansion_task()}) ->
          astranaut_return:struct(function_task_expansion()).
expand_functions(Forms, Tasks) ->
    RecordForms = record_forms(Forms),
    FunctionClausesUniplate = function_clauses_uniplate(),
    Monad =
        astranaut:map_m(
          fun({function, _Pos, Name, Arity, _Clauses} = Function) ->
                  FormId = {function, Name, Arity},
                  case maps:find(FormId, Tasks) of
                      error ->
                          astranaut_traverse:return(Function);
                      {ok, Task} ->
                          expand_function_task(
                            FormId, Task, RecordForms,
                            FunctionClausesUniplate)
                  end;
             (Form) ->
                  astranaut_traverse:return(Form)
          end, Forms, #{traverse => none}),
    astranaut_return:lift_m(
      fun({Forms1, TaskResults}) ->
              #{forms => Forms1, task_results => TaskResults}
      end,
      astranaut_traverse:run(
        astranaut_traverse:fail_on_error(Monad),
        astranaut_macro, #{}, #{})).

-spec attribute_macro_index(macro_map()) -> map().
attribute_macro_index(MacroMap) ->
    maps:fold(
      fun(_MacroKey, #{as_attr := Attribute,
                       call_arity := Arity} = Macro, Acc) ->
              AttributeMacros = maps:get(Attribute, Acc, #{}),
              maps:put(
                Attribute,
                maps:put({Attribute, Arity}, Macro, AttributeMacros),
                Acc);
         (_MacroKey, _Macro, Acc) ->
              Acc
      end, #{}, MacroMap).

-spec function_call_analysis([term()], macro_map()) ->
          #{form_id() => function_call_analysis()}.
function_call_analysis(Forms, MacroMap) ->
    function_call_analysis(Forms, MacroMap, closure).

-spec function_call_analysis([term()], macro_map(), closure | presence) ->
          #{form_id() => function_call_analysis()}.
function_call_analysis(Forms, MacroMap, Mode) ->
    lists:foldl(
      fun({function, _Pos, Name, Arity, Clauses} = Form, Acc) ->
              Analysis = analyze_function_calls(
                           Clauses, MacroMap, Mode),
              maps:put(
                {function, Name, Arity}, Analysis#{form => Form}, Acc);
         (_Form, Acc) ->
              Acc
      end, #{}, Forms).

-spec resolve_attribute_target(term(), macro_map(), map()) ->
          {ok, map()} | error | not_macro.
resolve_attribute_target(
  {attribute, Pos, exec_macro, {Function, Arguments}},
  Macros, _AttributeIndex) ->
    resolve_macro_target(
      Function, Arguments, Pos, Macros,
      {attribute, Pos, exec_macro, {Function, Arguments}});
resolve_attribute_target(
  {attribute, Pos, exec_macro, {Module, Function, Arguments}},
  Macros, _AttributeIndex) ->
    resolve_macro_target(
      {Module, Function}, Arguments, Pos, Macros,
      {attribute, Pos, exec_macro, {Module, Function, Arguments}});
resolve_attribute_target(
  {attribute, Pos, Attribute, Arguments},
  _Macros, AttributeIndex) ->
    resolve_attribute_target_by_name(
      Attribute, Arguments, Pos, AttributeIndex,
      {attribute, Pos, Attribute, Arguments});
resolve_attribute_target(_Node, _MacroMap, _AttributeIndex) ->
    not_macro.

-spec expand_attribute_target(map()) ->
          astranaut_traverse:struct(term(), term()).
expand_attribute_target(Target) ->
    Invocation = build_attribute_macro_invocation(Target),
    expand_macro(Invocation, #{expected_role => form}).

-spec function_macro_callers([term()], macro_map()) ->
          ordsets:ordset({function, atom(), non_neg_integer()}).
function_macro_callers(Forms, MacroMap) ->
    function_macro_callers(function_call_analysis(Forms, MacroMap)).

-spec function_macro_callers(#{form_id() => function_call_analysis()}) ->
          ordsets:ordset({function, atom(), non_neg_integer()}).
function_macro_callers(FunctionAnalysis) ->
    maps:fold(
      fun(FormId, #{has_macro_call := true}, Acc) ->
              ordsets:add_element(FormId, Acc);
         (_FormId, _Analysis, Acc) ->
              Acc
      end, ordsets:new(), FunctionAnalysis).

-spec format_mfa(map()) -> iolist().
format_mfa(#{function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(#{module := Module, function := Function, arity := Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).

%%%===================================================================
%%% Function expansion and whitelist observation
%%%===================================================================

expand_function_task(
  FormId,
  #{form := Function,
    macro_map := MacroMap,
    observation_control := ObservationControl} = Task,
  RecordForms, FunctionClausesUniplate) ->
    Expand =
        do([ traverse ||
               {Function1, ExpansionState} <-
                   astranaut_traverse:scoped_state_run(
                     initial_expansion_state(ObservationControl),
                     transform_task_function_if_needed(
                       Function, MacroMap, RecordForms,
                       ObservationControl, FunctionClausesUniplate,
                       maps:find(has_macro_call, Task))),
               #{form := Function2} = Result <-
                   astranaut:traverse_return(
                     finish_function_expansion(
                       Function1, ExpansionState, ObservationControl)),
               astranaut_traverse:modify(
                 fun(Results) -> maps:put(FormId, Result, Results) end),
               return(Function2)
           ]),
    astranaut_traverse:catch_on_error(
      Expand, fun() -> astranaut_traverse:return(Function) end).

transform_task_function_if_needed(
  {function, _Pos, _Name, _Arity, Clauses} = Function,
  MacroMap, RecordForms, ObservationControl, FunctionClausesUniplate,
  MacroCallHint) ->
    case function_has_macro_call(Clauses, MacroMap, MacroCallHint) of
        false ->
            astranaut_traverse:return(Function);
        true ->
            astranaut:map_m(
              fun(Clause) ->
                      transform_clause(
                        uniform, MacroMap, Clause, RecordForms,
                        ObservationControl)
              end, Function,
              #{traverse => subtree,
                uniplate => FunctionClausesUniplate})
    end.

function_has_macro_call(_Clauses, MacroMap, _Hint)
  when map_size(MacroMap) =:= 0 ->
    false;
function_has_macro_call(_Clauses, _MacroMap, {ok, HasMacroCall}) ->
    HasMacroCall;
function_has_macro_call(Clauses, MacroMap, error) ->
    has_macro_call(Clauses, MacroMap).

function_clauses_uniplate() ->
    fun({function, Pos, Name, Arity, Clauses}) ->
            {[Clauses],
             fun([NewClauses]) ->
                     {function, Pos, Name, Arity, NewClauses}
             end};
       (Node) ->
            {[[]], fun(_) -> Node end}
    end.

transform_clause(Module, MacroMap,
                 {clause, Pos, Patterns, Guards, Exprs},
                 RecordForms, ObservationControl) ->
    do([ traverse ||
           reset_macro_return_counter(),
           Guards1 <- transform_guard_sequences(
                        Module, MacroMap, Guards,
                        #{depth => 0,
                          expected_role => guard,
                          forms => RecordForms,
                          observation_control => ObservationControl}),
           Exprs1 <- transform_exprs(
                       Module, MacroMap, Exprs,
                       #{depth => 0,
                         expected_role => expression,
                         forms => RecordForms,
                         observation_control => ObservationControl}),
           return({clause, Pos, Patterns, Guards1, Exprs1})
       ]).

transform_guard_sequences(Module, MacroMap, Guards, DepthOpts) ->
    %% Erlang clauses store guards as a list of guard sequences.  Transform
    %% each sequence independently so conjunctions and disjunctions retain
    %% their original nesting and every element is visited as an AST node.
    astranaut_traverse:map_m(
      fun(GuardSequence) ->
              transform_exprs(
                Module, MacroMap, GuardSequence, DepthOpts)
      end, Guards).

initial_expansion_state(disabled) ->
    0;
initial_expansion_state(#{mode := Mode})
  when Mode =:= collect; Mode =:= verify ->
    #{macro_return_counter => 0,
      observed_macro_ids => ordsets:new(),
      needed_macro_ids => ordsets:new()}.

reset_macro_return_counter() ->
    astranaut_traverse:modify(
      fun(State) when is_integer(State) ->
              1;
         (State) ->
              State#{macro_return_counter => 1}
      end).

finish_function_expansion(Form, _State, disabled) ->
    astranaut_return:return(
      #{form => Form,
        observed_macro_ids => disabled,
        needed_macro_ids => ordsets:new()});
finish_function_expansion(Form, State, #{mode := collect}) ->
    astranaut_return:return(
      #{form => Form,
        observed_macro_ids => maps:get(observed_macro_ids, State),
        needed_macro_ids => maps:get(needed_macro_ids, State)});
finish_function_expansion(
  Form, State,
  #{mode := verify, form_id := FormId,
    expected := Expected} = Control) ->
    Observed = maps:get(observed_macro_ids, State),
    Needed = maps:get(needed_macro_ids, State),
    case Needed of
        [_ | _] ->
            astranaut_return:return(
              #{form => Form,
                observed_macro_ids => Observed,
                needed_macro_ids => Needed});
        [] ->
            Missing = ordsets:subtract(Expected, Observed),
            case Missing of
                [] ->
                    astranaut_return:return(
                      #{form => Form,
                        observed_macro_ids => Observed,
                        needed_macro_ids => Needed});
                _ ->
                    astranaut_return:error_fail(
                      observation_conflict(
                        FormId, Expected, Observed,
                        ordsets:new(), Missing, Control))
            end
    end.

record_forms(Forms) ->
    [Form || {attribute, _Anno, record, {_Name, _Fields}} = Form <- Forms].

transform_exprs(Module, MacroMap, Exprs, DepthOpts) ->
    ExpectedRole = maps:get(expected_role, DepthOpts, expression),
    InitAttr = #{node => ExpectedRole,
                 validator => {role, ExpectedRole}},
    Monad =
        astranaut:map_m(
          fun(Node) ->
                  expand_without_pending_dependency(
                    Node,
                    fun() ->
                            do([ traverse ||
                                   Attr = #{step := Step} <-
                                       astranaut_traverse:ask(),
                                   DepthOpts1 =
                                       DepthOpts#{
                                         rename_quoted_variables => true,
                                         step => Step,
                                         attr => Attr},
                                   case match_macro_call(
                                          Module, Node, MacroMap, Step) of
                                       {ok, Macro} ->
                                           expand_observed_macro(
                                             Module, MacroMap, Macro, Node,
                                             DepthOpts1);
                                       error ->
                                           astranaut_traverse:return(Node)
                                   end
                               ])
                    end)
          end, Exprs, #{traverse => all, normalize => false}),
    astranaut_traverse:local(fun(_) -> InitAttr end, Monad).

expand_without_pending_dependency(Node, Expand) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           case State of
               #{needed_macro_ids := [_ | _]} ->
                   astranaut_traverse:return(Node);
               _ ->
                   Expand()
           end
       ]).

expand_observed_macro(Module, MacroMap, Macro, Node, DepthOpts) ->
    do([ traverse ||
           Decision <- observe_macro(Macro, DepthOpts),
           case Decision of
               expand ->
                   expand_or_request_macro(
                     Module, MacroMap, Macro, Node, DepthOpts);
               skip ->
                   return(Node)
           end
       ]).

expand_or_request_macro(
  _Module, _MacroMap,
  #{observation_id := ObservationId,
    callable := false},
  Node, _DepthOpts) ->
    do([ traverse ||
           astranaut_traverse:modify(
             fun(State) ->
                     Needed = ordsets:add_element(
                                ObservationId,
                                maps:get(needed_macro_ids, State)),
                     State#{needed_macro_ids => Needed}
             end),
           return(Node)
       ]);
expand_or_request_macro(Module, MacroMap, Macro, _Node, DepthOpts) ->
    expand_macro_recursive(Module, MacroMap, Macro, DepthOpts).

observe_macro(
  #{observation_id := ObservationId},
  #{observation_control := Control})
  when Control =/= disabled ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           Observed0 = maps:get(observed_macro_ids, State),
           IsNew = not ordsets:is_element(ObservationId, Observed0),
           Observed = ordsets:add_element(ObservationId, Observed0),
           astranaut_traverse:put(
             State#{observed_macro_ids => Observed}),
           verify_observed_macro(
             Control, ObservationId, IsNew, Observed)
       ]);
observe_macro(_Macro, _Opts) ->
    astranaut_traverse:return(expand).

verify_observed_macro(
  #{mode := verify, form_id := FormId,
    expected := Expected} = Control,
  ObservationId, IsNew, Observed) ->
    case ordsets:is_element(ObservationId, Expected) of
        true ->
            astranaut_traverse:return(expand);
        false when IsNew ->
            Unexpected = ordsets:subtract(Observed, Expected),
            Error = observation_conflict(
                      FormId, Expected, Observed,
                      Unexpected, ordsets:new(), Control),
            do([ traverse ||
                   astranaut_traverse:error(Error),
                   return(skip)
               ]);
        false ->
            astranaut_traverse:return(skip)
    end;
verify_observed_macro(#{mode := collect}, _Id, _IsNew, _Observed) ->
    astranaut_traverse:return(expand).

observe_macro_return([], _Opts) ->
    astranaut_traverse:return(expand);
observe_macro_return(_ReturnObserved,
                     #{observation_control := disabled}) ->
    astranaut_traverse:return(expand);
observe_macro_return(ReturnObserved,
                     #{observation_control := Control}) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           Observed = ordsets:union(
                        ReturnObserved,
                        maps:get(observed_macro_ids, State)),
           astranaut_traverse:put(
             State#{observed_macro_ids => Observed}),
           verify_macro_return(Control, ReturnObserved, Observed)
       ]).

verify_macro_return(
  #{mode := verify, form_id := FormId,
    expected := Expected} = Control,
  ReturnObserved, Observed) ->
    case ordsets:subtract(ReturnObserved, Expected) of
        [] ->
            astranaut_traverse:return(expand);
        _ ->
            Unexpected = ordsets:subtract(Observed, Expected),
            Error = observation_conflict(
                      FormId, Expected, Observed,
                      Unexpected, ordsets:new(), Control),
            do([ traverse ||
                   astranaut_traverse:error(Error),
                   return(skip)
               ])
    end;
verify_macro_return(#{mode := collect}, _ReturnObserved, _Observed) ->
    astranaut_traverse:return(expand).

whitelist_conflict_detail(Expected, Observed, Unexpected, Missing) ->
    #{expected => Expected,
      observed => Observed,
      unexpected => Unexpected,
      missing => Missing}.

observation_conflict(
  FormId, Expected, Observed, Unexpected, Missing,
  #{conflict_tag := Tag}) ->
    {Tag, FormId,
     whitelist_conflict_detail(
       Expected, Observed, Unexpected, Missing)}.

%%%===================================================================
%%% Macro matching, invocation, and returned AST processing
%%%===================================================================

match_macro_call(Module, Node, Macros, Step) ->
    case call_find_macro(Module, Node, Macros) of
        {ok, Macro} ->
            case match_macro_order(Macro, Step) of
                true -> {ok, Macro};
                false -> error
            end;
        error ->
            error
    end.

call_find_macro(
  _Module,
  {call, Pos1, {atom, _Pos2, Function}, Arguments} = Node,
  Macros) ->
    find_macro_with_arguments(Function, Arguments, Pos1, Macros, Node);
call_find_macro(
  _Module,
  {call, Pos1,
   {remote, _Pos2,
    {atom, _Pos3, RemoteModule}, {atom, _Pos4, Function}},
   Arguments} = Node,
  Macros) ->
    find_macro_with_arguments(
      {RemoteModule, Function}, Arguments, Pos1, Macros, Node);
call_find_macro(_Module, _Node, _Macros) ->
    error.

match_macro_order(Macro, Step) ->
    Order = maps:get(order, Macro, inner),
    ((Order =:= inner) and (Step =:= post))
        or ((Order =:= outer) and (Step =:= pre)).

expand_macro_recursive(
  _Module, _MacroMap, #{max_depth := MaxDepth} = Macro,
  #{depth := Depth} = DepthOpts)
  when Depth >= MaxDepth ->
    CurrentMacro = maps:get(origin_macro, DepthOpts, Macro),
    recover_macro_call(
      Macro,
      astranaut_traverse:fail(
        {max_macro_expansion_depth_exceeded,
         maps:get(macro, CurrentMacro),
         maps:get(arguments, CurrentMacro, [])}));
expand_macro_recursive(Module, MacroMap, Macro,
                       #{step := post} = DepthOpts) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    expand_macro_with(
      Macro, DepthOpts1#{module => Module, macro_map => MacroMap},
      fun(Node1) ->
              transform_exprs(Module, MacroMap, Node1, DepthOpts1)
      end);
expand_macro_recursive(Module, MacroMap, Macro,
                       #{step := pre} = DepthOpts) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    expand_macro(
      Macro, DepthOpts1#{module => Module, macro_map => MacroMap}).

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
  #{pos := Pos, formatter := Formatter} = Macro, Opts, Success) ->
    recover_macro_call(
      Macro,
      do([ traverse ||
             Node <- astranaut_traverse:update_pos(
                       Pos, Formatter, invoke_macro_function(Macro)),
             {Node1, ReturnAnalysis} <-
                 process_macro_return(Node, Macro, Opts),
             Decision <- observe_macro_return(
                           maps:get(observed_macro_calls, ReturnAnalysis),
                           Opts),
             format_node(Node1, Macro),
             case {Decision,
                   maps:get(has_macro_call, ReturnAnalysis)} of
                 {expand, true} -> Success(Node1);
                 {expand, false} -> return(Node1);
                 {skip, _HasMacroCall} -> return(Node1)
             end
         ])).

recover_macro_call(Macro, Monad) ->
    astranaut_traverse:catch_on_error(
      Monad,
      fun() ->
              astranaut_traverse:return(maps:get(call_ast, Macro))
      end).

invoke_macro_function(
  #{module := Module, function := Function, arguments := Arguments} = Macro) ->
    try erlang:apply(Module, Function, Arguments) of
        Return ->
            astranaut_traverse:scoped_state(
              ok, astranaut:traverse_return(Return))
    catch
        Class:Exception:Stacktrace ->
            StackTraces1 =
                lists:takewhile(
                  fun({M, F, A, _Pos}) ->
                          {M, F, A} =/=
                              {?MODULE, invoke_macro_function, 1};
                     (_Stack) ->
                          false
                  end, Stacktrace),
            Error = macro_exception(
                      Arguments, Class, Exception, StackTraces1, Macro),
            astranaut_traverse:fail(Error)
    end.

process_macro_return(Return, Macro, Opts) ->
    ValidateOpts =
        #{record_defs => maps:get(forms, Opts, []), fail => collect},
    CollectObservedMacros =
        maps:get(observation_control, Opts, disabled) =/= disabled,
    Module = maps:get(module, Opts, uniform),
    MacroMap = maps:get(macro_map, Opts, #{}),
    do([ traverse ||
           Attr <- astranaut_traverse:ask(),
           RenameCounter <- macro_return_rename_counter(Opts),
           ProcessReturn =
               astranaut_traverse:with_all_error(
                 fun({invalid_transform_normalization, Detail}) ->
                         {invalid_macro_return,
                          macro_return_detail(Macro, Opts, Detail)};
                    (Error) ->
                         Error
                 end,
                 astranaut:map_m(
                   fun(Node) ->
                            Node1 = update_macro_return_node(
                                      Node, RenameCounter,
                                      maps:get(pos, Macro)),
                           collect_return_macro_calls(
                             Module, Node1, MacroMap,
                             CollectObservedMacros)
                   end, Return,
                   #{traverse => post,
                     validate => input,
                     attr => Attr,
                     validate_opts => ValidateOpts})),
           {Return1, ReturnAnalysis} <-
               astranaut_traverse:scoped_state_run(
                 #{observed_macro_calls => ordsets:new(),
                   has_macro_call => false},
                 astranaut_traverse:fail_on_error(ProcessReturn)),
           commit_macro_return_counter(Opts),
           return({Return1, ReturnAnalysis})
       ]).

collect_return_macro_calls(Module, Node, MacroMap, CollectObservedMacros) ->
    case call_find_macro(Module, Node, MacroMap) of
        {ok, #{observation_id := ObservationId}} ->
            do([ traverse ||
                   astranaut_traverse:modify(
                     fun(Observed) ->
                             Observed1 = Observed#{has_macro_call := true},
                             case CollectObservedMacros of
                                 true ->
                                     ObservedMacros = ordsets:add_element(
                                                        ObservationId,
                                                        maps:get(
                                                          observed_macro_calls,
                                                          Observed1)),
                                     Observed1#{observed_macro_calls :=
                                                    ObservedMacros};
                                 false ->
                                     Observed1
                             end
                     end),
                   return(Node)
               ]);
        {ok, _Macro} ->
            do([ traverse ||
                   astranaut_traverse:modify(
                     fun(Observed) ->
                             Observed#{has_macro_call := true}
                     end),
                   return(Node)
               ]);
        _ ->
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
  #{mfa := MFA, arguments := Arguments, ast := Ast}) ->
    true;
same_macro_call_ref(_Current, _Origin) ->
    false.

macro_call_ref(Macro) ->
    #{mfa => macro_mfa(Macro),
      arguments => maps:get(arguments, Macro, []),
      ast => maps:get(call_ast, Macro, undefined)}.

macro_mfa(#{macro := {Module, Function}, arguments := Arguments}) ->
    #{module => Module, function => Function, arity => length(Arguments)};
macro_mfa(
  #{module := LocalModule, macro_module := Module,
    macro := Function, arguments := Arguments})
  when LocalModule =/= Module ->
    #{function => Function, arity => length(Arguments), local => true};
macro_mfa(#{module := Module, function := Function,
            arguments := Arguments}) ->
    #{module => Module, function => Function, arity => length(Arguments)};
macro_mfa(#{function := Function, arity := Arity} = Macro) ->
    case maps:find(macro_module, Macro) of
        {ok, Module} ->
            #{module => Module, function => Function, arity => Arity};
        error ->
            #{function => Function, arity => Arity, local => true}
    end.

macro_exception(Arguments, Class, Exception, StackTraces,
                #{macro := {Module, Function}}) ->
    MFA = #{module => Module,
            function => Function,
            arity => length(Arguments)},
    {macro_exception, MFA, Arguments,
     {Class, Exception, StackTraces}};
macro_exception(
  Arguments, Class, Exception, StackTraces,
  #{module := LocalModule, macro_module := Module, macro := Function}) ->
    StackTraces1 =
        lists:map(
          fun({M, F, A, Pos}) when M =:= LocalModule ->
                  {Module, F, A, Pos};
             (Value) ->
                  Value
          end, StackTraces),
    MFA = #{function => Function,
            arity => length(Arguments),
            local => true},
    {macro_exception, MFA, Arguments,
     {Class, Exception, StackTraces1}}.

%%%===================================================================
%%% Attribute target resolution and invocation construction
%%%===================================================================

resolve_attribute_target_by_name(
  Attribute, Arguments, Pos, AttributeIndex, CallAst) ->
    case maps:find(Attribute, AttributeIndex) of
        error ->
            not_macro;
        {ok, AttributeMacros} ->
            RawArguments = to_list(Arguments),
            case find_attribute_macro(
                   Attribute, length(RawArguments), AttributeMacros) of
                {ok, Macro} ->
                    {ok, #{macro => Macro,
                           raw_arguments => RawArguments,
                           pos => Pos,
                           call_ast => CallAst}};
                error ->
                    error
            end
    end.

find_attribute_macro(Attribute, Arity, AttributeIndex) ->
    case maps:find({Attribute, Arity}, AttributeIndex) of
        {ok, Macro} ->
            {ok, Macro};
        error ->
            case maps:find({Attribute, 1}, AttributeIndex) of
                {ok, #{group_args := true} = Macro} ->
                    {ok, Macro};
                {ok, _Macro} ->
                    error;
                error ->
                    error
            end
    end.

resolve_macro_target(MacroName, Arguments, Pos, Macros, CallAst) ->
    RawArguments = to_list(Arguments),
    case find_macro(MacroName, length(RawArguments), Macros) of
        {ok, Macro} ->
            {ok, #{macro => Macro,
                   raw_arguments => RawArguments,
                   pos => Pos,
                   call_ast => CallAst}};
        error ->
            error
    end.

build_attribute_macro_invocation(
  #{macro := Macro0, raw_arguments := RawArguments,
    pos := Pos, call_ast := CallAst}) ->
    Macro = Macro0#{pos => Pos, call_ast => CallAst},
    GroupedArguments = group_arguments(RawArguments, Macro),
    Arguments = append_attrs(GroupedArguments, Macro),
    Macro#{arguments => Arguments}.

find_macro_with_arguments(MacroName, Arguments, Pos, Macros, CallAst) ->
    Arguments1 = to_list(Arguments),
    Arity = length(Arguments1),
    case find_macro(MacroName, Arity, Macros) of
        {ok, Macro} ->
            Macro1 = Macro#{pos => Pos, call_ast => CallAst},
            Arguments2 = group_arguments(Arguments1, Macro1),
            Arguments3 = append_attrs(Arguments2, Macro1),
            {ok, Macro1#{arguments => Arguments3}};
        error ->
            error
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

analyze_function_calls(Clauses, MacroMap, presence) ->
    #{has_macro_call => has_macro_call(Clauses, MacroMap)};
analyze_function_calls(Clauses, MacroMap, closure) ->
    astranaut:sreduce(
      fun({call, _Pos, {atom, _FunctionPos, Function}, Arguments} = Node,
          Analysis) ->
              analyze_macro_call(
                Node, MacroMap,
                Analysis#{local_calls :=
                              ordsets:add_element(
                                {Function, length(Arguments)},
                                maps:get(local_calls, Analysis))});
         ({call, _Pos,
           {remote, _RemotePos,
            {atom, _ModulePos, _Module},
            {atom, _FunctionPos, _Function}}, _Arguments} = Node,
          Analysis) ->
              analyze_macro_call(Node, MacroMap, Analysis);
         (_Node, Analysis) ->
              Analysis
      end,
      #{local_calls => ordsets:new(),
        observed_macro_calls => ordsets:new(),
        has_macro_call => false},
      Clauses, #{traverse => pre}).

analyze_macro_call(Node, MacroMap, Analysis) ->
    case call_find_macro(uniform, Node, MacroMap) of
        {ok, #{observation_id := ObservationId}} ->
            Analysis#{observed_macro_calls :=
                          ordsets:add_element(
                            ObservationId,
                            maps:get(observed_macro_calls, Analysis)),
                      has_macro_call := true};
        {ok, _Macro} ->
            Analysis#{has_macro_call := true};
        error ->
            Analysis
    end.

has_macro_call(Nodes, MacroMap) ->
    astranaut:sreduce(
      fun(_Node, true) ->
              true;
         (Node, false) ->
              case call_find_macro(uniform, Node, MacroMap) of
                  {ok, _Macro} -> true;
                  error -> false
              end
      end, false, Nodes, #{traverse => pre}).

macro_return_rename_counter(#{rename_quoted_variables := true}) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           return(macro_return_counter(State))
       ]);
macro_return_rename_counter(_Opts) ->
    astranaut_traverse:return(undefined).

commit_macro_return_counter(#{rename_quoted_variables := true}) ->
    astranaut_traverse:modify(
      fun(Counter) when is_integer(Counter) ->
              Counter + 1;
         (State) ->
              Counter = maps:get(macro_return_counter, State),
              State#{macro_return_counter => Counter + 1}
      end);
commit_macro_return_counter(#{}) ->
    astranaut_traverse:return(ok).

macro_return_counter(Counter) when is_integer(Counter) ->
    Counter;
macro_return_counter(State) ->
    maps:get(macro_return_counter, State).

update_macro_return_node(Node, RenameCounter, Pos) ->
    Node1 = rename_quoted_variable_node(Node, RenameCounter),
    Node2 = replace_pos_zero_node(Node1, Pos),
    astranaut_syntax:revert(Node2).

rename_quoted_variable_node(
  {var, Pos, VarName} = Var, Counter) when is_integer(Counter) ->
    case rename_quote_variable(VarName, Counter) of
        VarName ->
            Var;
        VarName1 ->
            {var, Pos, VarName1}
    end;
rename_quoted_variable_node(
  {named_fun, Pos, Name, Clauses} = NamedFun, Counter)
  when is_integer(Counter) ->
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
              OriginalName, Context, Counter);
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
              [filename:basename(File), Pos, format_mfa(Options)]),
            io:format("~s~n", [astranaut_lib:ast_safe_to_string(Node)]);
        false ->
            ok
    end,
    case maps:get(debug_ast, Options, false) of
        true ->
            io:format(
              "from ~s:~p ~s~n",
              [filename:basename(File), Pos, format_mfa(Options)]),
            io:format("~p~n", [Node]);
        false ->
            ok
    end;
format_node(_Node, _Options) ->
    ok.
