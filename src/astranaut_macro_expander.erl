%%%-------------------------------------------------------------------
%%% @doc Shared macro matching, invocation, and recursive function expansion.
%%%
%%% This module owns expansion-local traversal state. It does not own the
%%% source-ordered attribute queue, macro-environment updates, or local-macro
%%% generation lifecycle.
%%%-------------------------------------------------------------------
-module(astranaut_macro_expander).

-include("do.hrl").
-include("stacktrace.hrl").

-export([expand_function/5,
         resolve_local_references/2,
         resolve_attribute_target/2,
         expand_attribute_target/2,
         function_macro_callers/2,
         format_mfa/1]).
-export_type([local_macro_whitelist_control/0, function_expansion/0]).

-type fa() :: {atom(), non_neg_integer()}.
-type macro_map() :: map().
-type macro_runtime_context() :: #{macro_map := macro_map(),
                                   macro_options := map(),
                                   inject_forms := [term()]}.
-type local_macro_whitelist_control() ::
        disabled |
        #{mode := collect,
          form_id := {function, atom(), non_neg_integer()}} |
        #{mode := verify,
          form_id := {function, atom(), non_neg_integer()},
          expected := ordsets:ordset(fa())}.
-type function_expansion() ::
        #{forms := [term()],
          local_macro_whitelist := disabled | ordsets:ordset(fa()),
          needed_local_macros := ordsets:ordset(fa())}.

%%%===================================================================
%%% Public shared expansion operations
%%%===================================================================

-spec expand_function(macro_map(), [term()], [term()], fa(),
                      local_macro_whitelist_control()) ->
          astranaut_return:struct(function_expansion()).
expand_function(MacroEnv, InjectForms, Forms, TargetFA, WhitelistControl) ->
    ExecutionEnv = inject_macro_attributes(MacroEnv, InjectForms),
    expand_functions(ExecutionEnv, Forms, [TargetFA], WhitelistControl).

-spec resolve_local_references([{fa(), macro_map()}], [term()]) ->
          ordsets:ordset(fa()).
resolve_local_references(TargetEnvs, Forms) ->
    lists:foldl(
      fun({TargetFA, CandidateEnv}, Acc) ->
              ordsets:union(
                referenced_local_fas(TargetFA, CandidateEnv, Forms), Acc)
      end, ordsets:new(), TargetEnvs).

-spec resolve_attribute_target(term(), macro_runtime_context()) ->
          {ok, map()} | error | not_macro.
resolve_attribute_target(
  {attribute, Pos, exec_macro, {Function, Arguments}},
  #{macro_map := Macros}) ->
    resolve_macro_target(
      Function, Arguments, Pos, Macros,
      {attribute, Pos, exec_macro, {Function, Arguments}});
resolve_attribute_target(
  {attribute, Pos, exec_macro, {Module, Function, Arguments}},
  #{macro_map := Macros}) ->
    resolve_macro_target(
      {Module, Function}, Arguments, Pos, Macros,
      {attribute, Pos, exec_macro, {Module, Function, Arguments}});
resolve_attribute_target(
  {attribute, Pos, Attribute, Arguments},
  #{macro_map := Macros}) ->
    AttributeMacros = attribute_macro_map(Macros),
    resolve_attribute_target_by_name(
      Attribute, Arguments, Pos, AttributeMacros,
      {attribute, Pos, Attribute, Arguments});
resolve_attribute_target(_Node, _RuntimeContext) ->
    not_macro.

-spec expand_attribute_target(map(), macro_runtime_context()) ->
          astranaut_traverse:struct(term(), term()).
expand_attribute_target(Target, RuntimeContext) ->
    Invocation = build_attribute_macro_invocation(Target, RuntimeContext),
    expand_macro(Invocation, #{expected_role => form}).

-spec function_macro_callers([term()], macro_map()) ->
          ordsets:ordset({function, atom(), non_neg_integer()}).
function_macro_callers(Forms, MacroMap) ->
    ExecutionEnv = inject_macro_attributes(MacroMap, Forms),
    find_function_macro_callers(Forms, ExecutionEnv).

-spec format_mfa(map()) -> iolist().
format_mfa(#{function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(#{module := Module, function := Function, arity := Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).

%%%===================================================================
%%% Function expansion and whitelist observation
%%%===================================================================

expand_functions(MacroEnv, Forms, TargetFAs, WhitelistControl) ->
    MacroCallers = find_function_macro_callers(Forms, MacroEnv),
    TargetIds = function_ids(TargetFAs),
    TransformIds = ordsets:intersection(MacroCallers, TargetIds),
    transform_functions_if_needed(
      uniform, MacroEnv, Forms, TransformIds, WhitelistControl).

referenced_local_fas({Name, Arity}, CandidateEnv, Forms) ->
    case [Clauses || {function, _Pos, Name0, Arity0, Clauses} <- Forms,
                     Name0 =:= Name, Arity0 =:= Arity] of
        [Clauses | _] ->
            astranaut:sreduce(
              fun(Node, Acc) ->
                      case call_find_macro(uniform, Node, CandidateEnv) of
                          {ok, #{macro_source := local_macro,
                                 function := Function,
                                 arity := MacroArity}} ->
                              ordsets:add_element(
                                {Function, MacroArity}, Acc);
                          _ ->
                              Acc
                      end
              end, ordsets:new(), Clauses, #{traverse => pre});
        [] ->
            ordsets:new()
    end.

function_ids(Functions) ->
    lists:foldl(
      fun({Function, Arity}, Acc) ->
              ordsets:add_element({function, Function, Arity}, Acc)
      end, ordsets:new(), Functions).

transform_functions_if_needed(_Module, MacroMap, Forms, _TransformFunctions,
                              WhitelistControl)
  when map_size(MacroMap) =:= 0 ->
    finish_function_expansion(
      Forms, initial_expansion_state(WhitelistControl), WhitelistControl);
transform_functions_if_needed(_Module, _MacroMap, Forms, [],
                              WhitelistControl) ->
    finish_function_expansion(
      Forms, initial_expansion_state(WhitelistControl), WhitelistControl);
transform_functions_if_needed(Module, MacroMap, Forms, TransformFunctions,
                              WhitelistControl) ->
    transform_functions(
      Module, MacroMap, Forms, TransformFunctions, WhitelistControl).

transform_functions(Module, MacroMap, Forms, TransformFunctions,
                    WhitelistControl) ->
    RecordForms = record_forms(Forms),
    FunctionClausesUniplate =
        fun({function, Pos, Name, Arity, Clauses}) ->
                {[Clauses],
                 fun([NewClauses]) ->
                         {function, Pos, Name, Arity, NewClauses}
                 end};
           (Node) ->
                {[[]], fun(_) -> Node end}
        end,
    Monad =
        astranaut:map_m(
          fun({function, _Pos, Name, Arity, _Clauses} = Function) ->
                  case should_transform_function(
                         Name, Arity, TransformFunctions) of
                      false ->
                          astranaut_traverse:return(Function);
                      true ->
                          astranaut:map_m(
                            fun(Clause) ->
                                    transform_clause(
                                      Module, MacroMap, Clause, RecordForms,
                                      WhitelistControl)
                            end, Function,
                            #{traverse => subtree,
                              uniplate => FunctionClausesUniplate})
                  end;
             (Form) ->
                  astranaut_traverse:return(Form)
          end, Forms, #{traverse => none}),
    astranaut_return:bind(
      astranaut_traverse:run(
        Monad, astranaut_macro, #{},
        initial_expansion_state(WhitelistControl)),
      fun({Forms1, ExpansionState}) ->
              finish_function_expansion(
                Forms1, ExpansionState, WhitelistControl)
      end).

transform_clause(Module, MacroMap,
                 {clause, Pos, Patterns, Guards, Exprs},
                 RecordForms, WhitelistControl) ->
    do([ traverse ||
           reset_macro_return_counter(),
           Guards1 <- transform_exprs(
                        Module, MacroMap, Guards,
                        #{depth => 0,
                          expected_role => guard,
                          forms => RecordForms,
                          local_macro_whitelist => WhitelistControl}),
           Exprs1 <- transform_exprs(
                       Module, MacroMap, Exprs,
                       #{depth => 0,
                         expected_role => expression,
                         forms => RecordForms,
                         local_macro_whitelist => WhitelistControl}),
           return({clause, Pos, Patterns, Guards1, Exprs1})
       ]).

initial_expansion_state(disabled) ->
    0;
initial_expansion_state(#{mode := Mode})
  when Mode =:= collect; Mode =:= verify ->
    #{macro_return_counter => 0,
      observed_local_macro_whitelist => ordsets:new(),
      needed_local_macros => ordsets:new()}.

reset_macro_return_counter() ->
    astranaut_traverse:modify(
      fun(State) when is_integer(State) ->
              1;
         (State) ->
              State#{macro_return_counter => 1}
      end).

finish_function_expansion(Forms, _State, disabled) ->
    astranaut_return:return(
      #{forms => Forms,
        local_macro_whitelist => disabled,
        needed_local_macros => ordsets:new()});
finish_function_expansion(Forms, State, #{mode := collect}) ->
    astranaut_return:return(
      #{forms => Forms,
        local_macro_whitelist =>
            maps:get(observed_local_macro_whitelist, State),
        needed_local_macros => maps:get(needed_local_macros, State)});
finish_function_expansion(
  Forms, State,
  #{mode := verify, form_id := FormId, expected := Expected}) ->
    Observed = maps:get(observed_local_macro_whitelist, State),
    Needed = maps:get(needed_local_macros, State),
    case Needed of
        [_ | _] ->
            astranaut_return:return(
              #{forms => Forms,
                local_macro_whitelist => Observed,
                needed_local_macros => Needed});
        [] ->
            Missing = ordsets:subtract(Expected, Observed),
            case Missing of
                [] ->
                    astranaut_return:return(
                      #{forms => Forms,
                        local_macro_whitelist => Observed,
                        needed_local_macros => Needed});
                _ ->
                    astranaut_return:error_fail(
                      {conflicting_local_macro_whitelist, FormId,
                       whitelist_conflict_detail(
                         Expected, Observed, ordsets:new(), Missing)})
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
               #{needed_local_macros := [_ | _]} ->
                   astranaut_traverse:return(Node);
               _ ->
                   Expand()
           end
       ]).

expand_observed_macro(Module, MacroMap, Macro, Node, DepthOpts) ->
    do([ traverse ||
           Decision <- observe_local_macro(Macro, DepthOpts),
           case Decision of
               expand ->
                   expand_or_request_local_macro(
                     Module, MacroMap, Macro, Node, DepthOpts);
               skip ->
                   return(Node)
           end
       ]).

expand_or_request_local_macro(
  _Module, _MacroMap,
  #{macro_source := local_macro,
    local_macro_callable := false,
    function := Function, arity := Arity},
  Node, _DepthOpts) ->
    do([ traverse ||
           astranaut_traverse:modify(
             fun(State) ->
                     Needed = ordsets:add_element(
                                {Function, Arity},
                                maps:get(needed_local_macros, State)),
                     State#{needed_local_macros => Needed}
             end),
           return(Node)
       ]);
expand_or_request_local_macro(Module, MacroMap, Macro, _Node, DepthOpts) ->
    expand_macro_recursive(Module, MacroMap, Macro, DepthOpts).

observe_local_macro(
  #{macro_source := local_macro,
    function := Function, arity := Arity},
  #{local_macro_whitelist := Control})
  when Control =/= disabled ->
    FA = {Function, Arity},
    do([ traverse ||
           State <- astranaut_traverse:get(),
           Observed0 = maps:get(observed_local_macro_whitelist, State),
           IsNew = not ordsets:is_element(FA, Observed0),
           Observed = ordsets:add_element(FA, Observed0),
           astranaut_traverse:put(
             State#{observed_local_macro_whitelist => Observed}),
           verify_observed_local_macro(Control, FA, IsNew, Observed)
       ]);
observe_local_macro(_Macro, _Opts) ->
    astranaut_traverse:return(expand).

verify_observed_local_macro(
  #{mode := verify, form_id := FormId, expected := Expected},
  FA, IsNew, Observed) ->
    case ordsets:is_element(FA, Expected) of
        true ->
            astranaut_traverse:return(expand);
        false when IsNew ->
            Unexpected = ordsets:subtract(Observed, Expected),
            Error =
                {conflicting_local_macro_whitelist, FormId,
                 whitelist_conflict_detail(
                   Expected, Observed, Unexpected, ordsets:new())},
            do([ traverse ||
                   astranaut_traverse:error(Error),
                   return(skip)
               ]);
        false ->
            astranaut_traverse:return(skip)
    end;
verify_observed_local_macro(#{mode := collect}, _FA, _IsNew, _Observed) ->
    astranaut_traverse:return(expand).

observe_macro_return([], _Opts) ->
    astranaut_traverse:return(expand);
observe_macro_return(_ReturnObserved,
                     #{local_macro_whitelist := disabled}) ->
    astranaut_traverse:return(expand);
observe_macro_return(ReturnObserved,
                     #{local_macro_whitelist := Control}) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           Observed = ordsets:union(
                        ReturnObserved,
                        maps:get(observed_local_macro_whitelist, State)),
           astranaut_traverse:put(
             State#{observed_local_macro_whitelist => Observed}),
           verify_macro_return(Control, ReturnObserved, Observed)
       ]).

verify_macro_return(
  #{mode := verify, form_id := FormId, expected := Expected},
  ReturnObserved, Observed) ->
    case ordsets:subtract(ReturnObserved, Expected) of
        [] ->
            astranaut_traverse:return(expand);
        _ ->
            Unexpected = ordsets:subtract(Observed, Expected),
            Error =
                {conflicting_local_macro_whitelist, FormId,
                 whitelist_conflict_detail(
                   Expected, Observed, Unexpected, ordsets:new())},
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
             {Node1, ReturnObserved} <-
                 process_macro_return(Node, Macro, Opts),
             Decision <- observe_macro_return(ReturnObserved, Opts),
             format_node(Node1, Macro),
             case Decision of
                 expand -> Success(Node1);
                 skip -> return(Node1)
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
        Class:Exception?CAPTURE_STACKTRACE ->
            StackTraces1 =
                lists:takewhile(
                  fun({M, F, A, _Pos}) ->
                          {M, F, A} =/=
                              {?MODULE, invoke_macro_function, 1};
                     (_Stack) ->
                          false
                  end, ?GET_STACKTRACE),
            Error = macro_exception(
                      Arguments, Class, Exception, StackTraces1, Macro),
            astranaut_traverse:fail(Error)
    end.

process_macro_return(Return, Macro, Opts) ->
    ValidateOpts =
        #{record_defs => maps:get(forms, Opts, []), fail => collect},
    CollectLocalMacros =
        maps:get(local_macro_whitelist, Opts, disabled) =/= disabled,
    Module = maps:get(module, Opts, uniform),
    MacroMap = maps:get(macro_map, Opts, #{}),
    do([ traverse ||
           Attr <- astranaut_traverse:ask(),
           RenameContext <- macro_return_rename_context(Macro, Opts),
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
                                     Node, RenameContext,
                                     maps:get(pos, Macro)),
                           collect_return_local_macro(
                             Module, Node1, MacroMap, CollectLocalMacros)
                   end, Return,
                   #{traverse => post,
                     validate => input,
                     attr => Attr,
                     validate_opts => ValidateOpts})),
           {Return1, ReturnObserved} <-
               astranaut_traverse:scoped_state_run(
                 ordsets:new(),
                 astranaut_traverse:fail_on_error(ProcessReturn)),
           commit_macro_return_counter(Opts),
           return({Return1, ReturnObserved})
       ]).

collect_return_local_macro(_Module, Node, _MacroMap, false) ->
    astranaut_traverse:return(Node);
collect_return_local_macro(Module, Node, MacroMap, true) ->
    case call_find_macro(Module, Node, MacroMap) of
        {ok, #{macro_source := local_macro,
               function := Function, arity := Arity}} ->
            do([ traverse ||
                   astranaut_traverse:modify(
                     fun(Observed) ->
                             ordsets:add_element(
                               {Function, Arity}, Observed)
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
  Function, Arguments, Pos, AttributeMacroMap, CallAst) ->
    case maps:find(Function, AttributeMacroMap) of
        {ok, MacroMap} ->
            resolve_macro_target(
              Function, Arguments, Pos, MacroMap, CallAst);
        error ->
            not_macro
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
    pos := Pos, call_ast := CallAst},
  #{inject_forms := InjectForms}) ->
    Macro1 = inject_attrs(Macro0, InjectForms),
    Macro2 = Macro1#{pos => Pos, call_ast => CallAst},
    GroupedArguments = group_arguments(RawArguments, Macro2),
    Arguments = append_attrs(GroupedArguments, Macro2),
    Macro2#{arguments => Arguments}.

attribute_macro_map(MacroMap) ->
    AttributeMap =
        maps:fold(
          fun({_Function, Arity}, #{as_attr := Attr} = Macro, Acc) ->
                  maps:put({Attr, Arity}, Macro, Acc);
             (_Key, _Macro, Acc) ->
                  Acc
          end, #{}, MacroMap),
    maps:fold(
      fun({Name, Arity}, Macro, Acc) ->
              MacroNameMap = maps:get(Name, Acc, #{}),
              MacroNameMap1 = maps:put(
                                {Name, Arity}, Macro, MacroNameMap),
              maps:put(Name, MacroNameMap1, Acc)
      end, #{}, AttributeMap).

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

inject_macro_attributes(MacroMap, Forms) ->
    maps:map(
      fun(_MacroKey, Macro) -> inject_attrs(Macro, Forms) end,
      MacroMap).

inject_attrs(#{inject_attrs := true} = Options, Forms) ->
    inject_attrs(Options#{inject_attrs => []}, Forms);
inject_attrs(#{inject_attrs := Attr} = Options, Forms)
  when is_atom(Attr) ->
    inject_attrs(Options#{inject_attrs => [Attr]}, Forms);
inject_attrs(
  #{inject_attrs := Attrs, file := File, local_module := Module} = Options,
  Forms)
  when is_list(Attrs) ->
    AttributesMap =
        lists:foldl(
          fun(module, Acc) ->
                  Acc;
             (file, Acc) ->
                  Acc;
             (pos, Acc) ->
                  Acc;
             (Attr, Acc) ->
                  Attributes =
                      astranaut_lib:analyze_forms_attributes(Attr, Forms),
                  maps:put(Attr, Attributes, Acc)
          end, maps:new(), Attrs),
    Options#{attributes =>
                 maps:merge(
                   #{file => File, module => Module}, AttributesMap)};
inject_attrs(#{} = Options, _Forms) ->
    Options.

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

find_function_macro_callers(Forms, MacroMap) ->
    case maps:size(MacroMap) of
        0 ->
            ordsets:new();
        _ ->
            lists:foldl(
              fun({function, _Pos, Function, Arity, Clauses}, Acc) ->
                      case has_macro_call(Clauses, MacroMap) of
                          true ->
                              ordsets:add_element(
                                {function, Function, Arity}, Acc);
                          false ->
                              Acc
                      end;
                 (_Form, Acc) ->
                      Acc
              end, ordsets:new(), Forms)
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

should_transform_function(_Function, _Arity, all) ->
    true;
should_transform_function(Function, Arity, {except, Functions}) ->
    not ordsets:is_element({Function, Arity}, Functions);
should_transform_function(Function, Arity, TransformFunctions) ->
    ordsets:is_element(
      {function, Function, Arity}, TransformFunctions).

macro_return_rename_context(Macro,
                            #{rename_quoted_variables := true}) ->
    do([ traverse ||
           State <- astranaut_traverse:get(),
           Counter = macro_return_counter(State),
           return({macro_name_str(Macro), integer_to_list(Counter)})
       ]);
macro_return_rename_context(_Macro, #{}) ->
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

macro_name_str(#{module := Module, function := _Function,
                 arity := _Arity}) ->
    atom_to_list(Module).

update_macro_return_node(Node, RenameContext, Pos) ->
    Node1 = rename_quoted_variable_node(Node, RenameContext),
    Node2 = replace_pos_zero_node(Node1, Pos),
    astranaut_syntax:revert(Node2).

rename_quoted_variable_node(
  {var, Pos, VarName} = Var, {MacroNameStr, CounterStr}) ->
    case split_varname(atom_to_list(VarName)) of
        [Head, MacroNameStr1] when MacroNameStr =:= MacroNameStr1 ->
            VarName1 = list_to_atom(
                         Head ++ "@" ++ MacroNameStr ++ "_" ++ CounterStr),
            {var, Pos, VarName1};
        _ ->
            Var
    end;
rename_quoted_variable_node(Node, _RenameContext) ->
    Node.

replace_pos_zero_node(Node, 0) ->
    Node;
replace_pos_zero_node(Node, Pos) ->
    case astranaut_syntax:get_pos(Node) of
        0 -> astranaut_syntax:set_pos(Node, Pos);
        _ -> Node
    end.

split_varname(String) ->
    case lists:splitwith(fun(Char) -> Char =/= $@ end, String) of
        {Head, [$@ | Tail]} -> [Head, Tail];
        {Head, []} -> [Head]
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
