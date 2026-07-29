%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_disable_tco).

-include("quote.hrl").
%% API
-export([parse_transform/2, format_error/1]).
%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opt) ->
    RecursiveGroups = recursive_groups(Forms),
    Opts = #{traverse => all},
    astranaut:smap_with_state(
      fun(Node, Variables, Attr) ->
              walk(Node, Variables, Attr, RecursiveGroups)
      end, sets:new(), Forms, Opts).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk({function, _Pos, _Name, _Arity, _Clauses} = Function, _Variables,
     #{step := pre}, _RecursiveGroups) ->
    Opts = #{traverse => pre},
    Variables = astranaut:sreduce(fun walk_variables/3, sets:new(), Function, Opts),
    {Function, Variables};
walk({function, Pos, Name, Arity, Clauses}, Variables, #{step := post},
     RecursiveGroups) ->
    Caller = {function, Name, Arity},
    {NClauses, NVariables} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {{function, Pos, Name, Arity, NClauses}, NVariables};
walk({'fun', Pos, {clauses, Clauses}}, Variables, #{step := post},
     RecursiveGroups) ->
    {NClauses, NVariables} =
        walk_clauses(Clauses, anonymous, Variables, RecursiveGroups),
    {{'fun', Pos, {clauses, NClauses}}, NVariables};
walk({named_fun, Pos, Name, Clauses}, Variables, #{step := post},
     RecursiveGroups) ->
    Caller = {named_fun, Name, clause_arity(Clauses)},
    {NClauses, NVariables} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {{named_fun, Pos, Name, NClauses}, NVariables};
walk(Node, Variables, _Attr, _RecursiveGroups) ->
    {Node, Variables}.

walk_variables({var, _Pos, Name}, Variables, _Attr) ->
    sets:add_element(Name, Variables);
walk_variables(_Node, Variables, _Attr) ->
    Variables.

walk_clauses(Clauses, Caller, Variables, RecursiveGroups) ->
    {NClauses, NVariables} =
        lists:foldl(
          fun(Clause, {CAcc, VAcc}) ->
                  {NClause, NVAcc} =
                      walk_clause(
                        Clause, Caller, VAcc, RecursiveGroups),
                  {[NClause|CAcc], NVAcc}
          end, {[], Variables}, Clauses),
    {lists:reverse(NClauses), NVariables}.

walk_clause({clause, Pos, Patterns, Guards, Body}, Caller, Variables,
            RecursiveGroups) ->
    {NBody, NVariables} =
        walk_body(Body, Caller, Variables, RecursiveGroups),
    {{clause, Pos, Patterns, Guards, NBody}, NVariables}.

walk_body([Expression], Caller, Variables, RecursiveGroups) ->
    {NExpression, NVariables} =
        walk_tail_expression(
          Expression, Caller, Variables, RecursiveGroups),
    {[NExpression], NVariables};
walk_body([H|T], Caller, Variables, RecursiveGroups) ->
    {NT, NVariables} =
        walk_body(T, Caller, Variables, RecursiveGroups),
    {[H|NT], NVariables};
walk_body([], _Caller, Variables, _RecursiveGroups) ->
    {[], Variables}.

walk_tail_expression(
  {call, _Pos, {atom, _Pos1, Name}, Args} = Expression,
  {function, CallerName, CallerArity}, Variables, RecursiveGroups) ->
    Caller = {CallerName, CallerArity},
    Callee = {Name, length(Args)},
    case same_recursive_group(Caller, Callee, RecursiveGroups) of
        true -> {Expression, Variables};
        false -> add_try_catch(Expression, Variables)
    end;
walk_tail_expression(
  {call, _Pos, {var, _Pos1, Name}, Args} = Expression,
  {named_fun, Name, Arity}, Variables, _RecursiveGroups)
  when length(Args) =:= Arity ->
    {Expression, Variables};
walk_tail_expression(
  {call, _Pos, _Function, _Args} = Expression,
  _Caller, Variables, _RecursiveGroups) ->
    add_try_catch(Expression, Variables);
walk_tail_expression(
  {block, Pos, Body}, Caller, Variables, RecursiveGroups) ->
    {NBody, NVariables} =
        walk_body(Body, Caller, Variables, RecursiveGroups),
    {{block, Pos, NBody}, NVariables};
walk_tail_expression(
  {'case', Pos, Expression, Clauses}, Caller, Variables, RecursiveGroups) ->
    {NClauses, NVariables} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {{'case', Pos, Expression, NClauses}, NVariables};
walk_tail_expression(
  {'if', Pos, Clauses}, Caller, Variables, RecursiveGroups) ->
    {NClauses, NVariables} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {{'if', Pos, NClauses}, NVariables};
walk_tail_expression(
  {'receive', Pos, Clauses}, Caller, Variables, RecursiveGroups) ->
    {NClauses, NVariables} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {{'receive', Pos, NClauses}, NVariables};
walk_tail_expression(
  {'receive', Pos, Clauses, Timeout, TimeoutBody},
  Caller, Variables, RecursiveGroups) ->
    {NClauses, Variables1} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {NTimeoutBody, NVariables} =
        walk_body(
          TimeoutBody, Caller, Variables1, RecursiveGroups),
    {{'receive', Pos, NClauses, Timeout, NTimeoutBody}, NVariables};
walk_tail_expression(
  {'try', Pos, Body, Clauses, CatchClauses, []},
  Caller, Variables, RecursiveGroups) ->
    {NClauses, Variables1} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {NCatchClauses, NVariables} =
        walk_clauses(
          CatchClauses, Caller, Variables1, RecursiveGroups),
    {{'try', Pos, Body, NClauses, NCatchClauses, []}, NVariables};
walk_tail_expression(
  {op, Pos, Operator, Left, Right}, Caller, Variables, RecursiveGroups)
  when Operator =:= 'andalso'; Operator =:= 'orelse' ->
    {NRight, NVariables} =
        walk_tail_expression(
          Right, Caller, Variables, RecursiveGroups),
    {{op, Pos, Operator, Left, NRight}, NVariables};
walk_tail_expression(
  {'maybe', Pos, Body}, Caller, Variables, RecursiveGroups) ->
    {NBody, NVariables} =
        walk_body(Body, Caller, Variables, RecursiveGroups),
    {{'maybe', Pos, NBody}, NVariables};
walk_tail_expression(
  {'maybe', Pos, Body, Else}, Caller, Variables, RecursiveGroups) ->
    {NBody, Variables1} =
        walk_body(Body, Caller, Variables, RecursiveGroups),
    {NElse, NVariables} =
        walk_maybe_else(
          Else, Caller, Variables1, RecursiveGroups),
    {{'maybe', Pos, NBody, NElse}, NVariables};
walk_tail_expression(
  Expression, _Caller, Variables, _RecursiveGroups) ->
    {Expression, Variables}.

walk_maybe_else(
  {'else', Pos, Clauses}, Caller, Variables, RecursiveGroups) ->
    {NClauses, NVariables} =
        walk_clauses(Clauses, Caller, Variables, RecursiveGroups),
    {{'else', Pos, NClauses}, NVariables}.

clause_arity([{clause, _Pos, Patterns, _Guards, _Body}|_]) ->
    length(Patterns);
clause_arity([]) ->
    0.

same_recursive_group(Caller, Callee, RecursiveGroups) ->
    case {maps:find(Caller, RecursiveGroups),
          maps:find(Callee, RecursiveGroups)} of
        {{ok, Group}, {ok, Group}} -> true;
        _ -> false
    end.

recursive_groups(Forms) ->
    Functions =
        [{{Name, Arity}, Function} ||
            {function, _Pos, Name, Arity, _Clauses} = Function <- Forms],
    FunctionIds = sets:from_list([Id || {Id, _Function} <- Functions]),
    Graph = digraph:new(),
    try
        lists:foreach(
          fun({Id, _Function}) ->
                  digraph:add_vertex(Graph, Id)
          end, Functions),
        lists:foreach(
          fun({Caller, Function}) ->
                  lists:foreach(
                    fun(Callee) ->
                            case sets:is_element(Callee, FunctionIds) of
                                true ->
                                    digraph:add_edge(
                                      Graph, Caller, Callee);
                                false ->
                                    ok
                            end
                    end, local_calls(Function))
          end, Functions),
        lists:foldl(
          fun(Component, Acc) ->
                  lists:foldl(
                    fun(FunctionId, ComponentAcc) ->
                            maps:put(
                              FunctionId, Component, ComponentAcc)
                    end, Acc, Component)
          end, #{}, digraph_utils:strong_components(Graph))
    after
        digraph:delete(Graph)
    end.

local_calls(Node) ->
    sets:to_list(local_calls(Node, sets:new())).

local_calls({'fun', _Pos, _Definition}, Calls) ->
    Calls;
local_calls({named_fun, _Pos, _Name, _Clauses}, Calls) ->
    Calls;
local_calls({call, _Pos, {atom, _Pos1, Name}, Args}, Calls) ->
    local_calls(
      Args, sets:add_element({Name, length(Args)}, Calls));
local_calls(Tuple, Calls) when is_tuple(Tuple) ->
    local_calls(tuple_to_list(Tuple), Calls);
local_calls([Head|Tail], Calls) ->
    local_calls(Tail, local_calls(Head, Calls));
local_calls(_, Calls) ->
    Calls.

add_try_catch({call, Pos, _Fun, _Args} = Expr, Variables) ->
    Class = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Class" ++ integer_to_list(N)) end, Variables),
    Exception = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Exception" ++ integer_to_list(N)) end, Variables),
    StackTrace = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("StackTrace" ++ integer_to_list(N)) end, Variables),
    NVariables = sets:union(sets:from_list([Class, Exception, StackTrace]), Variables),
    ClassVar = {var, Pos, Class},
    ExceptionVar = {var, Pos, Exception},
    StackTraceVar = {var, Pos, StackTrace},
    Node = try_catch_node(Expr, Pos, ClassVar, ExceptionVar, StackTraceVar),
    {Node, NVariables}.

-include("otp_vsn.hrl").

-ifdef(ASTRANAUT_OTP_AT_LEAST_21).
try_catch_node(Expr, Pos, ClassVar, ExceptionVar, StackTraceVar) ->
    quote(
      try
          unquote(Expr)
      catch
          _@ClassVar:_@ExceptionVar:_@StackTraceVar ->
              erlang:raise(_@ClassVar, _@ExceptionVar, _@StackTraceVar)
      end, Pos).
-else.
try_catch_node(Expr, Pos, ClassVar, ExceptionVar, _StackTraceVar) ->
    quote(
      try
          unquote(Expr)
      catch
          _@ClassVar:_@ExceptionVar ->
              erlang:raise(_@ClassVar, _@ExceptionVar, erlang:get_stacktrace())
      end, Pos).
-endif.
