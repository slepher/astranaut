%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_disable_tco).

-compile({parse_transform, astranaut_quote}).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Ast, _Opt) ->
    astranaut_traverse:map_with_state(fun walk/3, sets:new(), Ast, #{module => ?MODULE, traverse => pre}).

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
walk({function, _Line, _Name, _Arity, _Clauses} = Function, _Variables, #{step := pre}) ->
    Variables = astranaut_traverse:reduce(fun walk_variables/3, sets:new(), Function, #{traverse => leaf, module => ?MODULE}),
    {Function, Variables};
walk({function, Line, Name, Arity, Clauses}, Variables, #{step := post}) ->
    {NClauses, NVariables} = walk_clauses(Clauses, {atom, Name}, Variables),
    {{function, Line, Name, Arity, NClauses}, NVariables};
walk({'fun', Line, {clauses, Clauses}}, Variables, #{step := post}) ->  
    {NClauses, NVariables} = walk_clauses(Clauses, undefined, Variables),
    {{'fun', Line, {clauses, NClauses}}, NVariables};
walk({named_fun, Line, Name, Clauses}, Variables, #{step := post}) ->
    {NClauses, NVariables} = walk_clauses(Clauses, {atom, Name}, Variables),
    {{named_fun, Line, Name, NClauses}, NVariables};
walk(Node, Variables, _Attr) ->
    {Node, Variables}.

walk_variables({var, _Line, Name}, Variables, _Attr) ->
    sets:add_element(Name, Variables);
walk_variables(_Node, Variables, _Attr) ->
    Variables.

walk_clauses(Clauses, Name, Variables) ->
    {NClauses, NVaraibles} = 
        lists:foldl(
          fun(Clause, {CAcc, VAcc}) ->
                  {NClause, NVAcc} = walk_clause(Clause, Name, VAcc),
                  {[NClause|CAcc], NVAcc}
          end, {[], Variables}, Clauses),
    {lists:reverse(NClauses), NVaraibles}.

walk_clause({clause, Line, Patterns, Guards, Body}, Name, Variables) ->
    {NBody, NVariables} = walk_body(Body, Name, Variables),
    {{clause, Line, Patterns, Guards, NBody}, NVariables}.

walk_body([{call, _Line, {Type, _Line1, FName}, _Args} = Rep], Name, Variables) ->
    if
        {Type, FName} == Name ->
            {[Rep], Variables};
        true ->
            {NRep, NVariables} = add_try_catch(Rep, Variables), 
            {[NRep], NVariables}
    end;
walk_body([{call, _Line, {remote, _Line1, _Module, _Function}, _Args} = Rep], _Name, Variables) ->
    {NRep, NVariables} = add_try_catch(Rep, Variables),
    {[NRep], NVariables};
walk_body([H|T], Name, Variables) ->
    {NT, NVariables} = walk_body(T, Name, Variables),
    {[H|NT], NVariables};
walk_body([], _Name, Variables) ->
    {[], Variables}.

-ifdef('OTP_RELEASE').
add_try_catch({call, Line, _Fun, _Args} = Expr, Variables) ->
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
    ClassVar = {var, Line, Class},
    ExceptionVar = {var, Line, Exception},
    StackTraceVar = {var, Line, StackTrace},
    Vars = [ClassVar, ExceptionVar, StackTraceVar],
    Node = 
        quote(
          try
              unquote(Expr)
          catch
              _@ClassVar:_@ExceptionVar:_@StackTraceVar ->
                  erlang:raise(_L@Vars)
          end, Line),
    {Node, NVariables}.
-else.
add_try_catch({call, Line, _Fun, _Args} = Expr, Variables) ->
    Class = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Class" ++ integer_to_list(N)) end, Variables),
    Exception = 
        erl_syntax_lib:new_variable_name(
          fun(N) -> list_to_atom("Exception" ++ integer_to_list(N)) end, Variables),
    NVariables = sets:union(sets:from_list([Class, Exception]), Variables),
    ClassVar = {var, Line, Class},
    ExceptionVar = {var, Line, Exception},
    Vars = [ClassVar, ExceptionVar],
    Raise = quote(erlang:raise(unquote_splicing(Vars), erlang:get_stacktrace()), Line),
    Node = 
        quote(
          try
              unquote(Expr)
          catch
              _@ClassVar:_@ExceptionVar ->
                  erlang:raise(_@ClassVar,_@ExceptionVar,erlang:get_stacktrace())
          end, Line),
    {Node, NVariables}.
-endif.
