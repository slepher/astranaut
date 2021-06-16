%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(quote_example).

-include_lib("astranaut/include/quote.hrl").

%% API
-compile(export_all).
-compile(nowarn_export_all).
%%%===================================================================
%%% API
%%%===================================================================
quote_atom() ->
    quote(ok).

quote_atom(Atom) ->
    quote(_A@Atom).
  
quote_integer() ->
    quote(10).

quote_tuple() ->
    quote({hello, world}).

quote_unquote(Ast) ->
    quote({ok, unquote(Ast)}).

quote_binding(Ast) ->
    quote({ok, _@Ast}).

quote_atom_binding(Atom) ->
    quote({ok, _A@Atom}).

quote_unquote_splicing_1(Ast1, Ast2) ->
    quote({ok, {hello, unquote_splicing([Ast1, Ast2]), world}}).

quote_unquote_splicing_2(Ast1, Ast2) ->
    quote({ok, [hello, unquote_splicing([Ast1, Ast2]), world]}).

quote_unquote_splicing_mix(Ast1, Ast2) ->
    AstList = [Ast1, Ast2],
    Fun1 = 
        quote(
          fun(unquote_splicing = AstList) ->
                  List = [hello, unquote_splicing(AstList), world],
                  Tuple = {hello, unquote_splicing(AstList), world},
                  {ok, List, Tuple}
          end),
    Fun2 = 
        quote(
          fun(unquote = Ast1, Two) ->
                  {error, unquote(Ast1), Two}
          end),
    astranaut_lib:merge_clauses([Fun1, Fun2]).

quote_match_pattern(Ast) ->
    quote(_A@Hello(_@Foo, _L@World)) = Ast,
    quote({_A@Hello, _@Foo, _L@World}).

quote_function_pattern(quote = {hello, _A@World = World2} = C) ->
    quote({ok, {hello2, _A@World, _@World2, _@C}});
quote_function_pattern(Ast) ->
    quote({error, unquote(Ast)}).

quote_case_pattern(Ast) ->
    case Ast of
        quote(_V@Function(_@Argument)) ->
            quote({ok, _V@Function(unquote(Argument) + 1)});
        quote(_@Module:_@Function(_L@Arguments)) ->
            quote({ok, {_@Module, _@Function, _L@Arguments}});
        _Other ->
            quote({error, unquote(Ast)})
    end.

quote_code() ->
    quote_code("test_fun()").

quote_line_1(Ast) ->
    Line = erl_syntax:get_pos(Ast),
    quote({hello, unquote(Ast)}, Line).

quote_line_2(Ast) ->
    Pos = erl_syntax:get_pos(Ast),
    Line = erl_anno:line(Pos),
    Ast1 = quote({hello, unquote(Ast)}, Line + 3),
    quote({ok, unquote(Ast1)}, #{line => Line + 2}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
