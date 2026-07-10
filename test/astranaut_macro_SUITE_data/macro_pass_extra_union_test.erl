%%%-------------------------------------------------------------------
%%% extra_functions from multiple macro declarations are unioned.
%%%-------------------------------------------------------------------
-module(macro_pass_extra_union_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value_a/0, value_b/0]).

-local_macro({[entry_a/1], [{extra_functions, [{helper_a, 1}]}]}).
-local_macro({[entry_b/1], [{extra_functions, [{helper_b, 1}]}]}).

value_a() ->
    entry_a(ok).

value_b() ->
    entry_b(ok).

entry_a(Ast) ->
    Fun = fun helper_a/1,
    Fun(Ast).

entry_b(Ast) ->
    Fun = fun helper_b/1,
    Fun(Ast).

helper_a(Ast) ->
    quote({extra_union_a, unquote(Ast)}).

helper_b(Ast) ->
    quote({extra_union_b, unquote(Ast)}).
