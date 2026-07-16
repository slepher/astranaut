%%%-------------------------------------------------------------------
%%% An imported macro alias is restored to its original remote function call
%%% when local_macro internalizes it.
%%%-------------------------------------------------------------------
-module(macro_pass_internal_alias_test).

-include("macro.hrl").

-export([value/0]).

-import_macro(macro_uniform_a).
-use_macro({macro_uniform_a, to_a/1, [{alias, direct_to_a}]}).
-local_macro({outer/1, [{internal_function, [direct_to_a/1]}]}).
-local_macro_retain([outer/1]).

value() ->
    outer(ok).

outer(Ast) ->
    direct_to_a(Ast).
