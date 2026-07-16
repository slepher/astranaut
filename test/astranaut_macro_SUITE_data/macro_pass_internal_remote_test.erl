%%%-------------------------------------------------------------------
%%% A remote imported macro can be retained as its ordinary function call.
%%%-------------------------------------------------------------------
-module(macro_pass_internal_remote_test).

-include("macro.hrl").

-export([value/0]).

-import_macro(macro_uniform_a).
-local_macro({outer/1,
              [{internal_function, [{macro_uniform_a, to_a, 1}]}]}).

value() ->
    outer(ok).

outer(Ast) ->
    macro_uniform_a:to_a(Ast).
