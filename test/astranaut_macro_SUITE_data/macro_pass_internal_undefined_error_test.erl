-module(macro_pass_internal_undefined_error_test).

-include("macro.hrl").

-export([dummy/0]).

-baseline(yep).
-local_macro({outer/1, [{internal_function, [helper/1]}]}).

outer(Ast) ->
    helper(Ast).

helper(Ast) ->
    Ast.

dummy() ->
    ok.
