%%%-------------------------------------------------------------------
%%% internal_function marks a macro definition helper as a direct call.
%%%-------------------------------------------------------------------
-module(macro_pass_internal_direct_test).

-include("quote.hrl").
-include("macro.hrl").

-export([value/0]).

-local_macro({[outer/1], [{internal_function, [{inner, 1}]}]}).

value() ->
    outer(ok).

outer(Ast) ->
    inner(Ast).

inner(Ast) ->
    quote({internal_direct, unquote(Ast)}).
