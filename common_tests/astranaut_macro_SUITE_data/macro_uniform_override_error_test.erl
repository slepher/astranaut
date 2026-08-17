%%%-------------------------------------------------------------------
%%% Override error test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_override_error_test).

-include("quote.hrl").
-include("macro.hrl").

-export([same_name_call/0]).

-import_macro(macro_uniform_a).
-use_macro({macro_uniform_a, to_a/1, [{alias, same_name}]}).

-baseline(yep).

-local_macro([same_name/1]).

same_name_call() ->
    same_name(ok).

same_name(Ast) ->
    quote({local_same_name, unquote(Ast)}).

