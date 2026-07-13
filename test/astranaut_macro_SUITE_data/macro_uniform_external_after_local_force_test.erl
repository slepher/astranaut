%%%-------------------------------------------------------------------
%%% A later external use with force_override wins over an earlier local macro.
%%%-------------------------------------------------------------------
-module(macro_uniform_external_after_local_force_test).

-include("quote.hrl").
-include("macro.hrl").

-export([same_name_call/0]).

-local_macro([same_name/1]).

-import_macro(macro_uniform_a).
-use_macro({macro_uniform_a, to_a/1, [{alias, same_name}, force_override]}).

same_name_call() ->
    same_name(ok).

same_name(Ast) ->
    quote({local_same_name, unquote(Ast)}).
