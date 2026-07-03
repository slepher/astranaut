%%%-------------------------------------------------------------------
%%% Import alias override error test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_import_override_error_test).

-include("quote.hrl").
-include("macro.hrl").

-export([same_name_call/0]).

-import_macro(macro_uniform_a).
-import_macro(macro_uniform_b).

-use_macro({macro_uniform_a, to_a/1, [{alias, same_name}]}).

-baseline(yep).

-use_macro({macro_uniform_b, to_b/1, [{alias, same_name}]}).

same_name_call() ->
    same_name(ok).

