%%%-------------------------------------------------------------------
%%% Import alias force_override test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_import_force_override_test).

-include("quote.hrl").
-include("macro.hrl").

-export([same_name_call/0]).

-import_macro(macro_uniform_a).
-import_macro(macro_uniform_b).

-use_macro({macro_uniform_a, to_a/1, [{alias, same_name}]}).
-use_macro({macro_uniform_b, to_b/1, [{alias, same_name}, force_override]}).

same_name_call() ->
    same_name(ok).

