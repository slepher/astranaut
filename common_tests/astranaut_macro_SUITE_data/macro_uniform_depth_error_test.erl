%%%-------------------------------------------------------------------
%%% Expansion depth test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_depth_error_test).

-include("quote.hrl").
-include("macro.hrl").

-export([depth_error/0]).

-macro_options([{max_depth, 10}]).

-import_macro(macro_uniform_a).
-import_macro(macro_uniform_b).

-baseline(yep).

depth_error() ->
    macro_uniform_a:recurse_a(12).

