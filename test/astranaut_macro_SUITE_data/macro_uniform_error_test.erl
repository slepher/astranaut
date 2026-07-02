%%%-------------------------------------------------------------------
%%% Error handling test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_error_test).

-include("quote.hrl").
-include("macro.hrl").

-export([macro_error/0]).

-import_macro(macro_uniform_a).
-import_macro(macro_uniform_b).

-baseline(yep).

macro_error() ->
    macro_uniform_a:fail_after_b(macro_uniform_b:to_b(ok)).

