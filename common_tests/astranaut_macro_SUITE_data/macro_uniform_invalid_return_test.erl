%%%-------------------------------------------------------------------
%%% Invalid macro return test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_invalid_return_test).

-include("quote.hrl").
-include("macro.hrl").

-export([direct_invalid_return/0, generated_invalid_return/0]).

-import_macro(macro_uniform_a).

-baseline(yep).

direct_invalid_return() ->
    macro_uniform_a:invalid_return(ok).

generated_invalid_return() ->
    macro_uniform_a:gen_invalid(ok).
