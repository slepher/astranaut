%%%-------------------------------------------------------------------
%%% Invalid local macro return test module for uniform macro expansion.
%%%-------------------------------------------------------------------
-module(macro_uniform_invalid_local_return_test).

-include("quote.hrl").
-include("macro.hrl").

-export([local_invalid_return/0]).

-local_macro([bad_local/0]).

-baseline(yep).

local_invalid_return() ->
    bad_local().

bad_local() ->
    {not_ast}.

