%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_with_error).

-include("quote.hrl").
-include("macro.hrl").

-export([format_error/1]).
-local_macro([exception_error/0, return_error/0]).

-baseline(yep).

error_macro_1() ->
    exception_error().

error_macro_2() ->
    return_error().

exception_error() ->
    erlang:error(foo).

return_error() ->
    {error, bar}.

format_error(bar) ->
    "oops, bar";
format_error(Error) ->
    astranaut_macro:format_error(Error).
