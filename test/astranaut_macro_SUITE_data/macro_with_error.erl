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

-macro_options([debug_module_ast]).

-export([format_error/1]).

-baseline(yep).

-import_macro({invalid_macro_tuple}).
-import_macro(non_exists_module).
-use_macro({unimported_macro_module, [macro_1/1]}).
-local_macro([exception_error/0]).
-local_macro([return_error/0, undefined_macro_0/0]).
-local_macro([undefined_macro_1/0]).
-export_macro([return_error/0, undefined_macro_2/0]).
-export_macro([undefined_macro_3/0]).
-import_macro(macro_example).

error_macro_1() ->
    exception_error().

error_macro_2(hello) ->
    return_error();
error_macro_2(_Other) ->
    at_least_one_clause.

exception_error() ->
    erlang:error(foo).

return_error() ->
    {error, bar}.

max_depth_error() ->
    macro_example:recursive_macro(6).

format_error(bar) ->
    "oops, bar";
format_error(Error) ->
    astranaut_macro:format_error(Error).
