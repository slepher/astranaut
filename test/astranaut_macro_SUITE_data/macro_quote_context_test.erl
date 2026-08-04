%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2026, Chen Slepher
%%% @doc Quote context macro fixture consumers.
%%% @end
%%% Created : 04 Aug 2026 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_quote_context_test).

-include("quote.hrl").
-include("macro.hrl").

-define(MACRO_MODULE, macro_quote_context_example).

%% API
-export([test_hygienic/0]).
-export([test_no_context_capture/0]).
-export([test_named_fun/0]).
-export([test_no_context_named_fun/0]).
-export([test_same_context/0]).
-export([test_different_context/0]).
-export([test_unquote_identity/0]).
-export([test_local_hygienic/0]).

-import_macro(?MACRO_MODULE).

-local_macro([local_hygienic/1]).

%%%===================================================================
%%% API
%%%===================================================================
test_hygienic() ->
    Temp = 10,
    ?MACRO_MODULE:hygienic_macro(20),
    Temp.

test_no_context_capture() ->
    ?MACRO_MODULE:no_context_capture_macro(20),
    Shared.

test_named_fun() ->
    ?MACRO_MODULE:named_fun_macro()(5).

test_no_context_named_fun() ->
    ?MACRO_MODULE:no_context_named_fun()(5).

test_same_context() ->
    ?MACRO_MODULE:same_context_macro(42).

test_different_context() ->
    ?MACRO_MODULE:different_context_macro(1, 2).

test_unquote_identity() ->
    CallerVar = 5,
    {MacroVal, CallerVal} =
        ?MACRO_MODULE:unquote_identity_macro(CallerVar),
    {MacroVal, CallerVal}.

test_local_hygienic() ->
    LocalTemp = 10,
    local_hygienic(20),
    LocalTemp.

local_hygienic(Value) ->
    quote(
      begin
          LocalTemp = unquote(Value),
          LocalTemp
      end).
