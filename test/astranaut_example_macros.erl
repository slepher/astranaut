%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_example_macros).

-include("quote.hrl").

%% API
-export([test_macro/0, exported_macro/0]).

%%%===================================================================
%%% API
%%%===================================================================
test_macro() ->
    astranaut:function(
      test,
      quote(
        fun() ->
                ok
        end)).

exported_macro() ->
    quote(
      fun() ->
              ok
      end).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
