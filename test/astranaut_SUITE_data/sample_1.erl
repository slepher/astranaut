%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_1).

-compile({parse_transform, sample_transformer_1}).

%% API
-export([warning_0/0, error_0/0]).

-baseline(mark_base).
-mark(mark_error_0).
-mark(mark_0).
%%%===================================================================
%%% API
%%%===================================================================
-baseline(function_base).
error_0() ->
    mark_error_1.

warning_0() ->
    mark_1.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================