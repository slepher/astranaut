%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_validator).

%% API
-export([is_boolean/1, is_number/1]).
-export([default/2, required/1, force_required/1]).
%%%===================================================================
%%% API
%%%===================================================================
is_boolean(Bool) ->
    erlang:is_boolean(Bool).

is_number(Number) ->
    erlang:is_number(Number).

default(undefined, Default) ->
    {ok, Default};
default(Value, _Default) ->
    Value.

required(undefined) ->
    {warning, required};
required(Value) ->
    {ok, Value}.

force_required(undefined) ->
    {error, required};
force_required(Value) ->
    {ok, Value}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
