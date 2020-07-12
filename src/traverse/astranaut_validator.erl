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
-export([is_boolean/1, is_number/1, is_atom/1]).
-export([list_of/2]).
-export([default/2, required/1, force_required/1]).
%%%===================================================================
%%% API
%%%===================================================================
is_boolean(Bool) ->
    erlang:is_boolean(Bool).

is_number(Number) ->
    erlang:is_number(Number).

is_atom(Atom) ->
    erlang:is_atom(Atom).

default(undefined, Default) ->
    {ok, Default};
default(Value, _Default) ->
    Value.

list_of(Values, Validator) ->
    list_of(Values, Validator, []).

list_of([H|T], Validator, Acc) ->
    case astranaut_options:by_validator(Validator, H) of
        {ok, H1} ->
            list_of(T, Validator, [H1|Acc]);
        {error, Reason} ->
            {error, Reason};
        {warning, Reason} ->
            {warning, Reason}
    end;
list_of([], _Validator, Acc) ->
    {ok, lists:reverse(Acc)};
list_of(_Other, _Validator, _Acc) ->
    {error, invalid_list}.

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
