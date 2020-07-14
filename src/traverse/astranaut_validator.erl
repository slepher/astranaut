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
-export([boolean/1, number/1, atom/1]).
-export([list_of/2]).
-export([default/3, required/2]).
%%%===================================================================
%%% API
%%%===================================================================
boolean(Bool) ->
    erlang:is_boolean(Bool).

number(Number) ->
    erlang:is_number(Number).

atom(Atom) ->
    erlang:is_atom(Atom).

list_of(Values, Validator) ->
    list_of(Values, Validator, []).

list_of([H|T], Validator, Acc) ->
    case astranaut_options:by_validator(Validator, H, true) of
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

default(_Value, Default, false) ->
    {ok, Default};
default(Value, _Default, true) ->
    {ok, Value}.

required(_Value, false) ->
    {error, required};
required(Value, true) ->
    {ok, Value}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
