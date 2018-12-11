%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_monad_state_t).

-export([state_t/1]).
-export([run/2]).
-export([bind/3, return/2]).
-export([fail/2]).
-export([lift/2]).
-export([get/1, put/2, state/2]).

state_t(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, State}, S)  -> 
    State(S).

bind(STA, KSTB, {?MODULE, IM}) ->
    state_t(
      fun(S) ->
              astranaut_monad:bind(
                run(STA, S),
                fun({A, NS}) ->
                        run(KSTB(A), NS)
                end, IM)
      end).

return(A, {?MODULE, _} = ST) -> 
    state(fun (S) -> {A, S} end, ST).

lift(MA, {?MODULE, IM}) ->
    state_t(
      fun(S) ->
              astranaut_monad:lift_m(fun(A) -> {A, S} end, MA, IM)
      end).

get({?MODULE, _IM} = ST) ->
    state(fun(S) -> {S, S} end, ST).

put(S, {?MODULE, _IM} = ST) ->
    state(fun(_) -> {ok, S} end, ST).

state(F, {?MODULE, IM}) ->
    state_t(fun (S) -> astranaut_monad:return(F(S), IM) end).

fail(E, {?MODULE, IM} = ST) ->
    lift(astranaut_monad:fail(E, IM), ST).
