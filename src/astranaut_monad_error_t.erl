%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_monad_error_t).

%% API
-export([error_t/1]).
-export([run/1]).
-export([bind/3, return/2]).
-export([fail/2]).
-export([lift/2]).
-export([get/1, put/2, state/2]).


%%%===================================================================
%%% API
%%%===================================================================
error_t(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, Inner}) ->
    Inner.

bind({?MODULE, ETA}, {?MODULE, KETB}, {?MODULE, IM}) ->
    error_t(
      astranaut_monad:bind(
        ETA,
        fun(EA) ->
                case EA of
                    {error, _Err}    -> astranaut_monad:return(EA, IM);
                    {ok,  A}         -> KETB(A)
                end
        end, IM)).

return(A, {?MODULE, IM}) ->
    error_t(astranaut_monad:return({ok, A}, IM)).

fail(E, {?MODULE, IM}) ->
    error_t(astranaut_monad:return({error, E}, IM)).

lift(MA, {?MODULE, IM}) ->
    error_t(astranaut_monad:lift_m(fun(A) -> {ok, A} end, MA, IM)).

get({?MODULE, IM} = ET) ->
    lift(astranaut_monad:get(IM), ET).

put(S, {?MODULE, IM} = ET) ->
    lift(astranaut_monad:put(S, IM), ET).

state(F, {?MODULE, IM} = ET) ->
    lift(astranaut_monad:state(F, IM), ET).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
