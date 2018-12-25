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
-export([tell/2]).

%%%===================================================================
%%% API
%%%===================================================================
error_t(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, Inner}) ->
    Inner.

bind({?MODULE, ETA}, KETB, {?MODULE, IM}) ->
    error_t(
      astranaut_monad:bind(
        ETA,
        fun(EA) ->
                case EA of
                    {error, _Err}    -> astranaut_monad:return(EA, IM);
                    {ok,  A}         -> run(KETB(A))
                end
        end, IM)).

return(A, {?MODULE, IM}) ->
    error_t(astranaut_monad:return({ok, A}, IM)).

fail(E, {?MODULE, IM}) ->
    error_t(astranaut_monad:return({error, E}, IM)).

lift(MA, {?MODULE, IM}) ->
    error_t(astranaut_monad:lift_m(fun(A) -> {ok, A} end, MA, IM)).

get(ET) ->
    astranaut_monad:lift_get(ET).

put(S, ET) ->
    astranaut_monad:lift_put(S, ET).

state(F, ET) ->
    astranaut_monad:lift_state(F, ET).

tell(Ms, ET) ->
    astranaut_monad:lift_tell(Ms, ET).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
