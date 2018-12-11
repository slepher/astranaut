%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_monad).

-export_type([monad/0, monadic/2]).

%% API
-export([new_state/0, new_state_error/0, run_state/2, run_state_error/2]).
-export([lift_m/3, map_m/3]).
-export([bind/3, return/2]).
-export([lift/2]).
-export([fail/2]).
-export([get/1, put/2, state/2]).

-type monad()         :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().

%%%===================================================================
%%% API
%%%===================================================================
new_state() ->
    astranaut_monad_state_t:state_t(astranaut_monad_identity).

new_state_error() ->
    astranaut_monad_state_t:state_t(astranaut_monad_error_t:error_t(astranaut_monad_identity)).

run_state(MonadM, State) ->
    astranaut_monad_identity:run(astranaut_monad_state_t:run(MonadM, State)).

run_state_error(MonadM, State) ->
    astranaut_monad_identity:run(astranaut_monad_error_t:run(astranaut_monad_state_t:run(MonadM, State))).

lift_m(F, X, Monad) ->
    bind(X,
         fun(A) ->
                 return(F(A), Monad)
         end, Monad).

map_m(F, [X|Xs], Monad) ->
    bind(F(X),
         fun(A) ->
                 bind(map_m(F, Xs, Monad),
                      fun(As) ->
                              return([A|As], Monad)
                      end, Monad)
         end, Monad);
map_m(_F, [], Monad) ->
    return([], Monad).

%% same as monad:bind/3
bind(X, F, {T, _IM} = M) ->
    T:bind(X, F, M);
bind(X, F, M) ->
    M:bind(X, F).

return(A, {T, _IM} = M) ->
    T:return(A, M);
return(A, M) ->
    M:return(A).

lift(MA, {T, _IM} = M) ->
    T:lift(MA, M).

fail(MA, {T, _IM} = M) ->
    T:fail(MA, M).

get({T, _IM} = M) ->
    T:get(M).

put(S, {T, _IM} = M) ->
    T:put(S, M).

state(F, {T, _IM} = M) ->
    T:state(F, M).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
