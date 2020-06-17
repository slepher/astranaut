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
-export([lift_m/3, sequence_m/2, map_m/3, foldl_m/4]).
-export([bind/3, then/3, return/2]).
-export([lift/2]).
-export([fail/2]).
-export([get/1, put/2, modify/2, state/2]).
-export([tell/2]).
-export([lift_fail/2]).
-export([lift_get/1, lift_put/2, lift_state/2]).
-export([lift_tell/2]).

-type monad()         :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().

%%%===================================================================
%%% API
%%%===================================================================
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

sequence_m(Xs, Monad) ->
    map_m(fun(A) -> A end, Xs, Monad).

foldl_m(F, Acc, [X|Xs], Monad) ->
    bind(
      F(X, Acc),
      fun(Acc1) ->
              foldl_m(F, Acc1, Xs, Monad)
      end, Monad);
foldl_m(_F, Acc, [], Monad) ->
    astranaut_monad:return(Acc, Monad).

%% same as monad:bind/3
bind(X, F, {T, _IM} = M) ->
    T:bind(X, F, M);
bind(X, F, M) ->
    M:bind(X, F).

then(X, Y, M) ->
    bind(X, fun(_) -> Y end, M).

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

modify(F, M) ->
    state(
      fun(S) ->
              {ok, F(S)}
      end, M).

state(F, {T, _IM} = M) ->
    T:state(F, M).

tell(Logs, {T, _IM} = M) ->
    T:tell(Logs, M).

lift_get({_, IM} = WT) ->
    lift(astranaut_monad:get(IM), WT).

lift_put(S, {_, IM} = WT) ->
    lift(astranaut_monad:put(S, IM), WT).

lift_state(F, {_, IM} = WT) ->
    lift(astranaut_monad:state(F, IM), WT).

lift_fail(E, {_, IM} = MT) ->
    lift(fail(E, IM), MT).

lift_tell(Ms, {_, IM} = MT) ->
    lift(tell(Ms, IM), MT).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
