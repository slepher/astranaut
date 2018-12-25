%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_monad).

%% API
-export([new/0]).
-export([run/2]).
-export([bind/2, then/2, return/1]).
-export([fail/1]).
-export([get/0, put/1, state/1]).
-export([warning/1, warnings/1, error/1, errors/1]).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    astranaut_monad_state_t:state_t(new_1()).

new_1() ->
    astranaut_monad_error_t:error_t(new_2()).

new_2() ->
    astranaut_monad_writer_t:writer_t(new_3()).

new_3() ->
    astranaut_monad_writer_t:writer_t(new_4()).

new_4() ->
    astranaut_monad_identity.

run(M0, State) ->
    M1 = astranaut_monad_state_t:run(M0, State),
    M2 = astranaut_monad_error_t:run(M1),
    M3 = astranaut_monad_writer_t:run(M2),
    M4 = astranaut_monad_writer_t:run(M3),
    case astranaut_monad_identity:run(M4) of
        {{{ok, {A, NState}}, Warnings}, Errors} ->
            {ok, {A, NState}, Errors, Warnings};
        {{{error, Reason}, Warnings}, Errors} ->
            {error, [Reason|Errors], Warnings}
    end.

bind(MA, KMB) ->
    astranaut_monad:bind(MA, KMB, new()).

then(MA, MB) ->
    astranaut_monad:then(MA, MB, new()).

return(A) ->
    astranaut_monad:return(A, new()).

fail(E) ->
    astranaut_monad:fail(E, new()).

get() ->
    astranaut_monad:get(new()).

put(S) ->
    astranaut_monad:put(S, new()).

state(F) ->
    astranaut_monad:state(F, new()).

warning(Warning) ->
    warning([Warning]).

warnings(Warnings) ->
    astranaut_monad:tell(Warnings, new()).

error(Error) ->
    errors([Error]).

errors(Errors) ->
    M2 = astranaut_monad:lift_tell(Errors, new_2()),
    M1 = astranaut_monad:lift(M2, new_1()),
    astranaut_monad:lift(M1, new()).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
