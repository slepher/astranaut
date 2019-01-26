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
-export([run/2, eval/2, exec/2]).
-export([bind/2, then/2, left_then/2, return/1]).
-export([fail/1]).
-export([lift_m/2, map_m/2]).
-export([get/0, put/1, modify/1, state/1, bind_state/1]).
-export([warning/1, warnings/1, error/1, errors/1]).

-compile({no_auto_import, [get/0, put/1]}).

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

eval(M0, State) ->
    astranaut_traverse:map_traverse_return(
      fun({A, _}) ->
              A
      end, run(M0, State)).

exec(M0, State) ->
    astranaut_traverse:map_traverse_return(
      fun({_, NState}) ->
              NState
      end, run(M0, State)). 

lift_m(F, MA) ->
    astranaut_monad:lift_m(F, MA, new()).

map_m(F, MAs) ->
    astranaut_monad:map_m(F, MAs, new()).

bind(MA, KMB) ->
    astranaut_monad:bind(MA, KMB, new()).

then(MA, MB) ->
    astranaut_monad:then(MA, MB, new()).

left_then(MA, MB) ->
    bind(
      MA, 
      fun(A) -> 
              bind(
                MB,
                fun(_B) ->
                        return(A)
                end)
      end).

return(A) ->
    astranaut_monad:return(A, new()).

fail(E) ->
    astranaut_monad:fail(E, new()).

get() ->
    astranaut_monad:get(new()).

put(S) ->
    astranaut_monad:put(S, new()).

modify(F) ->
    astranaut_monad:modify(F, new()).

state(F) ->
    astranaut_monad:state(F, new()).

bind_state(F) ->
    bind(
      get(),
      fun(State1) ->
              bind(
                F(State1),
                fun(State2) ->
                        put(State2)
                end)
      end).
    
warning(Warning) ->
    warnings([Warning]).

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
