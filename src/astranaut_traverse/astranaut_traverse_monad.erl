%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_monad).

-erlando_type(?MODULE).

-export_type([astranaut_traverse_monad/0]).

-opaque astranaut_traverse_monad() :: {?MODULE, any()}.

-erlando_future_behaviour(monad).

%% API
-export([new/0]).
-export([run/2, eval/2, exec/2]).
-export(['>>='/3, return/2, fail/2]).
-export([bind/2, then/2, left_then/2, return/1]).
-export([fail/1]).
-export([lift_m/2, sequence_m/1, r_sequence_m/1, map_m/2]).
-export([get/0, put/1, modify/1, state/1, bind_state/1]).
-export([update_file/1, merge_error_state/1]).
-export([warning/1, warnings/1, error/1, errors/1]).

-compile({no_auto_import, [get/0, put/1]}).

%%%===================================================================
%%% API
%%%===================================================================


new() ->
    astranaut_traverse_monad.

new_1() ->
    astranaut_monad_state_t:state_t(new_2()).

new_2() ->
    astranaut_monad_error_t:error_t(new_3()).

new_3() ->
    astranaut_monad_state_t:state_t(new_4()).

new_4() ->
    astranaut_monad_identity.

new(M) ->
    {?MODULE, M}.

run({?MODULE, M}) ->
    M.

init_error_state() ->
    #{errors => [], warnings => [], file_errors => #{}, file_warnings => #{}, file => undefined}.

run(M0, State) ->
    M1 = run(M0),
    M2 = astranaut_monad_state_t:run(M1, State),
    M3 = astranaut_monad_error_t:run(M2),
    M4 = astranaut_monad_state_t:run(M3, init_error_state()),
    case astranaut_monad_identity:run(M4) of
        {{ok, {A, NState}}, ErrorState} ->
            {ok, {A, NState}, ErrorState};
        {{error, Reason}, #{errors := Errors} = ErrorState} ->
            {error, ErrorState#{errors := [Reason|Errors]}}
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

sequence_m(MAs) ->
    astranaut_monad:sequence_m(MAs, new()).

r_sequence_m(MAs) ->
    lift_m(fun lists:reverse/1, sequence_m(lists:reverse(MAs))).

map_m(F, MAs) ->
    astranaut_monad:map_m(F, MAs, new()).

bind(MA, KMB) ->
    MA0 = run(MA),
    KMB0 = 
        fun(A) ->
                run(KMB(A))
        end,
    MC = astranaut_monad:bind(MA0, KMB0, new_1()),
    new(MC).

'>>='(MA, KMB, astranaut_traverse_monad) ->
    bind(MA, KMB).

then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

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
    MA = astranaut_monad:return(A, new_1()),
    new(MA).

return(A, astranaut_traverse_monad) ->
    return(A).

fail(E) ->
    ME = astranaut_monad:fail(E, new_1()),
    new(ME).

fail(E, astranaut_traverse_monad) ->
    fail(E).

update_file(File) ->
    modify_error_state(
      fun(State) ->
              astranaut_traverse_error_state:update_file(File, State)
      end).

merge_error_state(ErrorState) ->
    modify_error_state(
      fun(State) ->
              astranaut_traverse_error_state:merge(State, ErrorState)
      end).

modify_error_state(F) ->
    M2 = astranaut_monad:modify(F, new_3()),
    M1 = astranaut_monad:lift(M2, new_2()),
    M0 = astranaut_monad:lift(M1, new_1()),
    new(M0).

get() ->
    MG = astranaut_monad:get(new_1()),
    new(MG).

put(S) ->
    MP = astranaut_monad:put(S, new_1()),
    new(MP).

modify(F) ->
    MM = astranaut_monad:modify(F, new_1()),
    new(MM).

state(F) ->
    MS = astranaut_monad:state(F, new_1()),
    new(MS).

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
    modify_error_state(
      fun(State) ->
              astranaut_traverse_error_state:warnings(Warnings, State)
      end).

error(Error) ->
    errors([Error]).

errors(Errors) ->
    modify_error_state(
      fun(State) ->
              astranaut_traverse_error_state:errors(Errors, State)
      end).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
