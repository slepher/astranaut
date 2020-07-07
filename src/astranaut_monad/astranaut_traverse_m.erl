%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_m).

-compile({no_auto_import, [error/1]}).
-erlando_future_behaviour(monad).
-export_type([astranaut_traverse_m/2]).

-define(STATE, astranaut_traverse_m_state).

-opaque astranaut_traverse_m(S, A) :: 
          #{'__struct__' => ?MODULE, inner => astranaut_traverse_m_inner(S, A)}.

-type astranaut_traverse_m_inner(S, A) :: 
        fun((astranaut_traverse_m_error:formatter(), S, astranaut_traverse_m_error:astranaut_traverse_m_error()) ->
                   astranaut_traverse_m_state(S, A)).

-type astranaut_traverse_m_state(S, A) :: 
        #{'__struct__' => astranaut_traverse_m_state, 
          return => A,
          state => S, 
          error => astranaut_traverse_m_error:astranaut_traverse_m_error()}.

%% API
-export([new/0, new/1, new_state/2, new_state/3, run/3, run/4]).
-export([bind/2, return/1]).
-export(['>>='/3, return/2]).
-export([fail/2]).
-export([state/1]).
-export([get/0, put/1, modify/1]).
-export([with_formatter/2]).
-export([tell/1]).
-export([warning/1, warnings/1, error/1, errors/1]).
-export([update_file/1, eof/0, update_line/1]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    ?MODULE.

new(Inner) when is_function(Inner, 3) ->
    #{'__struct__' => ?MODULE, inner => Inner}.

new_state(Return, State) ->
    new_state(Return, State, astranaut_traverse_m_error:new()).

new_state(Return, State, Error) ->
    #{'__struct__' => ?STATE,
      return => Return,
      state => State,
      error => Error}.

run(#{'__struct__' := ?MODULE} = MA, Formatter, State) ->
    run(MA, Formatter, State, astranaut_traverse_m_error:new()).

run(#{'__struct__' := ?MODULE, inner := Inner}, Formatter, State, Error) ->
    Inner(Formatter, State, Error).

bind(MA, KMB) ->
    Inner = 
        fun(Formatter, State, Error) ->
                #{'__struct__' := ?STATE, return := A, state := State1, error := Error1} = 
                    run(MA, Formatter, State, Error),
                #{'__struct__' := ?STATE, return := B, state := State2, error := Error2} =
                    run(KMB(A), Formatter, State1, Error1),
                new_state(B, State2, Error2)
        end,
    new(Inner).

return(A) ->
    Inner = 
        fun(_Formatter, State, Error) ->
                new_state(A, State, Error)
        end,
    new(Inner).

'>>='(MA, KMB, ?MODULE) ->
    bind(MA, KMB).

return(A, ?MODULE) ->
    return(A).

fail(E, ?MODULE) ->
    error(E).

state(F) ->
    Inner = 
        fun(_Formatter, State0, Error) ->
                {A, State1} = F(State0),
                new_state(A, State1, Error)
        end,
    new(Inner).

modify(F) ->
    state(fun(State) -> State1 = F(State), {ok, State1} end).

get() ->
    state(fun(State) -> {State, State} end).

put(State) ->
    state(fun(_State) -> {ok, State} end).

with_formatter(Formatter, MA) ->
    Inner = 
        fun(_Formatter0,  State, Error) ->
                run(MA, Formatter, State, Error)
        end,
    new(Inner).

tell(Error) ->
    Inner = 
        fun(_Formatter, State, Error0) ->
                Error1 = astranaut_traverse_m_error:merge(Error0, Error),
                new_state(ok, State, Error1)
        end,
    new(Inner).

warning(Warning) ->
    warnings([Warning]).

warnings(Warnings) ->
    ErrorState0 = astranaut_traverse_m_error:new(),
    ErrorState1 = astranaut_traverse_m_error:warnings(Warnings, ErrorState0),
    tell(ErrorState1).

error(Error) ->
    errors([Error]).

errors(Errors) ->
    ErrorState0 = astranaut_traverse_m_error:new(),
    ErrorState1 = astranaut_traverse_m_error:errors(Errors, ErrorState0),
    tell(ErrorState1).

update_file(File) ->
    ErrorState = astranaut_traverse_m_error:new_file(File),
    tell(ErrorState).

eof() ->
    update_file(undefined).

update_line(Line) ->
    Inner = 
        fun(Formatter, State, Error0) ->
                Error1 = astranaut_traverse_m_error:update_line(Line, Formatter, Error0),
                new_state(ok, State, Error1)
        end,
    new(Inner).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
