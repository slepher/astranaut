%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_m).

-include("astranaut_do.hrl").

-compile({no_auto_import, [error/1]}).
-erlando_future_behaviour(monad).

-export_type([astranaut_traverse_m/2]).
-define(STATE_OK, astranaut_traverse_m_state).
-define(STATE_FAIL, astranaut_traverse_m_state_fail).
-define(TRAVERSE_FUN_RETURN, traverse_fun_return).

-opaque astranaut_traverse_m(S, A) :: 
          #{'__struct__' => ?MODULE, inner => astranaut_traverse_m_inner(S, A)}.

-type astranaut_traverse_m_inner(S, A) :: 
        fun((astranaut_traverse_m_error:formatter(), S, astranaut_traverse_m_error:astranaut_traverse_m_error()) ->
                   astranaut_traverse_m_state(S, A)).

-type astranaut_traverse_m_state(S, A) :: 
        #{'__struct__' => ?STATE_OK,
          return => A,
          state => S, 
          error => astranaut_traverse_m_error:astranaut_traverse_m_error()}.

%% API
-export([astranaut_traverse_m/1, traverse_fun_return/1, to_monad/1, to_monad/2]).
-export([new/1, state_ok/2, state_ok/3, state_fail/2, run/3, run/4]).
-export([bind/2, return/1]).
-export(['>>='/3, return/2]).
-export([fail/1, fail/2, fails/1]).
-export([fail_on_error/1, sequence_either/1]).
-export([state/1]).
-export([get/0, put/1, modify/1]).
-export([with_formatter/2]).
-export([tell/1]).
-export([warning/1, warnings/1, error/1, errors/1]).
-export([update_file/1, eof/0, update_line/1]).

%%%===================================================================
%%% API
%%%===================================================================
astranaut_traverse_m(#{'__struct__' := ?TRAVERSE_FUN_RETURN} = Map) ->
    Inner =
        fun(_Formatter, State, ErrorState0) ->
                State1 = maps:get(state, Map, State),
                case Map of
                    #{return := Return, errors := Errors, warnings := Warnings} ->
                        ErrorState1 = astranaut_traverse_m_error:new(Errors, Warnings),
                        ErrorState2 = astranaut_traverse_m_error:merge(ErrorState0, ErrorState1),
                        state_ok(Return, State1, ErrorState2);
                    #{errors := Errors, warnings := Warnings} ->
                        ErrorState1 = astranaut_traverse_m_error:new(Errors, Warnings),
                        ErrorState2 = astranaut_traverse_m_error:merge(ErrorState0, ErrorState1),
                        state_fail(State1, ErrorState2)
                end
        end,
    new(Inner);
astranaut_traverse_m(#{} = Map) ->
    Map1 = traverse_fun_return(Map),
    astranaut_traverse_m(Map1).

traverse_fun_return(#{} = Map) ->
    Map1 = astranaut_base_m:up_struct_base(Map),
    default(Map1).
    
to_monad(A) ->
    to_monad(ok, A).

to_monad(A, Return) ->
    case astranaut_base_m:to_struct_base(A, Return) of
        {ok, StructBase} ->
            astranaut_traverse_m(StructBase);
        error ->
            to_monad_1(Return)
    end.

new(Inner) when is_function(Inner, 3) ->
    #{'__struct__' => ?MODULE, inner => Inner}.

state_ok(Return, State) ->
    state_ok(Return, State, astranaut_traverse_m_error:new()).

state_ok(Return, State, Error) ->
    #{'__struct__' => ?STATE_OK,
      return => Return,
      state => State,
      error => Error}.

state_fail(State, Error) ->
    #{'__struct__' => ?STATE_FAIL, state => State, error => Error}.

run(#{'__struct__' := ?MODULE} = MA, Formatter, State) ->
    run(MA, Formatter, State, astranaut_traverse_m_error:new()).

run(#{'__struct__' := ?MODULE, inner := Inner}, Formatter, State, Error) ->
    Inner(Formatter, State, Error).

bind(MA, KMB) ->
    Inner = 
        fun(Formatter, State, Error) ->
                bind_state(
                  run(MA, Formatter, State, Error),
                  fun(#{return := A, state := State1, error := Error1}) ->
                          bind_state(
                            run(KMB(A), Formatter, State1, Error1),
                            fun(#{return := B, state := State2, error := Error2}) ->
                                    state_ok(B, State2, Error2)
                            end)
                  end)
        end,
    new(Inner).

return(A) ->
    Inner = 
        fun(_Formatter, State, Error) ->
                state_ok(A, State, Error)
        end,
    new(Inner).

'>>='(MA, KMB, ?MODULE) ->
    bind(MA, KMB).

return(A, ?MODULE) ->
    return(A).

fail_on_error(MA) ->
    Inner =
        fun(Formatter, State0, Error0) ->
                case run(MA, Formatter, State0) of
                    #{'__struct__' := ?STATE_OK, return := A, state := State1, error := Error1} ->
                          IsEmptyError = astranaut_traverse_m_error:is_empty_error(Error1),
                          Error2 = astranaut_traverse_m_error:merge(Error0, Error1),
                          case IsEmptyError of
                              true ->
                                  state_ok(A, State1, Error2);
                              false ->
                                  state_fail(State1, Error2)
                          end;
                    #{'__struct__' := ?STATE_FAIL, state := State1, error := Error1} ->
                        Error2 = astranaut_traverse_m_error:merge(Error0, Error1),
                        state_fail(State1, Error2)
                  end
        end,
    new(Inner).

sequence_either([MA|MAs]) ->
    Inner =
        fun(Formatter, State0, Error0) ->
                #{state := State1, error := Error1} = MState1 = run(MA, Formatter, State0, Error0),
                #{state := State2, error := Error2} = MState2 = run(sequence_either(MAs), Formatter, State1, Error1),
                case {MState1, MState2} of
                    {#{'__struct__' := ?STATE_OK, return := A}, #{'__struct__' := ?STATE_OK, return := As}} ->
                        state_ok([A|As], State2, Error2);
                    _ ->
                        state_fail(State2, Error2)
                end
        end,
    new(Inner);
sequence_either([]) ->
    astranaut_traverse_m:return([]).

fail(E) ->
    fails([E]).

fails(Es) ->
    Inner =
        fun(_Formatter, State, Error0) ->
                Error1 = astranaut_traverse_m_error:errors(Es, Error0),
                state_fail(State, Error1)
        end,
    new(Inner).

fail(E, ?MODULE) ->
    fail(E).

state(F) ->
    Inner = 
        fun(_Formatter, State0, Error) ->
                {A, State1} = F(State0),
                state_ok(A, State1, Error)
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
        fun(Formatter0,  State, Error0) ->
                Error1 = astranaut_traverse_m_error:update_formatter(Formatter, Error0),
                bind_state(
                  run(MA, Formatter, State, Error1),
                  fun(#{return := A, state := State1, error := Error2}) ->
                          Error3 = astranaut_traverse_m_error:update_formatter(Formatter0, Error2),
                          state_ok(A, State1, Error3)
                  end)
        end,
    new(Inner).

tell(Error) ->
    Inner = 
        fun(_Formatter, State, Error0) ->
                Error1 = astranaut_traverse_m_error:merge(Error0, Error),
                state_ok(ok, State, Error1)
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
                state_ok(ok, State, Error1)
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
to_monad_1(#{'__struct__' := ?MODULE} = MA) ->
    MA;
to_monad_1(#{'__struct__' := ?TRAVERSE_FUN_RETURN} = Struct) ->
    astranaut_traverse_m(Struct);
to_monad_1(Return) ->
    astranaut_traverse_m(#{return => Return}).

default(Map) ->
    maps:merge(#{'__struct__' => ?TRAVERSE_FUN_RETURN, errors => [], warnings => []}, Map).

bind_state(#{'__struct__' := ?STATE_OK} = State, Fun) ->
    Fun(State);
bind_state(#{'__struct__' := ?STATE_FAIL} = State, _Fun) ->
    State.
