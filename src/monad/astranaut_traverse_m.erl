%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_m).

%%%===================================================================
%%% macros
%%%===================================================================
-include("astranaut_struct_name.hrl").
-define(STATE_OK, astranaut_traverse_m_state_ok).
-define(STATE_FAIL, astranaut_traverse_m_state_fail).

%%%===================================================================
%%% erlando attributes
%%%===================================================================

-erlando_type(?MODULE).
-erlando_future_behaviour(monad).

-compile({no_auto_import, [error/1, get/0, put/1]}).

%%%===================================================================
%%% types
%%%===================================================================
-export_type([astranaut_traverse_m/2]).

-opaque astranaut_traverse_m(S, A) :: 
          #{?STRUCT_KEY => ?TRAVERSE_M, inner => astranaut_traverse_m_inner(S, A)}.

-type astranaut_traverse_m_inner(S, A) :: 
        fun((astranaut_error_state:formatter(), S, astranaut_error_ctx:astranaut_error_ctx()) ->
                   astranaut_traverse_m_state(S, A)).

-type astranaut_traverse_m_state(S, A) :: 
        #{?STRUCT_KEY => ?STATE_OK,
          return => A,
          state => S, 
          ctx => astranaut_error_ctx:astranaut_error_ctx(),
          error => astranaut_error_state:astranaut_error_state()} |
        #{?STRUCT_KEY => ?STATE_FAIL,
          state => S, 
          ctx => astranaut_error_ctx:astranaut_error_ctx(),
          error => astranaut_error_state:astranaut_error_state()}.

-type formatter() :: module().
-type line() :: integer().

%%%===================================================================
%%% API
%%%===================================================================
-export([astranaut_traverse_m/1, to_monad/1, to_monad/2]).
-export([convertable_struct/1]).
-export([run/3, eval/3, exec/3]).
-export([bind/2, then/2, return/1]).
-export(['>>='/3, return/2]).
-export([fail/1, fail/2, fails/1]).
-export([fail_on_error/1, sequence_either/1]).
-export([state/1]).
-export([with_error/2]).
-export([get/0, put/1, modify/1]).
-export([with_formatter/2]).
-export([modify_ctx/1]).
-export([tell/1]).
-export([warning/1, warnings/1, formatted_warnings/1, error/1, errors/1, formatted_errors/1]).
-export([update_file/1, eof/0, update_line/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec astranaut_traverse_m(astranaut_walk_return:astranaut_walk_return(S, A) |
                           astranaut_base_m:astranaut_base_m(A) |
                           astranaut_traverse_m_state(S, A) |
                           astranaut_return_m:astranaut_return_m(A)) ->
    astranaut_traverse_m(S, A).
astranaut_traverse_m(#{?STRUCT_KEY := ?WALK_RETURN} = Map) ->
    Inner =
        fun(_Formatter, State, Ctx0) ->
                State1 = maps:get(state, Map, State),
                #{errors := Errors, warnings := Warnings} = Map,
                Ctx1 = astranaut_error_ctx:append(Errors, Warnings, Ctx0),
                case Map of
                    #{return := Return} ->
                        Return1 = update_return_continue(Return, Map),
                        state_ok(Return1, State1, Ctx1);
                    #{} ->
                        state_fail(State1, Ctx1)
                end
        end,
    new(Inner);
astranaut_traverse_m(#{?STRUCT_KEY := ?BASE_M, return := Return, errors := Errors, warnings := Warnings}) ->
    Inner =
        fun(_Formatter, State, Ctx0) ->
                Ctx1 = astranaut_error_ctx:append_errors(Errors, Ctx0),
                Ctx2 = astranaut_error_ctx:append_warnings(Warnings, Ctx1),
                state_ok(Return, State, Ctx2)
        end,
    new(Inner);
astranaut_traverse_m(#{?STRUCT_KEY := ?RETURN_OK, return := Return, error := Error1}) ->
    Inner =
        fun(_Formatter, State, #{file := File} = Ctx) ->
                Error2 = astranaut_error_state:update_file(File, Error1),
                state_ok(Return, State, Ctx, Error2)
        end,
    new(Inner);
astranaut_traverse_m(#{?STRUCT_KEY := ?RETURN_FAIL, error := Error1}) ->
    Inner =
        fun(_Formatter, State, #{file := File} = Ctx) ->
                Error2 = astranaut_error_state:update_file(File, Error1),
                state_fail(State, Ctx, Error2)
        end,
    new(Inner);
astranaut_traverse_m(#{?STRUCT_KEY := ?TRAVERSE_M} = MA) ->
    MA.

convertable_struct(#{?STRUCT_KEY := Key}) ->
    convertable_struct_key(Key);
convertable_struct(_Other) ->
    false.

-spec to_monad(term()) -> astranaut_traverse_m(term(), term()).
to_monad(A) ->
    to_monad(ok, A).

-spec to_monad(A, term()) -> astranaut_traverse_m(term(), A).
to_monad(A, Return) ->
    Return1 = 
        case convertable_struct(Return) of
            true ->
                Return;
            false ->
                case astranaut_walk_return:to_map(A, Return) of
                    {ok, StructBase} ->
                        astranaut_walk_return:new(StructBase);
                    error ->
                        astranaut_walk_return:new(#{return => Return})
                end
        end,
    astranaut_traverse_m:astranaut_traverse_m(Return1).

-spec new(astranaut_traverse_m_inner(S, A)) -> astranaut_traverse_m(S, A).
new(Inner) when is_function(Inner, 3) ->
    #{?STRUCT_KEY => ?TRAVERSE_M, inner => Inner}.

-spec state_ok(A, S, astranaut_error_ctx:astranaut_error_ctx())
              -> astranaut_traverse_m(S, A).
state_ok(Return, State, Ctx) ->
    state_ok(Return, State, Ctx, astranaut_error_state:new(Ctx)).

-spec state_ok(A, S, astranaut_error_ctx:astranaut_error_ctx(),
               astranaut_error_state:astranaut_error_state())
              -> astranaut_traverse_m(S, A).
state_ok(Return, State, #{?STRUCT_KEY := ?ERROR_CTX} = Ctx, #{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    #{?STRUCT_KEY => ?STATE_OK,
      return => Return,
      state => State,
      ctx => Ctx,
      error => Error}.

-spec state_fail(S, astranaut_error_ctx:astranaut_error_ctx())
                -> astranaut_traverse_m(S, any()).
state_fail(State, #{?STRUCT_KEY := ?ERROR_CTX} = Ctx) ->
    state_fail(State, Ctx, astranaut_error_state:new(Ctx)).

-spec state_fail(S, 
                 astranaut_error_ctx:astranaut_error_ctx(),
                 astranaut_error_state:astranaut_error_state())
                -> astranaut_traverse_m(S, any()).
state_fail(State,
           #{?STRUCT_KEY := ?ERROR_CTX} = Ctx,
           #{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    #{?STRUCT_KEY => ?STATE_FAIL, state => State, ctx => Ctx, error => Error}.

-spec run(astranaut_traverse_m(S, A), formatter(), S) -> astranaut_base_m:astranaut_base_m({A, S}).
run(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    case run_0(MA, Formatter, State, astranaut_error_ctx:new()) of
        #{?STRUCT_KEY := ?STATE_OK, return := Return, state := State1, error := Error} ->
            astranaut_return_m:return_ok({Return, State1}, Error);
        #{?STRUCT_KEY := ?STATE_FAIL, error := Error} ->
            astranaut_return_m:return_fail(Error)
    end.

-spec eval(astranaut_traverse_m(S, A), formatter(), S) -> astranaut_base_m:astranaut_base_m(A).
eval(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    astranaut_monad:lift_m(fun({A, _State}) -> A end, run(MA, Formatter, State), astranaut_return_m).

-spec exec(astranaut_traverse_m(S, _A), formatter(), S) -> astranaut_base_m:astranaut_base_m(S).
exec(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    astranaut_monad:lift_m(fun({_A, State1}) -> State1 end, run(MA, Formatter, State), astranaut_return_m).

-spec bind(astranaut_traverse_m(S, A), fun((A) -> astranaut_traverse_m(S, B))) -> astranaut_traverse_m(S, B).
bind(MA, KMB) ->
    Inner = 
        fun(Formatter, State, Ctx0) ->
                with_state_ok(
                  run_0(MA, Formatter, State, Ctx0),
                  fun(#{return := A, state := State1, ctx := Ctx1, error := Error1}) ->
                          #{state := State2, error := Error2, ctx := Ctx2} = MB =
                              run_0(KMB(A), Formatter, State1, Ctx1),
                          Error3 = astranaut_error_state:merge(Error1, Error2),
                          case MB of
                              #{?STRUCT_KEY := ?STATE_OK, return := B} ->
                                  state_ok(B, State2, Ctx2, Error3);
                              #{?STRUCT_KEY := ?STATE_FAIL} ->
                                  state_fail(State2, Ctx2, Error3)
                          end
                  end)
        end,
    new(Inner).

-spec then(astranaut_traverse_m(S, _A), astranaut_traverse_m(S, B)) -> astranaut_traverse_m(S, B).
then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

-spec return(A) -> astranaut_traverse_m(_S, A).
return(A) ->
    Inner = 
        fun(_Formatter, State, Ctx) ->
                state_ok(A, State, Ctx)
        end,
    new(Inner).

-spec '>>='(astranaut_traverse_m(S, A), fun((A) -> astranaut_traverse_m(S, B)), ?TRAVERSE_M) -> astranaut_traverse_m(S, B).
'>>='(MA, KMB, ?TRAVERSE_M) ->
    bind(MA, KMB).

-spec return(A, ?TRAVERSE_M) -> astranaut_traverse_m(_S, A).
return(A, ?TRAVERSE_M) ->
    return(A).

-spec fail_on_error(astranaut_traverse_m(S, A)) -> astranaut_traverse_m(S, A).
fail_on_error(MA) ->
    Inner =
        fun(Formatter, State0, Ctx0) ->
                with_state_ok(
                  run_0(MA, Formatter, State0, Ctx0),
                  fun(#{state := State1, ctx := Ctx1, error := Error} = StateOk) ->
                          case astranaut_error_state:is_empty_error(Error) of
                              true ->
                                  StateOk;
                              false ->
                                  state_fail(State1, Ctx1, Error)
                          end
                  end)
        end,
    new(Inner).

-spec sequence_either([astranaut_traverse_m(S, A)]) -> astranaut_traverse_m(S, [A]).
sequence_either([MA|MAs]) ->
    Inner =
        fun(Formatter, State0, Ctx0) ->
                #{state := State1, ctx := Ctx1, error := Error1} = MState1 = 
                    run_0(MA, Formatter, State0, Ctx0),
                #{state := State2, ctx := Ctx2, error := Error2} = MState2 = 
                    run_0(sequence_either(MAs), Formatter, State1, Ctx1),
                Error3 = astranaut_error_state:merge(Error1, Error2),
                case {MState1, MState2} of
                    {#{?STRUCT_KEY := ?STATE_OK, return := A}, #{?STRUCT_KEY := ?STATE_OK, return := As}} ->
                        state_ok([A|As], State2, Ctx2, Error3);
                    _ ->
                        state_fail(State2, Ctx2, Error3)
                end
        end,
    new(Inner);
sequence_either([]) ->
    astranaut_traverse_m:return([]).

-spec fail(_E) -> astranaut_traverse_m(_S, _A).
fail(E) ->
    fails([E]).

-spec fail(_E, ?TRAVERSE_M) -> astranaut_traverse_m(_S, _A).
fail(E, ?TRAVERSE_M) ->
    fails([E]).

-spec fails([_E]) -> astranaut_traverse_m(_S, _A).
fails(Es) ->
    Inner =
        fun(_Formatter, State, Ctx) ->
                Ctx1 = astranaut_error_ctx:append_errors(Es, Ctx),
                state_fail(State, Ctx1)
        end,
    new(Inner).
%%%===================================================================
%%% state related functions.
%%%===================================================================
-spec state(fun((S) -> {A, S})) -> astranaut_traverse_m(S, A).
state(F) ->
    Inner = 
        fun(_Formatter, State0, Ctx) ->
                {A, State1} = F(State0),
                state_ok(A, State1, Ctx)
        end,
    new(Inner).

-spec modify(fun((S) -> S)) -> astranaut_traverse_m(S, ok).
modify(F) ->
    state(fun(State) -> State1 = F(State), {ok, State1} end).

-spec get() -> astranaut_traverse_m(S, S).
get() ->
    state(fun(State) -> {State, State} end).

-spec put(S) -> astranaut_traverse_m(S, ok).
put(State) ->
    state(fun(_State) -> {ok, State} end).

%%%===================================================================
%%% error_state related functions
%%%===================================================================

-spec with_error(fun((astranaut_error_state:astranaut_error_state())
                     -> astranaut_error_state:astranaut_error_state()),
                 astranaut_traverse_m(S, A))
                -> astranaut_traverse_m(S, A).
with_error(F, MA) ->
    Inner = 
        fun(Formatter, State, Ctx) ->
                #{error := Error1} = MState = run_0(MA, Formatter, State, Ctx),
                Error2 = F(Error1),
                case MState of
                    #{?STRUCT_KEY := ?STATE_OK, return := Return, state := State1} ->
                        state_ok(Return, State1, Ctx, Error2);
                    #{?STRUCT_KEY := ?STATE_FAIL, state := State1} ->
                        state_fail(State1, Ctx, Error2)
                end
        end,
    new(Inner).

-spec with_formatter(fun((formatter()) -> formatter()), astranaut_traverse_m(S, A)) -> astranaut_traverse_m(S, A).
with_formatter(Formatter, MA) ->
    Inner = 
        fun(_Formatter0, State, Ctx) ->
                  run_0(MA, Formatter, State, Ctx)
        end,
    new(Inner).

-spec modify_ctx(fun((astranaut_error_ctx:astranaut_error_ctx())
                     -> astranaut_error_ctx:astranaut_error_ctx()))
              -> astranaut_traverse_m(_S, _A).
modify_ctx(F) ->
    Inner = 
        fun(_Formatter, State, Ctx) ->
                Ctx1 = F(Ctx),
                state_ok(ok, State, Ctx1)
        end,
    new(Inner).

-spec warning(term()) -> astranaut_traverse_m(_S, _A).
warning(Warning) ->
    warnings([Warning]).

-spec warnings([term()]) -> astranaut_traverse_m(_S, _A).
warnings(Warnings) ->
    modify_ctx(
      fun(Ctx) ->
              astranaut_error_ctx:append_warnings(Warnings, Ctx)
      end).

-spec formatted_warnings([{line(), formatter(), term()}]) -> astranaut_traverse_m(_S, _A).
formatted_warnings(Warnings) ->
    ErrorState = astranaut_error_state:new(),
    ErrorState1 = astranaut_error_state:append_warnings(Warnings, ErrorState),
    tell(ErrorState1).

-spec error(term()) -> astranaut_traverse_m(_S, _A).
error(Error) ->
    errors([Error]).

-spec errors([term()]) -> astranaut_traverse_m(_S, _A).
errors(Errors) ->
    modify_ctx(
      fun(Ctx) ->
              astranaut_error_ctx:append_errors(Errors, Ctx)
      end).

-spec formatted_errors([{line(), formatter(), term()}]) -> astranaut_traverse_m(_S, _A).
formatted_errors(Errors) ->
    ErrorState = astranaut_error_state:new(),
    ErrorState1 = astranaut_error_state:append_errors(Errors, ErrorState),
    tell(ErrorState1).

-spec update_file(file:filename()) -> astranaut_traverse_m(_S, _A).
update_file(File) ->
    modify_ctx(
      fun(Ctx) ->
              astranaut_error_ctx:update_file(File, Ctx)
      end).

-spec eof() -> astranaut_traverse_m(_S, _A).
eof() ->
    update_file(undefined).

-spec update_line(line(), astranaut_traverse_m(S, A)) -> astranaut_traverse_m(S, A).
update_line(Line, MA) ->
    Inner = 
        fun(Formatter, State, #{?STRUCT_KEY := ?ERROR_CTX} = Ctx0) ->
                #{error := Error0, ctx := Ctx1} = MState = run_0(MA, Formatter, State, Ctx0), 
                case Ctx1 of
                    #{?STRUCT_KEY := ?ERROR_CTX, errors := [], warnings := []} ->
                        MState;
                    #{?STRUCT_KEY := ?ERROR_CTX} ->
                        Error1 = astranaut_error_state:update_ctx(Line, Formatter, Ctx1, Error0),
                        Ctx2 = Ctx1#{errors => [], warnings => []},
                        MState#{error => Error1, ctx => Ctx2}
                end
        end,
    new(Inner).

-spec tell(astranaut_error_state:astranaut_error_state()) -> astranaut_traverse_m(_S, _A).
tell(Error) ->
    Inner = 
        fun(_Formatter, State, Ctx) ->
                state_ok(ok, State, Ctx, Error)
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
update_return_continue(Return, #{continue := true}) ->
    {continue, Return};
update_return_continue(Return, #{}) ->
    Return.

run_0(#{?STRUCT_KEY := ?TRAVERSE_M, inner := Inner},
      Formatter, State,
      #{?STRUCT_KEY := ?ERROR_CTX} = Ctx) ->
    Inner(Formatter, State, Ctx).

with_state_ok(#{'__struct__' := ?STATE_OK} = State, Fun) ->
    Fun(State);
with_state_ok(#{'__struct__' := ?STATE_FAIL} = State, _Fun) ->
    State.

convertable_struct_key(?RETURN_OK) ->
    true;
convertable_struct_key(?RETURN_FAIL) ->
    true;
convertable_struct_key(?WALK_RETURN) ->
    true;
convertable_struct_key(?TRAVERSE_M) ->
    true;
convertable_struct_key(?BASE_M) ->
    true;
convertable_struct_key(_) ->
    false.

