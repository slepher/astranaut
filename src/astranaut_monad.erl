%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_monad).

%% API
-export([bind/3, return/2]).
-export([lift_m/3, sequence_m/2, map_m/3]).
-export([lift_m/4, sequence_m/3, map_m/4]).
-export([identity_bind/0, identity_return/0]).
-export([maybe_bind/0, maybe_return/0]).
-export([either_bind/0, either_return/0]).
-export([reader_bind/1, reader_return/1, reader_lift/0, reader_ask/1, reader_local/0, reader_state/1]).
-export([state_bind/1, state_return/1, state_lift/2, state_ask/2, state_local/1, state_state/1]).
-export([writer_bind/3, writer_return/2, writer_lift/3]).
-export([writer_ask/2, writer_local/1, writer_state/2]).
-export([writer_writer/1, writer_listen/2]).
-export([monad_bind/1, monad_return/1, monad_lift/1]).
-export([monad_state/1, monad_ask/1, monad_local/1]).
-export([monad_updated_writer/1, monad_updated_listen/1]).
-export([mappend/1, mempty/1]).

-export_type([maybe/1, either/2, state/2]).
-export_type([monad/2, monad_bind/1, monad_return/1]).
-export_type([monad_ask/1, monad_local/1, monad_state/1, monad_updated_writer/1, monad_updated_listen/1, monad_lift/1]).

-type maybe(A) :: {just, A} | nothing.
-type either(E, A) :: {left, E} | {right, A}.
-type state(S, A) :: fun((S) -> {A, S}).

-type monad(_M, _A) :: any().
-type monad_bind(M) :: fun((monad(M, A), fun((A) -> monad(M, B))) -> monad(M, B)).
-type monad_return(M) :: fun((A) -> monad(M, A)).
-type monad_ask(M) :: fun(() -> monad(M, _A)).
-type monad_local(M) :: fun((fun((R) -> R), monad(M, A)) -> monad(M, A)).
-type monad_state(M) :: fun((fun((S) -> {A, S})) -> monad(M, A)).
-type monad_updated_writer(M) :: fun(({A, _W}) -> monad(M, A)).
-type monad_updated_listen(M) :: fun((monad(M, A)) -> monad(M, {A, _W})).
-type monad_lift(T) :: fun((monad(M, A)) -> monad({T, M}, A)).
%%%===================================================================
%%% API
%%%===================================================================
bind(ok, KMB, _Monad) ->
    KMB(ok);
bind(MA, KMB, Monad) ->
    Bind = monad_bind(Monad),
    Bind(MA, KMB).

return(A, Monad) ->
    Return = monad_return(Monad),
    Return(A).

-spec lift_m(fun((A) -> B), monad(M, A), M) -> monad(M, B).
lift_m(F, MA, Monad) ->
    lift_m(F, MA, monad_bind(Monad), monad_return(Monad)).

-spec lift_m(fun((A) -> B), monad(M, A), monad_bind(M), monad_return(M)) -> monad(M, B).
lift_m(F, MA, Bind, Return) ->
    Bind(MA, fun(A) -> Return(F(A)) end).

-spec map_m(fun((A) -> monad(M, B)), [A], M) -> monad(M, [B]).
map_m(F, As, Monad) ->
    map_m(F, As, monad_bind(Monad), monad_return(Monad)).

map_m(F, [A|As], Bind, Return) ->
    Bind(
      F(A),
      fun(A1) ->
              Bind(
                map_m(F, As, Bind, Return),
                fun(As1) ->
                        Return([A1|As1])
                end)
      end);
map_m(_F, [], _Bind, Return) ->
    Return([]).

-spec sequence_m([monad(M, A)], M) -> monad(M, [A]).
sequence_m(MAs, Monad) ->
    sequence_m(MAs, monad_bind(Monad), monad_return(Monad)).

-spec sequence_m([monad(M, A)], monad_bind(M), monad_return(M)) -> monad(M, [A]).
sequence_m(MAs, Bind, Return) ->
    map_m(fun(A) -> A end, MAs, Bind, Return).

-spec identity_bind() -> fun((A, fun((A) -> B)) -> B).
identity_bind() ->
    fun(A, KIB) ->
            KIB(A)
    end.

-spec identity_return() -> fun((A) -> A).
identity_return() ->
    fun(A) -> A end.

-spec maybe_bind() -> fun((maybe(A), fun((A) -> maybe(B))) -> maybe(B)).
maybe_bind() ->
    fun(MA, AFB) ->
            case MA of
                {just, A} ->
                    AFB(A);
                nothing ->
                    nothing
            end
    end.

-spec maybe_return() -> fun((A) -> maybe(A)).
maybe_return() ->
    fun(A) ->
            {just, A}
    end.

-spec either_bind() -> fun((either(E, A), fun((A) -> either(E, B))) -> either(E, B)).
either_bind() ->
  fun({left, E}, _AFB) ->
          {left, E};
     ({right, A}, AFB) ->
          AFB(A)
  end.

-spec either_return() -> fun((A) -> either(_E, A)).
either_return() ->
    fun(A) ->
            {right, A}
    end.

reader_bind(Bind) ->
    fun(RA, KRB) ->
            fun(R) ->
                    Bind(
                      RA(R),
                      fun(A) ->
                              (KRB(A))(R)
                      end)
            end
    end.

reader_return(Return) ->
    fun(A) ->
            fun(_R) ->
                    Return(A)
            end
    end.

reader_lift() ->
    fun(MA) ->
            fun(_R) ->
                    MA
            end
    end.

reader_ask(Return) ->
    fun() ->
            fun(R) ->
                    Return(R)
            end
    end.

reader_local() ->
    fun(F, RA) ->
            fun(R) ->
                    RA(F(R))
            end
    end.

reader_state(State) ->
    Lift = reader_lift(),
    fun(F) ->
            Lift(State(F))
    end.

state_bind(Bind) ->
    fun(SA, KSB) ->
            fun(S) ->
                    Bind(
                      SA(S),
                      fun({A, S1}) ->
                              SB = KSB(A),
                              SB(S1)
                      end)
            end
    end.

state_return(Return) ->
    fun(A) ->
            fun(S) -> Return({A, S}) end
    end.

state_lift(Bind, Return) ->
    fun(MA) ->
            fun(S) -> lift_m(fun(A) -> {A, S} end, MA, Bind, Return) end
    end.

state_state(Return) ->
    fun(F) ->
            fun(S) ->
                    Return(F(S))
            end
    end.

state_ask(Lift, Ask) ->
    Lift(Ask()).

state_local(Local) ->
    fun(F, SA) ->
            fun(S) ->
                    Local(F, SA(S))
            end
    end.

writer_bind(Bind, Return, Mappend) ->
    fun(WA, KWB) ->
            Bind(
              WA,
              fun({A, W1}) ->
                      Bind(
                        KWB(A),
                        fun({B, W2}) ->
                                W3 = Mappend(W1, W2),
                                Return({B, W3})
                        end)
              end)
    end.

writer_return(Return, Mempty) ->
    fun(A) ->
            Return({A, Mempty()})
    end.

writer_lift(Bind, Return, Mempty) ->
    fun(MA) ->
            lift_m(fun(A) -> {A, Mempty()} end, MA, Bind, Return)
    end.

writer_ask(Lift, Ask) ->
    fun() ->
            Lift(Ask())
    end.

writer_local(Local) ->
    fun(F, WA) ->
            Local(F, WA)
    end.

writer_state(Lift, State) ->
    fun(F) ->
            Lift(State(F))
    end.

writer_writer(Return) ->
    fun({A, W}) ->
            Return({A, W})
    end.

writer_listen(Bind, Return) ->
    fun(WA) ->
            lift_m(fun({A, Ws}) -> {{A, Ws}, Ws} end, WA, Bind, Return)
    end.

-spec monad_bind(M) -> monad_bind(M).
monad_bind(reader) ->
    reader_bind(identity_bind());
monad_bind({reader, M}) ->
    Bind = monad_bind(M),
    reader_bind(Bind);
monad_bind(state) ->
    state_bind(identity_bind());
monad_bind({state, M}) ->
    Bind = monad_bind(M),
    state_bind(Bind);
monad_bind(either) ->
    either_bind();
monad_bind(maybe) ->
    maybe_bind();
monad_bind(identity) ->
    identity_bind();
monad_bind(traverse) ->
    fun astranaut_traverse:bind/2;
monad_bind(return) ->
    fun astranaut_return:bind/2.

-spec monad_return(M) -> monad_return(M).
monad_return(reader) ->
    reader_return(identity_return());
monad_return({reader, M}) ->
    Return = monad_return(M),
    reader_return(Return);
monad_return(state) ->
    state_return(identity_return());
monad_return({state, M}) ->
    Return = monad_return(M),
    state_return(Return);
monad_return(either) ->
    either_return();
monad_return(maybe) ->
    maybe_return();
monad_return(identity) ->
    identity_return();
monad_return(traverse) ->
    fun astranaut_traverse:return/1;
monad_return(return) ->
    fun astranaut_return:return/1.

monad_lift({reader, _M}) ->
    reader_lift();
monad_lift({state, M}) ->
    Bind = monad_bind(M),
    Return = monad_return(M),
    state_lift(Bind, Return).

monad_state(reader) ->
    undefined;
monad_state({reader, M}) ->
    case monad_state(M) of
        undefined ->
            undefined;
        State ->
            reader_state(State)
    end;
monad_state(state) ->
    state_state(identity_return());
monad_state({state, M}) ->
    Return = monad_return(M),
    state_state(Return);
monad_state(identity) ->
    undefined;
monad_state(maybe) ->
    undefined;
monad_state(either) ->
    undefined;
monad_state(traverse) ->
    fun astranaut_traverse:state/1.

monad_ask(reader) ->
    reader_ask(identity_return());
monad_ask({reader, M}) ->
    Return = monad_return(M),
    reader_ask(Return);
monad_ask(state) ->
    undefined;
monad_ask({state, M}) ->
    case monad_ask(M) of
        undefined ->
            undefined;
        Ask ->
            Lift = monad_lift({state, M}),
            state_ask(Lift, Ask)
    end;
monad_ask(identity) ->
    undefined;
monad_ask(maybe) ->
    undefined;
monad_ask(either) ->
    undefined;
monad_ask(traverse) ->
    fun astranaut_traverse:ask/0;
monad_ask(return) ->
    undefined.

monad_local(reader) ->
    reader_local();
monad_local({reader, _M}) ->
    reader_local();
monad_local({state, M}) ->
    case monad_local(M) of
        undefined ->
            undefined;
        Local ->
            state_local(Local)
    end;
monad_local(state) ->
    undefined;
monad_local(identity) ->
    undefined;
monad_local(maybe) ->
    undefined;
monad_local(either) ->
    undefined;
monad_local(traverse) ->
    fun astranaut_traverse:local/2;
monad_local(return) ->
    undefined.

monad_updated_writer(traverse) ->
    fun astranaut_traverse:writer_updated/1;
monad_updated_writer(_) ->
    undefined.

monad_updated_listen(traverse) ->
    fun astranaut_traverse:listen_updated/1;
monad_updated_listen(_) ->
    undefined.

mappend('or') ->
    fun(A, B) -> A or B end.

mempty('or') ->
    fun() -> false end.
