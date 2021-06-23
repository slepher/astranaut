%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%% traverse monad for {@link astranaut:map_m/3}
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse).

%%%===================================================================
%%% macros
%%%===================================================================
-include("astranaut_struct_name.hrl").

-define(STATE_OK, astranaut_traverse_state_ok).
-define(STATE_FAIL, astranaut_traverse_state_fail).

-compile({no_auto_import, [error/1, get/0, put/1, nodes/1]}).
%%%===================================================================
%%% types
%%%===================================================================
-export_type([struct/2]).

-type struct(S, A) :: #{?STRUCT_KEY => ?TRAVERSE_M, inner => inner_type(S, A)}.

-type inner_type(S, A) :: fun((module(), S, #{}, astranaut_error:struct()) -> state_struct(S, A)).

-type state_struct(S, A) :: 
        #{?STRUCT_KEY => ?STATE_OK,
          return => A,
          updated => boolean(),
          state => S, 
          error => astranaut_error:struct()} |
        #{?STRUCT_KEY => ?STATE_FAIL,
          state => S, 
          error => astranaut_error:struct()}.

-type formatter() :: module().
-type convertable(S, A) :: astranaut:walk_return(S, A) | astranaut_return:struct(A) | state_struct(S, A).

%%%===================================================================
%%% API
%%%===================================================================
-export([astranaut_traverse/1]).
-export([convertable_struct/1]).
-export([run/4, eval/4, exec/4]).
-export([lift_m/2, map_m/2, sequence_m/1]).
-export([bind/2, then/2, return/1]).
-export([bind_without_error/2]).
-export([fail/1, fail/2, fails/1]).
-export([fail_on_error/1]).
-export([with_error/2, catch_fail/2, set_fail/1, catched_nodes/1]).
-export([ask/0, local/2]).
-export([state/1, get/0, put/1, modify/1]).
-export([with_state_attr/1]).
-export([listen_error/1, writer_updated/1, listen_updated/1, set_updated/1]).
-export([with_formatter/2]).
-export([warning/1, warnings/1, formatted_warnings/1, error/1, errors/1, formatted_errors/1]).
-export([update_file/1, eof/0, update_pos/2, update_pos/3]).

-spec astranaut_traverse(convertable(S, A)) -> struct(S, A).
astranaut_traverse(#{?STRUCT_KEY := ?WALK_RETURN} = Map) ->
    Inner =
        fun(_Formatter, File, _Attr, State0) ->
                State1 = maps:get(state, Map, State0),
                Return = maps:get(return, Map, keep),
                Errors = maps:get(errors, Map, []),
                Warnings = maps:get(warnings, Map, []),
                Error1 = astranaut_error:append_ews(Errors, Warnings, astranaut_error:new(File)),
                state_ok(#{return => Return, state => State1, error => Error1})
        end,
    new(Inner);
astranaut_traverse(#{?STRUCT_KEY := ?RETURN_OK, return := Return, error := ErrorStruct}) ->
    Inner =
        fun(_Formatter, File, _Attr, State) ->
                state_ok(#{return => Return, state => State, error => astranaut_error:update_file(File, ErrorStruct)})
        end,
    new(Inner);
astranaut_traverse(#{?STRUCT_KEY := ?RETURN_FAIL, error := ErrorStruct}) ->
    Inner =
        fun(_Formatter, File, _Attr, State) ->
                state_fail(#{state => State, error => astranaut_error:update_file(File, ErrorStruct)})
        end,
    new(Inner);
astranaut_traverse(#{?STRUCT_KEY := ?TRAVERSE_M} = MA) ->
    MA.

convertable_struct(#{?STRUCT_KEY := Key}) ->
    convertable_struct_key(Key);
convertable_struct(_Other) ->
    false.

convertable_struct_key(?RETURN_OK) ->
    true;
convertable_struct_key(?RETURN_FAIL) ->
    true;
convertable_struct_key(?WALK_RETURN) ->
    true;
convertable_struct_key(?TRAVERSE_M) ->
    true;
convertable_struct_key(_) ->
    false.

-spec new(inner_type(S, A)) -> struct(S, A).
new(Inner) when is_function(Inner, 4) ->
    #{?STRUCT_KEY => ?TRAVERSE_M, inner => Inner}.

state_ok(Map) ->
    update_m_state(#{?STRUCT_KEY => ?STATE_OK}, Map).

state_fail(Map) ->
    update_m_state(#{?STRUCT_KEY => ?STATE_FAIL}, Map).

-spec run(struct(S, A), formatter(), #{}, S) -> astranaut_return:struct({A, S}).
run(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, Attr, State) ->
    case run_0(MA, Formatter, undefined, Attr, State) of
        #{?STRUCT_KEY := ?STATE_OK, return := Return, state := State1, error := Error} ->
            astranaut_return:ok({Return, State1}, Error);
        #{?STRUCT_KEY := ?STATE_FAIL, error := Error} ->
            astranaut_return:fail(Error)
    end.

-spec eval(struct(S, A), formatter(), #{}, S) -> astranaut_return:struct(A).
eval(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, Attr, State) ->
    astranaut_return:lift_m(fun({A, _State}) -> A end, run(MA, Formatter, Attr, State)).

-spec exec(struct(S, _A), formatter(), #{}, S) -> astranaut_return:struct(S).
exec(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, Attr, State) ->
    astranaut_return:lift_m(fun({_A, State1}) -> State1 end, run(MA, Formatter, Attr, State)).

-spec lift_m(fun((A) -> B), struct(S, A)) -> struct(S, B).
lift_m(F, X) ->
    bind(X, fun(A) -> return(F(A)) end).

-spec map_m(fun((A) -> struct(S, B)), [struct(S, A)]) -> struct(S, B).
map_m(F, [X|Xs]) ->
    bind(F(X),
         fun(A) ->
                 bind(map_m(F, Xs),
                      fun(As) ->
                              return([A|As])
                      end)
         end);
map_m(_F, []) ->
    return([]).

-spec sequence_m([struct(S, A)]) -> struct(S, [A]).
sequence_m(Xs) ->
    map_m(fun(A) -> A end, Xs).

-spec bind(struct(S, A) | ok, fun((A) -> struct(S, B))) -> struct(S, B).
bind(ok, KMB) ->
    KMB(ok);
bind(MA, KMB) when is_function(KMB, 1) ->
    map_m_state_ok(
      fun(Formatter, Attr, #{return := A, state := State1,
                             updated := Updated1, error := Error1}) ->
              File = astranaut_error:file(Error1),
              MB = run_0(KMB(A), Formatter, File, Attr, State1),
              case MB of
                  #{?STRUCT_KEY := ?STATE_OK, updated := Updated2, error := Error2} ->
                      Updated3 = Updated1 or Updated2,
                      Error3 = astranaut_error:merge(Error1, Error2),
                      update_m_state(MB, #{updated => Updated3, error => Error3});
                  #{?STRUCT_KEY := ?STATE_FAIL, error := Error2} ->
                      Error3 = astranaut_error:merge(Error1, Error2),
                      update_m_state(MB, #{error => Error3})
              end
      end, MA).

-spec then(struct(S, _A), struct(S, B)) -> struct(S, B).
then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

-spec return(A) -> struct(_S, A).
return(A) ->
    Inner =
        fun(_Formatter, File, _Attr, State) ->
                state_ok(#{return => A, state => State, error => astranaut_error:new(File)})
        end,
    new(Inner).

-spec bind_without_error(struct(S, A) | ok, fun((A) -> struct(S, B))) -> struct(S, B).
bind_without_error(ok, KMB) ->
    KMB(ok);
bind_without_error(MA, KMB) ->
    bind(
      listen_error(MA),
      fun({A, ErrorStruct}) ->
              case astranaut_error:is_empty(ErrorStruct) of
                  true ->
                      KMB(A);
                  false ->
                      return(A)
              end
      end).

-spec fail_on_error(struct(S, A)) -> struct(S, A).
fail_on_error(MA) ->
    map_m_state_ok(
      fun(_Formatter, _Attr, #{state := State1, error := Error} = StateOk) ->
              case astranaut_error:is_empty_error(Error) of
                  true ->
                      StateOk;
                  false ->
                      state_fail(#{state => State1, error => Error})
              end
        end, MA).

-spec fail(_E) -> struct(_S, _A).
fail(E) ->
    fails([E]).

-spec fail(_E, ?TRAVERSE_M) -> struct(_S, _A).
fail(E, ?TRAVERSE_M) ->
    fails([E]).

-spec fails([_E]) -> struct(_S, _A).
fails(Es) ->
    Inner =
        fun(_Formatter, File, _Attr, State) ->
                Error1 = astranaut_error:append_errors(Es, astranaut_error:new(File)),
                state_fail(#{state => State, error => Error1})
        end,
    new(Inner).
%%%===================================================================
%%% state related functions.
%%%===================================================================
ask() ->
    Inner =
        fun(_Formatter, File, Attr, State) ->
                state_ok(#{return => Attr, state => State, error => astranaut_error:new(File)})
        end,
    new(Inner).

local(F, MA) ->
    Inner = 
        fun(Formatter, File, Attr, State) ->
                Attr1 = F(Attr),
                run_0(MA, Formatter, File, Attr1, State)
        end,
    new(Inner).

-spec state(fun((S) -> {A, S})) -> struct(S, A).
state(F) ->
    Inner = 
        fun(_Formatter, File, _Attr, State0) ->
                {A, State1} = F(State0),
                state_ok(#{return => A, state => State1, error => astranaut_error:new(File)})
        end,
    new(Inner).

-spec modify(fun((S) -> S)) -> struct(S, ok).
modify(F) ->
    state(fun(State) -> State1 = F(State), {ok, State1} end).

-spec get() -> struct(S, S).
get() ->
    state(fun(State) -> {State, State} end).

-spec put(S) -> struct(S, ok).
put(State) ->
    state(fun(_State) -> {ok, State} end).

-spec listen_error(struct(S, A)) -> struct(S, {A, astranaut_error:struct()}).
listen_error(MA) ->
    map_m_state_ok(
      fun(#{return := Return, error := Error} = MState) ->
              MState#{return => {Return, Error}}
      end, MA).

writer_updated({A, Updated}) ->
    Inner = 
        fun(_Formatter, File, _Attr, State) ->
                state_ok(#{return => A, state => State, updated => Updated, error => astranaut_error:new(File)})
        end,
    new(Inner).

listen_updated(MA) ->
    map_m_state_ok(
      fun(#{return := Return, updated := Updated} = MState) ->
              update_m_state(MState, #{return => {Return, Updated}})
      end, MA).

set_updated(MA) ->
    map_m_state_ok(
      fun(MState) ->
              update_m_state(MState, #{updated => true})
      end, MA).

with_state_attr(F) ->
    bind(
      get(),
      fun(State) ->
              bind(
                ask(),
                fun(Attr) ->
                        F(State, Attr)
                end)
      end).
%%%===================================================================
%%% nodes updated related functions
%%%===================================================================

%%%===================================================================
%%% error_state related functions
%%%===================================================================

-spec with_error(fun((astranaut_error:struct())
                     -> astranaut_error:struct()),
                 struct(S, A))
                -> struct(S, A).
with_error(F, MA) ->
    map_m_state(
      fun(#{error := Error1} = MState) ->
              Error2 = F(Error1),
              update_m_state(MState, #{error => Error2})
        end, MA).

catch_fail(F, MA) ->
    map_m_state(
      fun(_Formatter, _Attr, #{?STRUCT_KEY := ?STATE_OK} = StateM) ->
              StateM;
         (Formatter, Attr, #{?STRUCT_KEY := ?STATE_FAIL, state := State1, error := Error1}) ->
              File = astranaut_error:file(Error1),
              #{error := Error2} = MB = run_0(F(), Formatter, File, Attr, State1),
              Error3 = astranaut_error:merge(Error1, Error2),
              update_m_state(MB, #{error => Error3})
      end, MA).

set_fail(MA) ->
    map_m_state(
        fun(_Formatter, #{?STRUCT_KEY := ?STATE_OK, state := State, error := Error}) ->
            state_fail(#{state => State, error => Error})
        end, MA).

catched_nodes(MA) ->
    catch_fail(fun() -> return([]) end, MA).

-spec generate_error(astranaut_error:struct()) -> struct(_S, _A).
generate_error(Error) ->
    Inner = 
        fun(_Formatter, File, _Attr, State) ->
                state_ok(#{return => ok, state => State, error => astranaut_error:update_file(File, Error)})
        end,
    new(Inner).

-spec with_formatter(fun((formatter()) -> formatter()), struct(S, A)) -> struct(S, A).
with_formatter(Formatter, MA) ->
    Inner = 
        fun(_Formatter0, File, Attr, State) ->
                  run_0(MA, Formatter, File, Attr, State)
        end,
    new(Inner).

-spec warning(term()) -> struct(_S, _A).
warning(Warning) ->
    warnings([Warning]).

-spec warnings([term()]) -> struct(_S, _A).
warnings(Warnings) ->
    generate_error(astranaut_error:new_warnings(Warnings)).

-spec formatted_warnings([{erl_anno:location(), formatter(), term()}]) -> struct(_S, _A).
formatted_warnings(Warnings) ->
    generate_error(astranaut_error:new_formatted_warnings(Warnings)).

-spec error(term()) -> struct(_S, _A).
error(Error) ->
    errors([Error]).

-spec errors([term()]) -> struct(_S, _A).
errors(Errors) ->
    generate_error(astranaut_error:new_errors(Errors)).

-spec formatted_errors([{erl_anno:location(), formatter(), term()}]) -> struct(_S, _A).
formatted_errors(Errors) ->
    generate_error(astranaut_error:new_formatted_errors(Errors)).

-spec update_file(astranaut_error:compile_file()) -> struct(_S, _A).
update_file(File) ->
    Inner = 
        fun(_Formatter, _File0, _Attr, State) ->
                state_ok(#{return => ok, state => State, error => astranaut_error:new(File)})
        end,
    new(Inner).

-spec eof() -> struct(_S, _A).
eof() ->
    update_file(eof).

-spec update_pos(erl_anno:location(), struct(S, A)) -> struct(S, A).
update_pos(Line, MA) ->
    map_m_state(
      fun(Formatter, #{error := Error0} = MState) ->
              case astranaut_error:no_pending(Error0) of
                  true ->
                      MState;
                  false ->
                      Error1 = astranaut_error:update_pos(Line, Formatter, Error0),
                      update_m_state(MState, #{error => Error1})
              end
        end, MA).

-spec update_pos(erl_anno:location(), module(), struct(S, A)) -> struct(S, A).
update_pos(Pos, Formatter, MA) ->
    with_formatter(Formatter, update_pos(Pos, MA)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
run_0(#{?STRUCT_KEY := ?TRAVERSE_M, inner := Inner}, Formatter, File, Attr, State) ->
    Inner(Formatter, File, Attr, State).

map_m_state(F, MA) ->
    Inner = 
        fun(Formatter, File, Attr, State) ->
                MState1 = run_0(MA, Formatter, File, Attr, State),
                MState2 = apply_map_state_m_f(F, Formatter, Attr, MState1),
                case MState2 of
                    #{?STRUCT_KEY := ?STATE_OK} ->
                        MState2;
                    #{?STRUCT_KEY := ?STATE_FAIL} ->
                        MState2;
                    _ ->
                        exit({invalid_m_state_after_map, MState1, MState2})
                end
        end,
    new(Inner).

map_m_state_ok(F, MA) ->
    map_m_state(
      fun(Formatter, Attr, #{?STRUCT_KEY := ?STATE_OK} = StateM) ->
              apply_map_state_m_f(F, Formatter, Attr, StateM);
         (_Formatter, _Attr, #{?STRUCT_KEY := ?STATE_FAIL} = StateM) ->
              StateM
      end, MA).

apply_map_state_m_f(F, _Formatter, _Attr, MState) when is_function(F, 1) ->
    F(MState);
apply_map_state_m_f(F, Formatter, _Attr, MState) when is_function(F, 2) ->
    F(Formatter, MState);
apply_map_state_m_f(F, Formatter, Attr, MState) when is_function(F, 3) ->
    F(Formatter, Attr, MState).

update_m_state(#{} = State, #{} = Map) ->
    merge_struct(State, Map, #{?STATE_OK => [return, state, error, updated],
                               ?STATE_FAIL => {[state, error], [return, updated]}}).

merge_struct(#{?STRUCT_KEY := StructName} = Struct, Map, KeyMap) when is_map(KeyMap)->
    case maps:find(StructName, KeyMap) of
        {ok, {Keys, OptionalKeys}} ->
            merge_struct(Struct, Map, Keys, OptionalKeys);
        {ok, Keys} ->
            merge_struct(Struct, Map, Keys, []);
        error ->
            erlang:error({invalid_struct, StructName})
    end.
merge_struct(#{?STRUCT_KEY := StructName} = Struct, Map, Keys, OptionalKeys) when is_list(Keys), is_list(OptionalKeys) ->
    RestKeys = maps:keys(Map) -- Keys -- OptionalKeys -- [?STRUCT_KEY],
    case RestKeys of
        [] ->
            lists:foldl(
              fun(Key, Acc) ->
                      case maps:find(Key, Map) of
                          {ok, Value} ->
                              case validate_struct_value(Key, Value) of
                                  true ->
                                      maps:put(Key, Value, Acc);
                                  false ->
                                      erlang:error({invalid_struct_value, StructName, Key, Value})
                              end;
                          error ->
                              case maps:is_key(Key, Acc) of
                                  true ->
                                      Acc;
                                  false ->
                                      init_struct_value(Acc, Key)
                              end
                      end
              end, Struct, Keys);
        _ ->
            erlang:error({invalid_merge_keys, StructName, RestKeys, Map})
    end.

validate_struct_value(return, _Return) ->
    true;
validate_struct_value(state, _State) ->
    true;
validate_struct_value(error, #{?STRUCT_KEY := ?ERROR_STATE}) ->
    true;
validate_struct_value(updated, Updated) when is_boolean(Updated) ->
    true;
validate_struct_value(_Key, _Value) ->
    false.

init_struct_value(#{} = Struct, error) ->
    Struct#{error => astranaut_error:new()};
init_struct_value(#{} = Struct, updated) ->
    Struct#{updated => false};
init_struct_value(#{?STRUCT_KEY := StructName}, Key) ->
    erlang:error({struct_value_required, StructName, Key}).
