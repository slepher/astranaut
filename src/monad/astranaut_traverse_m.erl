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
-include_lib("astranaut/include/astranaut_struct_name.hrl").
-define(STATE_OK, astranaut_traverse_m_state_ok).
-define(STATE_FAIL, astranaut_traverse_m_state_fail).

%%%===================================================================
%%% erlando attributes
%%%===================================================================

-erlando_type(?MODULE).
-erlando_future_behaviour(monad).

-compile({no_auto_import, [error/1, get/0, put/1, node/1, nodes/1]}).

%%%===================================================================
%%% types
%%%===================================================================
-export_type([astranaut_traverse_m/2]).

-opaque astranaut_traverse_m(S, A) :: 
          #{?STRUCT_KEY => ?TRAVERSE_M, inner => astranaut_traverse_m_inner(S, A)}.

-type astranaut_traverse_m_inner(S, A) :: 
        fun((astranaut_error_state:formatter(), S, astranaut_error_state:astranaut_error_state()) ->
                   astranaut_traverse_m_state(S, A)).

-type astranaut_traverse_m_state(S, A) :: 
        #{?STRUCT_KEY => ?STATE_OK,
          return => A,
          nodes => syntax_nodes(),
          updated => boolean(),
          continue => boolean(),
          state => S, 
          error => astranaut_error_state:astranaut_error_state()} |
        #{?STRUCT_KEY => ?STATE_FAIL,
          state => S, 
          error => astranaut_error_state:astranaut_error_state()}.

-type formatter() :: module().
-type line() :: integer().
-type syntax_node() :: erl_syntax:syntax_tree().
-type syntax_nodes() :: endo(syntax_node()).
-type endo(A) :: astranaut_endo:endo(A).

%%%===================================================================
%%% API
%%%===================================================================
-export([astranaut_traverse_m/1, to_monad/1, to_monad/2]).
-export([convertable_struct/1]).
-export([run/3, run_1/3, eval/3, exec/3, enodes/3]).
-export([bind/2, then/2, return/1]).
-export([bind_continue/2, bind_continue_nodes/2, updated_node/2]).
-export(['>>='/3, return/2]).
-export([fail/1, fail/2, fails/1]).
-export([fail_on_error/1, sequence_either/1, sequence_all/1]).
-export([state/1]).
-export([with_error/2]).
-export([get/0, put/1, modify/1]).
-export([set_continue/1, set_continue/2, listen_continue/1, listen_updated/1]).
-export([listen/1, listen_nodes/1, set_updated/1, pop_nodes/1, as_node/1, node/1, nodes/1]).
-export([with_formatter/2]).
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
        fun(_Formatter, State, Error0) ->
                State1 = maps:get(state, Map, State),
                Continue = maps:get(continue, Map, false),
                Updated = maps:is_key(nodes, Map),
                Nodes = maps:get(nodes, Map, []),
                Return = maps:get(return, Map, ok),
                #{errors := Errors, warnings := Warnings} = Map,
                Error1 = astranaut_error_state:append_ews(Errors, Warnings, Error0),
                case Errors of
                    [] ->
                        state_ok(#{return => Return, state => State1, nodes => astranaut_endo:endo(Nodes),
                                   continue => Continue, updated => Updated, error => Error1});
                    _ ->
                        state_fail(#{state => State1, error => Error1})
                end
        end,
    new(Inner);
astranaut_traverse_m(#{?STRUCT_KEY := ?BASE_M, return := Return, errors := Errors, warnings := Warnings}) ->
    Inner =
        fun(_Formatter, State, Error0) ->
                Error1 = astranaut_error_state:append_ews(Errors, Warnings, Error0),
                state_ok(#{return => Return, state => State, error => Error1, updated => true})
        end,
    new(Inner);
astranaut_traverse_m(#{?STRUCT_KEY := ?RETURN_OK, return := Return, error := Error1}) ->
    Inner =
        fun(_Formatter, State, Error0) ->
                Error2 = astranaut_error_state:merge(Error0, Error1),
                state_ok(#{return => Return, state => State, error => Error2})
        end,
    new(Inner);
astranaut_traverse_m(#{?STRUCT_KEY := ?RETURN_FAIL, error := Error1}) ->
    Inner =
        fun(_Formatter, State, Error0) ->
                Error2 = astranaut_error_state:merge(Error0, Error1),
                state_fail(#{state => State, error => Error2})
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

state_ok(Map) ->
    update_m_state(#{?STRUCT_KEY => ?STATE_OK}, Map).

state_fail(Map) ->
    update_m_state(#{?STRUCT_KEY => ?STATE_FAIL}, Map).

-spec run(astranaut_traverse_m(S, A), formatter(), S) -> astranaut_return_m:astranaut_return_m({A, S}).
run(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    astranaut_monad:lift_m(fun({A, State1, _Nodes}) -> {A, State1} end, run_1(MA, Formatter, State), astranaut_return_m).

-spec run_1(astranaut_traverse_m(S, A), formatter(), S) -> astranaut_return_m:astranaut_return_m({A, S, [any()]}).
run_1(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    case run_0(MA, Formatter, State, astranaut_error_state:new()) of
        #{?STRUCT_KEY := ?STATE_OK, return := Return, state := State1, nodes := Nodes, error := Error} ->
            astranaut_return_m:return_ok({Return, State1, astranaut_endo:run(Nodes)}, Error);
        #{?STRUCT_KEY := ?STATE_FAIL, error := Error} ->
            astranaut_return_m:return_fail(Error)
    end.

-spec eval(astranaut_traverse_m(S, A), formatter(), S) -> astranaut_return_m:astranaut_return_m(A).
eval(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    astranaut_monad:lift_m(fun({A, _State, _Nodes}) -> A end, run_1(MA, Formatter, State), astranaut_return_m).

-spec exec(astranaut_traverse_m(S, _A), formatter(), S) -> astranaut_return_m:astranaut_return_m(S).
exec(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    astranaut_monad:lift_m(fun({_A, State1, _Nodes}) -> State1 end, run_1(MA, Formatter, State), astranaut_return_m).

-spec enodes(astranaut_traverse_m(S, _A), formatter(), S) -> astranaut_return_m:astranaut_return_m(syntax_nodes()).
enodes(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    F = fun({_A, _State1, Nodes}) -> Nodes end,
    astranaut_monad:lift_m(F, run_1(MA, Formatter, State), astranaut_return_m).

-spec bind(astranaut_traverse_m(S, A), fun((A) -> astranaut_traverse_m(S, B))) -> astranaut_traverse_m(S, B).
bind(MA, KMB) when is_function(KMB, 1) ->
    map_m_state_ok(
      fun(Formatter, #{return := A, state := State1,
                       updated := Updated1, continue := Continue1,
                       nodes := Nodes1, error := Error1}) ->
              MB = run_0(KMB(A), Formatter, State1, Error1),
              case MB of
                  #{?STRUCT_KEY := ?STATE_OK, updated := Updated2, continue := Continue2, nodes := Nodes2} ->
                      Nodes3 = astranaut_endo:append(Nodes1, Nodes2),
                      Updated3 = Updated1 or Updated2,
                      Continue3 = Continue1 or Continue2,
                      update_m_state(MB, #{nodes => Nodes3, updated => Updated3, continue => Continue3});
                  #{?STRUCT_KEY := ?STATE_FAIL} ->
                      MB
              end
      end, MA).

-spec then(astranaut_traverse_m(S, _A), astranaut_traverse_m(S, B)) -> astranaut_traverse_m(S, B).
then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

-spec return(A) -> astranaut_traverse_m(_S, A).
return(A) ->
    Inner = 
        fun(_Formatter, State, Error) ->
                state_ok(#{return => A, state => State, error => Error})
        end,
    new(Inner).

-spec '>>='(astranaut_traverse_m(S, A), fun((A) -> astranaut_traverse_m(S, B)), ?TRAVERSE_M) -> astranaut_traverse_m(S, B).
'>>='(MA, KMB, ?TRAVERSE_M) ->
    bind(MA, KMB).

-spec return(A, ?TRAVERSE_M) -> astranaut_traverse_m(_S, A).
return(A, ?TRAVERSE_M) ->
    return(A).

bind_continue_nodes(MA, AMB) ->
    bind(
      listen_continue(pop_nodes(MA)),
      fun({NodeAs, true}) ->
              nodes(NodeAs);
         ({NodeAs, false}) ->
              astranaut_monad:map_m(AMB, NodeAs, ?MODULE)
      end).

%% updated_node(NodeA, MB) ->
%%     map_m_state_ok(
%%       fun(#{updated := false} = MState) ->
%%               update_m_state(MState, #{nodes => astranaut_endo:endo([NodeA])});
%%          (MState) ->
%%               MState
%%       end, MB).

%% bind_node(NodeA, MB, BMC, pre) ->
%%     bind(
%%       listen_continue(updated_node(NodeA, MB)),
%%       fun({NodeB, true}) ->
%%               return(NodeB);
%%          ({NodeB, false}) ->
%%               BMC(NodeB)
%%       end);
%% bind_node(NodeA, MB, BMC, _) ->
%%     bind(updated_node(NodeA, MB), BMC).

bind_continue(MA, AMB) ->
    bind(
      listen_continue(MA),
      fun({NodeA, true}) ->
              return(NodeA);
         ({NodeA, false}) ->
              AMB(NodeA)
      end).

updated_node(NodeA, MB) ->
    map_m_state_ok(
      fun(#{updated := false} = MState) ->
              update_m_state(MState, #{return => NodeA, nodes => astranaut_endo:endo([NodeA])});
         (MState) ->
              MState
      end, MB).

-spec fail_on_error(astranaut_traverse_m(S, A)) -> astranaut_traverse_m(S, A).
fail_on_error(MA) ->
    map_m_state_ok(
      fun(#{state := State1, error := Error} = StateOk) ->
              case astranaut_error_state:is_empty_error(Error) of
                  true ->
                      StateOk;
                  false ->
                      state_fail(#{state => State1, error => Error})
              end
        end, MA).

-spec sequence_either([astranaut_traverse_m(S, A)]) -> astranaut_traverse_m(S, [A]).
sequence_either([MA|MAs]) ->
    map_m_state(
      fun(Formatter, #{state := State1, error := Error1} = MState1) ->
              #{state := State2, error := Error2} = MState2 =
                  run_0(sequence_either(MAs), Formatter, State1, Error1),
              case {MState1, MState2} of
                  {#{?STRUCT_KEY := ?STATE_OK, return := A,  nodes := Nodes1},
                   #{?STRUCT_KEY := ?STATE_OK, return := As, nodes := Nodes2}} ->
                      Nodes3 = astranaut_endo:append(Nodes1, Nodes2),
                      update_m_state(MState2, #{return => [A|As], nodes => Nodes3, error => Error2});
                  _ ->
                      state_fail(#{state => State2, error => Error2})
              end
      end, MA);
sequence_either([]) ->
    astranaut_traverse_m:return([]).

-spec sequence_all([astranaut_traverse_m(S, A)]) -> astranaut_traverse_m(S, [A]).
sequence_all([MA|MAs]) ->
    map_m_state(
      fun(Formatter, #{state := State1, error := Error1} = MState1) ->
              #{error := Error2} = MState2 =
                  run_0(sequence_all(MAs), Formatter, State1, Error1),
              case {MState1, MState2} of
                  {#{?STRUCT_KEY := ?STATE_OK, return := A,  nodes := Nodes1},
                   #{?STRUCT_KEY := ?STATE_OK, return := As, nodes := Nodes2}} ->
                      Nodes3 = astranaut_endo:append(Nodes1, Nodes2),
                      update_m_state(MState2, #{return => [A|As], nodes => Nodes3, error => Error2});
                  _ ->
                      MState2
              end
      end, MA);
sequence_all([]) ->
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
        fun(_Formatter, State, Error0) ->
                Error1 = astranaut_error_state:append_errors(Es, Error0),
                state_fail(#{state => State, error => Error1})
        end,
    new(Inner).
%%%===================================================================
%%% state related functions.
%%%===================================================================
-spec state(fun((S) -> {A, S})) -> astranaut_traverse_m(S, A).
state(F) ->
    Inner = 
        fun(_Formatter, State0, Error) ->
                {A, State1} = F(State0),
                state_ok(#{return => A, state => State1, error => Error})
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
%%% nodes updated continue related functions
%%%===================================================================
listen(MA) ->
    map_m_state_ok(
      fun(#{return := Return, nodes := Nodes} = StateM) ->
              update_m_state(StateM, #{return => {Return, astranaut_endo:run(Nodes)}})
      end, MA).

listen_nodes(MA) ->
    map_m_state_ok(
      fun(#{return := Return, nodes := Nodes} = StateM) ->
              update_m_state(StateM, #{return => {Return, astranaut_endo:run(Nodes)}, nodes => astranaut_endo:empty()})
      end, MA).

set_continue(MA) ->
    set_continue(MA, true).

set_continue(MA, Continue) when is_boolean(Continue) ->
    map_m_state_ok(
      fun(#{} = StateM) ->
              update_m_state(StateM, #{continue => true})
      end, MA).

set_updated(MA) ->
    map_m_state_ok(
      fun(#{} = StateM) ->
              update_m_state(StateM, #{updated => true})
      end, MA).

listen_continue(MA) ->
    map_m_state_ok(
      fun(#{return := Return, continue := Continue} = StateM) ->
              update_m_state(StateM, #{return => {Return, Continue}, continue => false})
      end, MA).

listen_updated(MA) ->
    map_m_state_ok(
      fun(#{return := Return, updated := Updated} = StateM) ->
              update_m_state(StateM, #{return => {Return, Updated}})
      end, MA).

as_node(MA) ->
    bind(MA,
         fun(Node) ->
                 then(
                   node(Node),
                   return(Node))
         end).

pop_nodes(MA) ->
    map_m_state_ok(
      fun(#{nodes := Nodes} = StateM) ->
              update_m_state(StateM, #{return => astranaut_endo:run(Nodes), nodes => astranaut_endo:endo([])})
      end, MA).

node(Node) ->
    then(
      nodes([Node]),
      return(Node)).

nodes(Nodes) ->
    Inner =
        fun(_Formatter, State, Error) ->
                state_ok(#{return => ok, state => State, error => Error, nodes => astranaut_endo:endo(Nodes), updated => true})
        end,
    new(Inner).
%%%===================================================================
%%% error_state related functions
%%%===================================================================

-spec with_error(fun((astranaut_error_state:astranaut_error_state())
                     -> astranaut_error_state:astranaut_error_state()),
                 astranaut_traverse_m(S, A))
                -> astranaut_traverse_m(S, A).
with_error(F, MA) ->
    map_m_state(
      fun(#{error := Error1} = MState) ->
              Error2 = F(Error1),
              update_m_state(MState, #{error => Error2})
        end, MA).

-spec modify_error(fun((astranaut_error_state:astranaut_error_state())
                     -> astranaut_error_state:astranaut_error_state()))
                -> astranaut_traverse_m(_S, _A).
modify_error(F) ->
    Inner = 
        fun(_Formatter, State, Error0) ->
                Error1 = F(Error0),
                state_ok(#{return => ok, state => State, error => Error1})
        end,
    new(Inner).

-spec with_formatter(fun((formatter()) -> formatter()), astranaut_traverse_m(S, A)) -> astranaut_traverse_m(S, A).
with_formatter(Formatter, MA) ->
    Inner = 
        fun(_Formatter0, State, Error) ->
                  run_0(MA, Formatter, State, Error)
        end,
    new(Inner).


-spec warning(term()) -> astranaut_traverse_m(_S, _A).
warning(Warning) ->
    warnings([Warning]).

-spec warnings([term()]) -> astranaut_traverse_m(_S, _A).
warnings(Warnings) ->
    modify_error(
      fun(Error) ->
              astranaut_error_state:append_warnings(Warnings, Error)
      end).

-spec formatted_warnings([{line(), formatter(), term()}]) -> astranaut_traverse_m(_S, _A).
formatted_warnings(Warnings) ->
    modify_error(
      fun(Error) ->
              astranaut_error_state:append_formatted_warnings(Warnings, Error)
      end).

-spec error(term()) -> astranaut_traverse_m(_S, _A).
error(Error) ->
    errors([Error]).

-spec errors([term()]) -> astranaut_traverse_m(_S, _A).
errors(Errors) ->
    modify_error(
      fun(Error) ->
              astranaut_error_state:append_errors(Errors, Error)
      end).

-spec formatted_errors([{line(), formatter(), term()}]) -> astranaut_traverse_m(_S, _A).
formatted_errors(Errors) ->
    modify_error(
      fun(Error) ->
              astranaut_error_state:append_formatted_errors(Errors, Error)
      end).

-spec update_file(file:filename()) -> astranaut_traverse_m(_S, _A).
update_file(File) ->
    modify_error(
      fun(Error) ->
              astranaut_error_state:update_file(File, Error)
      end).

-spec eof() -> astranaut_traverse_m(_S, _A).
eof() ->
    modify_error(
      fun(Error) ->
              astranaut_error_state:eof(Error)
      end).

-spec update_line(line(), astranaut_traverse_m(S, A)) -> astranaut_traverse_m(S, A).
update_line(Line, MA) ->
    map_m_state(
      fun(Formatter, #{error := Error0, error := Error0} = MState) ->
              case astranaut_error_state:no_pending(Error0) of
                  true ->
                      MState;
                  false ->
                      Error1 = astranaut_error_state:update_line(Line, Formatter, Error0),
                      update_m_state(MState, #{error => Error1})
              end
        end, MA).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
run_0(#{?STRUCT_KEY := ?TRAVERSE_M, inner := Inner},
      Formatter, State, #{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    Inner(Formatter, State, Error).

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

map_m_state(F, MA) ->
    Inner = 
        fun(Formatter, State, #{?STRUCT_KEY := ?ERROR_STATE} = Error0) ->
                MState1 = run_0(MA, Formatter, State, Error0),
                MState2 = apply_map_state_m_f(F, Formatter, MState1),
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
      fun(Formatter, #{?STRUCT_KEY := ?STATE_OK} = StateM) ->
              apply_map_state_m_f(F, Formatter, StateM);
         (_Formatter, #{?STRUCT_KEY := ?STATE_FAIL} = StateM) ->
              StateM
      end, MA).

apply_map_state_m_f(F, _Formatter, MState) when is_function(F, 1) ->
    F(MState);
apply_map_state_m_f(F, Formatter, MState) when is_function(F, 2) ->
    F(Formatter, MState).

update_m_state(#{} = State, #{} = Map) ->
    merge_struct(State, Map, #{?STATE_OK => [return, nodes, state, error, updated, continue],
                               ?STATE_FAIL => {[state, error], [return, nodes, updated, continue]}}).

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
validate_struct_value(nodes, #{?STRUCT_KEY := ?ENDO}) ->
    true;
validate_struct_value(state, _State) ->
    true;
validate_struct_value(error, #{?STRUCT_KEY := ?ERROR_STATE}) ->
    true;
validate_struct_value(updated, Updated) when is_boolean(Updated) ->
    true;
validate_struct_value(continue, Continue) when is_boolean(Continue) ->
    true;
validate_struct_value(_Key, _Value) ->
    false.

init_struct_value(#{} = Struct, nodes) ->
    Struct#{nodes => astranaut_endo:endo([])};
init_struct_value(#{} = Struct, error) ->
    Struct#{error => astranaut_error_state:new()};
init_struct_value(#{} = Struct, updated) ->
    Struct#{updated => false};
init_struct_value(#{} = Struct, continue) ->
    Struct#{continue => false};
init_struct_value(#{?STRUCT_KEY := StructName}, Key) ->
    erlang:error({struct_value_required, StructName, Key}).
