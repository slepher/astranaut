%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_base_m).

-include_lib("astranaut/include/astranaut_struct_name.hrl").

-erlando_type(?MODULE).
-astranaut_future_behaviour(monad).

%% Types
-export_type([astranaut_base_m/1]).
-opaque astranaut_base_m(A) :: #{'__struct__' => ?MODULE, return => A, errors => [any()], warnings => [any()]}.

%% API
-export([astranaut_base_m/1, to_monad/1]).
-export([new/0, run/1]).
-export([bind/2, then/2, return/1]).
-export(['>>='/3, return/2]).
-export([warning_as_error/1]).
-export([error/1, warning/1, errors/1, warnings/1]).

%%%===================================================================
%%% Construct astranaut_base_m by map
%%%===================================================================
astranaut_base_m(#{?STRUCT_KEY := ?BASE_M} = BaseM) ->
    BaseM;
astranaut_base_m(#{?STRUCT_KEY := ?WALK_RETURN} = Walk) ->
    default(maps:remove(?STRUCT_KEY, Walk)).
%%%===================================================================
%%% convert traverse return value to astranaut_base_m
%%%===================================================================
to_monad(A) ->
    to_monad(ok, A).

-spec to_monad(A, term()) -> astranaut_base_m(A).
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
    astranaut_base_m(Return1).

convertable_struct(#{?STRUCT_KEY := Key}) ->
    convertable_struct_key(Key);
convertable_struct(_Other) ->
    false.
%%%===================================================================
%%% fill struct default values
%%%===================================================================
default(#{} = Map) ->
    maps:merge(#{?STRUCT_KEY => ?BASE_M, return => ok, errors => [], warnings => []}, Map).

run(#{warnings := Warnings, errors := Errors, return := A}) ->
    {A, Errors, Warnings}.

new() ->
    ?MODULE.
%%%===================================================================
%%% API
%%%===================================================================
bind(MA, KMB) ->
    #{'__struct__' := ?MODULE, return := A, errors := Errors0, warnings := Warnings0} = to_monad(MA),
    #{'__struct__' := ?MODULE, return := B, errors := Errors1, warnings := Warnings1} = to_monad(A, KMB(A)),
    default(#{return => B, errors => Errors0 ++ Errors1, warnings => Warnings0 ++ Warnings1}).

then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

return(A) ->
    default(#{return => A}).

'>>='(MA, KMB, ?MODULE) ->
    bind(MA, KMB).

return(A, ?MODULE) ->
    return(A).

warning_as_error(#{'__struct__' := ?MODULE, warnings := Warnings} = MA) ->
    astranaut_base_m:bind(
      errors(Warnings),
      MA#{warnings => []}).

error(Error) ->
    errors([Error]).

warning(Warning) ->
    warnings([Warning]).
    
errors(Errors) when is_list(Errors) ->
    default(#{errors => Errors}).

warnings(Warnings) when is_list(Warnings) ->
    default(#{warnings => Warnings}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
convertable_struct_key(?WALK_RETURN) ->
    true;
convertable_struct_key(?BASE_M) ->
    true;
convertable_struct_key(_) ->
    false.
