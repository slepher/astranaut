%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_base_m).

-erlando_type(?MODULE).

-export_type([astranaut_base_m/0]).
-opaque astranaut_base_m() :: #{'__struct__' => ?MODULE, return => any(), errors => [any()], warnings => [any()]}.

-astranaut_future_behaviour(monad).

%% API
-export([astranaut_base_m/1]).
-export([to_astranaut_base_m/1]).
-export([new/0, run/1]).
-export([bind/2, then/2, return/1]).
-export(['>>='/3, return/2]).
-export([error/1, warning/1, errors/1, warnings/1]).

%%%===================================================================
%%% Construct astranaut_base_m by map
%%%===================================================================
astranaut_base_m(#{warning := Warning} = Map) ->
    Map1 = maps:remove(warning, Map),
    default(Map1#{warnings => [Warning]});
astranaut_base_m(#{error := Error} = Map) ->
    Map1 = maps:remove(error, Map),
    default(Map1#{errors => [Error]});
astranaut_base_m(#{errors := Errors}) when not is_list(Errors) ->
    exit({errors_should_be_list, Errors});
astranaut_base_m(#{warnings := Warnings}) when not is_list(Warnings) ->
    exit({warnings_should_be_list, Warnings});
astranaut_base_m(#{} = Map) ->
    default(Map).
%%%===================================================================
%%% convert traverse return value to astranaut_base_m
%%%===================================================================
to_astranaut_base_m(A) ->
    to_astranaut_base_m(ok, A).

to_astranaut_base_m(A, {warning, Warning}) ->
    astranaut_base_m(#{return => A, warning => Warning});
to_astranaut_base_m(A, {warnings, Warnings}) ->
    astranaut_base_m(#{return => A, warnings => Warnings});
to_astranaut_base_m(_A, {warning, B, Warning}) ->
    astranaut_base_m(#{return => B, warning => Warning});
to_astranaut_base_m(_A, {warnings, B, Warnings}) ->
    astranaut_base_m(#{return => B, warnings => Warnings});
to_astranaut_base_m(A, {error, Error}) ->
    astranaut_base_m(#{return => A, error => Error});
to_astranaut_base_m(A, {errors, Errors}) ->
    astranaut_base_m(#{return => A, errors => Errors});
to_astranaut_base_m(_A, {ok, B}) ->
    astranaut_base_m(#{return => B});
to_astranaut_base_m(_A, #{'__struct__' := ?MODULE} = BaseM) ->
    BaseM;
to_astranaut_base_m(_A, B) ->
    astranaut_base_m(#{return => B}).

%%%===================================================================
%%% fill struct default values
%%%===================================================================
default(#{} = Map) ->
    maps:merge(#{'__struct__' => ?MODULE, return => ok, errors => [], warnings => []}, Map).

run(#{warnings := Warnings, errors := Errors, return := A}) ->
    {A, Errors, Warnings}.

new() ->
    ?MODULE.
%%%===================================================================
%%% API
%%%===================================================================
bind(#{'__struct__' := ?MODULE, return := A, errors := Errors0, warnings := Warnings0}, KMB) ->
    #{'__struct__' := ?MODULE, return := B, errors := Errors1, warnings := Warnings1} = KMB(A),
    default(#{return => B, errors => Errors0 ++ Errors1, warnings => Warnings0 ++ Warnings1}).

then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

return(A) ->
    default(#{return => A}).

'>>='(MA, KMB, ?MODULE) ->
    bind(MA, KMB).

return(A, ?MODULE) ->
    return(A).

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
