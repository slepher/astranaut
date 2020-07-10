%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_walk_return).

-include_lib("astranaut/include/astranaut_struct_name.hrl").

-export_type([astranaut_walk_return/2]).

-opaque astranaut_walk_return(S, A) :: #{?STRUCT_KEY => ?WALK_RETURN,
                                         return => A,
                                         state => S,
                                         errors => [any()],
                                         warnings => [any()],
                                         error => any(),
                                         warning => any(),
                                         continue => boolean()}.
    
-export([new/1]).
-export([to_map/1, to_map/2]).

%%%===================================================================
%%% API
%%%===================================================================
new(#{node := Node} = Map) ->
    Map1 = maps:remove(node, Map#{return => Node}),
    new(Map1);
new(#{} = Map) ->
    Map1 = up_map(Map),
    default(Map1);
new(Return) ->
    case to_map(Return) of
        {ok, Map} ->
            new(Map);
        error ->
            new(#{return => Return})
    end.

to_map({warning, Warning}) ->
    {ok, #{warnings => [Warning]}};
to_map({warnings, Warnings}) ->
    {ok, #{warnings => Warnings}};
to_map({warning, B, Warning}) ->
    {ok, #{return => B, warnings => [Warning]}};
to_map({warnings, B, Warnings}) ->
    {ok, #{return => B, warnings => Warnings}};
to_map({error, Error}) ->
    {ok, #{errors => [Error]}};
to_map({errors, Errors}) when is_list(Errors) ->
    {ok, #{errors => Errors}};
to_map({error, B, Error}) ->
    {ok, #{return => B, errors => [Error]}};
to_map({errors, B, Errors}) when is_list(Errors) ->
    {ok, #{return => B, errors => Errors}};
to_map({ok, B}) ->
    {ok, #{return => B}};
to_map(continue) ->
    {ok, #{continue => true}};
to_map({continue, B}) ->
    {ok, #{continue => true, return => B}};
to_map(_Other) ->
    error.

to_map(A, Return) ->
    case to_map(Return) of
        {ok, Map} ->
            Map1 = merge_a(A, Map),
            Map2 = up_map(Map1),
            {ok, Map2};
        error ->
            error
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
default(Map) ->
    maps:merge(#{?STRUCT_KEY => ?WALK_RETURN, errors => [], warnings => []}, Map).

merge_a(_A, #{errors := _Errors} = Map) ->
    Map;
merge_a(_A, #{return := _Return} = Map) ->
    Map;
merge_a(A, #{} = Map) ->
    Map#{return => A}.

up_map(#{warning := Warning} = Map) ->
    Map1 = maps:remove(warning, Map),
    Warnings = maps:get(warnings, Map, []),
    Map1#{warnings => [Warning|Warnings]};
up_map(#{error := Error} = Map) ->
    Map1 = maps:remove(error, Map),
    Errors = maps:get(warnings, Map, []),
    Map1#{errors => [Error|Errors]};
up_map(#{errors := Errors}) when not is_list(Errors) ->
    exit({errors_should_be_list, Errors});
up_map(#{warnings := Warnings}) when not is_list(Warnings) ->
    exit({warnings_should_be_list, Warnings});
up_map(#{} = Map) ->
    case maps:get(errors, Map, []) of
        [] ->
            case maps:is_key(return, Map) of
                false ->
                    exit({no_return_without_errors, Map});
                true ->
                    Map
            end;
        _ ->
            Map
    end.
