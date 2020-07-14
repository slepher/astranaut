%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_walk_return_v3).

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
-export([to_map/1]).

%%%===================================================================
%%% API
%%%===================================================================
new(#{} = Map) ->
    Map1 = up_map(Map),
    default(Map1);
new(Return) ->
    case to_map(Return) of
        {ok, Map} ->
            new(Map);
        error ->
            new(#{node => Return})
    end.

to_map({warning, Warning}) ->
    {ok, #{warnings => [Warning]}};
to_map({warnings, Warnings}) ->
    {ok, #{warnings => Warnings}};
to_map({warning, B, Warning}) ->
    {ok, #{node => B, warnings => [Warning]}};
to_map({warnings, B, Warnings}) ->
    {ok, #{node => B, warnings => Warnings}};
to_map({error, Error}) ->
    {ok, #{errors => [Error]}};
to_map({errors, Errors}) when is_list(Errors) ->
    {ok, #{errors => Errors}};
to_map({error, B, Error}) ->
    {ok, #{node => B, errors => [Error]}};
to_map({errors, B, Errors}) when is_list(Errors) ->
    {ok, #{node => B, errors => Errors}};
to_map({ok, B}) ->
    {ok, #{node => B}};
to_map(continue) ->
    {ok, #{continue => true}};
to_map({continue, B}) ->
    {ok, #{continue => true, return => B}};
to_map(_Other) ->
    error.
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

up_map(#{warning := Warning} = Map) ->
    Map1 = maps:remove(warning, Map),
    Warnings = maps:get(warnings, Map, []),
    Map1#{warnings => [Warning|Warnings]};
up_map(#{error := Error} = Map) ->
    Map1 = maps:remove(error, Map),
    Errors = maps:get(errors, Map, []),
    Map1#{errors => [Error|Errors]};
%% up_map(#{node := Node} = Map) ->
%%     Map1 = maps:remove(node, Map),
%%     Nodes = maps:get(nodes, Map, []),
%%     Map1#{nodes => [Node|Nodes]};
up_map(#{node := Node} = Map) ->
    Map1 = maps:remove(node, Map),
    Map1#{return => Node};
up_map(#{errors := Errors}) when not is_list(Errors) ->
    exit({errors_should_be_list, Errors});
up_map(#{warnings := Warnings}) when not is_list(Warnings) ->
    exit({warnings_should_be_list, Warnings});
up_map(#{nodes := Nodes}) when not is_list(Nodes) ->
    exit({nodes_should_be_list, Nodes});
up_map(#{} = Map) ->
    Map.
