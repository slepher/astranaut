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
-export([to_traverse_m/2, to_traverse_m/3]).

%%%===================================================================
%%% API
%%%===================================================================
to_traverse_m(Return, Node) ->
    to_traverse_m(Return, Node, #{}).

to_traverse_m(Return, Node, Opts) ->
    WithState = maps:get(with_state, Opts, false),
    case to_map(Return) of
        {ok, StructBase} ->
            WalkReturn = new(StructBase),
            astranaut_traverse_m:astranaut_traverse_m(WalkReturn);
        error ->
            case {Return, WithState} of
                {{Node1, State}, true} ->
                    WalkReturn = new(#{node => Node1, state => State}),
                    astranaut_traverse_m:astranaut_traverse_m(WalkReturn);
                _ ->
                    astranaut_traverse_m:to_monad(Node, Return)
            end
    end.

new(#{} = Map) ->
    Map1 = up_nodes(Map),
    Map2 = up_map(Map1),
    default(Map2);
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

up_nodes(#{nodes := _Nodes} = Map) ->
    Map;
up_nodes(#{return := Return} = Map) ->
    Map#{nodes => [Return]};
up_nodes(#{} = Map) ->
    Map.

up_map(#{warning := Warning} = Map) ->
    Map1 = maps:remove(warning, Map),
    Warnings = maps:get(warnings, Map, []),
    Map1#{warnings => [Warning|Warnings]};
up_map(#{error := Error} = Map) ->
    Map1 = maps:remove(error, Map),
    Errors = maps:get(errors, Map, []),
    Map1#{errors => [Error|Errors]};
up_map(#{node := Node} = Map) ->
    Map1 = maps:remove(node, Map),
    Nodes = maps:get(nodes, Map, []),
    Map2 = Map1#{nodes => [Node|Nodes]},
    case maps:is_key(return, Map2) of
        true ->
            Map2;
        false ->
            Map2#{return => Node}
    end;
%% up_map(#{node := Node} = Map) ->
%%     Map1 = maps:remove(node, Map),
%%     Map1#{return => Node};
up_map(#{errors := Errors}) when not is_list(Errors) ->
    exit({errors_should_be_list, Errors});
up_map(#{warnings := Warnings}) when not is_list(Warnings) ->
    exit({warnings_should_be_list, Warnings});
up_map(#{nodes := Nodes}) when not is_list(Nodes) ->
    exit({nodes_should_be_list, Nodes});
up_map(#{} = Map) ->
    Map.
