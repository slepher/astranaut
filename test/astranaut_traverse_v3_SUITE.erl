%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_v3_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [test_bind, test_bind_continue, test_bind_update].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_bind(_Config) -> 
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return_v3:new(#{}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodeC = {atom, 20, A},
                         astranaut_walk_return_v3:new(#{node => NodeC})
                 end),
    ?assertEqual({atom, 20, 'A'}, Return),
    ok.

test_bind_continue(_Config) -> 
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return_v3:new(#{continue => true, node => {atom, 10, 'B'}}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodeC = {atom, 20, A},
                         astranaut_walk_return_v3:new(#{node => NodeC})
                 end),
    ?assertEqual({atom, 10, 'B'}, Return),
    ok.

test_bind_update(_Config) ->
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return_v3:new(#{node => {atom, 10, 'B'}}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodeC = {atom, 20, A},
                         astranaut_walk_return_v3:new(#{node => NodeC})
                 end),
    ?assertEqual({atom, 20, 'B'}, Return),
    ok.

test_bind_2(_Config) -> 
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return_v3:new(#{}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodesC = [{atom, 20, A}, {atom, 30, A}],
                         astranaut_walk_return_v3:new(#{nodes => NodesC})
                 end),
    ?assertEqual([{atom, 20, 'A'}, {atom, 30, 'A'}], Return),
    ok.

test_bind_continue_2(_Config) -> 
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return_v3:new(#{continue => true, nodes => [{atom, 10, 'B'}, {atom, 10, 'C'}]}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodeC = {atom, 20, A},
                         astranaut_walk_return_v3:new(#{node => NodeC})
                 end),
    ?assertEqual([{atom, 10, 'B'}, {atom, 10, 'C'}], Return),
    ok.

test_bind_update_2(_Config) ->
    NodeA = {atom, 10, 'A'},
    Walk = astranaut_walk_return_v3:new(#{nodes => [{atom, 10, 'B'}, {atom, 10, 'C'}]}),
    Return = 
        bind_pre(NodeA, Walk,
                 fun({atom, _Line, A}) ->
                         NodesC = [{atom, 20, A}, {atom, 30, A}],
                         astranaut_walk_return_v3:new(#{nodes => NodesC})
                 end),
    ?assertEqual([{atom, 20, 'B'}, {atom, 30, 'B'}, {atom, 20, 'C'}, {atom, 30, 'C'}], Return),
    ok.

bind_pre(NodeA, Walk, BWC) ->
    MNodeB = astranaut_traverse_m_v3:astranaut_traverse_m(Walk),
    MNodeC = 
        astranaut_traverse_m_v3:bind_node(
          NodeA, MNodeB, 
          fun(NodeB) ->
                  Walk2 = BWC(NodeB),
                  MNodeC = astranaut_traverse_m_v3:astranaut_traverse_m(Walk2),
                  astranaut_traverse_m_v3:bind_node(NodeB, MNodeC, fun astranaut_traverse_m_v3:return/1, post)
          end, pre),
    io:format("new node is ~p~n", [MNodeC]),
    #{return := Return} = astranaut_traverse_m_v3:eval(MNodeC, hello, ok),
    Return.
