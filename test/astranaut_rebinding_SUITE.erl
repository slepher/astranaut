%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding_SUITE).

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
    [test_lc, test_function, test_case,
     test_map, test_map_update,
     test_rec, test_rec_update,
     test_operator, test_list, test_tuple
    ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
test_rebinding_lc() -> 
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
test_lc(_Config) -> 
    A = astranaut_rebinding_test:test_lc(10),
    B = astranaut_rebinding_test:test_lc_origin(10),
    ?assertEqual(A, B),
    ok.

test_function(_Config) ->
    A = astranaut_rebinding_test:test_function(10),
    B = astranaut_rebinding_test:test_function_origin(10),
    ?assertEqual(A, B),
    ok.

test_case(_Config) ->
    A = astranaut_rebinding_test:test_case(10),
    B = astranaut_rebinding_test:test_case_origin(10),
    C = astranaut_rebinding_test:test_case_pinned(10),
    ?assertEqual(A, B),
    ?assertEqual(A, C),
    ok.

test_rec(_Config) ->
    A = astranaut_rebinding_test:test_rec(10),
    B = astranaut_rebinding_test:test_rec_origin(10),
    ?assertEqual(A, B),
    ok.

test_rec_update(_Config) ->
    A = astranaut_rebinding_test:test_rec_update(10),
    B = astranaut_rebinding_test:test_rec_update_origin(10),
    ?assertEqual(A, B),
    ok.

test_map(_Config) ->
    A = astranaut_rebinding_test:test_map(10),
    B = astranaut_rebinding_test:test_map_origin(10),
    ?assertEqual(A, B),
    ok.

test_map_update(_Config) ->
    A = astranaut_rebinding_test:test_map_update(10),
    B = astranaut_rebinding_test:test_map_update_origin(10),
    ?assertEqual(A, B),
    ok.

test_operator(_Config) ->
    A = astranaut_rebinding_test:test_operator(10),
    B = astranaut_rebinding_test:test_operator_origin(10),
    ?assertEqual(A, B),
    ok.

test_tuple(_Config) ->
    A = astranaut_rebinding_test:test_tuple(10),
    B = astranaut_rebinding_test:test_tuple_origin(10),
    ?assertEqual(A, B),
    ok.

test_list(_Config) ->
    A = astranaut_rebinding_test:test_list(10),
    B = astranaut_rebinding_test:test_list_origin(10),
    ?assertEqual(A, B),
    ok.
