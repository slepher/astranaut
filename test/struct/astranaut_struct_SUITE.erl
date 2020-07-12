%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_struct_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("test_record.hrl").

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
    [test_struct_new, test_struct_update, test_struct_test,
     test_from_record, test_to_record, test_from_map, test_update_struct,
     test_from_map_missing_name, test_update_missing_name, test_update_fail,
     test_compile_enforece_fail, test_compile_non_record_fail].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_struct_new(_Config) -> 
    Test = astranaut_struct_test:new(),
    #{name := hello, value := <<"world">>} = Test,
    ok.

test_struct_update(_Config) -> 
    Test = astranaut_struct_test:new(),
    Test1 = astranaut_struct_test:update_name(Test, bye),
    #{name := bye, value := <<"world">>} = Test1,
    ok.

test_struct_test(_Config) ->
    Test = astranaut_struct_test:new(),
    hello = astranaut_struct_test:test(Test),
    ok.
    
test_from_record(_Config) ->
    Test = #test{name = hello, value = world},
    Test1 = astranaut_struct_test:from_record(Test),
    ?assertEqual(#{'__struct__' => test,
                   name => hello, value => world, enable => true,
                   desc => undefined}, Test1),
    ok.

test_to_record(_Config) ->
    Test = #{'__struct__' => test, name => hello, value => world},
    Test1 = astranaut_struct_test:to_record(Test),
    ?assertEqual(#test{name = hello, value = world, 
                       enable = undefined, desc = undefined}, Test1),
    ok.

test_from_map(_Config) ->
    Test = #{name => test_name, desc => test_desc, beep => none},
    Test1 = astranaut_struct_test:from_map(Test),
    ?assertEqual(#{'__struct__' => test,
                   name => test_name, 
                   value => <<"world">>,
                   desc => test_desc,
                   enable => true}, 
                 Test1),
    ok.

test_from_map_missing_name(_Config) ->
    Test = #{desc => test_desc, beep => none},
    ?assertException(exit, {missing_enforce_keys, test, [name]}, 
                     astranaut_struct_test:from_map(Test)),
    ok.

test_update_struct(_Config) ->
    Test = #{'__struct__' => test, name => bye},
    Test1 = astranaut_struct_test:update(Test),
    ?assertEqual(#{'__struct__' => test, 
                   name => bye, 
                   value => <<"world">>,
                   enable => true
                  }, Test1),
    ok.

test_update_missing_name(_Config) ->
    Test = #{'__struct__' => test, desc => bye},
    ?assertException(exit, {missing_enforce_keys, test, [name]}, 
                     astranaut_struct_test:update(Test)),
    ok.

test_update_fail(_Config) ->
    Test = #{'__struct__' => test2, name => bye},
    ?assertException(exit, {invalid_struct, test, Test}, astranaut_struct_test:update(Test)),
    ok.

test_compile_enforece_fail(_Config) ->
    [{File, [Error]}] = astranaut_struct_fail_0:errors(),
    Forms = astranaut_struct_fail_0:forms(),
    [{Line, {test, _Opts}}] = astranaut:attributes_with_line(astranaut_struct, Forms),
    ?assertEqual("astranaut_struct_fail_0.erl", filename:basename(File)),
    ?assertEqual({Line,astranaut_struct_transformer, {enforce_keys_not_in_struct,test,[desc]}}, Error),
    ok.

test_compile_non_record_fail(_Config) ->
    [{File, [Error]}] = astranaut_struct_fail_1:errors(),
    Forms = astranaut_struct_fail_1:forms(),
    [{Line, other_test}] = astranaut:attributes_with_line(astranaut_struct, Forms),
    ?assertEqual("test_record_1.hrl", filename:basename(File)),
    ?assertMatch({Line,astranaut_struct_transformer,{undefined_record,other_test}}, Error),
    ok.
