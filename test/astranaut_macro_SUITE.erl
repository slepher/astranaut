%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_SUITE).

-compile(export_all).

-include("astranaut_struct_name.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,60}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    TestModules = [macro_exports, macro_example, macro_test],
    astranaut_test_lib:load_data_modules(Config, TestModules).
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
    [test_ok_case, test_function_case, test_quote_case,
     test_unquote_splicing_case, test_pattern_case, test_other_case,
     test_macro_with_warnings, test_macro_with_vars, test_macro_order, test_merge_rename_function].

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
test_ok_case(_Config) ->
    ?assertEqual(ok, macro_test:test_ok()).

test_function_case(_Config) ->
    ?assertEqual(ok, macro_test:test_function(world)),
    ?assertEqual({error, foo}, macro_test:test_function(foo)),
    ok.

test_quote_case(_Config) ->
    ?assertEqual({ok, ok}, macro_test:test_unquote()),
    ?assertEqual({ok, ok}, macro_test:test_binding()),
    ok.

test_unquote_splicing_case(_Config) ->
    ?assertEqual({ok, {hello, foo, bar, world}}, macro_test:test_unquote_splicing()),
    {Value1, Value2} = macro_test:test_unquote_splicing_mix(),
    ?assertEqual({ok, [hello, foo, bar, world], {hello, foo, bar, world}}, Value1),
    ?assertEqual({error, foo, zaa}, Value2),
    ok.

test_pattern_case(_Config) ->
    ?assertEqual({hello, world, foo, bar}, macro_test:test_match_pattern()),
    ?assertEqual({ok, {hello2, world, world, {hello, world}}}, macro_test:test_function_pattern_1()),
    ?assertEqual({error, {foo, bar}}, macro_test:test_function_pattern_2()),
    ?assertEqual({ok, 11}, macro_test:test_case_pattern_1()),
    ?assertEqual({ok, {hello, world, foo, bar}}, macro_test:test_case_pattern_2()),
    ?assertEqual({error, task}, macro_test:test_case_pattern_3()),
    ok.

test_quote_code_case(_Config) ->
    ?assertEqual(ok, macro_test:test_quote_code()),
    ?assertEqual({hello, ok}, macro_test:test_quote_line_1()),
    Ast = {tuple, 20, [{atom, 20, a}, {atom, 20, b}]},
    NewAst = {tuple, 22, [{atom, 22, ok}, {tuple, 23, [{atom, 23, hello}, Ast]}]},
    ?assertEqual(NewAst,macro_example:quote_line_2(Ast)),
    ok.

test_other_case(_Config) ->
    ?assertEqual(true, macro_test:test_case()),
    ?assertException(exit, throw, macro_test:test_try_catch()),
    ?assertEqual({hello, ok, world}, macro_test:test_function()),
    ?assertMatch({ok, {_, _, macro_test}}, macro_test:test_attributes()),
    ?assertEqual({ok, {hello, world}}, macro_test:test_group_args()),
    ok.

test_macro_order(_Config) ->
    ?assertEqual({fail, ok}, macro_test:test_macro_order()),
    ok.

test_macro_with_warnings(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_with_warnings, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    io:format("error is ~p~n", [astranaut_error:printable(ErrorStruct)]),
    {[{File, Errors}], [{File, Warnings}]} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    Local = macro_with_warnings__local_macro,
    ?assertMatch(
       [{45,  Local, {macro_exception, _MFA, [], _StackTrace}},
        {48,  Local, bar}
       ], Errors),
    ?assertEqual("macro_with_warnings.erl", filename:basename(File)),
    ?assertMatch(
       [{3,  Local, noop_function},
        {4,  astranaut_macro, invalid_macro_attribute},
        {5,  astranaut_macro, invalid_macro_attribute},
        {12, Local, noop},
        {18, Local, noop},
        {20, Local, noop},
        {25, astranaut_quote,{unquote_splicing_pattern_non_empty_tail,[{atom, _, tail}]}}
       ],
       Warnings),
    ?assertEqual(ok, macro_with_warnings:test_attributes()),
    ok.

test_macro_with_vars(_Config) ->
    Value = macro_test:test_macro_with_vars(13),
    ?assertEqual(112, Value).

test_merge_rename_function(_Config) ->
    Value1 = macro_test:test_merged_function(ok_1),
    Value2 = macro_test:test_merged_function(ok_2),
    Value3 = macro_test:test_merged_function(ok_3),
    Value4 = macro_test:test_merged_function(ok_4),
    ?assertEqual({ok_1, ok_2, ok_3, ok_4}, {Value1, Value2, Value3, Value4}),
    ok.
