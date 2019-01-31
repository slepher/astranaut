%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_SUITE).

-compile(export_all).

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
    [test_ok_case, test_function_case, test_quote_case, 
     test_unquote_splicing_case, test_pattern_case, test_other_case,
     test_macro_pt_case, test_macro_with_warnings].

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
    ?assertEqual(ok, astranaut_macro_test:test_ok()).

test_function_case(_Config) ->
    ?assertEqual(ok, astranaut_macro_test:test_function(world)),
    ?assertEqual({error, foo}, astranaut_macro_test:test_function(foo)),
    ok.

test_quote_case(_Config) ->
    ?assertEqual({ok, ok}, astranaut_macro_test:test_unquote()),
    ?assertEqual({ok, ok}, astranaut_macro_test:test_binding()),
    ok.

test_unquote_splicing_case(_Config) ->
    ?assertEqual({ok, {hello, foo, bar, world}}, astranaut_macro_test:test_unquote_splicing()),
    ?assertEqual({ok, [hello, foo, bar, world], {hello, foo, bar, world}}, astranaut_macro_test:test_unquote_splicing_mix_1()),
    ?assertEqual({error, foo, zaa}, astranaut_macro_test:test_unquote_splicing_mix_2()),
    ok.

test_pattern_case(_Config) ->
    ?assertEqual({hello, world, foo, bar}, astranaut_macro_test:test_match_pattern()),
    ?assertEqual({ok, {hello2, world, world, {hello, world}}}, astranaut_macro_test:test_function_pattern_1()),
    ?assertEqual({error, {foo, bar}}, astranaut_macro_test:test_function_pattern_2()),
    ?assertEqual({ok, 11}, astranaut_macro_test:test_case_pattern_1()),
    ?assertEqual({ok, {hello, world, foo, bar}}, astranaut_macro_test:test_case_pattern_2()),
    ?assertEqual({error, task}, astranaut_macro_test:test_case_pattern_3()),
    ok.

test_quote_code_case(_Config) ->
    ?assertEqual(ok, astranaut_macro_test:test_quote_code()),
    ?assertEqual({hello, ok}, astranaut_macro_test:test_quote_line_1()),
    Ast = {tuple, 20, [{atom, 20, a}, {atom, 20, b}]},
    NewAst = {tuple, 22, [{atom, 22, ok}, {tuple, 23, [{atom, 23, hello}, Ast]}]},
    ?assertEqual(NewAst,astranaut_macro_example:quote_line_2(Ast)),
    ok.

test_other_case(_Config) ->
    ?assertEqual(true, astranaut_macro_test:test_case()),
    ?assertException(exit, throw, astranaut_macro_test:test_try_catch()),
    ?assertEqual({hello, ok, world}, astranaut_macro_test:test_function()),
    ?assertMatch({ok, {_, _, astranaut_macro_test}}, astranaut_macro_test:test_attributes()),
    ?assertEqual({ok, {hello, world}}, astranaut_macro_test:test_group_args()),
    ok.

test_macro_pt_case(_Config) ->
    ?assertEqual({ok, ok}, astranaut_macro_with_pt:test()),
    ?assertEqual(ok, astranaut_macro_with_pt:hello(world)),
    ?assertEqual({error, foo}, astranaut_macro_with_pt:hello(foo)),
    ok.

test_macro_with_warnings(_Config) ->
    Warnings = astranaut_warnings:warnings(),
    ?assertEqual(
       [{47, astranaut_quote,{non_empty_tail,[{atom,47,tail}]}},
        {28, astranaut_traverse, noop_function},
        {34, astranaut_warnings__local_macro, noop},
        {40, astranaut_warnings__local_macro, noop},
        {42, astranaut_warnings__local_macro, noop}], Warnings),
    ok.
    
