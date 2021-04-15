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
    erlang:system_flag(backtrace_depth, 20),
    TestModules = [rebinding_example, rebinding_test],
    erl_af_test_lib:load_data_modules(Config, TestModules),
    Forms = erl_af_test_lib:test_module_forms(rebinding_test, Config),
    Forms1 = erl_af_rebinding:parse_transform(Forms, erl_af_test_lib:compile_opts()),
    Functions =
        lists:foldl(
            fun({function, _Line, Name, _Arity, Clauses}, Acc) ->
                    Clauses1 = erl_af_lib:replace_line(Clauses, 0),
                    maps:put(Name, Clauses1, Acc);
               (_Form, Acc) ->
                   Acc
            end, #{}, Forms1),
    [{functions, Functions}|Config].

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
    [test_lc, test_function, test_case, test_if,
     test_map, test_map_update,
     test_rec, test_rec_update,
     test_operator, test_list, test_tuple,
     test_pattern_save_var, test_pattern_save_var_in_fun, test_pattern_save_var_in_case
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
test_lc(Config) ->
    equal_functions(test_lc, test_lc_origin, Config),
    A = rebinding_test:test_lc(10),
    B = rebinding_test:test_lc_origin(10),
    ?assertEqual(A, B),
    ok.

test_function(Config) ->
    equal_functions(test_function, test_function_origin, Config),
    A = rebinding_test:test_function(10),
    B = rebinding_test:test_function_origin(10),
    ?assertEqual(A, B),
    ok.

test_case(Config) ->
    equal_functions(test_case, test_case_origin, Config),
    equal_functions(test_case_pinned, test_case_origin, Config),
    A = rebinding_test:test_case(10),
    B = rebinding_test:test_case_origin(10),
    C = rebinding_test:test_case_pinned(10),
    ?assertEqual(A, B),
    ?assertEqual(A, C),
    ok.

test_if(Config) ->
    equal_functions(test_if, test_if_origin, Config),
    A = rebinding_test:test_if(10),
    B = rebinding_test:test_if_origin(10),
    ?assertEqual(A, B),
    ok.

test_rec(Config) ->
    equal_functions(test_rec, test_rec_origin, Config),
    A = rebinding_test:test_rec(10),
    B = rebinding_test:test_rec_origin(10),
    ?assertEqual(A, B),
    ok.

test_rec_update(Config) ->
    equal_functions(test_rec_update, test_rec_update_origin, Config),
    A = rebinding_test:test_rec_update(10),
    B = rebinding_test:test_rec_update_origin(10),
    ?assertEqual(A, B),
    ok.

test_map(Config) ->
    equal_functions(test_map, test_map_origin, Config),
    A = rebinding_test:test_map(10),
    B = rebinding_test:test_map_origin(10),
    ?assertEqual(A, B),
    ok.

test_map_update(Config) ->
    equal_functions(test_map_update, test_map_update_origin, Config),
    A = rebinding_test:test_map_update(10),
    B = rebinding_test:test_map_update_origin(10),
    ?assertEqual(A, B),
    ok.

test_operator(Config) ->
    equal_functions(test_operator, test_operator_origin, Config),
    A = rebinding_test:test_operator(10),
    B = rebinding_test:test_operator_origin(10),
    ?assertEqual(A, B),
    ok.

test_tuple(Config) ->
    equal_functions(test_tuple, test_tuple_origin, Config),
    A = rebinding_test:test_tuple(10),
    B = rebinding_test:test_tuple_origin(10),
    ?assertEqual(A, B),
    ok.

test_list(Config) ->
    equal_functions(test_list, test_list_origin, Config),
    A = rebinding_test:test_list(10),
    B = rebinding_test:test_list_origin(10),
    ?assertEqual(A, B),
    ok.

test_pattern_save_var(_Config) ->
    A = rebinding_test:test_pattern_same_var(1, 2),
    B = rebinding_test:test_pattern_same_var(3, 3),
    ?assertEqual(3, A),
    ?assertEqual(7, B),
    ok.

test_pattern_save_var_in_fun(_Config) ->
    A = rebinding_test:test_pattern_same_var_in_fun(1, 2),
    B = rebinding_test:test_pattern_same_var_in_fun(3, 3),
    ?assertEqual(3, A),
    ?assertEqual(7, B),
    ok.

test_pattern_save_var_in_case(_Config) ->
    A = rebinding_test:test_pattern_same_var_in_case(1, 2),
    B = rebinding_test:test_pattern_same_var_in_case(3, 3),
    ?assertEqual(3, A),
    ?assertEqual(7, B),
    ok.


equal_functions(F1, F2, Config) ->
    Functions = proplists:get_value(functions, Config),
    ?assertEqual(maps:get(F1, Functions), maps:get(F2, Functions)).
