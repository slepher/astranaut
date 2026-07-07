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
    TestModules = [macro_exports, macro_example,
                   macro_uniform_a, macro_uniform_b, macro_uniform_test,
                   macro_uniform_override_test,
                   macro_uniform_import_force_override_test,
                   macro_node_role_test,
                   macro_validator_slots,
                   macro_test],
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
     test_macro_with_warnings, test_macro_with_error,
     test_macro_with_vars, test_macro_order, test_merge_rename_function,
     test_nested_macro, test_recursive_macro, test_macro_literal,
     test_uniform_cross_import_order, test_uniform_nested_macros,
     test_uniform_outer_macro, test_uniform_generated_macro_chain,
     test_uniform_direct_macro_function_call, test_uniform_attribute_generates_local_macro,
     test_uniform_local_generates_external, test_uniform_macro_override_error,
     test_uniform_import_override_error, test_uniform_local_force_override,
     test_uniform_import_force_override,
     test_use_macro_errors,
     test_macro_format_error_predefined_errors,
     test_uniform_macro_error,
     test_uniform_macro_invalid_return,
     test_uniform_local_macro_invalid_return,
     test_macro_validator_slot_errors,
     test_uniform_macro_max_depth,
     test_macro_node_roles].

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
    ?assertEqual({hello, ok}, macro_test:test_quote_pos_1()),
    Ast = {tuple, 20, [{atom, 20, a}, {atom, 20, b}]},
    NewAst = {tuple, 22, [{atom, 22, ok}, {tuple, 23, [{atom, 23, hello}, Ast]}]},
    ?assertEqual(NewAst,macro_example:quote_pos_2(Ast)),
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
    Basepos = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {FileErrors, [{File, Warnings}]} = astranaut_test_lib:realize_with_baseline(Basepos, ErrorStruct),
    ?assertEqual([], FileErrors),
    Local = macro_with_warnings__local_macro,
    ?assertEqual("macro_with_warnings.erl", filename:basename(File)),
    ?assertMatch(
       [{2,  astranaut_macro, invalid_macro_attribute},
        {3,  astranaut_macro, invalid_macro_attribute},
        {5,  Local, noop_function},
        {12, Local, noop},
        {18, Local, noop},
        {20, Local, noop},
        {25, astranaut_quote,{unquote_splicing_pattern_non_empty_tail,[{atom, _, tail}]}}
       ],
       Warnings),
    assert_formatted_messages(Warnings),
    ?assertEqual(ok, macro_with_warnings:test_attributes()),
    ok.

test_macro_with_error(Config) ->
    ct:pal("Verifying Test Code Integrity: Expecting Line 27 check to exist."),
    Forms = astranaut_test_lib:test_module_forms(macro_with_error, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    Return = astranaut_test_lib:compile_test_forms(Forms),
    ErrorStruct = astranaut_return:run_error(Return),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),

    Local = macro_with_error__local_macro,
    ?assertMatch(
       [{2,  astranaut_macro, {invalid_import_macro_attr, {invalid_macro_tuple}}},
        {3,  astranaut_macro, {import_macro_failed, non_exists_module}},
        {4,  astranaut_macro, {unimported_macro_module, unimported_macro_module}},
        {6,  astranaut_macro, {undefined_macro, undefined_macro_0, 0}},
        {7,  astranaut_macro, {undefined_macro, undefined_macro_1, 0}},
        {8,  astranaut_macro, {undefined_macro, undefined_macro_2, 0}},
        {9,  astranaut_macro, {undefined_macro, undefined_macro_3, 0}},
        {13, Local, {macro_exception, _MFA, [], _StackTrace}},
        {16, Local, bar},
        {27,astranaut_macro, {max_macro_expansion_depth_exceeded, {macro_example,recursive_macro}, [{integer, _Pos, 6}]}}
       ], Errors),
    assert_formatted_messages(Errors),
    %% %% TODO: astranaut:map_m does not just return error, but inject error_maker to forms.
    %% %% while fixing this, uncomment testcase below.
    %% Forms1 = astranaut_return:run(Return),
    %% ClauseNums =
    %%     lists:foldl(
    %%       fun({function, _Pos, Name, Arity, Clauses}, Acc) ->
    %%               maps:put({Name, Arity}, length(Clauses), Acc);
    %%          (_Form, Acc) ->
    %%               Acc
    %%       end, #{}, Forms1),
    %% ErrorMacro1 = maps:get({error_macro_1, 0}, ClauseNums, 0),
    %% ErrorMacro2 = maps:get({error_macro_2, 1}, ClauseNums, 1),
    %% ?assertEqual({0, 1}, {ErrorMacro1, ErrorMacro2}),
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

test_nested_macro(_Config) ->
    Value = macro_test:test_nested_macro(7),
    ?assertEqual(45, Value).

test_recursive_macro(_Config) ->
    Value = macro_test:test_recursive_macro(),
    ?assertEqual({4, {3, {2, {1, blast_off}}}}, Value).

test_macro_literal(_Config) ->
    %%?assertMatch({atom, _, ok}, macro_test:test_macro_literal()).
    ok.

test_uniform_cross_import_order(_Config) ->
    ?assertEqual({b_generated, {a, {from_a, ok}}}, macro_uniform_test:later_generates_earlier()),
    ?assertEqual({a_generated, {b, {from_b, ok}}}, macro_uniform_test:earlier_generates_later()),
    ok.

test_uniform_nested_macros(_Config) ->
    ?assertEqual({a, {from_a, {b, {from_b, ok}}}}, macro_uniform_test:nested_external()),
    ok.

test_uniform_outer_macro(_Config) ->
    ?assertEqual({outer_seen_raw_b_call}, macro_uniform_test:outer_preserves_raw_child()),
    ok.

test_uniform_generated_macro_chain(_Config) ->
    ?assertEqual({a_generated_chain, {b_generated, {a, {from_a, ok}}}},
                 macro_uniform_test:generated_chain()),
    ok.

test_uniform_direct_macro_function_call(_Config) ->
    ?assertEqual({a_direct, {b, {from_b, ok}}}, macro_uniform_test:direct_macro_function_call()),
    ok.

test_uniform_attribute_generates_local_macro(_Config) ->
    ?assertEqual({a, {from_a, attribute_generated}}, macro_uniform_test:attribute_generated_local_macro()),
    ok.

test_uniform_local_generates_external(_Config) ->
    ?assertEqual({a, {from_a, ok}}, macro_uniform_test:local_generates_external()),
    ok.

test_uniform_macro_override_error(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_uniform_override_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{2, astranaut_macro,
         {macro_override,
          {same_name, 1},
          #{macro_module := macro_uniform_a, function := to_a, arity := 1},
          #{macro_module := macro_uniform_override_error_test, function := same_name, arity := 1}}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_uniform_import_override_error(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_uniform_import_override_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{2, astranaut_macro,
         {macro_override,
          {same_name, 1},
          #{macro_module := macro_uniform_a, function := to_a, arity := 1},
          #{macro_module := macro_uniform_b, function := to_b, arity := 1}}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_uniform_local_force_override(_Config) ->
    ?assertEqual({local_same_name, ok}, macro_uniform_override_test:same_name_call()),
    ok.

test_uniform_import_force_override(_Config) ->
    ?assertEqual({b, {from_b, ok}}, macro_uniform_import_force_override_test:same_name_call()),
    ok.

test_use_macro_errors(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_use_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro, {unexported_macro, macro_uniform_a, missing_export, 1}},
        {4, astranaut_macro, {undefined_macro, missing_local, 0}},
       {5, astranaut_macro, {invalid_function_with_arity, {bad_arity, -1}}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_macro_format_error_predefined_errors(_Config) ->
    ExistingMacro = #{macro_module => macro_a, function => to_a, arity => 1},
    OverridingMacro = #{macro_module => macro_b, function => to_b, arity => 1},
    DirectInvalidReturn =
        #{macro => #{mfa => #{module => macro_a, function => bad, arity => 1}},
          reason => invalid_role,
          expected_role => expression,
          actual_type => function},
    NestedInvalidReturn =
        #{origin_macro => #{mfa => #{module => macro_a, function => outer, arity => 0}},
          current_macro => #{mfa => #{module => macro_a, function => inner, arity => 0}},
          reason => invalid_role,
          expected_role => guard,
          actual_type => application},
    Errors =
        [{import_macro_failed, missing_macro_module},
         {invalid_import_macro_attr, {invalid_macro_tuple}},
         {unimported_macro_module, macro_a},
         {unexported_macro, macro_a, missing, 1},
         {undefined_macro, missing, 0},
         {invalid_use_macro, #{macro_module => macro_a, function => to_a, arity => 1}},
         {macro_override, {macro_a, to_a, 1}, ExistingMacro, OverridingMacro},
         {non_exported_formatter, macro_formatter},
         {unloaded_formatter_module, missing_formatter},
         invalid_macro_attribute,
         {max_macro_expansion_depth_exceeded, {macro_a, recurse}, [{integer, 1, 3}]},
         {max_macro_expansion_depth_exceeded, recurse, [{integer, 1, 3}]},
         {macro_exception,
          #{module => macro_a, function => explode, arity => 1},
          [{atom, 1, ok}],
          {error, bad_macro, []}},
         {invalid_macro_return, DirectInvalidReturn},
         {invalid_macro_return, NestedInvalidReturn}],
    lists:foreach(fun assert_macro_format_error/1, Errors).

test_uniform_macro_error(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_uniform_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, macro_uniform_a,
         {uniform_a_error,
          {tuple, _,
           [{atom, _, b},
            {tuple, _, [{atom, _, from_b}, {atom, _, ok}]}]}}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_uniform_macro_invalid_return(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_uniform_invalid_return_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {invalid_macro_return,
          #{macro := #{mfa := #{module := macro_uniform_a,
                                function := invalid_return,
                                arity := 1},
                       ast := {call, _, _, _}},
            reason := invalid_node,
            expected_role := expression}}},
        {6, astranaut_macro,
         {invalid_macro_return,
          #{origin_macro := #{mfa := #{module := macro_uniform_a,
                                       function := gen_invalid,
                                       arity := 1},
                              ast := {call, _, _, _}},
            current_macro := #{mfa := #{module := macro_uniform_a,
                                        function := invalid_return,
                                        arity := 1},
                               ast := {call, _, _, _}},
            reason := invalid_node,
            expected_role := expression}}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_uniform_local_macro_invalid_return(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_uniform_invalid_local_return_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {invalid_macro_return,
          #{macro := #{mfa := #{function := bad_local,
                                arity := 0,
                                local := true},
                       ast := {call, _, _, _}},
            reason := invalid_node,
            expected_role := expression}}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_macro_validator_slot_errors(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_validator_slot_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {invalid_macro_return,
          #{origin_macro := #{mfa := #{module := macro_validator_slots,
                                       function := pattern_outer,
                                       arity := 0}},
            current_macro := #{mfa := #{module := macro_validator_slots,
                                        function := pattern_inner,
                                        arity := 0}},
            validator := {slot, clause, patterns, pattern},
            expected_role := pattern,
            actual_type := application}}},
        {6, astranaut_macro,
         {invalid_macro_return,
          #{origin_macro := #{mfa := #{module := macro_validator_slots,
                                       function := guard_outer,
                                       arity := 0}},
            current_macro := #{mfa := #{module := macro_validator_slots,
                                        function := guard_inner,
                                        arity := 0}},
            validator := {slot, conjunction, elements, guard},
            expected_role := guard,
            actual_type := application}}},
        {9, astranaut_macro,
         {invalid_macro_return,
          #{origin_macro := #{mfa := #{module := macro_validator_slots,
                                       function := expression_outer,
                                       arity := 0}},
            current_macro := #{mfa := #{module := macro_validator_slots,
                                        function := expression_inner,
                                        arity := 0}},
            validator := {role, expression},
            expected_role := expression,
            actual_type := function}}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_uniform_macro_max_depth(Config) ->
    Forms = astranaut_test_lib:test_module_forms(macro_uniform_depth_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} = astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {max_macro_expansion_depth_exceeded,
          {macro_uniform_a, recurse_a},
           [{integer, _, 12}]}}],
       Errors),
    assert_formatted_messages(Errors),
    ok.

test_macro_node_roles(_Config) ->
    ?assertEqual(ok, macro_node_role_test:test_node_roles()),
    ok.

assert_macro_format_error(Error) ->
    Message = astranaut_macro:format_error(Error),
    ?assert(io_lib:deep_char_list(Message)),
    ?assertNotEqual([], lists:flatten(Message)).

assert_formatted_messages(Messages) ->
    lists:foreach(fun assert_formatted_message/1, Messages).

assert_formatted_message({_Line, Formatter, Error}) ->
    Message = Formatter:format_error(Error),
    ?assert(io_lib:deep_char_list(Message)),
    ?assertNotEqual([], lists:flatten(Message)).
