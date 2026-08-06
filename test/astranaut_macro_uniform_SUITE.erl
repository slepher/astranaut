%%%-------------------------------------------------------------------
%%% Source-ordered uniform macro resolution and recursive expansion.
%%%-------------------------------------------------------------------
-module(astranaut_macro_uniform_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config0) ->
    Config = astranaut_test_lib:with_suite_data_dir(
               Config0, astranaut_macro_SUITE),
    TestModules = [macro_uniform_a, macro_uniform_b, macro_uniform_test,
                   macro_uniform_override_test,
                   macro_uniform_import_force_override_test,
                   macro_uniform_external_after_local_force_test,
                   macro_validator_slots],
    astranaut_test_lib:load_data_modules(Config, TestModules).

end_per_suite(_Config) ->
    ok.

all() ->
    [test_uniform_cross_import_order,
     test_uniform_nested_macros,
     test_uniform_outer_macro,
     test_uniform_generated_macro_chain,
     test_uniform_direct_macro_function_call,
     test_uniform_attribute_generates_local_macro,
     test_uniform_local_generates_external,
     test_uniform_macro_override_error,
     test_uniform_import_override_error,
     test_uniform_local_force_override,
     test_uniform_import_force_override,
     test_uniform_external_after_local_force,
     test_uniform_external_after_local_error,
     test_uniform_macro_error,
     test_uniform_macro_invalid_return,
     test_uniform_local_macro_invalid_return,
     test_macro_validator_slot_errors,
     test_macro_validator_pre_local_validation,
     test_uniform_macro_max_depth].

test_uniform_cross_import_order(_Config) ->
    ?assertEqual({b_generated, {a, {from_a, ok}}},
                 macro_uniform_test:later_generates_earlier()),
    ?assertEqual({a_generated, {b, {from_b, ok}}},
                 macro_uniform_test:earlier_generates_later()),
    ok.

test_uniform_nested_macros(_Config) ->
    ?assertEqual({a, {from_a, {b, {from_b, ok}}}},
                 macro_uniform_test:nested_external()),
    ok.

test_uniform_outer_macro(_Config) ->
    ?assertEqual({outer_seen_raw_b_call},
                 macro_uniform_test:outer_preserves_raw_child()),
    ok.

test_uniform_generated_macro_chain(_Config) ->
    ?assertEqual({a_generated_chain, {b_generated, {a, {from_a, ok}}}},
                 macro_uniform_test:generated_chain()),
    ok.

test_uniform_direct_macro_function_call(_Config) ->
    ?assertEqual({a_direct, {b, {from_b, ok}}},
                 macro_uniform_test:direct_macro_function_call()),
    ok.

test_uniform_attribute_generates_local_macro(_Config) ->
    ?assertEqual({a, {from_a, attribute_generated}},
                 macro_uniform_test:attribute_generated_local_macro()),
    ok.

test_uniform_local_generates_external(_Config) ->
    ?assertEqual({a, {from_a, ok}},
                 macro_uniform_test:local_generates_external()),
    ok.

test_uniform_macro_override_error(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_uniform_override_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], [{_WarningFile, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{2, astranaut_macro,
         {macro_override,
          {same_name, 1},
          #{macro_module := macro_uniform_a, function := to_a, arity := 1},
          #{macro_module := macro_uniform_override_error_test,
            function := same_name, arity := 1}}}],
       Errors),
    ?assertEqual(
       [{2, astranaut_macro,
         {missing_macro_formatter, macro_uniform_override_error_test}}],
       Warnings),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_uniform_import_override_error(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_uniform_import_override_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], [{_WarningFile, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{2, astranaut_macro,
         {macro_override,
          {same_name, 1},
          #{macro_module := macro_uniform_a, function := to_a, arity := 1},
          #{macro_module := macro_uniform_b, function := to_b, arity := 1}}}],
       Errors),
    ?assertEqual(
       [{-4, astranaut_macro,
         {missing_macro_formatter, macro_uniform_b}}],
       Warnings),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_uniform_local_force_override(_Config) ->
    ?assertEqual({local_same_name, ok},
                 macro_uniform_override_test:same_name_call()),
    ok.

test_uniform_import_force_override(_Config) ->
    ?assertEqual({b, {from_b, ok}},
                 macro_uniform_import_force_override_test:same_name_call()),
    ok.

test_uniform_external_after_local_force(_Config) ->
    ?assertEqual(
       {a, {from_a, ok}},
       macro_uniform_external_after_local_force_test:same_name_call()),
    ok.

test_uniform_external_after_local_error(Config) ->
    assert_macro_error(
      macro_uniform_external_after_local_error_test, Config,
      fun({macro_override, {same_name, 1},
           #{macro_source := local_macro},
           #{macro_module := macro_uniform_a,
             function := to_a, arity := 1}}) ->
              true;
         (_) ->
              false
      end,
      [{2, astranaut_macro,
        {missing_macro_formatter,
         macro_uniform_external_after_local_error_test}}]),
    ok.

test_uniform_macro_error(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_uniform_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], [{_WarningFile, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, macro_uniform_a,
         {uniform_a_error,
          {tuple, _,
           [{atom, _, b},
            {tuple, _, [{atom, _, from_b}, {atom, _, ok}]}]}}}],
       Errors),
    ?assertEqual(
       [{-2, astranaut_macro,
         {missing_macro_formatter, macro_uniform_b}}],
       Warnings),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_uniform_macro_invalid_return(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_uniform_invalid_return_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
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
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_uniform_local_macro_invalid_return(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_uniform_invalid_local_return_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], [{_WarningFile, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
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
    ?assertEqual(
       [{-2, astranaut_macro,
         {missing_macro_formatter, macro_uniform_invalid_local_return_test}}],
       Warnings),
    astranaut_test_lib:assert_formatted_messages(Errors ++ Warnings),
    ok.

test_macro_validator_slot_errors(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_validator_slot_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {invalid_macro_return,
          #{macro := #{mfa := #{module := macro_validator_slots,
                                function := pattern_outer,
                                arity := 0}},
            validator := {slot, clause, patterns, pattern},
            expected_role := pattern,
            actual_type := application}}},
        {6, astranaut_macro,
         {invalid_macro_return,
          #{macro := #{mfa := #{module := macro_validator_slots,
                                function := guard_outer,
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
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_macro_validator_pre_local_validation(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_validator_pre_local_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {invalid_macro_return,
          #{macro := #{mfa := #{module := macro_validator_slots,
                                function := pre_pattern_outer,
                                arity := 0}},
            validator := {slot, clause, patterns, pattern},
            expected_role := pattern,
            actual_type := application}}}],
       Errors),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

test_uniform_macro_max_depth(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_uniform_depth_error_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], [{_WarningFile, Warnings}]} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assertMatch(
       [{3, astranaut_macro,
         {max_macro_expansion_depth_exceeded,
          {macro_uniform_a, recurse_a},
          [{integer, _, 12}]}}],
       Errors),
    ?assertEqual(
       [{-2, astranaut_macro,
         {missing_macro_formatter, macro_uniform_b}}],
       Warnings),
    astranaut_test_lib:assert_formatted_messages(Errors),
    ok.

assert_macro_error(Module, Config, MatchError, ExpectedWarnings) ->
    Forms = astranaut_test_lib:test_module_forms(Module, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], WarningFiles} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    Warnings = lists:append([FileWarnings || {_WarningFile, FileWarnings} <-
                                                     WarningFiles]),
    ?assert(
       lists:any(
         fun({_Line, astranaut_macro, Error}) ->
                 MatchError(Error);
            (_) ->
                 false
         end, Errors)),
    ?assertEqual(ExpectedWarnings, Warnings),
    astranaut_test_lib:assert_formatted_messages(Errors ++ Warnings).
