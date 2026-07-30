%%%-------------------------------------------------------------------
%%% Macro pass orchestration and local/external scan integration.
%%%-------------------------------------------------------------------
-module(astranaut_macro_pass_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config0) ->
    Config = astranaut_test_lib:with_suite_data_dir(
               Config0, astranaut_macro_SUITE),
    TestModules =
        [macro_uniform_a, macro_uniform_b,
         macro_pass_boot, macro_pass_generated, macro_pass_depth,
         macro_pass_test, macro_pass_attribute_buffer_test,
         macro_pass_no_backscan_test, macro_pass_local_chain_test,
         macro_pass_local_dependency_test,
         macro_pass_local_replacement_whitelist_test,
         macro_pass_external_replacement_local_test,
         macro_pass_local_declaration_group_test,
         macro_pass_external_helper, macro_pass_inject_attrs,
         macro_pass_external_local_helper_test,
         macro_pass_scan_local_attr_test,
         macro_pass_local_generated_import_test,
         macro_pass_generated_local_attribute_test,
         macro_pass_local_no_backscan_test,
         macro_pass_external_remaining_test,
         macro_pass_closure_roots_test,
         macro_pass_closure_roots_union_test,
         macro_function_helper,
         macro_pass_function_helper_test,
         macro_pass_retained_helper_test,
         macro_pass_final_outside_snapshot_test,
         macro_pass_generated_function_delay_test,
         macro_pass_export_not_local_test,
         macro_pass_export_and_local_test,
         macro_pass_scoped_attribute_state_test,
         macro_pass_scoped_function_state_test,
         macro_pass_local_compile_context_test,
         macro_pass_local_runtime_context_test],
    astranaut_test_lib:load_data_modules(Config, TestModules).

end_per_suite(_Config) ->
    ok.

all() ->
    [test_macro_pass_generated_import,
     test_macro_pass_no_backscan,
     test_macro_pass_local_attribute_chain,
     test_macro_pass_local_replacement_whitelist,
     test_macro_pass_external_replacement_local_dependency,
     test_macro_pass_local_dependency,
     test_macro_pass_local_declaration_group,
     test_macro_pass_external_generated_local_helper,
     test_macro_pass_scan_local_attribute,
     test_macro_pass_local_generated_import,
     test_macro_pass_generated_local_attribute,
     test_macro_pass_local_no_backscan,
     test_macro_pass_external_remaining_cases,
     test_macro_pass_closure_roots,
     test_macro_pass_closure_roots_missing_error,
     test_macro_pass_closure_roots_union,
     test_macro_pass_function_helper,
     test_macro_pass_retained_helper,
     test_macro_pass_local_body_environment_mutation_error,
     test_macro_pass_locked_spec_mutation_error,
     test_macro_pass_final_expands_outside_snapshot,
     test_macro_pass_generated_function_delay,
     test_macro_pass_export_not_local,
     test_macro_pass_export_and_local,
     test_macro_pass_scoped_attribute_state,
     test_macro_pass_scoped_function_state,
     test_macro_pass_local_compile_context,
     test_macro_pass_local_macro_environment,
     test_macro_pass_generated_macro_options,
     test_macro_pass_attribute_buffer,
     test_macro_pass_attribute_buffer_cross_depth,
     test_macro_pass_attribute_buffer_self_depth,
     test_macro_pass_attribute_buffer_total_depth,
     test_macro_pass_export_helper_unlocked,
     test_macro_pass_local_environment_mutation_errors,
     test_macro_pass_locked_snapshot_mutation_error,
     test_local_macro_module_cleanup,
     test_local_macro_module_name_conflict,
     test_module_lock_serializes_processes].

test_macro_pass_generated_import(_Config) ->
    ?assertEqual({pass_generated, ok},
                 macro_pass_test:pass_generated_value()),
    ok.

test_macro_pass_no_backscan(_Config) ->
    ?assertEqual({pass_generated, ok},
                 macro_pass_no_backscan_test:pass_generated_value()),
    ok.

test_macro_pass_local_attribute_chain(_Config) ->
    ?assertEqual({local_attribute_chain, ok},
                 macro_pass_local_chain_test:local_chain_value()),
    ok.

test_macro_pass_local_replacement_whitelist(_Config) ->
    ?assertEqual(
       {replacement_whitelist, ok},
       macro_pass_local_replacement_whitelist_test:value()),
    ok.

test_macro_pass_external_replacement_local_dependency(_Config) ->
    ?assertEqual(
       {external_replacement_local, ok},
       macro_pass_external_replacement_local_test:value()),
    ok.

test_macro_pass_local_dependency(_Config) ->
    ?assertEqual({wrapped, ok},
                 macro_pass_local_dependency_test:value()),
    ok.

test_macro_pass_local_declaration_group(_Config) ->
    ?assertEqual({group_foo, ok},
                 macro_pass_local_declaration_group_test:value()),
    ok.

test_macro_pass_external_generated_local_helper(_Config) ->
    ?assertEqual(
       {external_generated_helper, ok},
       macro_pass_external_local_helper_test:value()),
    ok.

test_macro_pass_scan_local_attribute(_Config) ->
    ?assertEqual(ok, macro_pass_scan_local_attr_test:value()),
    ok.

test_macro_pass_local_generated_import(_Config) ->
    ?assertEqual(
       {pass_generated, ok},
       macro_pass_local_generated_import_test:pass_generated_value()),
    ok.

test_macro_pass_generated_local_attribute(_Config) ->
    ?assertEqual(ok,
                 macro_pass_generated_local_attribute_test:value()),
    ok.

test_macro_pass_local_no_backscan(_Config) ->
    ?assertEqual(
       {pass_generated, ok},
       macro_pass_local_no_backscan_test:pass_generated_value()),
    ok.

test_macro_pass_external_remaining_cases(_Config) ->
    ?assertEqual({a, {from_a, ok}},
                 macro_pass_external_remaining_test:alias_value()),
    ?assertEqual({a, {from_a, ok}},
                 macro_pass_external_remaining_test:alias_attr_value()),
    ?assertEqual(
       {external_attr_chain, ok},
       macro_pass_external_remaining_test:chained_attr_value()),
    ?assertEqual(
       {pass_generated, ok},
       macro_pass_external_remaining_test:pass_generated_value()),
    ?assertEqual(
       {external_non_env_attr, ok},
       macro_pass_external_remaining_test:non_env_attr_value()),
    ?assertEqual(
       {a, {from_a, ok}},
       macro_pass_external_remaining_test:final_external_value()),
    ?assertEqual(
       {injected_attribute_attrs, [early]},
       macro_pass_external_remaining_test:injected_attribute_attrs_value()),
    ?assertEqual(
       {injected_attrs, [early, source_late, generated_late]},
       macro_pass_external_remaining_test:injected_attrs_value()),
    ok.

test_macro_pass_closure_roots(_Config) ->
    ?assertEqual({extra_helper, ok},
                 macro_pass_closure_roots_test:value()),
    ok.

test_macro_pass_closure_roots_missing_error(Config) ->
    assert_macro_pass_error(
      macro_pass_closure_roots_missing_error_test, Config,
      fun({invalid_closure_roots, Missing}) ->
              lists:member({missing_helper, 1}, Missing);
         (_) ->
              false
      end),
    ok.

test_macro_pass_closure_roots_union(_Config) ->
    ?assertEqual({extra_union_a, ok},
                 macro_pass_closure_roots_union_test:value_a()),
    ?assertEqual({extra_union_b, ok},
                 macro_pass_closure_roots_union_test:value_b()),
    ok.

test_macro_pass_function_helper(_Config) ->
    ?assertEqual({a, {from_a, ok}},
                 macro_pass_function_helper_test:value()),
    ok.

test_macro_pass_retained_helper(_Config) ->
    ?assertEqual({a, {from_a, ok}},
                 macro_pass_retained_helper_test:value()),
    ok.

test_macro_pass_local_body_environment_mutation_error(Config) ->
    assert_macro_pass_error(
      macro_pass_local_body_env_error_test, Config,
      fun({invalid_macro_return,
           #{reason := invalid_role,
             expected_role := expression}}) ->
              true;
         (_) ->
              false
      end),
    ok.

test_macro_pass_locked_spec_mutation_error(Config) ->
    assert_macro_pass_error(
      macro_pass_locked_spec_error_test, Config,
      fun({illegal_locked_form_mutation,
           {attribute, _, spec, {{helper, 1}, _}}}) ->
              true;
         (_) ->
              false
      end),
    ok.

test_macro_pass_final_expands_outside_snapshot(_Config) ->
    ?assertEqual({locked_snapshot, ok},
                 macro_pass_final_outside_snapshot_test:local_value()),
    ?assertEqual(
       {a, {from_a, ok}},
       macro_pass_final_outside_snapshot_test:final_external_value()),
    ok.

test_macro_pass_generated_function_delay(_Config) ->
    ?assertEqual(
       {a, {from_a, ok}},
       macro_pass_generated_function_delay_test:delayed_value()),
    ok.

test_macro_pass_export_not_local(_Config) ->
    ?assertMatch({atom, _, exported_only},
                 macro_pass_export_not_local_test:value()),
    ok.

test_macro_pass_export_and_local(_Config) ->
    ?assertEqual(shared_macro,
                 macro_pass_export_and_local_test:local_value()),
    ?assertMatch({atom, _, shared_macro},
                 macro_pass_export_and_local_test:shared()),
    ok.

test_macro_pass_scoped_attribute_state(_Config) ->
    ?assertEqual(
       stateful,
       macro_pass_scoped_attribute_state_test:stateful_value()),
    ?assertMatch(
       {tuple, _, [{atom, _, external_generated_helper},
                   {atom, _, ok}]},
       macro_pass_scoped_attribute_state_test:generated_helper()),
    ok.

test_macro_pass_scoped_function_state(_Config) ->
    ?assertEqual(function_stateful,
                 macro_pass_scoped_function_state_test:value()),
    ok.

test_macro_pass_local_compile_context(_Config) ->
    ?assertEqual({injected_attrs, [early]},
                 macro_pass_local_compile_context_test:value()),
    ok.

test_macro_pass_local_macro_environment(_Config) ->
    ?assertEqual({runtime_attrs, [call_site]},
                 macro_pass_local_runtime_context_test:value()),
    ok.

test_macro_pass_generated_macro_options(Config) ->
    assert_macro_pass_error(
      macro_pass_generated_options_error_test, Config,
      fun({max_macro_expansion_depth_exceeded,
           {macro_pass_depth, chain_a}, []}) ->
              true;
         (_) ->
              false
      end),
    ok.

test_macro_pass_attribute_buffer(_Config) ->
    ?assertEqual({buffer_head, buffer_tail},
                 macro_pass_attribute_buffer_test:value()),
    ok.

test_macro_pass_attribute_buffer_cross_depth(Config) ->
    assert_macro_pass_error(
      macro_pass_attribute_buffer_cross_depth_error_test, Config,
      fun({max_macro_expansion_depth_exceeded,
           {macro_pass_depth, buffer_chain}, [_]}) ->
              true;
         (_) ->
              false
      end),
    ok.

test_macro_pass_attribute_buffer_self_depth(Config) ->
    assert_macro_pass_error(
      macro_pass_attribute_buffer_self_depth_error_test, Config,
      fun({max_macro_expansion_depth_exceeded,
           {macro_pass_depth, buffer_self}, [_]}) ->
              true;
         (_) ->
              false
      end),
    ok.

test_macro_pass_attribute_buffer_total_depth(Config) ->
    assert_macro_pass_error(
      macro_pass_attribute_buffer_total_depth_error_test, Config,
      fun({max_macro_expansion_depth_exceeded,
           {macro_pass_depth, buffer_siblings}, [_]}) ->
              true;
         (_) ->
              false
      end),
    ok.

test_macro_pass_export_helper_unlocked(Config) ->
    Forms = astranaut_test_lib:test_module_forms(
              macro_pass_export_helper_unlocked_test, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assert(
       lists:any(
         fun({_Line, erl_lint, {redefine_spec, {helper, 1}}}) ->
                 true;
            (_) ->
                 false
         end, Errors)),
    ?assertNot(
       lists:any(
         fun({_Line, astranaut_macro,
              {illegal_local_macro_definition_mutation, _}}) ->
                 true;
            (_) ->
                 false
         end, Errors)),
    ok.

test_macro_pass_local_environment_mutation_errors(Config) ->
    Forms1 = astranaut_test_lib:test_module_forms(
               macro_pass_local_import_error_test, Config),
    Forms2 = astranaut_test_lib:test_module_forms(
               macro_pass_local_macro_error_test, Config),
    ?assertEqual(
       {[], []},
       astranaut_test_lib:realize_with_baseline(
         astranaut_test_lib:get_baseline(yep, Forms1),
         astranaut_return:run_error(
           astranaut_test_lib:compile_test_forms(Forms1)))),
    {[{_File,
       [{3, astranaut_macro,
         {undefined_macro, generated_local, 0}}]}], []} =
        astranaut_test_lib:realize_with_baseline(
          astranaut_test_lib:get_baseline(yep, Forms2),
          astranaut_return:run_error(
            astranaut_test_lib:compile_test_forms(Forms2))),
    ok.

test_macro_pass_locked_snapshot_mutation_error(Config) ->
    assert_macro_pass_error(
      macro_pass_locked_helper_error_test, Config,
      fun({illegal_locked_form_mutation,
           {function, _, helper, 1, _}}) ->
              true;
         (_) ->
              false
      end),
    ok.

test_local_macro_module_cleanup(_Config) ->
    Module = macro_local_cleanup_test,
    LocalModule = astranaut_macro_local:module_name(Module),
    Output = astranaut_macro:parse_transform(
               local_macro_lifecycle_forms(Module, expanded), []),
    ?assert(is_list(Output)),
    ?assertEqual(false, code:is_loaded(LocalModule)),
    ?assertEqual(non_existing, code:which(LocalModule)),
    ok.

test_local_macro_module_name_conflict(_Config) ->
    Module = macro_local_name_conflict_test,
    LocalModule = astranaut_macro_local:module_name(Module),
    {just, {LocalModule, _Binary}} =
        astranaut_return:run(
          astranaut_lib:reload_forms(
            real_module_forms(LocalModule), [without_warnings])),
    try
        Return = astranaut_macro:parse_transform(
                   local_macro_lifecycle_forms(Module, expanded), []),
        {error, FileErrors, _Warnings} = Return,
        Errors = [Error || {_File, Diagnostics} <- FileErrors,
                           {_Pos, _Formatter, Error} <- Diagnostics],
        ?assert(
           lists:member(
             {local_macro_module_name_conflict, LocalModule},
             Errors)),
        astranaut_test_lib:assert_formatted_messages(
          [{0, astranaut_macro,
            {local_macro_module_name_conflict, LocalModule}}]),
        ?assertEqual(real, LocalModule:value())
    after
        code:purge(LocalModule),
        code:delete(LocalModule),
        code:purge(LocalModule)
    end,
    ok.

test_module_lock_serializes_processes(_Config) ->
    Module = macro_local_lock_test,
    Parent = self(),
    Holder =
        spawn(
          fun() ->
                  astranaut_lib:with_module_lock(
                    Module,
                    fun() ->
                            Parent ! lock_held,
                            receive release -> ok end
                    end)
          end),
    receive lock_held -> ok end,
    Waiter =
        spawn(
          fun() ->
                  Parent ! waiter_ready,
                  astranaut_lib:with_module_lock(
                    Module,
                    fun() -> Parent ! waiter_entered end)
          end),
    receive waiter_ready -> ok end,
    try
        receive waiter_entered -> error(module_lock_did_not_serialize)
        after 50 -> ok
        end,
        Holder ! release,
        receive waiter_entered -> ok
        after 1000 -> error(second_lock_timeout)
        end
    after
        Holder ! release,
        exit(Holder, kill),
        exit(Waiter, kill)
    end,
    ok.

assert_macro_pass_error(Module, Config, MatchError) ->
    Forms = astranaut_test_lib:test_module_forms(Module, Config),
    Baseline = astranaut_test_lib:get_baseline(yep, Forms),
    ErrorStruct = astranaut_return:run_error(
                    astranaut_test_lib:compile_test_forms(Forms)),
    {[{_File, Errors}], []} =
        astranaut_test_lib:realize_with_baseline(Baseline, ErrorStruct),
    ?assert(
       lists:any(
         fun({_Line, astranaut_macro, Error}) ->
                 MatchError(Error);
            (_) ->
                 false
         end, Errors)),
    astranaut_test_lib:assert_formatted_messages(Errors).

local_macro_lifecycle_forms(Module, Value) ->
    File = atom_to_list(Module) ++ ".erl",
    [{attribute, 1, file, {File, 1}},
     {attribute, 1, module, Module},
     {attribute, 2, local_macro, {macro_value, 0}},
     {function, 3, macro_value, 0,
      [{clause, 3, [], [], [erl_parse:abstract({atom, 3, Value})]}]},
     {function, 4, value, 0,
      [{clause, 4, [], [],
        [{call, 4, {atom, 4, macro_value}, []}]}]}].

real_module_forms(Module) ->
    [{attribute, 1, module, Module},
     {attribute, 1, export, [{value, 0}]},
     {function, 2, value, 0,
      [{clause, 2, [], [], [{atom, 2, real}]}]}].
