%%%-------------------------------------------------------------------
%%% Focused tests for the local-macro state machine.  These intentionally do
%%% not invoke the parse transform: the scanner integration has separate tests.
%%%-------------------------------------------------------------------
-module(astranaut_macro_local_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

all() -> [register_freezes_static_closure,
          duplicate_declaration_fails_atomically,
          cache_rejects_conflicting_environments,
          cache_rejects_conflicting_whitelists,
          cache_reuses_environment_after_intervening_context,
          execute_plan_rejects_conflicting_snapshots,
          execute_plan_rejects_conflicting_inject_snapshots,
          cache_hits_same_fingerprint,
          retain_controls_final_skip_ids,
          source_view_only_contains_materialised_forms,
          declaration_inject_snapshot_is_preserved,
          fingerprint_includes_injected_forms,
          frozen_splice_is_rejected,
          later_declaration_remains_helper_in_earlier_closure,
          declaration_snapshot_and_actual_local_references,
          extra_functions_and_self_recursion,
          internal_function_conflict,
          minimal_cumulative_compile_boundaries,
          shared_declaration_stays_in_one_boundary,
          same_declaration_members_share_order_and_context,
          same_declaration_members_remain_direct_in_final_context,
          declaration_expansion_uses_reference_whitelist,
          final_local_macro_environment_uses_reference_whitelist,
          ordinary_final_function_sees_all_local_macros,
          shared_expander_collects_recursive_replacement_whitelist,
          shared_expander_rejects_unexpected_whitelist_immediately,
          shared_expander_batches_return_whitelist_conflicts,
          shared_expander_expands_expected_after_unexpected,
          shared_expander_rejects_missing_whitelist_after_completion,
          shared_expander_requests_uncallable_local_macro,
          shared_expander_disables_whitelist_for_ordinary_function,
          declaration_preexpands_without_compiling,
          independent_declaration_does_not_compile,
          dependency_preexpansion_compiles_only_needed_boundary,
          compiler_reuses_canonical_forms,
          independent_macros_share_one_boundary,
          attribute_between_independent_macros_shares_one_boundary,
          final_retained_helper_comparison,
          safe_load_replaces_current_generation,
          safe_load_refuses_module_with_old_code_in_use,
          non_frozen_retain_root_has_no_effect].

register_freezes_static_closure(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{imports => [a]},
                                                  astranaut_macro_local:new()),
    #{ {foo, 0} := Entry } = astranaut_macro_local:local_macros(State),
    ?assertEqual([{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}],
                 maps:get(closure_ids, Entry)),
    ?assertEqual(ordsets:from_list([{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}]),
                 astranaut_macro_local:frozen_ids(State)),
    ok.

duplicate_declaration_fails_atomically(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                  astranaut_macro_local:new()),
    ?assertEqual({error, {duplicate_local_macro_declaration, {foo, 0}}},
                 register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, State)),
    ?assertEqual(1, map_size(astranaut_macro_local:local_macros(State))),
    ok.

cache_rejects_conflicting_environments(_Config) ->
    State0 = astranaut_macro_local:new(),
    {ok, State1} = astranaut_macro_local:cache_expanded({function, helper, 0}, env_a, [], helper_form(a), State0),
    {ok, _State2} = astranaut_macro_local:cache_expanded({function, helper, 0}, env_b, [], helper_form(a), State1),
    ?assertEqual({error, {conflicting_local_macro_closure_environment, {function, helper, 0}}},
                 astranaut_macro_local:cache_expanded({function, helper, 0}, env_c, [], helper_form(b), State1)),
    ok.

cache_rejects_conflicting_whitelists(_Config) ->
    FormId = {function, helper, 0},
    Form = helper_form(a),
    {ok, State1} = astranaut_macro_local:cache_expanded(
                     FormId, env_a, [{a, 0}], Form,
                     astranaut_macro_local:new()),
    ?assertEqual(
       {error,
        {conflicting_local_macro_whitelist, FormId,
         #{expected => [{a, 0}], observed => [{b, 0}],
           unexpected => [{b, 0}], missing => [{a, 0}]}}},
       astranaut_macro_local:cache_expanded(
         FormId, env_b, [{b, 0}], Form, State1)),
    ok.

cache_hits_same_fingerprint(_Config) ->
    State0 = astranaut_macro_local:new(),
    {ok, State1} = astranaut_macro_local:cache_expanded({function, helper, 0}, env_a, [], helper_form(a), State0),
    {ok, _State2} = astranaut_macro_local:cache_expanded({function, helper, 0}, env_a, [], helper_form(a), State1),
    ok.

cache_reuses_environment_after_intervening_context(_Config) ->
    FormId = {function, helper, 0},
    Form = helper_form(a),
    {ok, State1} = astranaut_macro_local:cache_expanded(
                     FormId, env_a, [], Form, astranaut_macro_local:new()),
    {ok, State2} = astranaut_macro_local:cache_expanded(
                     FormId, env_b, [], Form, State1),
    {ok, State3} = astranaut_macro_local:cache_expanded(
                     FormId, env_a, [], Form, State2),
    Record = maps:get(FormId, maps:get(expansion_records, State3)),
    ?assertEqual(2, map_size(maps:get(results_by_input, Record))),
    ?assertEqual([], maps:get(canonical_whitelist, Record)),
    ?assertEqual(Form, maps:get(canonical_result, Record)),
    ok.

execute_plan_rejects_conflicting_snapshots(_Config) ->
    Source = [{attribute, 1, module, local_macro_conflict_plan_test},
              first_form(), second_form(), helper_form(ok)],
    {ok, S1} = register(
                 [{first, 0}], #{}, Source, #{snapshot => first},
                 astranaut_macro_local:new()),
    {ok, S2} = register(
                 [{second, 0}], #{}, Source, #{snapshot => second}, S1),
    {ok, Plan} = astranaut_macro_local:finalize_plan(S2),
    Expand =
        fun(MacroEnv, _InjectForms, Forms, {helper, 0}, Control) ->
                Value = case maps:is_key(snapshot, MacroEnv) of
                            true -> maps:get(snapshot, MacroEnv);
                            false -> none
                        end,
                expansion_return(
                  astranaut_macro_local:materialize_forms(
                    Forms, #{{function, helper, 0} => helper_form(Value)}),
                  Control);
           (_MacroEnv, _InjectForms, Forms, _TargetFA, Control) ->
                expansion_return(Forms, Control)
        end,
    MacroOps = #{expand_function => Expand},
    Context = #{source_view => Source, compile_opts => []},
    Error = astranaut_return:run_error(
              astranaut_macro_local:execute_plan(Plan, Context, MacroOps, S2)),
    ?assert(lists:member(
              {conflicting_local_macro_closure_environment, {function, helper, 0}},
              astranaut_error:errors(Error))),
    ok.

execute_plan_rejects_conflicting_inject_snapshots(_Config) ->
    Source = [{attribute, 1, module, local_macro_inject_conflict_plan_test},
              first_form(), second_form(), helper_form(ok)],
    MacroOps0 = identity_macro_ops(),
    {ok, S1} = astranaut_macro_local:register(
                 [{first, 0}], #{}, Source, runtime_context(#{}, [early]),
                 MacroOps0, astranaut_macro_local:new()),
    {ok, S2} = astranaut_macro_local:register(
                 [{second, 0}], #{}, Source, runtime_context(#{}, [late]),
                 MacroOps0, S1),
    {ok, Plan} = astranaut_macro_local:finalize_plan(S2),
    Expand =
        fun(_MacroEnv, InjectForms, Forms, {helper, 0}, Control) ->
                Value = case InjectForms of
                            [early] -> early;
                            [late] -> late
                        end,
                expansion_return(
                  astranaut_macro_local:materialize_forms(
                    Forms, #{{function, helper, 0} => helper_form(Value)}),
                  Control);
           (_MacroEnv, _InjectForms, Forms, _TargetFA, Control) ->
                expansion_return(Forms, Control)
        end,
    MacroOps = MacroOps0#{expand_function => Expand},
    Context = #{source_view => Source, compile_opts => []},
    Error = astranaut_return:run_error(
              astranaut_macro_local:execute_plan(
                Plan, Context, MacroOps, S2)),
    ?assert(lists:member(
              {conflicting_local_macro_closure_environment,
               {function, helper, 0}},
              astranaut_error:errors(Error))),
    ok.

retain_controls_final_skip_ids(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State0} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                   astranaut_macro_local:new()),
    Forms = #{{function, foo, 0} => Foo, {function, helper, 0} => Helper, {spec, helper, 0} => Spec},
    State1 = astranaut_macro_local:commit_compiled([{foo, 0}], Forms, State0),
    {_Env0, Skip0, _} = astranaut_macro_local:finalize([], State1),
    ?assertEqual(ordsets:from_list(maps:keys(Forms)), Skip0),
    {_Env1, Skip1, _} = astranaut_macro_local:finalize([{helper, 0}], State1),
    ?assertEqual([], Skip1),
    ok.

source_view_only_contains_materialised_forms(_Config) ->
    ?assertEqual([passed, queued], astranaut_macro_local:source_view([passed], [queued])),
    ok.

declaration_inject_snapshot_is_preserved(_Config) ->
    [Foo, Helper, Spec] = forms(),
    Source = [early, Foo, Helper, Spec, late],
    InjectForms = [early],
    MacroOps = identity_macro_ops(),
    {ok, State} = astranaut_macro_local:register(
                    [{foo, 0}], #{}, Source,
                    runtime_context(#{}, InjectForms), MacroOps,
                    astranaut_macro_local:new()),
    #{{foo, 0} := Entry} = astranaut_macro_local:local_macros(State),
    ?assertEqual(Source, maps:get(source_view, Entry)),
    ?assertNot(maps:is_key(env_snapshot, Entry)),
    ?assertNot(maps:is_key(inject_forms_snapshot, Entry)),
    ?assertEqual(
       InjectForms,
       maps:get(inject_forms, maps:get(runtime_context_snapshot, Entry))),
    {ok, [Boundary]} = astranaut_macro_local:compile_plan({foo, 0}, State),
    [Request] = maps:get(requests, Boundary),
    ?assertEqual(
       lists:sort([closure_ids, closure_fas, candidate_local_macros,
                   internal_macro_bindings, referenced_local_macros,
                   runtime_context_snapshot,
                   source_view, forms]),
       lists:sort(maps:keys(Request))),
    ?assertEqual(
       InjectForms,
       maps:get(inject_forms, maps:get(runtime_context_snapshot, Request))),
    ok.

fingerprint_includes_injected_forms(_Config) ->
    A = astranaut_macro_local:env_fingerprint(#{imports => [a]}, #{foo => 1}, #{}, [early]),
    B = astranaut_macro_local:env_fingerprint(#{imports => [a]}, #{foo => 1}, #{}, [late]),
    ?assertNotEqual(A, B),
    ok.

frozen_splice_is_rejected(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                  astranaut_macro_local:new()),
    ?assertEqual({error, {illegal_locked_form_mutation, Helper}},
                 astranaut_macro_local:reject_locked_mutation([Helper], State)),
    ?assertEqual(ok, astranaut_macro_local:reject_locked_mutation([helper_form(other)], astranaut_macro_local:new())),
    ok.

later_declaration_remains_helper_in_earlier_closure(_Config) ->
    Source = [a_calls_b(), b_form_independent()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    #{ {a, 0} := A, {b, 0} := B } = astranaut_macro_local:local_macros(S2),
    ?assert(lists:member({function, b, 0}, maps:get(closure_ids, A))),
    ?assertEqual([], maps:get(referenced_local_macros, A)),
    ?assert(lists:member({function, b, 0}, maps:get(closure_ids, B))),
    ok.

declaration_snapshot_and_actual_local_references(_Config) ->
    Source = [a_form(), b_form_calls_a(), unused_form()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{imports => [early]}, astranaut_macro_local:new()),
    {ok, S2} = register([{unused, 0}], #{}, Source, #{imports => [middle]}, S1),
    {ok, S3} = register([{b, 0}], #{}, Source, #{imports => [late]}, S2),
    #{ {a, 0} := A, {b, 0} := B } = astranaut_macro_local:local_macros(S3),
    ?assertEqual(#{imports => [early]},
                 maps:get(macro_map, maps:get(runtime_context_snapshot, A))),
    BMacroMap = maps:get(
                  macro_map, maps:get(runtime_context_snapshot, B)),
    ?assertEqual([late], maps:get(imports, BMacroMap)),
    ?assert(maps:is_key({a, 0}, BMacroMap)),
    ?assert(maps:is_key({unused, 0}, BMacroMap)),
    ?assertNot(maps:is_key({b, 0}, BMacroMap)),
    ?assertEqual([{a, 0}], maps:get(referenced_local_macros, B)),
    ok.

extra_functions_and_self_recursion(_Config) ->
    Source = [recursive_form(), helper_form(ok)],
    {ok, State} = register([{recursive, 0}], #{extra_functions => [{helper, 0}]}, Source, #{},
                                                  astranaut_macro_local:new()),
    #{ {recursive, 0} := Entry } = astranaut_macro_local:local_macros(State),
    ?assertEqual([], maps:get(referenced_local_macros, Entry)),
    ?assert(lists:member({function, helper, 0}, maps:get(closure_ids, Entry))),
    ?assertEqual({error, {invalid_extra_functions, [{missing, 0}]}},
                 register([{recursive, 0}], #{extra_functions => [{missing, 0}]}, Source, #{},
                                                  astranaut_macro_local:new())),
    ok.

internal_function_conflict(_Config) ->
    Source = [first_form(), second_form(), helper_form(ok)],
    MacroMap = local_macro_map([{helper, 0}]),
    {ok, State} = register([{first, 0}], #{internal_function => [{helper, 0}]}, Source, MacroMap,
                                                  astranaut_macro_local:new()),
    ?assertMatch({error, {conflicting_internal_function_policy, {helper, 0}, _}},
                 register([{second, 0}], #{}, Source, MacroMap, State)),
    ok.

minimal_cumulative_compile_boundaries(_Config) ->
    Source = [a_form(), b_form_calls_a()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [P1, P2]} = astranaut_macro_local:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}], maps:get(members, P1)),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, P2)),
    [RequestA] = maps:get(requests, P1),
    ?assert(maps:is_key({function, a, 0}, maps:get(forms, RequestA))),
    {ok, [FinalDependency, Final]} = astranaut_macro_local:finalize_plan(S2),
    ?assertEqual([{a, 0}], maps:get(members, FinalDependency)),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Final)),
    ok.

independent_macros_share_one_boundary(_Config) ->
    Source = [a_form(), b_form_independent()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [Plan]} = astranaut_macro_local:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    ok.

attribute_between_independent_macros_shares_one_boundary(_Config) ->
    Source = [a_form(), {attribute, 1, attr_a, a}, b_form_independent()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{},
                        astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [Plan]} = astranaut_macro_local:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    ok.

shared_declaration_stays_in_one_boundary(_Config) ->
    Source = [a_form(), b_form_independent(), c_form_calls_a()],
    {ok, S1} = register(
                 [{a, 0}, {b, 0}], #{}, Source, #{}, astranaut_macro_local:new()),
    {ok, S2} = register([{c, 0}], #{}, Source, #{}, S1),
    {ok, [Dependency, Final]} = astranaut_macro_local:finalize_plan(S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Dependency)),
    ?assertEqual([{a, 0}, {b, 0}, {c, 0}], maps:get(members, Final)),
    ok.

same_declaration_members_share_order_and_context(_Config) ->
    Source = [a_form(), b_form_independent()],
    {ok, State} = register(
                    [{a, 0}, {b, 0}], #{}, Source,
                    #{shared => true},
                    astranaut_macro_local:new()),
    #{{a, 0} := A, {b, 0} := B} =
        astranaut_macro_local:local_macros(State),
    ?assertEqual(maps:get(order, A), maps:get(order, B)),
    ?assertEqual(maps:get(runtime_context_snapshot, A),
                 maps:get(runtime_context_snapshot, B)),
    {ok, [Plan]} = astranaut_macro_local:compile_plan({b, 0}, State),
    ?assertEqual(2, length(maps:get(requests, Plan))),
    ok.

same_declaration_members_remain_direct_in_final_context(_Config) ->
    Source = [a_form(), b_form_calls_a()],
    MacroOps0 = identity_macro_ops(),
    {ok, State} = astranaut_macro_local:register(
                    [{a, 0}, {b, 0}], #{}, Source,
                    runtime_context(#{}, []), MacroOps0,
                    astranaut_macro_local:new()),
    Context = #{source_view => Source, compile_opts => []},
    {just, PreparedState} = astranaut_return:run(
                              astranaut_macro_local:prepare_declaration(
                                [{a, 0}, {b, 0}], Context, MacroOps0, State)),
    FinalMacroMap =
        #{{a, 0} => #{macro_source => local_macro,
                       function => a, arity => 0},
          {b, 0} => #{macro_source => local_macro,
                       function => b, arity => 0}},
    Expand =
        fun(MacroEnv, _InjectForms, Forms, {b, 0}, Control) ->
                ?assertNot(maps:is_key({a, 0}, MacroEnv)),
                ?assertNot(maps:is_key({b, 0}, MacroEnv)),
                expansion_return(Forms, Control)
        end,
    MacroOps = MacroOps0#{expand_function => Expand},
    {just, {_Forms, _State1}} = astranaut_return:run(
                                 astranaut_macro_local:expand_final_functions(
                                   Source, [{b, 0}],
                                   runtime_context(FinalMacroMap, []),
                                   MacroOps, PreparedState)),
    ok.

declaration_expansion_uses_reference_whitelist(_Config) ->
    Module = local_macro_declaration_whitelist_test,
    Source = [{attribute, 1, module, Module},
              a_form(), unused_form(), b_form_calls_a()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{},
                        astranaut_macro_local:new()),
    {ok, S2} = register([{unused, 0}], #{}, Source, #{}, S1),
    {ok, State} = register([{b, 0}], #{}, Source, #{}, S2),
    Expand =
        fun(MacroEnv, _InjectForms, Forms, {b, 0}, Control) ->
                ?assert(maps:is_key({a, 0}, MacroEnv)),
                ?assert(maps:is_key({unused, 0}, MacroEnv)),
                expansion_return(Forms, Control, [{a, 0}]);
           (_MacroEnv, _InjectForms, Forms, _TargetFA, Control) ->
                expansion_return(Forms, Control)
        end,
    MacroOps = (identity_macro_ops())#{expand_function => Expand},
    Context = #{source_view => Source, compile_opts => []},
    {just, _State1} = astranaut_return:run(
                        astranaut_macro_local:prepare_declaration(
                          [{b, 0}], Context, MacroOps, State)),
    ok.

final_local_macro_environment_uses_reference_whitelist(_Config) ->
    Source = [a_form(), b_form_calls_a(), c_form_calls_b()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{},
                        astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, RegisteredState} = register([{c, 0}], #{}, Source, #{}, S2),
    {ok, State0} = astranaut_macro_local:cache_expanded(
                     {function, b, 0}, seeded, [{a, 0}],
                     b_form_calls_a(), RegisteredState),
    State = astranaut_macro_local:commit_compiled(
              [{a, 0}, {b, 0}, {c, 0}], #{}, State0),
    erlang:put(local_whitelist_expand_count, 0),
    Expand =
        fun(MacroEnv, _InjectForms, Forms, {b, 0}, Control) ->
                ?assert(maps:is_key({a, 0}, MacroEnv)),
                ?assertNot(maps:is_key({b, 0}, MacroEnv)),
                ?assertNot(maps:is_key({c, 0}, MacroEnv)),
                erlang:put(
                  local_whitelist_expand_count,
                  erlang:get(local_whitelist_expand_count) + 1),
                expansion_return(Forms, Control, [{a, 0}])
        end,
    MacroOps = (identity_macro_ops())#{expand_function => Expand},
    FinalContext = runtime_context(
                     local_macro_map([{a, 0}, {b, 0}, {c, 0}]), []),
    {just, {_Forms, State1}} = astranaut_return:run(
                                astranaut_macro_local:expand_final_functions(
                                  Source, [{b, 0}], FinalContext,
                                  MacroOps, State)),
    State2 = astranaut_macro_local:commit_compiled([{c, 0}], #{}, State1),
    {just, {_Forms2, State3}} = astranaut_return:run(
                                 astranaut_macro_local:expand_final_functions(
                                   Source, [{b, 0}], FinalContext,
                                   MacroOps, State2)),
    {just, {_Forms3, _State4}} = astranaut_return:run(
                                  astranaut_macro_local:expand_final_functions(
                                    Source, [{b, 0}], FinalContext,
                                    MacroOps, State3)),
    %% The canonical whitelist is part of the final local environment, so
    %% unrelated descriptors and generation changes never enter its key.
    ?assertEqual(1, erlang:erase(local_whitelist_expand_count)),
    ok.

ordinary_final_function_sees_all_local_macros(_Config) ->
    Source = [a_form(), b_form_independent(), unused_form()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{},
                        astranaut_macro_local:new()),
    {ok, State} = register([{b, 0}], #{}, Source, #{}, S1),
    Expand =
        fun(MacroEnv, _InjectForms, Forms, {unused, 0}, disabled) ->
                ?assert(maps:is_key({a, 0}, MacroEnv)),
                ?assert(maps:is_key({b, 0}, MacroEnv)),
                expansion_return(Forms, disabled)
        end,
    MacroOps = (identity_macro_ops())#{expand_function => Expand},
    {just, {_Forms, State1}} = astranaut_return:run(
                                astranaut_macro_local:expand_final_functions(
                                  Source, [{unused, 0}],
                                  runtime_context(
                                    local_macro_map([{a, 0}, {b, 0}]), []),
                                  MacroOps, State)),
    ?assertEqual(maps:get(expansion_records, State),
                 maps:get(expansion_records, State1)),
    ok.

shared_expander_collects_recursive_replacement_whitelist(_Config) ->
    FormId = {function, whitelist_target, 0},
    Control = #{mode => collect, form_id => FormId},
    reset_whitelist_macro_counts(),
    {just, #{forms := Forms, local_macro_whitelist := Whitelist}} =
        astranaut_return:run(
          astranaut_macro:expand_function(
            whitelist_macro_map(), [], [whitelist_target_form()],
            {whitelist_target, 0}, Control)),
    ?assertEqual([{whitelist_chain_a, 0}, {whitelist_chain_b, 0}],
                 Whitelist),
    ?assertMatch(
       [{function, _, whitelist_target, 0,
         [{clause, _, [], [], [{atom, _, whitelist_done}]}]}],
       Forms),
    ?assertEqual(1, erlang:erase(whitelist_chain_a_count)),
    ?assertEqual(1, erlang:erase(whitelist_chain_b_count)),
    ok.

shared_expander_rejects_unexpected_whitelist_immediately(_Config) ->
    FormId = {function, whitelist_target, 0},
    Expected = [{whitelist_chain_a, 0}],
    reset_whitelist_macro_counts(),
    erlang:erase(whitelist_after_unexpected),
    Error = astranaut_return:run_error(
              astranaut_macro:expand_function(
                whitelist_macro_map(), [],
                [whitelist_immediate_target_form()],
                {whitelist_target, 0},
                #{mode => verify, form_id => FormId,
                  expected => Expected})),
    ?assertEqual(
       [{1, astranaut_macro,
         {conflicting_local_macro_whitelist, FormId,
          #{expected => Expected,
            observed => [{whitelist_chain_a, 0},
                         {whitelist_chain_b, 0}],
            unexpected => [{whitelist_chain_b, 0}],
            missing => []}}},
        {2, astranaut_macro,
         {conflicting_local_macro_whitelist, FormId,
          #{expected => Expected,
            observed => [{whitelist_after, 0},
                         {whitelist_chain_a, 0},
                         {whitelist_chain_b, 0}],
            unexpected => [{whitelist_after, 0},
                           {whitelist_chain_b, 0}],
            missing => []}}}],
       maps:get(formatted_errors, astranaut_error:printable(Error))),
    ?assertEqual(undefined, erlang:erase(whitelist_after_unexpected)),
    ?assertEqual(1, erlang:erase(whitelist_chain_a_count)),
    %% Each unexpected match is rejected before its macro function runs.
    ?assertEqual(undefined, erlang:erase(whitelist_chain_b_count)),
    ok.

shared_expander_batches_return_whitelist_conflicts(_Config) ->
    FormId = {function, whitelist_target, 0},
    Expected = [{whitelist_return_batch, 0}],
    reset_whitelist_macro_counts(),
    erlang:erase(whitelist_after_unexpected),
    Error = astranaut_return:run_error(
              astranaut_macro:expand_function(
                whitelist_macro_map(), [],
                [whitelist_batch_target_form()],
                {whitelist_target, 0},
                #{mode => verify, form_id => FormId,
                  expected => Expected})),
    Formatted = maps:get(
                  formatted_errors, astranaut_error:printable(Error)),
    ?assertEqual(1, length(Formatted)),
    ?assertMatch(
       [{1, astranaut_macro,
         {conflicting_local_macro_whitelist, FormId,
          #{expected := Expected,
            observed := [{whitelist_after, 0},
                         {whitelist_chain_b, 0},
                         {whitelist_return_batch, 0}],
            unexpected := [{whitelist_after, 0},
                           {whitelist_chain_b, 0}],
            missing := []}}}],
       Formatted),
    %% Collection sees both calls, but the conflicting return AST is never
    %% handed to recursive macro expansion.
    ?assertEqual(undefined, erlang:erase(whitelist_after_unexpected)),
    ?assertEqual(undefined, erlang:erase(whitelist_chain_b_count)),
    ok.

shared_expander_expands_expected_after_unexpected(_Config) ->
    FormId = {function, whitelist_target, 0},
    Expected = [{whitelist_chain_a, 0}, {whitelist_chain_b, 0}],
    reset_whitelist_macro_counts(),
    erlang:erase(whitelist_after_unexpected),
    Error = astranaut_return:run_error(
              astranaut_macro:expand_function(
                whitelist_macro_map(), [],
                [whitelist_expected_after_unexpected_form()],
                {whitelist_target, 0},
                #{mode => verify, form_id => FormId,
                  expected => Expected})),
    ?assertEqual(1, length(maps:get(
                             formatted_errors,
                             astranaut_error:printable(Error)))),
    ?assertEqual(undefined, erlang:erase(whitelist_after_unexpected)),
    ?assertEqual(1, erlang:erase(whitelist_chain_a_count)),
    ?assertEqual(1, erlang:erase(whitelist_chain_b_count)),
    ok.

shared_expander_rejects_missing_whitelist_after_completion(_Config) ->
    FormId = {function, whitelist_target, 0},
    Expected = [{whitelist_chain_a, 0}, {whitelist_chain_b, 0},
                {whitelist_chain_c, 0}],
    Error = astranaut_return:run_error(
              astranaut_macro:expand_function(
                whitelist_macro_map(), [], [whitelist_target_form()],
                {whitelist_target, 0},
                #{mode => verify, form_id => FormId,
                  expected => Expected})),
    ?assertEqual(
       [{conflicting_local_macro_whitelist, FormId,
         #{expected => Expected,
           observed => [{whitelist_chain_a, 0},
                        {whitelist_chain_b, 0}],
           unexpected => [], missing => [{whitelist_chain_c, 0}]}}],
       astranaut_error:errors(Error)),
    ok.

shared_expander_requests_uncallable_local_macro(_Config) ->
    FormId = {function, whitelist_target, 0},
    MacroMap0 = whitelist_macro_map(),
    Macro = maps:get({whitelist_chain_a, 0}, MacroMap0),
    MacroMap = maps:put(
                 {whitelist_chain_a, 0},
                 Macro#{local_macro_callable => false}, MacroMap0),
    reset_whitelist_macro_counts(),
    {just, #{local_macro_whitelist := [{whitelist_chain_a, 0}],
             needed_local_macros := [{whitelist_chain_a, 0}]}} =
        astranaut_return:run(
          astranaut_macro:expand_function(
            MacroMap, [], [whitelist_target_form()],
            {whitelist_target, 0},
            #{mode => collect, form_id => FormId})),
    ?assertEqual(undefined, erlang:erase(whitelist_chain_a_count)),
    ok.

shared_expander_disables_whitelist_for_ordinary_function(_Config) ->
    {just, #{forms := _Forms, local_macro_whitelist := disabled}} =
        astranaut_return:run(
          astranaut_macro:expand_function(
            whitelist_macro_map(), [], [whitelist_target_form()],
            {whitelist_target, 0}, disabled)),
    ok.

declaration_preexpands_without_compiling(_Config) ->
    Source = [a_form()],
    MacroOps = identity_macro_ops(),
    {ok, State0} = astranaut_macro_local:register(
                     [{a, 0}], #{}, Source, runtime_context(#{}, []),
                     MacroOps, astranaut_macro_local:new()),
    Context = #{source_view => Source, compile_opts => []},
    {just, State1} = astranaut_return:run(
                       astranaut_macro_local:prepare_declaration(
                         [{a, 0}], Context, MacroOps, State0)),
    #{{a, 0} := #{status := pending}} =
        astranaut_macro_local:local_macros(State1),
    ?assertEqual(0, maps:get(generation, State1)),
    ?assert(maps:is_key({function, a, 0},
                        maps:get(canonical_expanded_forms, State1))),
    ok.

independent_declaration_does_not_compile(_Config) ->
    Module = local_macro_independent_declaration_test,
    Source = [{attribute, 1, module, Module}, a_form(), b_form_independent()],
    MacroOps = identity_macro_ops(),
    Context = #{source_view => Source, compile_opts => []},
    {ok, S0} = astranaut_macro_local:register(
                 [{a, 0}], #{}, Source, runtime_context(#{}, []),
                 MacroOps, astranaut_macro_local:new()),
    {just, S1} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{a, 0}], Context, MacroOps, S0)),
    {ok, S2} = astranaut_macro_local:register(
                 [{b, 0}], #{}, Source, runtime_context(#{}, []),
                 MacroOps, S1),
    {just, S3} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{b, 0}], Context, MacroOps, S2)),
    ?assertEqual(0, maps:get(generation, S3)),
    {ok, [Plan]} = astranaut_macro_local:finalize_plan(S3),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    {just, S4} = astranaut_return:run(
                   astranaut_macro_local:execute_plan(
                     [Plan], Context, MacroOps, S3)),
    ?assertEqual(1, maps:get(generation, S4)),
    ok.

dependency_preexpansion_compiles_only_needed_boundary(_Config) ->
    Module = local_macro_dependency_preexpand_test,
    Source = [{attribute, 1, module, Module}, a_form(), b_form_calls_a()],
    MacroOps = identity_macro_ops(),
    AMap = #{{a, 0} => #{macro_source => local_macro,
                         macro_module => astranaut_macro_local:module_name(Module),
                         function => a, arity => 0}},
    {ok, S0} = astranaut_macro_local:register(
                 [{a, 0}], #{}, Source, runtime_context(#{}, []),
                 MacroOps, astranaut_macro_local:new()),
    Context0 = #{source_view => Source, compile_opts => []},
    {just, S1} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{a, 0}], Context0, MacroOps, S0)),
    {ok, S2} = astranaut_macro_local:register(
                 [{b, 0}], #{}, Source, runtime_context(AMap, []),
                 MacroOps, S1),
    Context1 = Context0,
    {just, S3} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{b, 0}], Context1, MacroOps, S2)),
    #{{a, 0} := #{status := compiled},
      {b, 0} := #{status := pending}} =
        astranaut_macro_local:local_macros(S3),
    ?assertEqual(1, maps:get(generation, S3)),
    ok.

compiler_reuses_canonical_forms(_Config) ->
    Module = local_macro_canonical_compile_test,
    Source = [{attribute, 1, module, Module}, a_form()],
    MacroOps = identity_macro_ops(),
    {ok, S0} = astranaut_macro_local:register(
                 [{a, 0}], #{}, Source, runtime_context(#{}, []),
                 MacroOps, astranaut_macro_local:new()),
    Context = #{source_view => Source, compile_opts => []},
    {just, S1} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{a, 0}], Context, MacroOps, S0)),
    NoExpandOps = MacroOps#{expand_function =>
                               fun(_, _, _, _, _) -> error(unexpected_reexpand) end},
    {ok, Plan} = astranaut_macro_local:compile_plan({a, 0}, S1),
    {just, S2} = astranaut_return:run(
                   astranaut_macro_local:execute_plan(
                     Plan, Context, NoExpandOps, S1)),
    Generation = maps:get(generation, S2),
    LaterTriggerContext = Context#{source_view =>
                                       Source ++ [{attribute, 2, later, changed}]},
    {just, S3} = astranaut_return:run(
                   astranaut_macro_local:execute_plan(
                     Plan, LaterTriggerContext, NoExpandOps, S2)),
    ?assertEqual(Generation, maps:get(generation, S3)),
    ok.

final_retained_helper_comparison(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, S0} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, astranaut_macro_local:new()),
    {ok, S00} = astranaut_macro_local:cache_expanded(
                  {function, foo, 0}, declaration_env, [], Foo, S0),
    {ok, S01} = astranaut_macro_local:cache_expanded(
                  {function, helper, 0}, declaration_env, [], Helper, S00),
    S1 = astranaut_macro_local:commit_compiled(
           [{foo, 0}],
           #{{function, foo, 0} => Foo,
             {function, helper, 0} => Helper}, S01),
    {_Env, _Skip, S2} = astranaut_macro_local:finalize([{helper, 0}], S1),
    HelperOps = (identity_macro_ops())#{
                  expand_function =>
                      fun(_MacroEnv, _InjectForms, Forms0, {helper, 0},
                          #{mode := verify} = Control) ->
                              expansion_return(
                                astranaut_macro_local:materialize_forms(
                                  Forms0,
                                  #{{function, helper, 0} =>
                                        helper_form(changed)}), Control)
                      end},
    HelperError = astranaut_return:run_error(
                    astranaut_macro_local:expand_final_functions(
                      [Foo, Helper, Spec], [{helper, 0}],
                      runtime_context(#{}, [final]), HelperOps, S2)),
    ?assertEqual(
       [{conflicting_local_macro_closure_environment,
         {function, helper, 0}}],
       astranaut_error:errors(HelperError)),
    {_Env2, _Skip2, S3} = astranaut_macro_local:finalize([{foo, 0}], S1),
    FooOps = (identity_macro_ops())#{
               expand_function =>
                   fun(_MacroEnv, _InjectForms, Forms0, {foo, 0},
                       #{mode := verify} = Control) ->
                           expansion_return(
                             astranaut_macro_local:materialize_forms(
                               Forms0,
                               #{{function, foo, 0} => foo_form(changed)}),
                             Control)
                   end},
    FooError = astranaut_return:run_error(
                 astranaut_macro_local:expand_final_functions(
                   [Foo, Helper, Spec], [{foo, 0}],
                   runtime_context(#{}, [final]), FooOps, S3)),
    ?assertEqual(
       [{conflicting_local_macro_closure_environment,
         {function, foo, 0}}],
       astranaut_error:errors(FooError)),
    ok.

safe_load_replaces_current_generation(_Config) ->
    Module = astranaut_macro_local_safe_load_test,
    {just, {Module, _}} = astranaut_return:run(astranaut_macro_local:safe_load(Module, load_forms(Module, first), [without_warnings])),
    ?assertEqual(first, Module:value()),
    {just, {Module, _}} = astranaut_return:run(astranaut_macro_local:safe_load(Module, load_forms(Module, second), [without_warnings])),
    ?assertEqual(second, Module:value()),
    ok.

safe_load_refuses_module_with_old_code_in_use(_Config) ->
    Module = astranaut_macro_local_busy_load_test,
    {just, {Module, _}} = astranaut_return:run(astranaut_macro_local:safe_load(Module, busy_forms(Module, first), [without_warnings])),
    Pid = spawn(Module, hold, []),
    timer:sleep(10),
    {ok, Module, Binary} = compile:forms(busy_forms(Module, second), [binary]),
    {module, Module} = code:load_binary(Module, [], Binary),
    Error = astranaut_return:run_error(astranaut_macro_local:safe_load(Module, busy_forms(Module, third), [without_warnings])),
    ?assertEqual([local_macro_module_in_use], astranaut_error:errors(Error)),
    Pid ! stop,
    ok.

non_frozen_retain_root_has_no_effect(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, S0} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, astranaut_macro_local:new()),
    ?assertEqual([{ordinary, 0}],
                 astranaut_macro_local:nonclosure_retain_roots(
                   [{foo, 0}, {ordinary, 0}], S0)),
    S1 = astranaut_macro_local:commit_compiled([{foo, 0}], #{{function, foo, 0} => Foo}, S0),
    {_Env, Skip, _S2} = astranaut_macro_local:finalize([{ordinary, 0}], S1),
    ?assertEqual([{function, foo, 0}], Skip),
    ok.

register(FAs, Options, Source, MacroMap, State) ->
    CandidateMap =
        maps:from_list(
          [{{Name, Arity}, #{macro_source => local_macro,
                            function => Name, arity => Arity}}
           || {Name, Arity} <- maps:keys(astranaut_macro_local:local_macros(State))]),
    MacroOps = identity_macro_ops(),
    EffectiveMacroMap = maps:merge(MacroMap, CandidateMap),
    astranaut_macro_local:register(
      FAs, Options, Source, runtime_context(EffectiveMacroMap, Source),
      MacroOps, State).

runtime_context(MacroMap, InjectForms) ->
    #{macro_map => MacroMap,
      macro_options => #{},
      inject_forms => InjectForms}.

local_macro_map(FAs) ->
    maps:from_list(
      [{FA, #{macro_source => local_macro,
              function => element(1, FA), arity => element(2, FA)}}
       || FA <- FAs]).

test_resolve_local_references(TargetEnvs, Forms) ->
    lists:foldl(
      fun({{Name, Arity}, CandidateEnv}, Acc) ->
              case [Clauses || {function, _, Name0, Arity0, Clauses} <- Forms,
                               Name0 =:= Name, Arity0 =:= Arity] of
                  [Clauses | _] ->
                      astranaut:sreduce(
                        fun({call, _, {atom, _, Called}, Args}, Refs) ->
                                CalledFA = {Called, length(Args)},
                                case maps:is_key(CalledFA, CandidateEnv) of
                                    true -> ordsets:add_element(CalledFA, Refs);
                                    false -> Refs
                                end;
                           (_, Refs) -> Refs
                        end, Acc, Clauses, #{traverse => pre});
                  [] -> Acc
              end
      end, ordsets:new(), TargetEnvs).

identity_macro_ops() ->
    #{resolve_local_references => fun test_resolve_local_references/2,
      expand_function =>
          fun(_MacroEnv, _InjectForms, Forms, _TargetFA, Control) ->
                  expansion_return(Forms, Control)
          end}.

expansion_return(Forms, Control) ->
    Observed = case Control of
                   #{mode := verify, expected := Expected} -> Expected;
                   _ -> ordsets:new()
               end,
    expansion_return(Forms, Control, Observed).

expansion_return(Forms, disabled, _Observed) ->
    astranaut_return:return(
      #{forms => Forms,
        local_macro_whitelist => disabled,
        needed_local_macros => ordsets:new()});
expansion_return(Forms, _Control, Observed) ->
    astranaut_return:return(
      #{forms => Forms,
        local_macro_whitelist => ordsets:from_list(Observed),
        needed_local_macros => ordsets:new()}).

whitelist_macro_map() ->
    maps:from_list(
      [{{Function, 0}, whitelist_macro(Function)}
       || Function <- [whitelist_chain_a, whitelist_chain_b,
                       whitelist_after, whitelist_return_batch]]).

whitelist_macro(Function) ->
    #{macro_source => local_macro,
      module => ?MODULE,
      macro_module => whitelist_origin,
      macro => Function,
      function => Function,
      arity => 0,
      call_arity => 0,
      max_depth => 100,
      order => inner,
      formatter => astranaut_macro,
      file => [],
      local_module => ?MODULE}.

whitelist_target_form() ->
    {function, 1, whitelist_target, 0,
     [{clause, 1, [], [],
       [{call, 1, {atom, 1, whitelist_chain_a}, []}]}]}.

whitelist_immediate_target_form() ->
    {function, 1, whitelist_target, 0,
     [{clause, 1, [], [],
       [{call, 1, {atom, 1, whitelist_chain_a}, []},
        {call, 2, {atom, 2, whitelist_after}, []}]}]}.

whitelist_batch_target_form() ->
    {function, 1, whitelist_target, 0,
     [{clause, 1, [], [],
       [{call, 1, {atom, 1, whitelist_return_batch}, []}]}]}.

whitelist_expected_after_unexpected_form() ->
    {function, 1, whitelist_target, 0,
     [{clause, 1, [], [],
       [{call, 1, {atom, 1, whitelist_after}, []},
        {call, 2, {atom, 2, whitelist_chain_a}, []}]}]}.

whitelist_chain_a() ->
    increment_whitelist_macro_count(whitelist_chain_a_count),
    {call, 1, {atom, 1, whitelist_chain_b}, []}.

whitelist_chain_b() ->
    increment_whitelist_macro_count(whitelist_chain_b_count),
    {atom, 1, whitelist_done}.

whitelist_return_batch() ->
    {block, 1,
     [{call, 1, {atom, 1, whitelist_chain_b}, []},
      {call, 2, {atom, 2, whitelist_after}, []}]}.

whitelist_after() ->
    erlang:put(whitelist_after_unexpected, reached),
    {atom, 2, should_not_be_reached}.

reset_whitelist_macro_counts() ->
    erlang:erase(whitelist_chain_a_count),
    erlang:erase(whitelist_chain_b_count).

increment_whitelist_macro_count(Key) ->
    Count = case erlang:get(Key) of undefined -> 0; Value -> Value end,
    erlang:put(Key, Count + 1).

forms() ->
    [foo_form(), helper_form(ok), {attribute, 1, spec, {{helper, 0}, []}}].

foo_form() ->
    {function, 1, foo, 0,
     [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}.

foo_form(Value) ->
    {function, 1, foo, 0,
     [{clause, 1, [], [], [{atom, 1, Value}]}]}.

helper_form(Value) ->
    {function, 1, helper, 0, [{clause, 1, [], [], [{atom, 1, Value}]}]}.

recursive_form() -> {function, 1, recursive, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, recursive}, []}]}]}.
first_form() -> {function, 1, first, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}.
second_form() -> {function, 1, second, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}.
a_form() -> {function, 1, a, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
b_form_calls_a() -> {function, 1, b, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, a}, []}]}]}.
b_form_independent() -> {function, 1, b, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
c_form_calls_a() -> {function, 1, c, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, a}, []}]}]}.
c_form_calls_b() -> {function, 1, c, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, b}, []}]}]}.
a_calls_b() -> {function, 1, a, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, b}, []}]}]}.
unused_form() -> {function, 1, unused, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.

load_forms(Module, Value) ->
    [{attribute, 1, module, Module},
     {attribute, 1, export, [{value, 0}]},
     {function, 1, value, 0, [{clause, 1, [], [], [{atom, 1, Value}]}]}].

busy_forms(Module, Value) ->
    [{attribute, 1, module, Module},
     {attribute, 1, export, [{hold, 0}, {value, 0}]},
     {function, 1, hold, 0, [{clause, 1, [], [], [{'receive', 1, [{clause, 1, [{atom, 1, stop}], [], [{atom, 1, ok}]}]}]}]},
     {function, 1, value, 0, [{clause, 1, [], [], [{atom, 1, Value}]}]}].
