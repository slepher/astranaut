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
          cache_hits_same_fingerprint,
          final_retained_function_rejects_changed_real_expansion,
          retain_controls_final_skip_ids,
          source_view_only_contains_materialised_forms,
          declaration_environment_snapshot_is_resolved,
          fingerprint_includes_resolved_attributes,
          frozen_splice_is_rejected,
          later_declaration_remains_helper_in_earlier_closure,
          declaration_snapshot_and_actual_local_references,
          closure_roots_and_self_recursion,
          minimal_cumulative_compile_boundaries,
          shared_declaration_stays_in_one_boundary,
          same_declaration_members_share_order_and_context,
          function_call_analysis_combines_closure_and_macro_presence,
          shared_expander_uses_each_task_environment_in_one_pass,
          shared_expander_follows_external_replacement_presence,
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

final_retained_function_rejects_changed_real_expansion(_Config) ->
    SourceForm = task_macro_form(foo, retained_conflict_macro),
    FormId = {function, foo, 0},
    {ok, State0} = astranaut_macro_local:register(
                     [{foo, 0}], #{}, [SourceForm],
                     macro_environment(#{}),
                     astranaut_macro_local:new()),
    {ok, State1} = astranaut_macro_local:cache_expanded(
                     FormId, declaration_environment, [],
                     foo_form(declaration_value), State0),
    {_LocalEnv, _SkipIds, State2} =
        astranaut_macro_local:finalize([{foo, 0}], State1),
    MacroMap = #{{retained_conflict_macro, 0} =>
                     external_macro(retained_conflict_macro)},
    Error = astranaut_return:run_error(
              astranaut_macro_local:expand_final_functions(
                [SourceForm], [{foo, 0}], macro_environment(MacroMap),
                State2)),
    ?assertEqual(
       [{conflicting_local_macro_closure_environment, FormId}],
       astranaut_error:errors(Error)),
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

declaration_environment_snapshot_is_resolved(_Config) ->
    [Foo, Helper, Spec] = forms(),
    Source = [Foo, Helper, Spec],
    MacroMap = #{{snapshot_macro, 0} =>
                     #{attributes =>
                           #{module => snapshot_module,
                             file => "snapshot.erl",
                             pass_seen_attr => [early]}}},
    {ok, State} = astranaut_macro_local:register(
                    [{foo, 0}], #{}, Source,
                    macro_environment(MacroMap),
                    astranaut_macro_local:new()),
    #{{foo, 0} := Entry} = astranaut_macro_local:local_macros(State),
    ?assertEqual(Source, maps:get(source_view, Entry)),
    Snapshot = maps:get(macro_environment_snapshot, Entry),
    ?assertNot(maps:is_key(inject_forms, Snapshot)),
    ?assertEqual(MacroMap, maps:get(macro_map, Snapshot)),
    {ok, [Boundary]} = astranaut_macro_local:compile_plan({foo, 0}, State),
    [Request] = maps:get(requests, Boundary),
    ?assertEqual(
       lists:sort([closure_ids, closure_fas, candidate_local_macros,
                    function_call_analysis,
                    referenced_local_macros,
                    macro_environment_snapshot,
                   source_view, forms]),
       lists:sort(maps:keys(Request))),
    ?assertEqual(Snapshot, maps:get(macro_environment_snapshot, Request)),
    ok.

fingerprint_includes_resolved_attributes(_Config) ->
    A = astranaut_macro_local:env_fingerprint(
          #{macro => #{attributes => #{seen => [early]}}},
          #{foo => 1}, #{}),
    B = astranaut_macro_local:env_fingerprint(
          #{macro => #{attributes => #{seen => [late]}}},
          #{foo => 1}, #{}),
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
                 maps:get(macro_map,
                          maps:get(macro_environment_snapshot, A))),
    BMacroMap = maps:get(
                  macro_map, maps:get(macro_environment_snapshot, B)),
    ?assertEqual([late], maps:get(imports, BMacroMap)),
    ?assert(maps:is_key({a, 0}, BMacroMap)),
    ?assert(maps:is_key({unused, 0}, BMacroMap)),
    ?assertNot(maps:is_key({b, 0}, BMacroMap)),
    ?assertEqual([{a, 0}], maps:get(referenced_local_macros, B)),
    ?assertEqual(
       [{function, a, 0}, {function, b, 0}],
       lists:sort(maps:keys(maps:get(function_call_analysis, B)))),
    ok.

closure_roots_and_self_recursion(_Config) ->
    Source = [recursive_form(), helper_form(ok)],
    {ok, State} = register([{recursive, 0}], #{closure_roots => [{helper, 0}]}, Source, #{},
                                                  astranaut_macro_local:new()),
    #{ {recursive, 0} := Entry } = astranaut_macro_local:local_macros(State),
    ?assertEqual([], maps:get(referenced_local_macros, Entry)),
    ?assert(lists:member({function, helper, 0}, maps:get(closure_ids, Entry))),
    ?assertEqual({error, {invalid_closure_roots, [{missing, 0}]}},
                 register([{recursive, 0}], #{closure_roots => [{missing, 0}]}, Source, #{},
                                                  astranaut_macro_local:new())),
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
    ?assertEqual(maps:get(macro_environment_snapshot, A),
                 maps:get(macro_environment_snapshot, B)),
    {ok, [Plan]} = astranaut_macro_local:compile_plan({b, 0}, State),
    ?assertEqual(2, length(maps:get(requests, Plan))),
    ok.

function_call_analysis_combines_closure_and_macro_presence(_Config) ->
    Form = function_call_analysis_form(),
    MacroMap =
        #{{whitelist_chain_a, 0} =>
              whitelist_macro(whitelist_chain_a),
          {{analysis_remote, whitelist_chain_b}, 0} =>
              external_macro(whitelist_chain_b)},
    Analysis = astranaut_macro_expander:function_call_analysis(
                 [Form, helper_form(ok)], MacroMap),
    FormId = {function, analysis_target, 0},
    ?assertMatch(
       #{form := Form,
         local_calls := [{helper, 0}, {whitelist_chain_a, 0}],
         local_macro_calls := [{whitelist_chain_a, 0}],
         has_macro_call := true},
       maps:get(FormId, Analysis)),
    ?assertEqual([FormId],
                 astranaut_macro_expander:function_macro_callers(
                   Analysis)),
    ?assertMatch(
       #{local_calls := [],
         local_macro_calls := [],
         has_macro_call := false},
       maps:get({function, helper, 0}, Analysis)),
    PresenceAnalysis =
        astranaut_macro_expander:function_call_analysis(
          [Form, helper_form(ok)], MacroMap, presence),
    ?assertEqual(
       #{form => Form, has_macro_call => true},
       maps:get(FormId, PresenceAnalysis)),
    ?assertEqual(
       [FormId],
       astranaut_macro_expander:function_macro_callers(
         PresenceAnalysis)),
    ok.

shared_expander_uses_each_task_environment_in_one_pass(_Config) ->
    FormA = task_macro_form(task_a, whitelist_chain_a),
    FormB = task_macro_form(task_b, whitelist_chain_b),
    MacroMap = whitelist_macro_map(),
    Tasks =
        #{{function, task_a, 0} =>
              #{form => FormA,
                macro_map => maps:with([{whitelist_chain_a, 0}], MacroMap),
                whitelist_control =>
                    #{mode => collect,
                      form_id => {function, task_a, 0}}},
          {function, task_b, 0} =>
              #{form => FormB,
                macro_map => maps:with([{whitelist_chain_b, 0}], MacroMap),
                whitelist_control => disabled}},
    reset_whitelist_macro_counts(),
    {just, #{forms := ExpandedForms, task_results := Results}} =
        astranaut_return:run(
          astranaut_macro_expander:expand_functions(
            [FormA, FormB], Tasks)),
    ?assertMatch(
       [{function, _, task_a, 0,
         [{clause, _, [], [],
           [{call, _, {atom, _, whitelist_chain_b}, []}]}]},
        {function, _, task_b, 0,
         [{clause, _, [], [], [{atom, _, whitelist_done}]}]}],
       ExpandedForms),
    ?assertEqual(
       ordsets:from_list([{function, task_a, 0}, {function, task_b, 0}]),
       ordsets:from_list(maps:keys(Results))),
    ?assertMatch(
       #{local_macro_whitelist := [{whitelist_chain_a, 0}]},
       maps:get({function, task_a, 0}, Results)),
    ?assertMatch(
       #{local_macro_whitelist := disabled},
       maps:get({function, task_b, 0}, Results)),
    ?assertEqual(1, erlang:erase(whitelist_chain_a_count)),
    ?assertEqual(1, erlang:erase(whitelist_chain_b_count)),
    ok.

shared_expander_follows_external_replacement_presence(_Config) ->
    MacroMap = maps:map(
                 fun(_Key, Macro) ->
                         Macro#{macro_source => external_macro}
                 end, whitelist_macro_map()),
    reset_whitelist_macro_counts(),
    {just, #{forms := Forms, local_macro_whitelist := disabled}} =
        astranaut_return:run(
          expand_single_function(
            MacroMap, [whitelist_target_form()],
            {whitelist_target, 0}, disabled)),
    ?assertMatch(
       [{function, _, whitelist_target, 0,
         [{clause, _, [], [], [{atom, _, whitelist_done}]}]}],
       Forms),
    ?assertEqual(1, erlang:erase(whitelist_chain_a_count)),
    ?assertEqual(1, erlang:erase(whitelist_chain_b_count)),
    ok.

shared_expander_collects_recursive_replacement_whitelist(_Config) ->
    FormId = {function, whitelist_target, 0},
    Control = #{mode => collect, form_id => FormId},
    reset_whitelist_macro_counts(),
    {just, #{forms := Forms, local_macro_whitelist := Whitelist}} =
        astranaut_return:run(
          expand_single_function(
            whitelist_macro_map(), [whitelist_target_form()],
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
              expand_single_function(
                whitelist_macro_map(),
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
              expand_single_function(
                whitelist_macro_map(),
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
              expand_single_function(
                whitelist_macro_map(),
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
              expand_single_function(
                whitelist_macro_map(), [whitelist_target_form()],
                {whitelist_target, 0},
                #{mode => verify, form_id => FormId,
                  expected => Expected})),
    ?assertEqual(
       [{1, astranaut_macro,
         {conflicting_local_macro_whitelist, FormId,
          #{expected => Expected,
            observed => [{whitelist_chain_a, 0},
                         {whitelist_chain_b, 0}],
            unexpected => [], missing => [{whitelist_chain_c, 0}]}}}],
       maps:get(formatted_errors, astranaut_error:printable(Error))),
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
          expand_single_function(
            MacroMap, [whitelist_target_form()],
            {whitelist_target, 0},
            #{mode => collect, form_id => FormId})),
    ?assertEqual(undefined, erlang:erase(whitelist_chain_a_count)),
    ok.

shared_expander_disables_whitelist_for_ordinary_function(_Config) ->
    {just, #{forms := _Forms, local_macro_whitelist := disabled}} =
        astranaut_return:run(
          expand_single_function(
            whitelist_macro_map(), [whitelist_target_form()],
            {whitelist_target, 0}, disabled)),
    ok.

declaration_preexpands_without_compiling(_Config) ->
    Source = [a_form()],
    {ok, State0} = astranaut_macro_local:register(
                     [{a, 0}], #{}, Source, macro_environment(#{}),
                     astranaut_macro_local:new()),
    Context = #{source_view => Source, compile_opts => []},
    {just, State1} = astranaut_return:run(
                       astranaut_macro_local:prepare_declaration(
                         [{a, 0}], Context, State0)),
    #{{a, 0} := #{status := pending}} =
        astranaut_macro_local:local_macros(State1),
    ?assertEqual(0, maps:get(generation, State1)),
    ?assert(maps:is_key({function, a, 0},
                        maps:get(canonical_expanded_forms, State1))),
    ok.

independent_declaration_does_not_compile(_Config) ->
    Module = local_macro_independent_declaration_test,
    Source = [{attribute, 1, module, Module}, a_form(), b_form_independent()],
    Context = #{source_view => Source, compile_opts => []},
    {ok, S0} = astranaut_macro_local:register(
                 [{a, 0}], #{}, Source, macro_environment(#{}),
                 astranaut_macro_local:new()),
    {just, S1} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{a, 0}], Context, S0)),
    {ok, S2} = astranaut_macro_local:register(
                 [{b, 0}], #{}, Source, macro_environment(#{}),
                 S1),
    {just, S3} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{b, 0}], Context, S2)),
    ?assertEqual(0, maps:get(generation, S3)),
    {ok, [Plan]} = astranaut_macro_local:finalize_plan(S3),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    {just, S4} = astranaut_return:run(
                   astranaut_macro_local:execute_plan(
                     [Plan], Context, S3)),
    ?assertEqual(1, maps:get(generation, S4)),
    ok.

dependency_preexpansion_compiles_only_needed_boundary(_Config) ->
    Module = local_macro_dependency_preexpand_test,
    Source = [{attribute, 1, module, Module}, macro_a_form(), b_form_calls_a()],
    AMap = #{{a, 0} => #{macro_source => local_macro,
                         module => astranaut_macro_local:module_name(Module),
                         macro_module => Module,
                         macro => a, function => a,
                         arity => 0, call_arity => 0,
                         max_depth => 100, order => inner,
                         formatter => astranaut_macro,
                         file => [], local_module => Module}},
    {ok, S0} = astranaut_macro_local:register(
                 [{a, 0}], #{}, Source, macro_environment(#{}),
                 astranaut_macro_local:new()),
    Context0 = #{source_view => Source, compile_opts => []},
    {just, S1} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{a, 0}], Context0, S0)),
    {ok, S2} = astranaut_macro_local:register(
                 [{b, 0}], #{}, Source, macro_environment(AMap),
                 S1),
    Context1 = Context0,
    {just, S3} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{b, 0}], Context1, S2)),
    #{{a, 0} := #{status := compiled},
      {b, 0} := #{status := pending}} =
        astranaut_macro_local:local_macros(S3),
    ?assertEqual(1, maps:get(generation, S3)),
    ok.

compiler_reuses_canonical_forms(_Config) ->
    Module = local_macro_canonical_compile_test,
    Source = [{attribute, 1, module, Module}, a_form()],
    {ok, S0} = astranaut_macro_local:register(
                 [{a, 0}], #{}, Source, macro_environment(#{}),
                 astranaut_macro_local:new()),
    Context = #{source_view => Source, compile_opts => []},
    {just, S1} = astranaut_return:run(
                   astranaut_macro_local:prepare_declaration(
                     [{a, 0}], Context, S0)),
    {ok, Plan} = astranaut_macro_local:compile_plan({a, 0}, S1),
    {just, S2} = astranaut_return:run(
                   astranaut_macro_local:execute_plan(
                     Plan, Context, S1)),
    Generation = maps:get(generation, S2),
    LaterTriggerContext = Context#{source_view =>
                                       Source ++ [{attribute, 2, later, changed}]},
    {just, S3} = astranaut_return:run(
                   astranaut_macro_local:execute_plan(
                     Plan, LaterTriggerContext, S2)),
    ?assertEqual(Generation, maps:get(generation, S3)),
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
    EffectiveMacroMap = maps:merge(MacroMap, CandidateMap),
    astranaut_macro_local:register(
      FAs, Options, Source, macro_environment(EffectiveMacroMap),
      State).

macro_environment(MacroMap) ->
    #{macro_map => MacroMap,
      macro_options => #{}}.

expand_single_function(MacroMap, Forms, {Name, Arity}, Control) ->
    FormId = {function, Name, Arity},
    [Form] =
        [FunctionForm
         || {function, _Pos, FunctionName, FunctionArity, _Clauses}
                = FunctionForm <- Forms,
            FunctionName =:= Name,
            FunctionArity =:= Arity],
    Task = #{form => Form,
             macro_map => MacroMap,
             whitelist_control => Control},
    astranaut_return:lift_m(
      fun(#{forms := ExpandedForms,
            task_results := #{FormId := Result}}) ->
              Result#{forms => ExpandedForms}
      end,
      astranaut_macro_expander:expand_functions(
        Forms, #{FormId => Task})).

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

external_macro(Function) ->
    (whitelist_macro(Function))#{macro_source => external_macro}.

retained_conflict_macro() ->
    {atom, 1, final_value}.

whitelist_target_form() ->
    {function, 1, whitelist_target, 0,
     [{clause, 1, [], [],
       [{call, 1, {atom, 1, whitelist_chain_a}, []}]}]}.

task_macro_form(Name, MacroName) ->
    {function, 1, Name, 0,
     [{clause, 1, [], [],
       [{call, 1, {atom, 1, MacroName}, []}]}]}.

function_call_analysis_form() ->
    {function, 1, analysis_target, 0,
     [{clause, 1, [], [],
       [{call, 1, {atom, 1, helper}, []},
        {call, 2, {atom, 2, whitelist_chain_a}, []},
        {call, 3,
         {remote, 3,
          {atom, 3, analysis_remote},
          {atom, 3, whitelist_chain_b}}, []}]}]}.

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
a_form() -> {function, 1, a, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
macro_a_form() ->
    {function, 1, a, 0,
     [{clause, 1, [], [], [erl_parse:abstract({atom, 1, ok})]}]}.
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
