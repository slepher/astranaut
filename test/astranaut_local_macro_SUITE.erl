%%%-------------------------------------------------------------------
%%% Focused tests for the local-macro state machine.  These intentionally do
%%% not invoke the parse transform: the scanner integration has separate tests.
%%%-------------------------------------------------------------------
-module(astranaut_local_macro_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

all() -> [register_freezes_static_closure,
          duplicate_declaration_fails_atomically,
          cache_rejects_conflicting_environments,
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
          shared_declaration_uses_one_group_context,
          declaration_preexpands_without_compiling,
          independent_declaration_does_not_compile,
          dependency_preexpansion_compiles_only_needed_boundary,
          compiler_reuses_canonical_forms,
          independent_macros_share_one_boundary,
          final_retained_helper_comparison,
          safe_load_replaces_current_generation,
          safe_load_refuses_module_with_old_code_in_use,
          non_frozen_retain_root_has_no_effect].

register_freezes_static_closure(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{imports => [a]},
                                                  astranaut_local_macro:new()),
    #{ {foo, 0} := Entry } = astranaut_local_macro:local_macros(State),
    ?assertEqual([{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}],
                 maps:get(closure_ids, Entry)),
    ?assertEqual(ordsets:from_list([{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}]),
                 astranaut_local_macro:frozen_ids(State)),
    ok.

duplicate_declaration_fails_atomically(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                  astranaut_local_macro:new()),
    ?assertEqual({error, {duplicate_local_macro_declaration, {foo, 0}}},
                 register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, State)),
    ?assertEqual(1, map_size(astranaut_local_macro:local_macros(State))),
    ok.

cache_rejects_conflicting_environments(_Config) ->
    State0 = astranaut_local_macro:new(),
    {ok, State1} = astranaut_local_macro:cache_expanded({function, helper, 0}, env_a, helper_form(a), State0),
    {ok, _State2} = astranaut_local_macro:cache_expanded({function, helper, 0}, env_b, helper_form(a), State1),
    ?assertEqual({error, {conflicting_local_macro_closure_environment, {function, helper, 0}}},
                 astranaut_local_macro:cache_expanded({function, helper, 0}, env_c, helper_form(b), State1)),
    ok.

cache_hits_same_fingerprint(_Config) ->
    State0 = astranaut_local_macro:new(),
    {ok, State1} = astranaut_local_macro:cache_expanded({function, helper, 0}, env_a, helper_form(a), State0),
    {ok, _State2} = astranaut_local_macro:cache_expanded({function, helper, 0}, env_a, helper_form(a), State1),
    ok.

cache_reuses_environment_after_intervening_context(_Config) ->
    FormId = {function, helper, 0},
    Form = helper_form(a),
    {ok, State1} = astranaut_local_macro:cache_expanded(
                     FormId, env_a, Form, astranaut_local_macro:new()),
    {ok, State2} = astranaut_local_macro:cache_expanded(
                     FormId, env_b, Form, State1),
    {ok, State3} = astranaut_local_macro:cache_expanded(
                     FormId, env_a, Form, State2),
    Record = maps:get(FormId, maps:get(expansion_records, State3)),
    ?assertEqual(2, map_size(maps:get(results_by_env, Record))),
    ?assertEqual(Form, maps:get(canonical_result, Record)),
    ok.

execute_plan_rejects_conflicting_snapshots(_Config) ->
    Source = [{attribute, 1, module, local_macro_conflict_plan_test},
              first_form(), second_form(), helper_form(ok)],
    {ok, S1} = register(
                 [{first, 0}], #{}, Source, #{snapshot => first},
                 astranaut_local_macro:new()),
    {ok, S2} = register(
                 [{second, 0}], #{}, Source, #{snapshot => second}, S1),
    {ok, Plan} = astranaut_local_macro:finalize_plan(S2),
    Expand =
        fun(MacroEnv, _InjectForms, Forms, {helper, 0}) ->
                Value = case maps:is_key(snapshot, MacroEnv) of
                            true -> maps:get(snapshot, MacroEnv);
                            false -> none
                        end,
                astranaut_return:return(
                  astranaut_local_macro:materialize_forms(
                    Forms, #{{function, helper, 0} => helper_form(Value)}));
           (_MacroEnv, _InjectForms, Forms, _TargetFA) ->
                astranaut_return:return(Forms)
        end,
    MacroOps = #{expand_function => Expand,
                 merge_macro_maps =>
                     fun(First, Second) ->
                             astranaut_return:return(maps:merge(First, Second))
                     end},
    Context = #{local_macro_map => #{}, source_view => Source, compile_opts => []},
    Error = astranaut_return:run_error(
              astranaut_local_macro:execute_plan(Plan, Context, MacroOps, S2)),
    ?assert(lists:member(
              {conflicting_local_macro_closure_environment, {function, helper, 0}},
              astranaut_error:errors(Error))),
    ok.

execute_plan_rejects_conflicting_inject_snapshots(_Config) ->
    Source = [{attribute, 1, module, local_macro_inject_conflict_plan_test},
              first_form(), second_form(), helper_form(ok)],
    MacroOps0 = identity_macro_ops(),
    {ok, S1} = astranaut_local_macro:register(
                 [{first, 0}], #{}, Source, runtime_context(#{}, [early]),
                 #{}, MacroOps0, astranaut_local_macro:new()),
    {ok, S2} = astranaut_local_macro:register(
                 [{second, 0}], #{}, Source, runtime_context(#{}, [late]),
                 #{}, MacroOps0, S1),
    {ok, Plan} = astranaut_local_macro:finalize_plan(S2),
    Expand =
        fun(_MacroEnv, InjectForms, Forms, {helper, 0}) ->
                Value = case InjectForms of
                            [early] -> early;
                            [late] -> late
                        end,
                astranaut_return:return(
                  astranaut_local_macro:materialize_forms(
                    Forms, #{{function, helper, 0} => helper_form(Value)}));
           (_MacroEnv, _InjectForms, Forms, _TargetFA) ->
                astranaut_return:return(Forms)
        end,
    MacroOps = MacroOps0#{expand_function => Expand,
                          merge_macro_maps =>
                              fun(First, Second) ->
                                      astranaut_return:return(
                                        maps:merge(First, Second))
                              end},
    Context = #{local_macro_map => #{}, source_view => Source,
                compile_opts => []},
    Error = astranaut_return:run_error(
              astranaut_local_macro:execute_plan(
                Plan, Context, MacroOps, S2)),
    ?assert(lists:member(
              {conflicting_local_macro_closure_environment,
               {function, helper, 0}},
              astranaut_error:errors(Error))),
    ok.

retain_controls_final_skip_ids(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State0} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                   astranaut_local_macro:new()),
    Forms = #{{function, foo, 0} => Foo, {function, helper, 0} => Helper, {spec, helper, 0} => Spec},
    State1 = astranaut_local_macro:commit_compiled([{foo, 0}], Forms, State0),
    {_Env0, Skip0, _} = astranaut_local_macro:finalize([], State1),
    ?assertEqual(ordsets:from_list(maps:keys(Forms)), Skip0),
    {_Env1, Skip1, _} = astranaut_local_macro:finalize([{helper, 0}], State1),
    ?assertEqual([], Skip1),
    ok.

source_view_only_contains_materialised_forms(_Config) ->
    ?assertEqual([passed, queued], astranaut_local_macro:source_view([passed], [queued])),
    ok.

declaration_inject_snapshot_is_preserved(_Config) ->
    [Foo, Helper, Spec] = forms(),
    Source = [early, Foo, Helper, Spec, late],
    InjectForms = [early],
    MacroOps = identity_macro_ops(),
    {ok, State} = astranaut_local_macro:register(
                    [{foo, 0}], #{}, Source,
                    runtime_context(#{}, InjectForms), #{}, MacroOps,
                    astranaut_local_macro:new()),
    #{{foo, 0} := Entry} = astranaut_local_macro:local_macros(State),
    ?assertEqual(Source, maps:get(source_view, Entry)),
    ?assertNot(maps:is_key(env_snapshot, Entry)),
    ?assertNot(maps:is_key(inject_forms_snapshot, Entry)),
    ?assertEqual(
       InjectForms,
       maps:get(inject_forms, maps:get(runtime_context_snapshot, Entry))),
    {ok, [Boundary]} = astranaut_local_macro:compile_plan({foo, 0}, State),
    [Request] = maps:get(requests, Boundary),
    ?assertEqual(
       lists:sort([group_members, closure_ids, closure_fas,
                   referenced_local_macros, runtime_context_snapshot,
                   source_view, options, forms]),
       lists:sort(maps:keys(Request))),
    ?assertEqual(
       InjectForms,
       maps:get(inject_forms, maps:get(runtime_context_snapshot, Request))),
    ok.

fingerprint_includes_injected_forms(_Config) ->
    A = astranaut_local_macro:env_fingerprint(#{imports => [a]}, #{foo => 1}, #{}, [early]),
    B = astranaut_local_macro:env_fingerprint(#{imports => [a]}, #{foo => 1}, #{}, [late]),
    ?assertNotEqual(A, B),
    ok.

frozen_splice_is_rejected(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                  astranaut_local_macro:new()),
    ?assertEqual({error, {illegal_locked_form_mutation, Helper}},
                 astranaut_local_macro:reject_locked_mutation([Helper], State)),
    ?assertEqual(ok, astranaut_local_macro:reject_locked_mutation([helper_form(other)], astranaut_local_macro:new())),
    ok.

later_declaration_remains_helper_in_earlier_closure(_Config) ->
    Source = [a_calls_b(), b_form_independent()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_local_macro:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    #{ {a, 0} := A, {b, 0} := B } = astranaut_local_macro:local_macros(S2),
    ?assert(lists:member({function, b, 0}, maps:get(closure_ids, A))),
    ?assertEqual([], maps:get(referenced_local_macros, A)),
    ?assert(lists:member({function, b, 0}, maps:get(closure_ids, B))),
    ok.

declaration_snapshot_and_actual_local_references(_Config) ->
    Source = [a_form(), b_form_calls_a(), unused_form()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{imports => [early]}, astranaut_local_macro:new()),
    {ok, S2} = register([{unused, 0}], #{}, Source, #{imports => [middle]}, S1),
    {ok, S3} = register([{b, 0}], #{}, Source, #{imports => [late]}, S2),
    #{ {a, 0} := A, {b, 0} := B } = astranaut_local_macro:local_macros(S3),
    ?assertEqual(#{imports => [early]},
                 maps:get(macro_map, maps:get(runtime_context_snapshot, A))),
    ?assertEqual(#{imports => [late]},
                 maps:get(macro_map, maps:get(runtime_context_snapshot, B))),
    ?assertEqual([{a, 0}], maps:get(referenced_local_macros, B)),
    ok.

extra_functions_and_self_recursion(_Config) ->
    Source = [recursive_form(), helper_form(ok)],
    {ok, State} = register([{recursive, 0}], #{extra_functions => [{helper, 0}]}, Source, #{},
                                                  astranaut_local_macro:new()),
    #{ {recursive, 0} := Entry } = astranaut_local_macro:local_macros(State),
    ?assertEqual([], maps:get(referenced_local_macros, Entry)),
    ?assert(lists:member({function, helper, 0}, maps:get(closure_ids, Entry))),
    ?assertEqual({error, {invalid_extra_functions, [{missing, 0}]}},
                 register([{recursive, 0}], #{extra_functions => [{missing, 0}]}, Source, #{},
                                                  astranaut_local_macro:new())),
    ok.

internal_function_conflict(_Config) ->
    Source = [first_form(), second_form(), helper_form(ok)],
    {ok, State} = register([{first, 0}], #{internal_function => [{helper, 0}]}, Source, #{},
                                                  astranaut_local_macro:new()),
    ?assertMatch({error, {conflicting_internal_function_policy, {helper, 0}, _}},
                 register([{second, 0}], #{}, Source, #{}, State)),
    ok.

minimal_cumulative_compile_boundaries(_Config) ->
    Source = [a_form(), b_form_calls_a()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_local_macro:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [P1, P2]} = astranaut_local_macro:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}], maps:get(members, P1)),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, P2)),
    [RequestA] = maps:get(requests, P1),
    ?assert(maps:is_key({function, a, 0}, maps:get(forms, RequestA))),
    {ok, [FinalDependency, Final]} = astranaut_local_macro:finalize_plan(S2),
    ?assertEqual([{a, 0}], maps:get(members, FinalDependency)),
    ?assertEqual(false, maps:get(final, FinalDependency, false)),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Final)),
    ?assertEqual(true, maps:get(final, Final)),
    ok.

independent_macros_share_one_boundary(_Config) ->
    Source = [a_form(), b_form_independent()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_local_macro:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [Plan]} = astranaut_local_macro:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    ok.

shared_declaration_stays_in_one_boundary(_Config) ->
    Source = [a_form(), b_form_independent(), c_form_calls_a()],
    {ok, S1} = register(
                 [{a, 0}, {b, 0}], #{}, Source, #{}, astranaut_local_macro:new()),
    {ok, S2} = register([{c, 0}], #{}, Source, #{}, S1),
    {ok, [Dependency, Final]} = astranaut_local_macro:finalize_plan(S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Dependency)),
    ?assertEqual([{a, 0}, {b, 0}, {c, 0}], maps:get(members, Final)),
    ok.

shared_declaration_uses_one_group_context(_Config) ->
    Source = [a_form(), b_form_independent()],
    {ok, State} = register(
                    [{a, 0}, {b, 0}], #{}, Source,
                    #{shared => true},
                    astranaut_local_macro:new()),
    #{{a, 0} := A, {b, 0} := B} =
        astranaut_local_macro:local_macros(State),
    ?assertEqual(maps:get(group_id, A), maps:get(group_id, B)),
    ?assertEqual(ordsets:from_list([{a, 0}, {b, 0}]),
                 maps:get(group_members, A)),
    ?assertEqual(maps:get(runtime_context_snapshot, A),
                 maps:get(runtime_context_snapshot, B)),
    {ok, [Plan]} = astranaut_local_macro:compile_plan({b, 0}, State),
    [RequestA, RequestB] = maps:get(requests, Plan),
    ?assertEqual(maps:get(group_members, RequestA),
                 maps:get(group_members, RequestB)),
    ok.

declaration_preexpands_without_compiling(_Config) ->
    Source = [a_form()],
    MacroOps = identity_macro_ops(),
    {ok, State0} = astranaut_local_macro:register(
                     [{a, 0}], #{}, Source, runtime_context(#{}, []),
                     #{}, MacroOps, astranaut_local_macro:new()),
    Context = #{local_macro_map => #{}, source_view => Source,
                compile_opts => []},
    {just, State1} = astranaut_return:run(
                       astranaut_local_macro:prepare_declaration(
                         [{a, 0}], Context, MacroOps, State0)),
    #{{a, 0} := #{status := pending}} =
        astranaut_local_macro:local_macros(State1),
    ?assertEqual(0, maps:get(generation, State1)),
    ?assert(maps:is_key({function, a, 0},
                        maps:get(canonical_expanded_forms, State1))),
    ok.

independent_declaration_does_not_compile(_Config) ->
    Module = local_macro_independent_declaration_test,
    Source = [{attribute, 1, module, Module}, a_form(), b_form_independent()],
    MacroOps = identity_macro_ops(),
    Context = #{local_macro_map => #{}, source_view => Source,
                compile_opts => []},
    {ok, S0} = astranaut_local_macro:register(
                 [{a, 0}], #{}, Source, runtime_context(#{}, []),
                 #{}, MacroOps, astranaut_local_macro:new()),
    {just, S1} = astranaut_return:run(
                   astranaut_local_macro:prepare_declaration(
                     [{a, 0}], Context, MacroOps, S0)),
    {ok, S2} = astranaut_local_macro:register(
                 [{b, 0}], #{}, Source, runtime_context(#{}, []),
                 #{}, MacroOps, S1),
    {just, S3} = astranaut_return:run(
                   astranaut_local_macro:prepare_declaration(
                     [{b, 0}], Context, MacroOps, S2)),
    ?assertEqual(0, maps:get(generation, S3)),
    {ok, [Plan]} = astranaut_local_macro:finalize_plan(S3),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    {just, S4} = astranaut_return:run(
                   astranaut_local_macro:execute_plan(
                     [Plan], Context, MacroOps, S3)),
    ?assertEqual(1, maps:get(generation, S4)),
    ok.

dependency_preexpansion_compiles_only_needed_boundary(_Config) ->
    Module = local_macro_dependency_preexpand_test,
    Source = [{attribute, 1, module, Module}, a_form(), b_form_calls_a()],
    MacroOps = identity_macro_ops(),
    AMap = #{{a, 0} => #{macro_source => local_macro,
                         macro_module => astranaut_local_macro:module_name(Module),
                         function => a, arity => 0}},
    {ok, S0} = astranaut_local_macro:register(
                 [{a, 0}], #{}, Source, runtime_context(#{}, []),
                 #{}, MacroOps, astranaut_local_macro:new()),
    Context0 = #{local_macro_map => #{}, source_view => Source,
                 compile_opts => []},
    {just, S1} = astranaut_return:run(
                   astranaut_local_macro:prepare_declaration(
                     [{a, 0}], Context0, MacroOps, S0)),
    {ok, S2} = astranaut_local_macro:register(
                 [{b, 0}], #{}, Source, runtime_context(#{}, []),
                 AMap, MacroOps, S1),
    Context1 = Context0#{local_macro_map => AMap},
    {just, S3} = astranaut_return:run(
                   astranaut_local_macro:prepare_declaration(
                     [{b, 0}], Context1, MacroOps, S2)),
    #{{a, 0} := #{status := compiled},
      {b, 0} := #{status := pending}} =
        astranaut_local_macro:local_macros(S3),
    ?assertEqual(1, maps:get(generation, S3)),
    ok.

compiler_reuses_canonical_forms(_Config) ->
    Module = local_macro_canonical_compile_test,
    Source = [{attribute, 1, module, Module}, a_form()],
    MacroOps = identity_macro_ops(),
    {ok, S0} = astranaut_local_macro:register(
                 [{a, 0}], #{}, Source, runtime_context(#{}, []),
                 #{}, MacroOps, astranaut_local_macro:new()),
    Context = #{local_macro_map => #{}, source_view => Source,
                compile_opts => []},
    {just, S1} = astranaut_return:run(
                   astranaut_local_macro:prepare_declaration(
                     [{a, 0}], Context, MacroOps, S0)),
    NoExpandOps = MacroOps#{expand_function =>
                               fun(_, _, _, _) -> error(unexpected_reexpand) end},
    {ok, Plan} = astranaut_local_macro:compile_plan({a, 0}, S1),
    {just, S2} = astranaut_return:run(
                   astranaut_local_macro:execute_plan(
                     Plan, Context, NoExpandOps, S1)),
    Generation = maps:get(generation, S2),
    LaterTriggerContext = Context#{source_view =>
                                       Source ++ [{attribute, 2, later, changed}]},
    {just, S3} = astranaut_return:run(
                   astranaut_local_macro:execute_plan(
                     Plan, LaterTriggerContext, NoExpandOps, S2)),
    ?assertEqual(Generation, maps:get(generation, S3)),
    ok.

final_retained_helper_comparison(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, S0} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, astranaut_local_macro:new()),
    S1 = astranaut_local_macro:commit_compiled([{foo, 0}], #{{function, foo, 0} => Foo, {function, helper, 0} => Helper}, S0),
    {_Env, _Skip, S2} = astranaut_local_macro:finalize([{helper, 0}], S1),
    ?assertEqual(ok, astranaut_local_macro:verify_retained(#{{function, helper, 0} => Helper}, S2)),
    ?assertEqual({error, {conflicting_local_macro_closure_environment, {function, helper, 0}}},
                 astranaut_local_macro:verify_retained(#{{function, helper, 0} => helper_form(changed)}, S2)),
    {_Env2, _Skip2, S3} = astranaut_local_macro:finalize([{foo, 0}], S1),
    ?assertEqual(
       {error, {conflicting_local_macro_closure_environment,
                {function, foo, 0}}},
       astranaut_local_macro:verify_retained(
         #{{function, foo, 0} => helper_form(changed)}, S3)),
    ok.

safe_load_replaces_current_generation(_Config) ->
    Module = astranaut_local_macro_safe_load_test,
    {just, {Module, _}} = astranaut_return:run(astranaut_local_macro:safe_load(Module, load_forms(Module, first), [without_warnings])),
    ?assertEqual(first, Module:value()),
    {just, {Module, _}} = astranaut_return:run(astranaut_local_macro:safe_load(Module, load_forms(Module, second), [without_warnings])),
    ?assertEqual(second, Module:value()),
    ok.

safe_load_refuses_module_with_old_code_in_use(_Config) ->
    Module = astranaut_local_macro_busy_load_test,
    {just, {Module, _}} = astranaut_return:run(astranaut_local_macro:safe_load(Module, busy_forms(Module, first), [without_warnings])),
    Pid = spawn(Module, hold, []),
    timer:sleep(10),
    {ok, Module, Binary} = compile:forms(busy_forms(Module, second), [binary]),
    {module, Module} = code:load_binary(Module, [], Binary),
    Error = astranaut_return:run_error(astranaut_local_macro:safe_load(Module, busy_forms(Module, third), [without_warnings])),
    ?assertEqual([local_macro_module_in_use], astranaut_error:errors(Error)),
    Pid ! stop,
    ok.

non_frozen_retain_root_has_no_effect(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, S0} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, astranaut_local_macro:new()),
    S1 = astranaut_local_macro:commit_compiled([{foo, 0}], #{{function, foo, 0} => Foo}, S0),
    {_Env, Skip, _S2} = astranaut_local_macro:finalize([{ordinary, 0}], S1),
    ?assertEqual([{function, foo, 0}], Skip),
    ok.

register(FAs, Options, Source, MacroMap, State) ->
    CandidateMap =
        maps:from_list(
          [{{Name, Arity}, #{macro_source => local_macro,
                            function => Name, arity => Arity}}
           || {Name, Arity} <- maps:keys(astranaut_local_macro:local_macros(State))]),
    MacroOps = identity_macro_ops(),
    astranaut_local_macro:register(
      FAs, Options, Source, runtime_context(MacroMap, Source), CandidateMap,
      MacroOps, State).

runtime_context(MacroMap, InjectForms) ->
    #{macro_map => MacroMap,
      macro_options => #{},
      inject_forms => InjectForms}.

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
          fun(_MacroEnv, _InjectForms, Forms, _TargetFA) ->
                  astranaut_return:return(Forms)
          end,
      merge_macro_maps =>
          fun(First, Second) ->
                  astranaut_return:return(maps:merge(First, Second))
          end}.

forms() ->
    [foo_form(), helper_form(ok), {attribute, 1, spec, {{helper, 0}, []}}].

foo_form() ->
    {function, 1, foo, 0,
     [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}.

helper_form(Value) ->
    {function, 1, helper, 0, [{clause, 1, [], [], [{atom, 1, Value}]}]}.

recursive_form() -> {function, 1, recursive, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, recursive}, []}]}]}.
first_form() -> {function, 1, first, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}.
second_form() -> {function, 1, second, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}.
a_form() -> {function, 1, a, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
b_form_calls_a() -> {function, 1, b, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, a}, []}]}]}.
b_form_independent() -> {function, 1, b, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
c_form_calls_a() -> {function, 1, c, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, a}, []}]}]}.
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
