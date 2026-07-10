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
          cache_hits_same_fingerprint,
          retain_controls_final_skip_ids,
          source_view_only_contains_materialised_forms,
          fingerprint_includes_injected_forms,
          frozen_splice_is_rejected,
          later_declaration_remains_helper_in_earlier_closure,
          declaration_snapshot_and_actual_local_references,
          extra_functions_and_self_recursion,
          internal_function_conflict,
          minimal_cumulative_compile_boundaries,
          independent_macros_share_one_boundary,
          final_retained_helper_comparison,
          safe_load_replaces_current_generation,
          safe_load_refuses_module_with_old_code_in_use,
          non_frozen_retain_root_has_no_effect].

register_freezes_static_closure(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = astranaut_local_macro:register([{foo, 0}], #{}, [Foo, Helper, Spec], #{imports => [a]},
                                                  astranaut_local_macro:new()),
    #{ {foo, 0} := Entry } = astranaut_local_macro:local_macros(State),
    ?assertEqual([{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}],
                 maps:get(closure_ids, Entry)),
    ?assertEqual(ordsets:from_list([{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}]),
                 astranaut_local_macro:frozen_ids(State)),
    ok.

duplicate_declaration_fails_atomically(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = astranaut_local_macro:register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                  astranaut_local_macro:new()),
    ?assertEqual({error, {duplicate_local_macro_declaration, {foo, 0}}},
                 astranaut_local_macro:register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, State)),
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

retain_controls_final_skip_ids(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State0} = astranaut_local_macro:register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
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

fingerprint_includes_injected_forms(_Config) ->
    A = astranaut_local_macro:env_fingerprint(#{imports => [a]}, #{foo => 1}, #{}, [early]),
    B = astranaut_local_macro:env_fingerprint(#{imports => [a]}, #{foo => 1}, #{}, [late]),
    ?assertNotEqual(A, B),
    ok.

frozen_splice_is_rejected(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = astranaut_local_macro:register([{foo, 0}], #{}, [Foo, Helper, Spec], #{},
                                                  astranaut_local_macro:new()),
    ?assertEqual({error, {illegal_locked_form_mutation, Helper}},
                 astranaut_local_macro:reject_locked_mutation([Helper], State)),
    ?assertEqual(ok, astranaut_local_macro:reject_locked_mutation([helper_form(other)], astranaut_local_macro:new())),
    ok.

later_declaration_remains_helper_in_earlier_closure(_Config) ->
    Source = [a_calls_b(), b_form_independent()],
    {ok, S1} = astranaut_local_macro:register([{a, 0}], #{}, Source, #{}, astranaut_local_macro:new()),
    {ok, S2} = astranaut_local_macro:register([{b, 0}], #{}, Source, #{}, S1),
    #{ {a, 0} := A, {b, 0} := B } = astranaut_local_macro:local_macros(S2),
    ?assert(lists:member({function, b, 0}, maps:get(closure_ids, A))),
    ?assertEqual([], maps:get(referenced_local_macros, A)),
    ?assert(lists:member({function, b, 0}, maps:get(closure_ids, B))),
    ok.

declaration_snapshot_and_actual_local_references(_Config) ->
    Source = [a_form(), b_form_calls_a(), unused_form()],
    {ok, S1} = astranaut_local_macro:register([{a, 0}], #{}, Source, #{imports => [early]}, astranaut_local_macro:new()),
    {ok, S2} = astranaut_local_macro:register([{unused, 0}], #{}, Source, #{imports => [middle]}, S1),
    {ok, S3} = astranaut_local_macro:register([{b, 0}], #{}, Source, #{imports => [late]}, S2),
    #{ {a, 0} := A, {b, 0} := B } = astranaut_local_macro:local_macros(S3),
    ?assertEqual(#{imports => [early]}, maps:get(env_snapshot, A)),
    ?assertEqual(#{imports => [late]}, maps:get(env_snapshot, B)),
    ?assertEqual([{a, 0}], maps:get(referenced_local_macros, B)),
    ok.

extra_functions_and_self_recursion(_Config) ->
    Source = [recursive_form(), helper_form(ok)],
    {ok, State} = astranaut_local_macro:register([{recursive, 0}], #{extra_functions => [{helper, 0}]}, Source, #{},
                                                  astranaut_local_macro:new()),
    #{ {recursive, 0} := Entry } = astranaut_local_macro:local_macros(State),
    ?assertEqual([], maps:get(referenced_local_macros, Entry)),
    ?assert(lists:member({function, helper, 0}, maps:get(closure_ids, Entry))),
    ?assertEqual({error, {invalid_extra_functions, [{missing, 0}]}},
                 astranaut_local_macro:register([{recursive, 0}], #{extra_functions => [{missing, 0}]}, Source, #{},
                                                  astranaut_local_macro:new())),
    ok.

internal_function_conflict(_Config) ->
    Source = [first_form(), second_form(), helper_form(ok)],
    {ok, State} = astranaut_local_macro:register([{first, 0}], #{internal_function => [{helper, 0}]}, Source, #{},
                                                  astranaut_local_macro:new()),
    ?assertMatch({error, {conflicting_internal_function_policy, {helper, 0}, _}},
                 astranaut_local_macro:register([{second, 0}], #{}, Source, #{}, State)),
    ok.

minimal_cumulative_compile_boundaries(_Config) ->
    Source = [a_form(), b_form_calls_a()],
    {ok, S1} = astranaut_local_macro:register([{a, 0}], #{}, Source, #{}, astranaut_local_macro:new()),
    {ok, S2} = astranaut_local_macro:register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [P1, P2]} = astranaut_local_macro:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}], maps:get(members, P1)),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, P2)),
    [RequestA] = maps:get(requests, P1),
    ?assert(maps:is_key({function, a, 0}, maps:get(forms, RequestA))),
    {ok, [Final]} = astranaut_local_macro:finalize_plan(S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Final)),
    ok.

independent_macros_share_one_boundary(_Config) ->
    Source = [a_form(), b_form_independent()],
    {ok, S1} = astranaut_local_macro:register([{a, 0}], #{}, Source, #{}, astranaut_local_macro:new()),
    {ok, S2} = astranaut_local_macro:register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [Plan]} = astranaut_local_macro:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    ok.

final_retained_helper_comparison(_Config) ->
    [Foo, Helper, Spec] = forms(),
    {ok, S0} = astranaut_local_macro:register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, astranaut_local_macro:new()),
    S1 = astranaut_local_macro:commit_compiled([{foo, 0}], #{{function, foo, 0} => Foo, {function, helper, 0} => Helper}, S0),
    {_Env, _Skip, S2} = astranaut_local_macro:finalize([{helper, 0}], S1),
    ?assertEqual(ok, astranaut_local_macro:verify_retained(#{{function, helper, 0} => Helper}, S2)),
    ?assertEqual({error, {conflicting_local_macro_closure_environment, {function, helper, 0}}},
                 astranaut_local_macro:verify_retained(#{{function, helper, 0} => helper_form(changed)}, S2)),
    {_Env2, _Skip2, S3} = astranaut_local_macro:finalize([{foo, 0}], S1),
    ?assertEqual(ok, astranaut_local_macro:verify_retained(#{{function, foo, 0} => helper_form(changed)}, S3)),
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
    {ok, S0} = astranaut_local_macro:register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, astranaut_local_macro:new()),
    S1 = astranaut_local_macro:commit_compiled([{foo, 0}], #{{function, foo, 0} => Foo}, S0),
    {_Env, Skip, _S2} = astranaut_local_macro:finalize([{ordinary, 0}], S1),
    ?assertEqual([{function, foo, 0}], Skip),
    ok.

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
