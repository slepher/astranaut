%%%-------------------------------------------------------------------
%%% Focused tests for the local-macro state machine.  These intentionally do
%%% not invoke the parse transform: the scanner integration has separate tests.
%%%-------------------------------------------------------------------
-module(astranaut_macro_local_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

prepare_target_only_requests_context_when_compilation_is_needed_test() ->
    State = (astranaut_macro_local:new(test_lazy_context))#{
        local_macros => #{{ready, 0} => #{}},
        compiler => (astranaut_macro_local_compile:new(test_lazy_context))#{
            versions => #{{ready, 0} => 1}
        }
    },
    UnusedContext = fun() -> error(unexpected_context_request) end,
    Targets = [
        #{macro => #{macro_source => imported_macro}},
        #{macro => #{macro_source => local_macro, function => ready, arity => 0}}
    ],
    lists:foreach(
        fun(Target) ->
            ?assertEqual(
                {just, State},
                astranaut_return:run(
                    astranaut_macro_local:prepare_target(Target, UnusedContext, State)
                )
            )
        end,
        Targets
    ).

module_name_is_unique_per_allocation_test() ->
    Module = local_macro_unique_name_test,
    First = astranaut_macro_local_compile:module_name(Module),
    Second = astranaut_macro_local_compile:module_name(Module),
    ?assertNotEqual(First, Second),
    Prefix = atom_to_list(Module) ++ "__local_macro__",
    ?assert(lists:prefix(Prefix, atom_to_list(First))),
    ?assert(lists:prefix(Prefix, atom_to_list(Second))),
    ok.

register_freezes_static_closure_test() ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register(
        [{foo, 0}],
        #{},
        [Foo, Helper, Spec],
        #{imports => [a]},
        astranaut_macro_local:new()
    ),
    #{{foo, 0} := Entry} = astranaut_macro_local:local_macros(State),
    ?assertEqual(
        [{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}],
        maps:get(closure_ids, Entry)
    ),
    ?assertEqual(
        ordsets:from_list([{function, foo, 0}, {function, helper, 0}, {spec, helper, 0}]),
        astranaut_macro_local:frozen_ids(State)
    ),
    ok.

duplicate_declaration_fails_atomically_test() ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register(
        [{foo, 0}],
        #{},
        [Foo, Helper, Spec],
        #{},
        astranaut_macro_local:new()
    ),
    ?assertEqual(
        {error, {duplicate_local_macro_declaration, {foo, 0}}},
        register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, State)
    ),
    ?assertEqual(1, map_size(astranaut_macro_local:local_macros(State))),
    ok.

retain_controls_final_skip_ids_test() ->
    [Foo, Helper, Spec] = forms(),
    {ok, State0} = register(
        [{foo, 0}],
        #{},
        [Foo, Helper, Spec],
        #{},
        astranaut_macro_local:new()
    ),
    Forms = #{
        {function, foo, 0} => Foo, {function, helper, 0} => Helper, {spec, helper, 0} => Spec
    },
    State1 = compile_forms([{foo, 0}], Forms, State0),
    {_Env0, Skip0, _} = astranaut_macro_local:finalize([], State1),
    ?assertEqual(ordsets:from_list(maps:keys(Forms)), Skip0),
    {_Env1, Skip1, _} = astranaut_macro_local:finalize([{helper, 0}], State1),
    ?assertEqual([], Skip1),
    ok.

source_view_only_contains_materialised_forms_test() ->
    ?assertEqual([passed, queued], astranaut_macro_local:source_view([passed], [queued])),
    ok.

frozen_splice_reports_generated_form_position_test() ->
    Frozen = {function, {7, 1}, foo, 0, [{clause, {7, 1}, [], [], [{atom, {7, 5}, original}]}]},
    Generated =
        {function, {42, 3}, foo, 0, [{clause, {42, 3}, [], [], [{atom, {42, 7}, replacement}]}]},
    {ok, State} = astranaut_macro_local:register(
        [{foo, 0}],
        #{},
        [Frozen],
        macro_environment(#{}),
        astranaut_macro_local:new()
    ),
    Error = astranaut_return:run_error(
        astranaut_macro_local:validate_generated(
            [Generated], State
        )
    ),
    ?assertEqual(
        [{{42, 3}, astranaut_macro_local, {illegal_locked_form_mutation, Generated}}],
        maps:get(
            formatted_errors,
            astranaut_error:printable(Error)
        )
    ),
    ok.

local_compiler_preserves_loaded_code_on_compile_failure_test() ->
    Module = astranaut_macro_local_compile:module_name(local_compiler_failure_test),
    FooId = {function, foo, 0},
    HelperId = {function, helper, 0},
    Source = [{attribute, 1, module, source}, foo_form(original), helper_form(original)],
    Snapshot = #{source_view => Source, closure_ids => [FooId], closure_fas => [{foo, 0}]},
    Context = #{compile_opts => [], formatter_info => #{related_fas => [], exports => []}},
    Compiler0 = astranaut_macro_local_compile:new(Module),
    {just, Compiler1} = astranaut_return:run(
        astranaut_macro_local_compile:ensure(
            [{foo, 0}], [Snapshot], #{FooId => foo_form(compiled)}, Context, Compiler0
        )
    ),
    ?assertEqual(compiled, Module:foo()),
    ?assertEqual(#{{foo, 0} => 1}, astranaut_macro_local_compile:versions([{foo, 0}], Compiler1)),
    %% A committed boundary must not inspect inputs or attempt another load.
    ?assertEqual(
        {just, Compiler1},
        astranaut_return:run(
            astranaut_macro_local_compile:ensure([{foo, 0}], [], #{}, #{}, Compiler1)
        )
    ),
    Invalid = {function, 1, helper, 0, [{clause, 1, [], [], [{var, 1, 'Unbound'}]}]},
    NextSnapshot = Snapshot#{
        closure_ids => [FooId, HelperId], closure_fas => [{foo, 0}, {helper, 0}]
    },
    Failed = astranaut_macro_local_compile:ensure(
        [{foo, 0}, {helper, 0}],
        [NextSnapshot],
        #{FooId => foo_form(changed), HelperId => Invalid},
        Context,
        Compiler1
    ),
    ?assert(astranaut_return:has_error(Failed)),
    ?assertEqual(compiled, Module:foo()),
    ?assertNot(astranaut_macro_local_compile:is_callable({helper, 0}, Compiler1)),
    {just, Compiler2} = astranaut_return:run(
        astranaut_macro_local_compile:ensure(
            [{foo, 0}, {helper, 0}],
            [NextSnapshot],
            #{FooId => foo_form(changed), HelperId => helper_form(ready)},
            Context,
            Compiler1
        )
    ),
    ?assertEqual(changed, Module:foo()),
    ?assertEqual(ready, Module:helper()),
    ?assertEqual(
        #{{foo, 0} => 2, {helper, 0} => 2},
        astranaut_macro_local_compile:versions([{foo, 0}, {helper, 0}], Compiler2)
    ).

frozen_splice_is_rejected_test() ->
    [Foo, Helper, Spec] = forms(),
    {ok, State} = register(
        [{foo, 0}],
        #{},
        [Foo, Helper, Spec],
        #{},
        astranaut_macro_local:new()
    ),
    ?assertEqual(
        {error, {illegal_locked_form_mutation, Helper}},
        astranaut_macro_local:reject_locked_mutation([Helper], State)
    ),
    ?assertEqual(
        ok,
        astranaut_macro_local:reject_locked_mutation(
            [helper_form(other)], astranaut_macro_local:new()
        )
    ),
    ok.

closure_roots_and_self_recursion_test() ->
    Source = [recursive_form(), helper_form(ok)],
    {ok, State} = register(
        [{recursive, 0}],
        #{closure_roots => [{helper, 0}]},
        Source,
        #{},
        astranaut_macro_local:new()
    ),
    #{{recursive, 0} := Entry} = astranaut_macro_local:local_macros(State),
    ?assert(lists:member({function, helper, 0}, maps:get(closure_ids, Entry))),
    ?assertEqual(
        {error, {invalid_closure_roots, [{missing, 0}]}},
        register(
            [{recursive, 0}],
            #{closure_roots => [{missing, 0}]},
            Source,
            #{},
            astranaut_macro_local:new()
        )
    ),
    ok.

minimal_cumulative_compile_boundaries_test() ->
    Source = [a_form(), b_form_calls_a()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [P1, P2]} = astranaut_macro_local:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}], maps:get(members, P1)),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, P2)),
    [RequestA] = maps:get(requests, P1),
    ?assert(lists:member({function, a, 0}, maps:get(closure_ids, RequestA))),
    {ok, [FinalDependency, Final]} = astranaut_macro_local:finalize_plan(S2),
    ?assertEqual([{a, 0}], maps:get(members, FinalDependency)),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Final)),
    ok.

independent_macros_share_one_boundary_test() ->
    Source = [a_form(), b_form_independent()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [Plan]} = astranaut_macro_local:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    ok.

attribute_between_independent_macros_shares_one_boundary_test() ->
    Source = [a_form(), {attribute, 1, attr_a, a}, b_form_independent()],
    {ok, S1} = register(
        [{a, 0}],
        #{},
        Source,
        #{},
        astranaut_macro_local:new()
    ),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    {ok, [Plan]} = astranaut_macro_local:compile_plan({b, 0}, S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Plan)),
    ok.

shared_declaration_stays_in_one_boundary_test() ->
    Source = [a_form(), b_form_independent(), c_form_calls_a()],
    {ok, S1} = register(
        [{a, 0}, {b, 0}], #{}, Source, #{}, astranaut_macro_local:new()
    ),
    {ok, S2} = register([{c, 0}], #{}, Source, #{}, S1),
    {ok, [Dependency, Final]} = astranaut_macro_local:finalize_plan(S2),
    ?assertEqual([{a, 0}, {b, 0}], maps:get(members, Dependency)),
    ?assertEqual([{a, 0}, {b, 0}, {c, 0}], maps:get(members, Final)),
    ok.

non_frozen_retain_root_has_no_effect_test() ->
    [Foo, Helper, Spec] = forms(),
    {ok, S0} = register([{foo, 0}], #{}, [Foo, Helper, Spec], #{}, astranaut_macro_local:new()),
    ?assertEqual(
        [{ordinary, 0}],
        astranaut_macro_local:nonclosure_retain_roots(
            [{foo, 0}, {ordinary, 0}], S0
        )
    ),
    S1 = compile_forms([{foo, 0}], #{{function, foo, 0} => foo_form(foo)}, S0),
    {_Env, Skip, _S2} = astranaut_macro_local:finalize([{ordinary, 0}], S1),
    ?assertEqual([{function, foo, 0}], Skip),
    ok.

formatter_protocol_none_test() ->
    Definition = formatter_definition(
        local_macro_formatter_none,
        [macro_member_form()]
    ),
    ?assertEqual(
        astranaut_macro,
        maps:get(formatter, Definition)
    ),
    ok.

formatter_protocol_v1_only_test() ->
    LocalModule = local_macro_formatter_v1_only,
    Definition = formatter_definition(
        LocalModule,
        [macro_member_form(), formatter_v1_form()]
    ),
    ?assertEqual(
        LocalModule,
        maps:get(formatter, Definition)
    ),
    ok.

formatter_protocol_v1_is_present_test() ->
    LocalModule = local_macro_formatter_v1,
    Definition = formatter_definition(
        LocalModule,
        [macro_member_form(), formatter_v1_form()]
    ),
    ?assertEqual(
        LocalModule,
        maps:get(formatter, Definition)
    ),
    ok.

formatter_definition(LocalModule, Source) ->
    Context = #{
        module => formatter_definition_context,
        source_view => Source,
        compile_opts => [],
        macro_environment => macro_environment(#{}),
        global_macro_opts => #{formatter => astranaut_macro}
    },
    {just, {keep, Definitions, _State}} =
        astranaut_return:run(
            astranaut_macro_local:handle_form(
                {attribute, 1, local_macro, [{macro_member, 0}]},
                Context,
                astranaut_macro_local:new(LocalModule)
            )
        ),
    maps:get({macro_member, 0}, Definitions).

macro_member_form() ->
    {function, 1, macro_member, 0, [{clause, 1, [], [], [{atom, 1, macro_member_result}]}]}.

formatter_v1_form() ->
    {function, 1, format_error, 1, [
        {clause, 1, [{atom, 1, error}], [], [{call, 1, {atom, 1, formatter_private_helper}, []}]}
    ]}.

formatter_private_helper_form() ->
    {function, 1, formatter_private_helper, 0, [
        {clause, 1, [], [], [{string, 1, "formatter helper"}]}
    ]}.

register(FAs, Options, Source, MacroMap, State) ->
    CandidateMap =
        maps:from_list(
            [
                {{Name, Arity}, #{
                    macro_source => local_macro,
                    function => Name,
                    arity => Arity
                }}
             || {Name, Arity} <- maps:keys(astranaut_macro_local:local_macros(State))
            ]
        ),
    EffectiveMacroMap = maps:merge(MacroMap, CandidateMap),
    astranaut_macro_local:register(
        FAs,
        Options,
        Source,
        macro_environment(EffectiveMacroMap),
        State
    ).

macro_environment(MacroMap) ->
    #{
        macro_map => MacroMap,
        macro_options => #{}
    }.

forms() ->
    [foo_form(), helper_form(ok), {attribute, 1, spec, {{helper, 0}, []}}].

foo_form() ->
    {function, 1, foo, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}.

foo_form(Value) ->
    {function, 1, foo, 0, [{clause, 1, [], [], [{atom, 1, Value}]}]}.

helper_form(Value) ->
    {function, 1, helper, 0, [{clause, 1, [], [], [{atom, 1, Value}]}]}.

recursive_form() ->
    {function, 1, recursive, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, recursive}, []}]}]}.
a_form() -> {function, 1, a, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
macro_a_form() ->
    {function, 1, a, 0, [{clause, 1, [], [], [erl_parse:abstract({atom, 1, ok})]}]}.
b_form_calls_a() -> {function, 1, b, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, a}, []}]}]}.
b_form_independent() -> {function, 1, b, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
c_form_calls_a() -> {function, 1, c, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, a}, []}]}]}.
c_form_calls_b() -> {function, 1, c, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, b}, []}]}]}.
a_calls_b() -> {function, 1, a, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, b}, []}]}]}.
unused_form() -> {function, 1, unused, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.

compile_forms(Members, Forms, State) ->
    Compiler = maps:get(compiler, State),
    Snapshot = #{
        source_view => [{attribute, 1, module, source} | maps:values(Forms)],
        closure_ids => ordsets:from_list(maps:keys(Forms)),
        closure_fas => ordsets:from_list([
            {Name, Arity}
         || {function, Name, Arity} <- maps:keys(Forms)
        ])
    },
    {just, Compiler1} = astranaut_return:run(
        astranaut_macro_local_compile:ensure(
            Members,
            [Snapshot],
            Forms,
            #{compile_opts => [], formatter_info => maps:get(formatter_info, State)},
            Compiler
        )
    ),
    State#{compiler => Compiler1}.

first_binding_and_lazy_registration_test() ->
    Source = [{attribute, 1, module, first_binding}, a_calls_b(), b_form_independent()],
    Early = #{{external, 0} => #{attributes => #{seen => [early]}}},
    {ok, S1} = register([{a, 0}], #{}, Source, Early, astranaut_macro_local:new()),
    {ok, S2} = register(
        [{b, 0}],
        #{},
        Source,
        #{{external, 0} => #{attributes => #{seen => [late]}}},
        S1
    ),
    ?assertEqual(maps:get(frozen_forms, S1), maps:get(frozen_forms, S2)),
    ?assertEqual(#{}, maps:get(canonical_forms, S2)),
    ?assertEqual(0, generation(S2)),
    {just, S3} = astranaut_return:run(astranaut_macro_local:need_callable({b, 0}, context(), S2)),
    ?assertEqual(1, generation(S3)),
    ?assertEqual(b_form_independent(), maps:get({function, b, 0}, maps:get(canonical_forms, S3))).

independent_declarations_compile_once_test() ->
    Source = [{attribute, 1, module, independent}, a_form(), b_form_independent()],
    {ok, S1} = register([{a, 0}], #{}, Source, #{}, astranaut_macro_local:new()),
    {ok, S2} = register([{b, 0}], #{}, Source, #{}, S1),
    ?assertEqual(0, generation(S2)),
    ?assertEqual(#{}, maps:get(canonical_forms, S2)),
    {just, {_, _, S3}} = astranaut_return:run(astranaut_macro_local:finalize([], context(), S2)),
    ?assertEqual(1, generation(S3)),
    {just, S4} = astranaut_return:run(astranaut_macro_local:need_callable({a, 0}, context(), S3)),
    {just, {_, _, S5}} = astranaut_return:run(astranaut_macro_local:finalize([], context(), S4)),
    ?assertEqual(1, generation(S5)).

prefix_member_dependencies_are_prepared_test() ->
    Module = astranaut_macro_local_compile:module_name(prefix_dependencies),
    Source = [
        {attribute, 1, module, prefix_dependencies},
        macro_a_form(),
        b_form_calls_a(),
        {function, 1, c, 0, [{clause, 1, [], [], [{atom, 1, independent}]}]}
    ],
    {ok, S1} = astranaut_macro_local:register(
        [{a, 0}],
        #{},
        Source,
        macro_environment(#{}),
        astranaut_macro_local:new(Module)
    ),
    AMap = #{{a, 0} => (test_macro(Module, a))#{macro_source => local_macro}},
    {ok, S2} = astranaut_macro_local:register([{b, 0}], #{}, Source, macro_environment(AMap), S1),
    {ok, S3} = astranaut_macro_local:register([{c, 0}], #{}, Source, macro_environment(AMap), S2),
    ?assertEqual(0, generation(S3)),
    {just, S4} = astranaut_return:run(astranaut_macro_local:need_callable({c, 0}, context(), S3)),
    ?assertEqual(2, generation(S4)),
    ?assertEqual(ok, Module:b()),
    ?assertEqual(independent, Module:c()).

macro_call_does_not_retain_runtime_closure_test() ->
    Module = astranaut_macro_local_compile:module_name(macro_only_dependency),
    Source = [{attribute, 1, module, macro_only_dependency}, macro_a_form(), b_form_calls_a()],
    {ok, S1} = astranaut_macro_local:register(
        [{a, 0}], #{}, Source, macro_environment(#{}), astranaut_macro_local:new(Module)
    ),
    Map = #{{a, 0} => (test_macro(Module, a))#{macro_source => local_macro}},
    {ok, S2} = astranaut_macro_local:register([{b, 0}], #{}, Source, macro_environment(Map), S1),
    {just, {_, SkipIds, S3}} = astranaut_return:run(
        astranaut_macro_local:finalize([{b, 0}], context(), S2)
    ),
    ?assertEqual(ok, Module:b()),
    ?assertEqual([{function, b, 0}], astranaut_macro_local:retained_form_ids(S3)),
    ?assertEqual([{function, a, 0}], SkipIds).

external_replacement_executes_once_test() ->
    erlang:erase(replacement_count),
    Module = astranaut_macro_local_compile:module_name(dynamic_prefix),
    Source = [
        {attribute, 1, module, dynamic_prefix},
        macro_a_form(),
        {function, 1, b, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, replacement}, []}]}]}
    ],
    {ok, S1} = astranaut_macro_local:register(
        [{a, 0}],
        #{},
        Source,
        macro_environment(#{}),
        astranaut_macro_local:new(Module)
    ),
    Map = #{
        {a, 0} => (test_macro(Module, a))#{macro_source => local_macro},
        {replacement, 0} => test_macro(?MODULE, replacement)
    },
    {ok, S2} = astranaut_macro_local:register([{b, 0}], #{}, Source, macro_environment(Map), S1),
    ?assertEqual(undefined, erlang:get(replacement_count)),
    {just, S3} = astranaut_return:run(astranaut_macro_local:need_callable({b, 0}, context(), S2)),
    ?assertEqual(2, generation(S3)),
    ?assertEqual(1, erlang:get(replacement_count)),
    ?assertEqual(ok, Module:b()),
    {just, _} = astranaut_return:run(astranaut_macro_local:finalize([], context(), S3)),
    ?assertEqual(1, erlang:get(replacement_count)).

shared_helper_retains_first_canonical_result_test() ->
    erlang:erase(canonical_count),
    Helper = {function, 1, helper, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, bound}, []}]}]},
    Source = [
        {attribute, 1, module, shared_canonical},
        {attribute, 1, export, [{foo, 0}]},
        foo_form(),
        Helper,
        {function, 1, second, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, helper}, []}]}]}
    ],
    Map = #{{bound, 0} => test_macro(?MODULE, bound)},
    {ok, S1} = astranaut_macro_local:register(
        [{foo, 0}],
        #{},
        Source,
        macro_environment(Map),
        astranaut_macro_local:new()
    ),
    LaterMap = #{{bound, 0} => test_macro(?MODULE, must_not_run)},
    {ok, S2} = astranaut_macro_local:register(
        [{second, 0}], #{}, Source, macro_environment(LaterMap), S1
    ),
    {just, {FinalForms, SkipIds, S3, _}} = astranaut_return:run(
        astranaut_macro_local:finish_attribute_pass(Source, [], S2)
    ),
    ?assertEqual(1, erlang:get(canonical_count)),
    ?assertEqual(1, generation(S3)),
    ?assert(lists:member(helper_form(first), FinalForms)),
    ?assert(lists:member({function, helper, 0}, SkipIds)),
    ?assert(lists:member({function, foo, 0}, SkipIds)),
    {just, _} = astranaut_return:run(astranaut_macro_local:finish_attribute_pass(Source, [], S3)),
    ?assertEqual(1, erlang:get(canonical_count)).

failed_expansion_does_not_commit_canonical_or_generation_test() ->
    Source = [
        {attribute, 1, module, failed_canonical},
        {function, 1, b, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, bad}, []}]}]}
    ],
    Map = #{{bad, 0} => test_macro(?MODULE, must_not_run)},
    {ok, State} = astranaut_macro_local:register(
        [{b, 0}],
        #{},
        Source,
        macro_environment(Map),
        astranaut_macro_local:new()
    ),
    Return = astranaut_macro_local:need_callable({b, 0}, context(), State),
    ?assert(astranaut_return:has_error(Return)),
    ?assertEqual(#{}, maps:get(canonical_forms, State)),
    ?assertEqual(0, generation(State)).

context() -> #{source_view => [], compile_opts => []}.
generation(State) -> maps:get(generation, maps:get(compiler, State)).
test_macro(Module, Function) ->
    #{
        module => Module,
        function => Function,
        macro => {Module, Function},
        arity => 0,
        call_arity => 0,
        max_depth => 100,
        order => inner,
        formatter => astranaut_macro
    }.
replacement() ->
    count(replacement_count),
    {call, 1, {atom, 1, a}, []}.
bound() ->
    count(canonical_count),
    {atom, 1, first}.
must_not_run() -> error(unexpected_reexpansion).
count(Key) ->
    N =
        case erlang:get(Key) of
            undefined -> 0;
            Value -> Value
        end,
    erlang:put(Key, N + 1).
