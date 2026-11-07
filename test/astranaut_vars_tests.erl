-module(astranaut_vars_tests).
-include_lib("eunit/include/eunit.hrl").
-include("otp_vsn.hrl").

exprs(Text) ->
    {ok, Tokens, _} = erl_scan:string(Text),
    {ok, Nodes} = erl_parse:parse_exprs(Tokens),
    Nodes.

bound_vars(N) -> maps:get(bound, astranaut_vars:analyze(N, [bound])).
used_vars(N) -> maps:get(used, astranaut_vars:analyze(N, [used])).
exported_vars(N) -> maps:get(exported, astranaut_vars:analyze(N, [exported])).

custom_bind({var, _, Name}, #{event := bind} = Scope) ->
    {bind, astranaut_vars:fresh_var(Name, maps:get(allocated, Scope), #{sep => "_"})};
custom_bind({var, _, Name}, #{event := use, role := pattern, same_pattern := false} = Scope) ->
    {bind, astranaut_vars:fresh_var(Name, maps:get(allocated, Scope), #{sep => "_"})};
custom_bind(_, _) ->
    keep.

custom_opts(Opts) ->
    F = maps:get(transform, Opts, fun(_, _) -> keep end),
    Opts#{
        transform => fun(Node, Scope) ->
            case F(Node, Scope) of
                keep -> custom_bind(Node, Scope);
                Action -> Action
            end
        end
    }.

fresh_test() ->
    ?assertEqual('A', astranaut_vars:fresh_var('A', [])),
    ?assertEqual('A@2', astranaut_vars:fresh_var('A', ['A', 'A@1'])),
    ?assertEqual(
        'A_2',
        astranaut_vars:fresh_var(
            'A',
            ['A_1'],
            #{sep => "_", always_number => true}
        )
    ).

sequence_test() ->
    N = exprs("A = X + 1, B = A + Y."),
    R = astranaut_vars:analyze(N),
    ?assertEqual(['A', 'B', 'X', 'Y'], maps:get(all, R)),
    ?assertEqual(['X', 'Y'], maps:get(free, R)),
    ?assertEqual(['A', 'B'], maps:get(exported, R)),
    ?assertEqual(['A', 'B'], bound_vars(N)),
    ?assertEqual(['A', 'X', 'Y'], used_vars(N)),
    ?assertEqual(#{'A' => 1, 'B' => 0, 'X' => 1, 'Y' => 1}, maps:get(counts, R)),
    ?assertEqual(#{all => ['A', 'B', 'X', 'Y']}, astranaut_vars:analyze(N, [all])).

repeated_pattern_test() ->
    R = astranaut_vars:analyze(exprs("X = 1, {X, Y, Y} = Tuple.")),
    ?assertEqual(#{'X' => 1, 'Y' => 1, 'Tuple' => 1}, maps:get(counts, R)),
    ?assertEqual(#{'X' => ['X'], 'Y' => ['Y']}, maps:get(rename_history, R)),
    ?assertEqual(['Tuple'], maps:get(free, R)).

pattern_reads_test() ->
    R = astranaut_vars:analyze(exprs("<<Payload:Len/binary>> = Bin, #{Key := Val} = Map.")),
    ?assertEqual(['Payload', 'Val'], maps:get(bound, R)),
    ?assertEqual(['Bin', 'Key', 'Len', 'Map'], maps:get(free, R)),
    ?assertEqual(['Bin', 'Key', 'Len', 'Map'], maps:get(used, R)).

branch_test() ->
    R = astranaut_vars:analyze(exprs("case X of a -> A = 1, B = 2; b -> A = 3 end, A, B.")),
    ?assertEqual(['A'], maps:get(exported, R)),
    ?assertEqual(['B', 'X'], maps:get(free, R)).

isolation_test_() ->
    [
        ?_assertEqual([], exported_vars(exprs(S)))
     || S <-
            [
                "[X || X <- Xs].",
                "<< <<X>> || X <- Xs >>.",
                "fun(X) -> X end.",
                "fun F(X) -> F(X) end."
            ]
    ].

shadow_test() ->
    R = astranaut_vars:analyze(exprs("X = 1, F = fun(X) -> X end, X.")),
    ?assertEqual([], maps:get(free, R)),
    ?assertEqual(['F', 'X'], maps:get(exported, R)),
    ?assertEqual(['X', 'X'], maps:get('X', maps:get(rename_history, R))).

rename_test() ->
    {N, R} = astranaut_vars:rewrite(
        exprs("A = 1, {A, A} = Pair, A."),
        custom_opts(#{collect_as => pair})
    ),
    ?assertEqual(exprs("A = 1, {A_1, A_1} = Pair, A_1."), N),
    ?assertEqual([{'A', 'A'}, {'A', 'A_1'}], maps:get(bound, R)),
    ?assertEqual(#{'A' => 'A_1'}, maps:get(rename_map, R)),
    ?assertEqual(#{'A' => ['A', 'A_1']}, maps:get(rename_history, R)).

binary_rename_test() ->
    {N, _} = astranaut_vars:rewrite(
        exprs("Len = 8, Len = Len + 8, <<P:Len/integer>> = Bin, P."), custom_opts(#{})
    ),
    ?assertEqual(exprs("Len = 8, Len_1 = Len + 8, <<P:Len_1/integer>> = Bin, P."), N).

expand_test() ->
    [Expansion] = exprs("begin A = A + 2, A + B end."),
    F = fun
        ({var, _, 'X'}, _) -> {expand, Expansion};
        (_, _) -> keep
    end,
    {N, R} = astranaut_vars:rewrite(exprs("A = 1, X."), custom_opts(#{transform => F})),
    ?assertEqual(exprs("A = 1, begin A_1 = A + 2, A_1 + B end."), N),
    ?assertEqual(['B'], maps:get(free, R)),
    ?assertEqual(['A', 'A_1', 'B'], maps:get(all, R)).

self_expand_test() ->
    [Expansion] = exprs("X + 1."),
    F = fun
        ({var, _, _}, _) -> {expand, Expansion};
        (_, _) -> keep
    end,
    {N, R} = astranaut_vars:rewrite(exprs("X."), #{transform => F}),
    ?assertEqual([Expansion], N),
    ?assertEqual(#{'X' => 1}, maps:get(counts, R)).

silence_test() ->
    {ok, Tokens, _} = erl_scan:string(
        "f(Unused, Bin, Len, Map, Key, {X, X}) -> <<P:Len/binary>> = Bin, #{Key := V} = Map, {P, V}."
    ),
    {ok, Form} = erl_parse:parse_form(Tokens),
    New = astranaut_lib:silence_unused_vars(Form),
    ?assert(lists:member('_Unused', astranaut_vars:all_vars(New))),
    ?assert(lists:member('X', astranaut_vars:all_vars(New))),
    Forms = [{attribute, 1, module, vars_silence_test}, {attribute, 1, export, [{f, 6}]}, New],
    ?assertMatch(
        {ok, vars_silence_test, _, []}, compile:forms(Forms, [return_errors, return_warnings])
    ).

conditional_exports_test_() ->
    [
        ?_assertEqual(Expected, exported_vars(exprs(S)))
     || {S, Expected} <- [
            {"receive a -> A = 1 after 0 -> ok end.", []},
            {"receive a -> A = 1 after 0 -> A = 2 end.", ['A']},
            {"try A = 1 of _ -> B = 2 catch _:_ -> B = 3 end.", []},
            {"catch (A = 1).", []},
            {"X andalso (A = true).", []},
            {"X orelse (A = true).", []}
        ]
    ].

initial_mapping_test() ->
    {N, R} = astranaut_vars:rewrite(
        exprs("A = X, A."),
        #{init_allocated => ['A_1'], init_renames => #{'A' => 'A_1'}}
    ),
    ?assertEqual(exprs("A_1 = X, A_1."), N),
    ?assertEqual(['X'], maps:get(free, R)),
    ?assertEqual([], maps:get(bound, R)),
    ?assertEqual([], maps:get(exported, R)).

standard_arguments_test() ->
    R = astranaut_vars:analyze(exprs("{A = 1, B = A}, A, B.")),
    ?assertEqual(['A'], maps:get(free, R)),
    ?assertEqual(['A', 'B'], maps:get(exported, R)).

analyze_expand_test() ->
    [Expansion] = exprs("begin A = Y, A end."),
    F = fun
        ({var, _, 'X'}, _) -> Expansion;
        (_, _) -> keep
    end,
    R = astranaut_vars:analyze(exprs("X."), #{transform => F}),
    ?assertEqual(['A', 'Y'], maps:get(all, R)),
    ?assertEqual(['Y'], maps:get(free, R)),
    ?assertEqual(['A'], maps:get(exported, R)).

silence_shadow_test() ->
    {ok, Tokens, _} = erl_scan:string("f(X) -> F = fun(X) -> ok end, F(X)."),
    {ok, Form} = erl_parse:parse_form(Tokens),
    New = astranaut_lib:silence_unused_vars(Form),
    ?assertMatch({function, _, f, 1, [{clause, _, [{var, _, 'X'}], _, _}]}, New),
    Forms = [{attribute, 1, module, vars_shadow_test}, {attribute, 1, export, [{f, 1}]}, New],
    ?assertMatch(
        {ok, vars_shadow_test, _, []}, compile:forms(Forms, [return_errors, return_warnings])
    ).

silence_branch_test() ->
    {ok, Tokens, _} = erl_scan:string("f(X) -> case X of a -> Y = 1; b -> Y = 2 end, Y."),
    {ok, Form} = erl_parse:parse_form(Tokens),
    New = astranaut_lib:silence_unused_vars(Form),
    ?assertEqual(Form, New),
    Forms = [{attribute, 1, module, vars_branch_test}, {attribute, 1, export, [{f, 1}]}, New],
    ?assertMatch(
        {ok, vars_branch_test, _, []}, compile:forms(Forms, [return_errors, return_warnings])
    ).

-if(?ASTRANAUT_OTP_VSN_GE(26)).
map_comprehension_test() ->
    N =
        {mc, 1, {map_field_assoc, 1, {var, 1, 'K'}, {var, 1, 'V'}}, [
            {m_generate, 1, {map_field_exact, 1, {var, 1, 'K'}, {var, 1, 'V'}}, {var, 1, 'Map'}}
        ]},
    R = astranaut_vars:analyze(N),
    ?assertEqual(['Map'], maps:get(free, R)),
    ?assertEqual(['K', 'V'], maps:get(bound, R)),
    ?assertEqual([], maps:get(exported, R)).
-endif.

expand_match_root_test() ->
    [Expansion] = exprs("A = A + 1."),
    F = fun
        ({var, _, 'X'}, _) -> {expand, Expansion};
        (_, _) -> keep
    end,
    {N, _} = astranaut_vars:rewrite(exprs("A = 1, X, A."), custom_opts(#{transform => F})),
    ?assertEqual(exprs("A = 1, A_1 = A + 1, A_1."), N).

try_visibility_test() ->
    N = exprs("try A = 1 of _ -> A catch _:_ -> A after A end."),
    R = astranaut_vars:analyze(N),
    ?assertEqual(['A'], maps:get(free, R)),
    ?assertEqual(#{'A' => 3}, maps:get(counts, R)),
    ?assertEqual([], maps:get(exported, R)).

initial_rebind_test() ->
    {_, R} = astranaut_vars:rewrite(
        exprs("A = A + 1."),
        custom_opts(#{init_renames => #{'A' => 'A'}, init_allocated => ['A']})
    ),
    ?assertEqual(['A_1'], maps:get(exported, R)).

branch_rebind_does_not_escape_test() ->
    {N, _} = astranaut_vars:rewrite(
        exprs("A = 1, case Tag of a -> A = A + 1; b -> A = A + 2 end, A."), custom_opts(#{})
    ),
    ?assertEqual(exprs("A = 1, case Tag of a -> A_1 = A + 1; b -> A_1 = A + 2 end, A."), N).

silence_one_branch_test() ->
    N = exprs("case X of a -> Y = 1, Y; b -> Y = 2, ok end."),
    ?assertEqual(
        exprs("case X of a -> Y = 1, Y; b -> _Y = 2, ok end."),
        astranaut_lib:silence_unused_vars(N)
    ).

fresh_inputs_test() ->
    Ast = exprs("{Class, Exception, StackTrace, Arg, Arg@1}."),
    {[{var, 4, 'Class@1'}, {var, 4, 'Exception@1'}, {var, 4, 'StackTrace@1'}], Used} =
        astranaut_vars:alloc_var_asts(['Class', "Exception", 'StackTrace'], 4, Ast),
    ?assert(ordsets:is_element('Class@1', Used)),
    ?assertEqual(
        [{var, 4, 'Arg@2'}, {var, 4, 'Arg@3'}],
        astranaut_vars:fresh_var_asts("Arg", 2, 4, Ast)
    ).

silence_compiler_branches_test() ->
    {ok, Tokens, _} = erl_scan:string("f(X) -> case X of a -> Y = 1, Y; b -> Y = 2, ok end."),
    {ok, Form} = erl_parse:parse_form(Tokens),
    New = astranaut_lib:silence_unused_vars(Form),
    Forms = [{attribute, 1, module, vars_branches}, {attribute, 1, export, [{f, 1}]}, New],
    ?assertMatch(
        {ok, vars_branches, _, []}, compile:forms(Forms, [return_errors, return_warnings])
    ).

try_after_no_export_test() ->
    {ok, Tokens, _} = erl_scan:string("f() -> try A = 1 after ok end, A."),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Forms = [{attribute, 1, module, vars_try}, {attribute, 1, export, [{f, 0}]}, Form],
    ?assertMatch({error, _, _}, compile:forms(Forms, [return_errors, return_warnings])),
    ?assertEqual([], exported_vars(exprs("try A = 1 after ok end."))).

allocation_tools_test() ->
    ?assertEqual(
        [{var, 7, 'Arg'}, {var, 7, 'Arg@1'}],
        astranaut_vars:fresh_var_asts('Arg', 2, 7, [])
    ),
    ?assertEqual([], astranaut_vars:fresh_var_asts('Arg', 0, 7, [])),
    ?assertEqual(
        {[{var, 2, 'Z'}, {var, 2, 'A'}, {var, 2, 'Z@1'}], ['A', 'Z', 'Z@1']},
        astranaut_vars:alloc_var_asts(['Z', 'A', 'Z'], 2, [])
    ).

monadic_transform_test() ->
    F = fun
        (_, #{event := bind}) ->
            astranaut_traverse:then(
                astranaut_traverse:modify(fun(N) -> N + 1 end),
                astranaut_traverse:then(
                    astranaut_traverse:warning(renamed_binding),
                    astranaut_traverse:return({rename, 'Renamed'})
                )
            );
        (_, _) ->
            keep
    end,
    M = astranaut_vars:mapfold_m(exprs("A = 1, A."), #{transform => F}),
    #{return := {{New, R}, 1}, error := Error} = astranaut_traverse:run(M, ?MODULE, #{}, 0),
    ?assertEqual(exprs("Renamed = 1, Renamed."), New),
    ?assertEqual(#{'Renamed' => 1}, maps:get(counts, R)),
    ?assertEqual([{1, ?MODULE, renamed_binding}], astranaut_error:formatted_warnings(Error)).

monadic_diagnostic_defaults_test() ->
    F = fun
        (_, #{event := bind}) -> {warning, {rename, 'B'}, binding_warning};
        (_, #{event := use}) -> {error, reference_error};
        (_, _) -> keep
    end,
    M = astranaut_vars:map_m(exprs("A = 1, A."), #{transform => F}),
    #{return := {New, caller}, error := Error} = astranaut_traverse:run(M, ?MODULE, #{}, caller),
    %% The traversal collects errors and drops the failed reference node.
    ?assertEqual(exprs("B = 1."), New),
    ?assertEqual([{1, ?MODULE, binding_warning}], astranaut_error:formatted_warnings(Error)),
    ?assertEqual([{1, ?MODULE, reference_error}], astranaut_error:formatted_errors(Error)).

monadic_walk_state_scope_test() ->
    Walk = fun(Node, Seen, Scope) ->
        case Node of
            {var, _, _} ->
                Action = custom_bind(Node, Scope),
                astranaut:walk_return(#{
                    return => Action, state => Seen + 1, warning => saw_variable
                });
            _ ->
                keep
        end
    end,
    M = astranaut_vars:mapfold_m(Walk, exprs("A = 1, A = A + 1, A."), #{}),
    #{return := {{New, R}, 4}, error := Error} = astranaut_traverse:run(M, ?MODULE, #{}, 0),
    ?assertEqual(exprs("A = 1, A_1 = A + 1, A_1."), New),
    ?assertEqual(#{'A' => 'A_1'}, maps:get(rename_map, R)),
    ?assertEqual(4, length(astranaut_error:formatted_warnings(Error))).

monadic_expand_and_scope_restore_test() ->
    [Expansion] = exprs("begin A = A + 1, A end."),
    Walk = fun(Node, Scope) ->
        Action =
            case Node of
                {var, _, 'X'} ->
                    {expand, Expansion};
                {var, _, _} ->
                    custom_bind(Node, Scope);
                _ ->
                    keep
            end,
        astranaut_traverse:then(
            astranaut_traverse:modify(fun(N) -> N + 1 end),
            astranaut_traverse:return(Action)
        )
    end,
    M = astranaut_vars:map_m(Walk, exprs("A = 1, X, A."), #{}),
    #{return := {New, Count}} = astranaut_traverse:run(M, ?MODULE, #{}, 0),
    ?assertEqual(exprs("A = 1, begin A_1 = A + 1, A_1 end, A_1."), New),
    ?assert(Count > 0).

monadic_fail_restores_caller_test() ->
    F = fun(_, _) ->
        astranaut_traverse:then(astranaut_traverse:put(changed), astranaut_traverse:fail(rejected))
    end,
    M = astranaut_traverse:catch_on_error(
        astranaut_vars:map_m(exprs("X."), #{transform => F}),
        fun() -> astranaut_traverse:get() end
    ),
    #{return := {original, original}} = astranaut_traverse:run(M, ?MODULE, #{}, original).

monadic_validation_test() ->
    Bad = fun(Node, _) ->
        case Node of
            {integer, Pos, _} -> {attribute, Pos, module, invalid};
            _ -> Node
        end
    end,
    Result = astranaut_traverse:run(
        astranaut_vars:map_m(Bad, exprs("1."), #{
            traverse_opts => #{validate => output, validate_opts => #{fail => collect}}
        }),
        ?MODULE,
        #{},
        caller
    ),
    ?assertNot(astranaut_error:is_empty_error(astranaut_return:run_error(Result))),
    InputResult = astranaut_traverse:run(
        astranaut_vars:map_m(
            [{attribute, 1, module, invalid}],
            #{
                traverse_opts => #{
                    role => expression, validate => input, validate_opts => #{fail => collect}
                }
            }
        ),
        ?MODULE,
        #{},
        caller
    ),
    ?assertNot(astranaut_error:is_empty_error(astranaut_return:run_error(InputResult))).

monadic_walk_pattern_flags_test() ->
    Walk = fun(Node, Flags, Scope) ->
        case Node of
            {var, _, 'A'} ->
                astranaut:walk_return(#{
                    return => keep,
                    state => [{maps:get(event, Scope), maps:get(same_pattern, Scope)} | Flags]
                });
            _ ->
                keep
        end
    end,
    M = astranaut_vars:map_m(Walk, exprs("{A, A} = Pair, A = Value."), #{}),
    #{return := {_, Flags}} = astranaut_traverse:run(M, ?MODULE, #{}, []),
    ?assertEqual([{use, false}, {use, true}, {bind, false}], Flags).

parent_action_test() ->
    Walk = fun
        ({op, _, '+', Var}, #{role := pattern}) ->
            {use, Var};
        ({var, _, _} = Var, Scope) ->
            custom_bind(Var, Scope);
        (_, _) ->
            keep
    end,
    M = astranaut_vars:mapfold_m(Walk, exprs("A = 1, A = 2, +A = Value, A."), #{}),
    #{return := {{New, R}, _}} = astranaut_traverse:run(M, ?MODULE, #{}, ok),
    ?assertEqual(exprs("A = 1, A_1 = 2, A_1 = Value, A_1."), New),
    ?assertEqual(2, maps:get('A_1', maps:get(counts, R))).

standard_plus_is_not_pin_test() ->
    N = exprs("+A = Value."),
    {New, _} = astranaut_vars:rewrite(N, #{}),
    ?assertEqual(N, New).

explicit_variable_actions_test() ->
    Walk = fun
        ({atom, P, binding}, _) -> {bind, {var, P, 'A'}, 'Renamed'};
        ({atom, P, reading}, _) -> {use, {var, P, 'A'}, 'Renamed'};
        (_, _) -> keep
    end,
    M = astranaut_vars:mapfold_m(Walk, exprs("binding, reading."), #{}),
    #{return := {{New, R}, _}} = astranaut_traverse:run(M, ?MODULE, #{}, ok),
    ?assertEqual(exprs("Renamed, Renamed."), New),
    ?assertEqual(#{'Renamed' => 1}, maps:get(counts, R)).

transform_parent_test() ->
    F = fun
        ({op, _, '+', Var}, #{role := pattern}) -> {use, Var};
        (_, _) -> keep
    end,
    {New, R} = astranaut_vars:rewrite(exprs("+A = Value."), #{transform => F}),
    ?assertEqual(exprs("A = Value."), New),
    ?assertEqual([], maps:get(bound, R)),
    ?assertEqual(['A', 'Value'], maps:get(free, R)).

caller_survives_lexical_scope_test() ->
    Walk = fun(Node, Seen, Scope) ->
        case Node of
            {var, _, 'X'} ->
                astranaut:walk_return(#{
                    return => keep,
                    state => [{maps:get(event, Scope), maps:get(is_bound, Scope)} | Seen]
                });
            _ ->
                keep
        end
    end,
    M = astranaut_vars:map_m(Walk, exprs("fun(X) -> X end, X."), #{}),
    #{return := {_, Seen}} = astranaut_traverse:run(M, ?MODULE, #{}, []),
    ?assertEqual([{use, false}, {use, true}, {bind, false}], Seen).

diagnostic_file_position_test() ->
    Walk = fun
        ({var, _, _}, _) -> {warning, keep, variable_seen};
        (_, _) -> keep
    end,
    Forms = [
        {attribute, 1, file, {"vars_input.erl", 1}},
        {function, 7, f, 0, [{clause, 7, [], [], [{var, {8, 3}, 'X'}]}]},
        {eof, 9}
    ],
    M = astranaut_vars:map_m(Walk, Forms, #{}),
    Return = astranaut_traverse:eval(M, ?MODULE, #{}, ok),
    ?assertMatch(
        {warning, _, [{"vars_input.erl", [{{8, 3}, _, _}]}]},
        astranaut_return:to_compiler(Return)
    ).

argument_scope_modes_test() ->
    Nodes = exprs("{A = 1, B = A}, A, B."),
    #{free := ['A']} = astranaut_vars:analyze(Nodes),
    #{free := []} = astranaut_vars:analyze(Nodes, #{argument_scope => sequential}),
    {ok, Tokens, _} = erl_scan:string("f() -> {A = 1, B = A}, A, B."),
    {ok, Form} = erl_parse:parse_form(Tokens),
    ?assertMatch(
        {error, _, _},
        compile:forms(
            [{attribute, 1, module, sibling_scope}, Form], [return_errors, return_warnings]
        )
    ).

binding_sites_test() ->
    #{bindings := Sites} = astranaut_vars:analyze(
        exprs("X = 1, fun(X) -> ok end, X."), [bindings]
    ),
    ?assertEqual(
        #{
            1 => #{orig => 'X', final => 'X', count => 1},
            2 => #{orig => 'X', final => 'X', count => 0}
        },
        Sites
    ).

selective_collection_equivalence_test() ->
    Nodes = exprs("case Tag of a -> X = 1; b -> X = 2 end, X, fun(X) -> X end."),
    lists:foreach(
        fun(As) ->
            {Expected, All} = astranaut_vars:rewrite(Nodes, #{collect_as => As}),
            lists:foreach(
                fun(Keys) ->
                    ?assertEqual(
                        {Expected, maps:with(Keys, All)},
                        astranaut_vars:rewrite(Nodes, #{collect => Keys, collect_as => As})
                    )
                end,
                [
                    [],
                    [exported],
                    [counts],
                    [bindings],
                    [bindings, exported],
                    [all, bound, used, free, rename_map, rename_history]
                ]
            )
        end,
        [orig, final, pair]
    ),
    #{bindings := Sites} = astranaut_vars:analyze(Nodes, [bindings]),
    ?assertEqual([1, 1, 1], lists:sort([maps:get(count, Site) || Site <- maps:values(Sites)])).

%% A returned variable AST is an expansion, independent of its source position.
variable_action_test() ->
    Nodes = exprs("A = 1, A."),
    Rename = fun
        ({var, _, 'A'}, _) -> 'B';
        (_, _) -> keep
    end,
    {Renamed, _} = astranaut_vars:rewrite(Nodes, #{transform => Rename}),
    ?assertEqual(exprs("B = 1, B."), Renamed),
    Expand = fun
        ({var, Pos, 'A'}, _) -> {var, Pos, 'B'};
        ({var, _, 'B'}, _) -> 'C';
        (_, _) -> keep
    end,
    {Expanded, _} = astranaut_vars:rewrite(Nodes, #{transform => Expand}),
    ?assertEqual(exprs("C = 1, C."), Expanded).

original_name_action_test() ->
    Transform = fun
        ({var, _, _}, #{event := bind}) -> 'B';
        ({var, _, Name}, _) -> Name;
        (_, _) -> keep
    end,
    {New, Analysis} = astranaut_vars:rewrite(exprs("A = 1, A."), #{transform => Transform}),
    ?assertEqual(exprs("B = 1, B."), New),
    ?assertEqual(#{'B' => 1}, maps:get(counts, Analysis)).
