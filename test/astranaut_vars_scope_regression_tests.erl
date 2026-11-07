-module(astranaut_vars_scope_regression_tests).
-include_lib("eunit/include/eunit.hrl").

try_section_allocation_test() ->
    ?assertEqual(
        [0],
        run_rebinding(
            "f(_) -> X = 0, try ok of ok -> X = 1 after X = 2 end, X.", [ok]
        )
    ),
    R = astranaut_vars:analyze(
        exprs(
            "try ok of ok -> B = 1 catch _:_ -> B = 2 after B end."
        )
    ),
    ?assertEqual(['B'], maps:get(free, R)),
    ?assertEqual([], maps:get(exported, R)).

receive_timeout_allocation_test() ->
    self() ! a,
    ?assertEqual(
        [1],
        run_rebinding(
            "f(_) -> X = 0, receive a -> X = 1 after begin X = 2, 0 end -> ok end.", [ok]
        )
    ),
    ?assertEqual(
        [2],
        run_rebinding(
            "f(_) -> X = 0, receive a -> ok after begin X = 2, 0 end -> ok end, X.", [ok]
        )
    ).

receive_timeout_visibility_test() ->
    ?assertEqual(
        ['T'],
        maps:get(
            free,
            astranaut_vars:analyze(
                exprs("receive a -> ok after (T = 0) -> T end.")
            )
        )
    ),
    R = astranaut_vars:analyze(exprs("receive a -> T after (T = 0) -> T end.")),
    ?assertEqual(['T'], maps:get(free, R)),
    ?assertEqual(['T'], maps:get(exported, R)),
    ?assertEqual(
        [0],
        run_rebinding(
            "f(_) -> receive a -> ok after (T = 0) -> ok end, T.", [ok]
        )
    ).

branch_final_binding_test() ->
    ?assertEqual(
        [2, 10],
        run_rebinding(
            "f(Tag) -> case Tag of 1 -> B = 1, B = B + 1; 2 -> B = 10 end, B.", [1, 2]
        )
    ),
    ?assertEqual(
        [2, 10, 100],
        run_rebinding(
            "f(Tag) -> case Tag of 1 -> B = 1, B = B + 1; "
            "2 -> B = 10; 3 -> B = 0, B = B + 10, B = B * 10 end, B.",
            [1, 2, 3]
        )
    ),
    ?assertEqual(
        [2, 10],
        run_rebinding(
            "f(Tag) -> if Tag =:= 1 -> B = 1, B = B + 1; true -> B = 10 end, B.", [1, 2]
        )
    ).

branch_binding_identity_test() ->
    ?assertEqual(
        [{1, 2}, {11, 10}],
        run_rebinding(
            "f(Tag) -> case Tag of "
            "1 -> B = 1, F = fun() -> B end, B = B + 1; "
            "2 -> B = 10, F = fun() -> B + 1 end end, {F(), B}.",
            [1, 2]
        )
    ).

nested_branch_binding_test() ->
    ?assertEqual(
        [2, 3, 10],
        run_rebinding(
            "f({Outer, Inner}) -> case Outer of "
            "1 -> case Inner of 1 -> B = 1, B = B + 1; 2 -> B = 3 end; "
            "2 -> B = 10 end, B.",
            [{1, 1}, {1, 2}, {2, 1}]
        )
    ),
    self() ! a,
    ?assertEqual(
        [2, 10],
        run_rebinding(
            "f(_) -> receive a -> B = 1, B = B + 1 after 0 -> B = 10 end, B.",
            [message, timeout]
        )
    ).

branch_active_map_test() ->
    Walk = fun
        ({var, _, 'A'}, #{event := use, current_var := Current, active_map := Map}) ->
            ?assertEqual(Current, maps:get('A', Map)),
            keep;
        (Node, Scope) ->
            rebind(Node, Scope)
    end,
    {New, R} = astranaut_vars:rewrite(
        exprs("catch (A = 0), case Tag of 1 -> A = 1; 2 -> A = 2 end, A."),
        #{transform => Walk}
    ),
    {var, _, Final} = lists:last(New),
    ?assertEqual(Final, maps:get('A', maps:get(rename_map, R))).

branch_metrics_projection_test() ->
    N = exprs("case Tag of 1 -> B = 1, B = B + 1; 2 -> B = 10 end, B."),
    Opts = #{transform => fun rebind/2},
    {New, All} = astranaut_vars:rewrite(N, Opts),
    lists:foreach(
        fun(Keys) ->
            ?assertEqual(
                {New, maps:with(Keys, All)},
                astranaut_vars:rewrite(N, Opts#{collect => Keys})
            )
        end,
        [[], [all], [counts, rename_history], [bindings, exported, rename_map]]
    ),
    {var, _, Final} = lists:last(New),
    ?assertEqual([Final], maps:get(exported, All)),
    ?assertEqual(Final, maps:get('B', maps:get(rename_map, All))),
    ?assertEqual(['B', Final, Final], maps:get('B', maps:get(rename_history, All))).

join_preserves_available_names_test() ->
    {New, _} = astranaut_vars:rewrite(
        exprs("case Tag of 1 -> B = 1, B = B + 1; 2 -> B = 10 end, B."),
        #{transform => fun rebind/2}
    ),
    ?assertMatch({var, _, 'B_1'}, lists:last(New)),
    ?assertEqual(
        [2, 11],
        run_rebinding(
            "f(Tag) -> case Tag of "
            "1 -> B = 1, B_1 = 100, B = B + 1; "
            "2 -> B = 10, B_2 = 200, B = B + 1 end, B.",
            [1, 2]
        )
    ).

receive_export_precedence_test() ->
    self() ! a,
    ?assertEqual(
        [1, 3],
        run_rebinding(
            "f(_) -> receive a -> X = 1 after begin X = 2, 0 end -> X = 3 end, X.",
            [message, timeout]
        )
    ),
    self() ! a,
    ?assertEqual(
        [2, 2],
        run_rebinding(
            "f(_) -> X = 0, receive a -> X = 1 after begin X = 2, 0 end -> X = 3 end, X.",
            [message, timeout]
        )
    ).

argument_binding_constraints_test() ->
    #{bindings := Sites} = astranaut_vars:analyze(exprs("{A = 1, A = 2}, A."), [bindings]),
    ?assert(lists:all(fun(#{count := Count}) -> Count > 0 end, maps:values(Sites))),
    lists:foreach(
        fun(Source) ->
            ?assertError(
                {badmatch, 2},
                run_function(Source, [ok], fun([Module, Exports, Form]) ->
                    [Module, Exports, astranaut_lib:silence_unused_vars(Form)]
                end)
            )
        end,
        ["f(_) -> {A = 1, A = 2}, A.", "f(_) -> {A = 1, A = 2}."]
    ).

annotation_validity_test() ->
    N = exprs("case Tag of 1 -> B = 1, B = B + 1; 2 -> B = 10 end, B."),
    Uniplate = fun(Node) ->
        Pos = erl_syntax:get_pos(Node),
        ?assert(erl_anno:is_anno(Pos)),
        _ = erl_anno:set_generated(true, Pos),
        {astranaut_syntax:subtrees(Node), fun(Subtrees) ->
            astranaut_syntax:update_tree(Node, Subtrees)
        end}
    end,
    {New, _} = astranaut_vars:rewrite(N, #{
        transform => fun rebind/2,
        traverse_opts => #{validate => output, uniplate => Uniplate}
    }),
    ?assert(
        lists:all(
            fun(P) -> P =:= 1 end,
            astranaut:sreduce(fun(Node, Ps) -> [erl_syntax:get_pos(Node) | Ps] end, [], New, #{})
        )
    ).

unused_join_does_not_reserve_name_test() ->
    Walk = fun
        ({atom, _, after_branch}, #{active_map := Map, allocated := Allocated}) ->
            ?assertNot(maps:is_key('B', Map)),
            ?assertEqual(['B', 'B_1'], Allocated),
            keep;
        (Node, Scope) ->
            rebind(Node, Scope)
    end,
    astranaut_vars:rewrite(
        exprs("case Tag of 1 -> B = 1, B = B + 1; 2 -> B = 10 end, after_branch."),
        #{transform => Walk}
    ).

argument_rebinding_names_remain_independent_test() ->
    ?assertEqual(
        [{{1, 2}, 2}],
        run_function(
            "f(_) -> V = {A = 1, A = 2}, {V, A}.", [ok], fun([Module, Exports, Form]) ->
                {New, _} = astranaut_vars:rewrite(
                    Form,
                    #{transform => fun rebind/2, argument_scope => isolated}
                ),
                [Module, Exports, New]
            end
        )
    ).

annotation_preservation_and_plain_walk_test() ->
    Plain = exprs("A = 1, A."),
    Uniplate = fun(Node) ->
        ?assertEqual(undefined, erl_anno:text(erl_syntax:get_pos(Node))),
        {astranaut_syntax:subtrees(Node), fun(Subtrees) ->
            astranaut_syntax:update_tree(Node, Subtrees)
        end}
    end,
    ?assertMatch(
        {Plain, _},
        astranaut_vars:rewrite(
            Plain,
            #{traverse_opts => #{uniplate => Uniplate}}
        )
    ),
    Pos = erl_anno:set_text(
        "original",
        erl_anno:set_file(
            "scope.erl",
            erl_anno:set_generated(true, erl_anno:new({4, 2}))
        )
    ),
    N = astranaut:smap(
        fun
            ({var, _, Name}) -> {var, Pos, Name};
            (Node) -> Node
        end,
        exprs("case Tag of 1 -> B = 1, B = B + 1; 2 -> B = 10 end, B."),
        #{}
    ),
    {New, _} = astranaut_vars:rewrite(N, #{transform => fun rebind/2}),
    ?assertEqual(
        [Pos],
        lists:usort(
            astranaut:sreduce(
                fun
                    ({var, P, _}, Ps) -> [P | Ps];
                    (_, Ps) -> Ps
                end,
                [],
                New,
                #{}
            )
        )
    ).

exprs(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    {ok, Nodes} = erl_parse:parse_exprs(Tokens),
    Nodes.

rebind({var, _, '_'}, _) ->
    keep;
rebind({var, _, Name}, #{role := pattern, same_pattern := false, allocated := Allocated}) ->
    {bind, astranaut_vars:fresh_var(Name, Allocated, #{sep => "_"})};
rebind(_, _) ->
    keep.

run_rebinding(Source, Args) ->
    run_function(Source, Args, fun(Forms) -> astranaut_rebinding:parse_transform(Forms, []) end).

run_function(Source, Args, Transform) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Module = astranaut_vars_scope_probe,
    Forms = [{attribute, 1, module, Module}, {attribute, 1, export, [{f, 1}]}, Form],
    New = Transform(Forms),
    {ok, Module, Binary, _} = compile:forms(New, [return_errors, return_warnings]),
    {module, Module} = code:load_binary(Module, "scope_probe", Binary),
    try
        [Module:f(Arg) || Arg <- Args]
    after
        code:delete(Module),
        code:purge(Module)
    end.
