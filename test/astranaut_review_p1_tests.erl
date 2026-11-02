-module(astranaut_review_p1_tests).
-include_lib("eunit/include/eunit.hrl").

parse_file_options_test() ->
    Dir = filename:join(
        temp_dir(), "astranaut_p1_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    File = filename:join(Dir, "source.erl"),
    Source = unicode:characters_to_binary(
        "%% coding: utf-8\n-module(p1_source).\nvalue() -> {?FLAG, ?VALUE, '中文'}.\n"
    ),
    try
        ok = file:write_file(File, Source),
        Opts = [{d, 'FLAG'}, {d, 'VALUE', 42}],
        Forms = astranaut_lib:parse_file(File, Opts),
        ?assertEqual(Forms, astranaut_lib:original_forms(Forms, Opts)),
        ?assertMatch(
            {tuple, _, [{atom, _, true}, {integer, _, 42}, {atom, _, '中文'}]}, body(value, Forms)
        ),
        ?assert(
            lists:member(
                {attribute, 1, file, {File, 1}},
                astranaut_lib:parse_file(File, [{error_location, line} | Opts])
            )
        ),
        Deterministic = astranaut_lib:parse_file(File, [deterministic | Opts]),
        ?assertMatch([{attribute, _, file, {"source.erl", 1}} | _], Deterministic)
    after
        file:delete(File),
        file:del_dir(Dir)
    end,
    ?assertMatch(
        {error, [{File, [{none, compile, {epp, enoent}}]}], []},
        astranaut_lib:parse_file(File, [])
    ).

temp_dir() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        Dir -> Dir
    end.

safe_ast_string_test() ->
    Invalid = {not_an_ast, 1, payload},
    Text = iolist_to_binary(astranaut_lib:ast_safe_to_string(Invalid)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"ast could not format">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"not_an_ast">>)).

annotation_replacement_test() ->
    Anno = erl_anno:set_generated(true, erl_anno:set_file("original.erl", erl_anno:new({2, 3}))),
    Tree = {tuple, Anno, [{atom, {3, 4}, value}, {integer, 0, 7}]},
    ?assertEqual(
        {tuple, {9, 2}, [{atom, {9, 2}, value}, {integer, {9, 2}, 7}]},
        astranaut_lib:replace_pos(Tree, {9, 2})
    ),
    ?assertEqual(
        {tuple, Anno, [{atom, {3, 4}, value}, {integer, 10, 7}]},
        astranaut_lib:replace_pos_zero(Tree, 10)
    ).

original_chain_collision_test() ->
    Base = [form("foo() -> [base]."), form("foo_1() -> reserved.")],
    First = astranaut_forms:insert_forms([form("foo() -> [first | '__original__'()].")], Base),
    Second = astranaut_forms:insert_forms([form("foo() -> [second | '__original__'()].")], First),
    ?assertEqual([foo, foo_1, foo_2, foo_3], lists:sort([N || {function, _, N, _, _} <- Second])),
    run_module(Second, [{foo, 0}, {foo_1, 0}], fun(M) ->
        ?assertEqual([second, first, base], M:foo()),
        ?assertEqual(reserved, M:foo_1())
    end).

sort_diagnostics_test() ->
    Module = {attribute, 1, module, sample},
    Attr = {attribute, 2, custom, value},
    F = form("foo() -> ok."),
    W = {warning, {3, ?MODULE, warning}},
    E = {error, {4, ?MODULE, error}},
    ?assertEqual(
        [Module, Attr, W, F, E, {eof, 5}],
        astranaut_forms:sort_forms([Module, Attr, W, F, E, {eof, 5}])
    ).

return_short_circuit_test() ->
    F = fun
        (first) ->
            astranaut_return:then(
                astranaut_return:warning(before_failure),
                astranaut_return:error_ok(earlier_error, first)
            );
        (fail) ->
            astranaut_return:error_fail(failed);
        (unreachable) ->
            error(callback_after_failure)
    end,
    Map = astranaut_return:map_m(F, [first, fail, unreachable]),
    Fold = astranaut_return:foldl_m(
        fun(X, Acc) ->
            astranaut_return:lift_m(fun(Y) -> [Y | Acc] end, F(X))
        end,
        [],
        [first, fail, unreachable]
    ),
    lists:foreach(
        fun(Result) ->
            ?assertEqual(nothing, astranaut_return:run(Result)),
            Errors = astranaut_return:run_error(Result),
            ?assertEqual([earlier_error, failed], astranaut_error:errors(Errors)),
            ?assertEqual([before_failure], astranaut_error:warnings(Errors))
        end,
        [Map, Fold]
    ).

sequence_and_writer_test() ->
    ?assertEqual({just, [1, 2]}, astranaut_monad:sequence_m([{just, 1}, {just, 2}], monad_maybe)),
    ?assertEqual(nothing, astranaut_monad:sequence_m([{just, 1}, nothing, invalid], monad_maybe)),
    Bind = astranaut_monad:writer_bind(fun(A, F) -> F(A) end, fun(A) -> A end, fun erlang:'++'/2),
    Return = astranaut_monad:writer_return(fun(A) -> A end, fun() -> [] end),
    Listen = astranaut_monad:writer_listen(fun(A, F) -> F(A) end, fun(A) -> A end),
    Result = astranaut_monad:sequence_m([{a, [first]}, {b, [second]}], Bind, Return),
    ?assertEqual({[a, b], [first, second]}, Result),
    ?assertEqual({{[a, b], [first, second]}, [first, second]}, Listen(Result)),
    ?assertEqual({[], []}, astranaut_monad:sequence_m([], Bind, Return)).

disable_tco_named_fun_test() ->
    F = form("loop() -> fun Loop(0) -> erlang:error(done); Loop(N) -> Loop(N - 1) end."),
    [Out] = astranaut_disable_tco:parse_transform([F], []),
    {named_fun, _, 'Loop', [Base, Recursive]} = body(loop, [Out]),
    ?assertMatch({clause, _, _, _, [{'try', _, _, _, _, _}]}, Base),
    ?assertMatch({clause, _, _, _, [{call, _, {var, _, 'Loop'}, [_]}]}, Recursive).

disable_tco_orelse_test() ->
    [Out] = astranaut_disable_tco:parse_transform(
        [form("value(X) -> X orelse erlang:error(failed).")], []
    ),
    ?assertMatch({op, _, 'orelse', {var, _, 'X'}, {'try', _, _, _, _, _}}, body(value, [Out])),
    run_module([Out], [{value, 1}], fun(M) ->
        ?assertEqual(true, M:value(true)),
        ?assertError(failed, M:value(false))
    end).

disable_tco_builtin_calls_test() ->
    %% Current implementation wraps these calls; it has no blacklist.
    Calls = [
        "erlang:error(reason)",
        "erlang:error(reason, [])",
        "erlang:exit(reason)",
        "erlang:throw(reason)",
        "erlang:raise(error, reason, [])",
        "record_info(size, sample)"
    ],
    lists:foreach(
        fun(Call) ->
            [Out] = astranaut_disable_tco:parse_transform([form("value() -> " ++ Call ++ ".")], []),
            ?assertMatch({'try', _, [{call, _, _, _}], _, _, _}, body(value, [Out]))
        end,
        Calls
    ).

rebinding_try_scope_test() ->
    Function = form(
        "value(Mode) -> K = outer, "
        "R = try X = initial, X = inner, case Mode of ok -> X; fail -> erlang:error(broken) end of "
        "Y -> Y = {success, Y}, Y catch error:E -> E = {caught, E}, E "
        "after C = initial, C = cleanup, put(p1_after, C) end, {K, R}."
    ),
    run_rebinding([Function], [{value, 1}], fun(M) ->
        try
            ?assertEqual({outer, {success, inner}}, M:value(ok)),
            ?assertEqual(cleanup, get(p1_after)),
            ?assertEqual({outer, {caught, broken}}, M:value(fail)),
            ?assertEqual(cleanup, get(p1_after))
        after
            erase(p1_after)
        end
    end).

rebinding_receive_scope_test() ->
    Function = form(
        "value() -> X = outer, R = receive "
        "{first, Y} -> X = first, Y = {first, Y}, Y; "
        "{second, Y} -> X = second, Y = {second, Y}, Y "
        "after 0 -> X = timeout, X end, {X, R}."
    ),
    run_rebinding([Function], [{value, 0}], fun(M) ->
        self() ! {first, 1},
        ?assertEqual({first, {first, 1}}, M:value()),
        self() ! {second, 2},
        ?assertEqual({second, {second, 2}}, M:value()),
        ?assertEqual({timeout, timeout}, M:value())
    end).

rebinding_named_fun_scope_test() ->
    Function = form(
        "value() -> F = fun Loop(0) -> Loop = done, Loop; "
        "Loop(N) -> Loop(N - 1) end, F(2)."
    ),
    run_rebinding([Function], [{value, 0}], fun(M) -> ?assertEqual(done, M:value()) end).

run_rebinding(Functions, Exports, Check) ->
    Forms = module_forms([{attribute, 1, rebinding_all, []} | Functions], Exports),
    Out = astranaut_rebinding:parse_transform(Forms, []),
    load_run(Out, Check).

run_module(Functions, Exports, Check) -> load_run(module_forms(Functions, Exports), Check).
module_forms(Functions, Exports) ->
    [
        {attribute, 1, file, {"p1.erl", 1}},
        {attribute, 1, module, astranaut_review_p1_fixture},
        {attribute, 1, export, Exports}
        | Functions
    ].
load_run(Forms, Check) ->
    {ok, Module, Binary, _} = compile:forms(Forms, [return_errors, return_warnings]),
    {module, Module} = code:load_binary(Module, "p1.erl", Binary),
    try
        Check(Module)
    after
        code:delete(Module),
        code:purge(Module)
    end.
form(Text) ->
    {ok, Tokens, _} = erl_scan:string(Text),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.
body(Name, Forms) ->
    [{function, _, Name, _, [{clause, _, _, _, [Body]}]}] =
        [F || {function, _, N, _, _} = F <- Forms, N =:= Name],
    Body.
