-module(astranaut_review_regression_tests).
-include_lib("eunit/include/eunit.hrl").

error_constructors_test() ->
    %% Bound the old infinite recursion so a regression cannot exhaust the VM.
    {Pid, Ref} = spawn_opt(
        fun() ->
            exit({result, astranaut_error:errors(astranaut_error:new_error(oops))})
        end,
        [monitor, {max_heap_size, #{size => 100000, kill => true, error_logger => false}}]
    ),
    receive
        {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual({result, [oops]}, Reason)
    after 1000 ->
        exit(Pid, kill),
        error(new_error_timeout)
    end,
    ?assertEqual([a, b], astranaut_error:errors(astranaut_error:new_errors([a, b]))),
    ?assertEqual([oops], astranaut_error:warnings(astranaut_error:new_warning(oops))),
    ?assertEqual([a, b], astranaut_error:warnings(astranaut_error:new_warnings([a, b]))).

eof_without_file_test() ->
    Empty = astranaut_error:new(),
    ?assert(astranaut_error:is_empty(astranaut_error:eof(Empty))),
    lists:foreach(
        fun(State) ->
            Formatted = astranaut_error:update_pos(1, ?MODULE, State),
            Eof = astranaut_error:eof(Formatted),
            ?assertEqual(eof, astranaut_error:file(Eof)),
            ?assertEqual(
                astranaut_error:formatted_errors(Formatted), astranaut_error:formatted_errors(Eof)
            ),
            ?assertEqual(
                astranaut_error:formatted_warnings(Formatted),
                astranaut_error:formatted_warnings(Eof)
            )
        end,
        [astranaut_error:new_errors([oops]), astranaut_error:new_warnings([oops])]
    ).

named_fun_test() ->
    Cases = [
        {"fun Loop(0) -> 1; Loop(N) -> N * Loop(N - 1) end.", [5], 120},
        {"fun Self() -> Self end.", [], self},
        {"fun Self(N) -> AstranautArg1 = N + 1, AstranautArg1 end.", [4], 5},
        {"fun Self(N) -> (fun(Self) -> Self + 1 end)(N) end.", [4], 5},
        {"fun Self(N) -> (fun Self(0) -> inner; Self(X) -> Self(X - 1) end)(N) end.", [3], inner},
        {"fun Self(0) -> done; Self(N) -> (fun() -> Self end)()(N - 1) end.", [3], done}
    ],
    %% Compile each result to exercise real Erlang scope and call semantics.
    lists:foreach(
        fun({Source, Args, Expected}) ->
            {ok, Tokens, _} = erl_scan:string(Source),
            {ok, [Fun]} = erl_parse:parse_exprs(Tokens),
            Module = astranaut_review_named_fun,
            Function = astranaut_lib:gen_function(generated, Fun),
            Forms = [
                {attribute, 1, module, Module},
                {attribute, 1, export, [{generated, length(Args)}]},
                Function
            ],
            {ok, Module, Binary, _} = compile:forms(Forms, [return_errors, return_warnings]),
            {module, Module} = code:load_binary(Module, "review.erl", Binary),
            try
                case Expected of
                    self -> ?assert(is_function(Module:generated(), 0));
                    _ -> ?assertEqual(Expected, apply(Module, generated, Args))
                end
            after
                code:delete(Module),
                code:purge(Module)
            end
        end,
        Cases
    ).

attribute_return_contract_test() ->
    Forms = [{attribute, 1, mark, a}, {attribute, 2, mark, b}],
    F = fun(Value, Acc) -> Acc ++ [Value] end,
    lists:foreach(
        fun(Opts) ->
            ?assertEqual(
                {just, [a, b]},
                astranaut_return:run(
                    astranaut_lib:with_attribute(F, [], Forms, mark, Opts)
                )
            )
        end,
        [#{}, #{simplify_return => true}, #{simplify_return => false}]
    ),
    ?assertEqual(
        {just, initial},
        astranaut_return:run(
            astranaut_lib:with_attribute(F, initial, [], mark, #{})
        )
    ),
    Warning = astranaut_lib:with_attribute(
        fun(_, Acc) -> astranaut_return:warning_ok(notice, Acc) end,
        [],
        Forms,
        mark,
        #{formatter => astranaut_do}
    ),
    ?assertEqual({just, []}, astranaut_return:run(Warning)),
    ?assertEqual(
        [{1, astranaut_do, notice}, {2, astranaut_do, notice}],
        astranaut_error:formatted_warnings(astranaut_return:run_error(Warning))
    ).

function_state_replacement_test() ->
    Node = {tuple, 1, [{atom, 1, ok}]},
    F = fun
        ({tuple, _, _} = N, State) ->
            {
                astranaut_uniplate:with_subtrees(
                    fun(Subtrees) -> astranaut_uniplate:with(replaced, final, Subtrees) end, N
                ),
                State
            };
        (N, replaced) ->
            {N, replaced}
    end,
    ?assertEqual({Node, final}, astranaut:smapfold(F, fun() -> original end, Node, #{})).

do_invalid_input_test() ->
    Result = astranaut_do:do({atom, 1, invalid}, #{}),
    ?assertEqual({error, expected_list_comprehension}, Result),
    Bridged = astranaut_traverse:eval(astranaut:traverse_return(Result), astranaut_do, #{}, #{}),
    ?assertEqual(nothing, astranaut_return:run(Bridged)),
    ?assertEqual(
        [expected_list_comprehension], astranaut_error:errors(astranaut_return:run_error(Bridged))
    ),
    ?assert(is_binary(iolist_to_binary(astranaut_do:format_error(expected_list_comprehension)))).

macro_return_formatter_test() ->
    lists:foreach(
        fun(Detail) ->
            Message = iolist_to_binary(
                astranaut_macro:format_error({invalid_macro_return, Detail})
            ),
            ?assertNotEqual(nomatch, binary:match(Message, <<"returned invalid AST">>))
        end,
        [
            #{},
            #{macro => #{mfa => #{module => sample, function => macro, arity => 1}}},
            #{current_macro => #{mfa => #{module => sample, function => macro, arity => 1}}}
        ]
    ).

missing_macro_import_test() ->
    Module = astranaut_review_missing_provider,
    State = astranaut_macro_registry:new(consumer, "consumer.erl", #{}),
    Result = astranaut_macro_registry:apply_directive({attribute, 1, import_macro, Module}, State),
    ?assertEqual(nothing, astranaut_return:run(Result)),
    ?assertEqual(
        [{import_macro_failed, Module}], astranaut_error:errors(astranaut_return:run_error(Result))
    ).

missing_macro_import_is_silent_test() ->
    Eval =
        "S = astranaut_macro_registry:new(consumer, \"consumer.erl\", #{}), "
        "R = astranaut_macro_registry:apply_directive("
        "{attribute, 1, import_macro, astranaut_review_missing_provider}, S), "
        "nothing = astranaut_return:run(R), halt().",
    Port = open_port(
        {spawn_executable, os:find_executable("erl")},
        [
            binary,
            exit_status,
            stderr_to_stdout,
            use_stdio,
            {args, [
                "-noshell",
                "-pa",
                filename:join(code:lib_dir(astranaut), "ebin"),
                "-eval",
                Eval
            ]}
        ]
    ),
    try
        ?assertEqual({0, <<>>}, collect_output(Port, <<>>))
    after
        case erlang:port_info(Port) of
            undefined -> ok;
            _ -> port_close(Port)
        end
    end.

collect_output(Port, Output) ->
    receive
        {Port, {data, Data}} -> collect_output(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Output}
    after 3000 -> error(macro_import_timeout)
    end.

map_m_result_test() ->
    Result = astranaut_return:map_m(fun(N) -> astranaut_return:return(N + 1) end, [1, 2]),
    ?assertEqual({just, [2, 3]}, astranaut_return:run(Result)).
