-module(astranaut_do_tests).
-include_lib("eunit/include/eunit.hrl").

badmatch_variable_avoids_outer_bindings_test() ->
    Name = atom_to_list(astranaut_quote:encode_quote_variable('Var', astranaut_do)),
    Source =
        "f(" ++ Name ++
            ") -> do([identity || "
            "{ok, X} <- return({ok, 7}), return({X, " ++ Name ++ "})]).",
    with_compiled(Source, fun(Module, Warnings) ->
        ?assertEqual([], Warnings),
        ?assertEqual({7, context}, Module:f(context))
    end).

badmatch_variable_avoids_unused_outer_bindings_test() ->
    Name = atom_to_list(astranaut_quote:encode_quote_variable('Var', astranaut_do)),
    Source =
        "f(" ++ Name ++
            ") -> do([identity || "
            "{ok, X} <- return({ok, 7}), return(X)]).",
    with_compiled(Source, fun(Module, Warnings) ->
        ?assertEqual([], [
            Reason
         || {_, Messages} <- Warnings,
            {_, erl_lint, {shadowed_var, _, _} = Reason} <- Messages
        ]),
        ?assertEqual(7, Module:f(context))
    end).

badmatch_diagnostic_is_preserved_test() ->
    with_compiled(
        "f(Value) -> do([identity || {ok, X} <- return(Value), return(X)]).",
        fun(Module, Warnings) ->
            ?assertEqual([], Warnings),
            ?assertEqual(5, Module:f({ok, 5})),
            ?assertExit({monad_badmatch, unexpected, 1, "{ok, X}"}, Module:f(unexpected))
        end
    ).

nested_do_variable_allocation_test() ->
    Name = atom_to_list(astranaut_quote:encode_quote_variable('Var', astranaut_do)),
    Source =
        "f(" ++ Name ++
            ") -> do([identity || "
            "{ok, X} <- return({ok, do([identity || {ok, Y} <- return({ok, 3}), return(Y)])}), "
            "{ok, Z} <- return({ok, 4}), return({" ++ Name ++ ", X, Z})]).",
    with_compiled(Source, fun(Module, Warnings) ->
        ?assertEqual([], Warnings),
        ?assertEqual({context, 3, 4}, Module:f(context))
    end).

with_compiled(Source, Check) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    {ok, Function} = erl_parse:parse_form(Tokens),
    Module = astranaut_do_probe,
    Forms = [{attribute, 1, module, Module}, {attribute, 1, export, [{f, 1}]}, Function],
    New = astranaut_do:parse_transform(Forms, []),
    {ok, Module, Binary, Warnings} = compile:forms(New, [
        return_errors, return_warnings, warn_shadow_vars
    ]),
    {module, Module} = code:load_binary(Module, "do_probe", Binary),
    try
        Check(Module, Warnings)
    after
        code:delete(Module),
        code:purge(Module)
    end.
