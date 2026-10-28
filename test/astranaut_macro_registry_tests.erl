-module(astranaut_macro_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-export([format_error/1]).
-exported_macro({[{sample, 1}], #{}}).

format_error(Reason) -> io_lib:format("~p", [Reason]).

import_use_history_test() ->
    S0 = astranaut_macro_registry:new(consumer, "consumer.erl", #{}),
    S1 = directive(import_macro, ?MODULE, S0),
    ?assertEqual(S1, directive(import_macro, ?MODULE, S1)),
    S2 = directive(use_macro, {?MODULE, {sample, 1}, [{alias, first}]}, S1),
    ?assertEqual(S2, directive(use_macro, {?MODULE, {sample, 1}, [{alias, first}]}, S2)),
    S3 = directive(use_macro, {?MODULE, {sample, 1}, [{alias, second}, debug]}, S2),
    Macros = astranaut_macro_registry:effective_macro_map(S3),
    ?assertEqual(false, maps:get(debug, maps:get({first, 1}, Macros), false)),
    ?assertEqual(true, maps:get(debug, maps:get({second, 1}, Macros))),
    ?assert(maps:is_key({{?MODULE, sample}, 1}, Macros)),
    ?assertEqual(S3, directive(import_macro, ?MODULE, S3)),
    ?assertEqual(
        nothing,
        astranaut_return:run(
            astranaut_macro_registry:apply_directive(
                {attribute, 1, use_macro, {?MODULE, {sample, 1}, [{alias, first}, debug]}}, S3
            )
        )
    ),
    S4 = directive(use_macro, {?MODULE, {sample, 1}, [{alias, first}, debug, force_override]}, S3),
    ?assertEqual(
        true,
        maps:get(debug, maps:get({first, 1}, astranaut_macro_registry:effective_macro_map(S4)))
    ).

cross_module_force_override_test() ->
    Provider = a_registry_test_provider,
    {ok, Provider, Binary} = compile:forms([
        {attribute, 1, module, Provider},
        {attribute, 1, exported_macro, {[{sample, 1}], #{}}}
    ]),
    {module, Provider} = code:load_binary(Provider, "registry_test.erl", Binary),
    try
        lists:foreach(
            fun({First, Second}) ->
                S0 = astranaut_macro_registry:new(consumer, "consumer.erl", #{}),
                S1 = directive(import_macro, First, S0),
                S2 = directive(import_macro, Second, S1),
                S3 = directive(use_macro, {First, {sample, 1}, [{alias, shared}]}, S2),
                S4 = directive(
                    use_macro,
                    {Second, {sample, 1}, [{alias, shared}, force_override]},
                    S3
                ),
                Macro = maps:get({shared, 1}, astranaut_macro_registry:effective_macro_map(S4)),
                ?assertEqual(Second, maps:get(macro_module, Macro)),
                S5 = directive(
                    use_macro,
                    {Second, {sample, 1}, [{alias, shared}, force_override, debug]},
                    S4
                ),
                Updated = maps:get({shared, 1}, astranaut_macro_registry:effective_macro_map(S5)),
                ?assertEqual(Second, maps:get(macro_module, Updated)),
                ?assertEqual(true, maps:get(debug, Updated))
            end,
            [{?MODULE, Provider}, {Provider, ?MODULE}]
        )
    after
        code:purge(Provider),
        code:delete(Provider)
    end.

local_override_then_use_test() ->
    S0 = directive(
        import_macro,
        ?MODULE,
        astranaut_macro_registry:new(consumer, "consumer.erl", #{})
    ),
    S1 = directive(use_macro, {?MODULE, {sample, 1}, [{alias, shared}]}, S0),
    Local = #{
        macro => shared,
        macro_module => consumer,
        function => shared,
        arity => 1,
        force_override => true
    },
    {just, S2} = astranaut_return:run(
        astranaut_macro_registry:add_macro_definitions(#{local => Local}, S1)
    ),
    %% Re-applying the imported environment must still detect the local conflict.
    ?assertEqual(
        nothing,
        astranaut_return:run(
            astranaut_macro_registry:apply_directive(
                {attribute, 1, use_macro, {?MODULE, {sample, 1}, [{alias, shared}]}}, S2
            )
        )
    ).

directive(Name, Value, State) ->
    {just, {consume, Next}} = astranaut_return:run(
        astranaut_macro_registry:apply_directive({attribute, 1, Name, Value}, State)
    ),
    Next.
