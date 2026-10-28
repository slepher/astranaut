#!/usr/bin/env escript
%%! -noshell +S 1
-mode(compile).

main(Args) ->
    N =
        case Args of
            [] -> 31;
            [Arg] -> list_to_integer(Arg)
        end,
    true = N > 0,
    true = code:add_patha("_build/default/lib/astranaut/ebin"),
    io:format(
        "OTP ~s, ~s, schedulers ~p, samples ~p, warmups 3~n",
        [
            erlang:system_info(otp_release),
            erlang:system_info(system_architecture),
            erlang:system_info(schedulers_online),
            N
        ]
    ),
    lists:foreach(
        fun(Size) ->
            Forms = [{attribute, I, value, I} || I <- lists:seq(1, Size)],
            report(
                {splice, Size},
                fun() -> splice(Forms, true) end,
                fun() -> splice(Forms, false) end,
                N
            )
        end,
        [100, 1000, 10000]
    ),
    %% Build the disabled-only counterfactual in memory; never edit the scanner.
    {ok, Original} = file:read_file("src/astranaut_macro_scan.erl"),
    Variant = binary:replace(Original, <<"queue_state => true">>, <<"queue_state => false">>),
    true = Variant =/= Original,
    Tmp = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "astranaut_queue_bench_" ++ os:getpid() ++ ".erl"
    ),
    ok = file:write_file(Tmp, Variant),
    Disabled =
        try
            compiled(compile:file(Tmp, [binary, return_errors, {i, "include"}]))
        after
            file:delete(Tmp)
        end,
    {astranaut_macro_scan, Enabled, _} = code:get_object_code(astranaut_macro_scan),
    load(compiled(compile:file("benchmark/do_macro.erl", [binary, return_errors]))),
    {ok, RealForms} = epp:parse_file("benchmark/real_benchmark.erl", [], []),
    [] = [F || F = {attribute, _, local_macro, _} <- RealForms],
    [] = [F || F = {error, _} <- RealForms],
    Transform = fun() -> astranaut_macro:parse_transform(RealForms, []) end,
    %% Loading is outside the measured interval. Samples alternate AB / BA.
    try
        report(
            real_transform,
            {fun() -> load({astranaut_macro_scan, Enabled}) end, Transform},
            {fun() -> load(Disabled) end, Transform},
            N
        )
    after
        load({astranaut_macro_scan, Enabled})
    end.

splice(Forms, Queue) ->
    {just, {Result, _}} = astranaut_return:run(
        astranaut_traverse:run(
            astranaut_macro_scan:map_forms_splice(
                fun astranaut_traverse:return/1,
                Forms,
                #{traverse => none, queue_state => Queue}
            ),
            astranaut_macro,
            #{},
            #{capability => disabled}
        )
    ),
    Result.

compiled({ok, M, B}) -> {M, B};
compiled({ok, M, B, _}) -> {M, B};
compiled(Error) -> erlang:error(Error).

load({M, B}) ->
    code:purge(M),
    {module, M} = code:load_binary(M, "queue-benchmark", B),
    ok.

report(Label, A, B, N) ->
    {_, _, Expected} = sample(A),
    {_, _, Expected} = sample(B),
    true = is_list(Expected),
    lists:foreach(
        fun(_) ->
            sample(A),
            sample(B)
        end,
        lists:seq(1, 3)
    ),
    Pairs = [
        case I rem 2 of
            0 ->
                {sample(A), sample(B)};
            1 ->
                SB = sample(B),
                SA = sample(A),
                {SA, SB}
        end
     || I <- lists:seq(1, N)
    ],
    true = lists:all(
        fun({{_, _, RA}, {_, _, RB}}) -> RA =:= Expected andalso RB =:= Expected end, Pairs
    ),
    AT = median([T || {{T, _, _}, _} <- Pairs]),
    BT = median([T || {_, {T, _, _}} <- Pairs]),
    AR = median([R || {{_, R, _}, _} <- Pairs]),
    BR = median([R || {_, {_, R, _}} <- Pairs]),
    io:format(
        "~p: snapshot ~p us / ~p reductions; bypass ~p us / ~p reductions; time delta ~.2f%~n",
        [Label, AT, AR, BT, BR, 100 * (AT - BT) / AT]
    ).

sample({Setup, Fun}) ->
    Setup(),
    sample(Fun);
sample(Fun) ->
    %% Fresh process prevents previous results/heaps affecting the next sample.
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        garbage_collect(),
        {reductions, Before} = process_info(self(), reductions),
        {Time, Result} = timer:tc(Fun),
        {reductions, After} = process_info(self(), reductions),
        Parent ! {self(), {Time, After - Before, Result}}
    end),
    receive
        {Pid, Value} ->
            receive
                {'DOWN', Ref, process, Pid, normal} -> Value
            end;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    end.

median(Values) -> lists:nth((length(Values) + 1) div 2, lists:sort(Values)).
