#!/usr/bin/env escript
%%! -noshell +S 1
-mode(compile).

main(Args) ->
    Samples = case Args of [] -> 15; [N] -> list_to_integer(N) end,
    true = Samples > 0,
    true = code:add_patha("_build/default/lib/astranaut/ebin"),
    {ok, Source} = file:read_file("src/astranaut_vars.erl"),
    Variants = [{eager, false, false}, {skip_exported, true, false},
        {skip_counts, false, true}, {current, true, true}],
    Modules = [{Label, build(Label, Source, E, C)} || {Label, E, C} <- Variants],
    io:format("OTP ~s ~s; schedulers=~p samples=~p warmups=3; medians, fresh process/sample~n",
        [erlang:system_info(otp_release), erlang:system_info(system_architecture),
         erlang:system_info(schedulers_online), Samples]),
    lists:foreach(fun({Shape, Nodes}) ->
        Jobs = [{Label, Collect, fun() -> Module:rewrite(Nodes, #{collect => Collect}) end}
            || {Label, Module} <- Modules, Collect <- [[], [exported], [bindings], all]],
        %% These counterfactuals must preserve every requested result, not just ASTs.
        lists:foreach(fun({_, Collect, F}) ->
            Expected = astranaut_vars:rewrite(Nodes, #{collect => Collect}),
            Expected = F()
        end, Jobs),
        lists:foreach(fun({_, _, F}) -> [sample(F) || _ <- lists:seq(1, 3)] end, Jobs),
        Results = lists:foldl(fun(Round, Acc) ->
            Order = case Round rem 2 of 0 -> lists:reverse(Jobs); _ -> Jobs end,
            lists:foldl(fun({Label, Collect, F}, A) ->
                Key = {Label, Collect},
                Measurement = sample(F),
                maps:update_with(Key, fun(V) -> [Measurement | V] end, [Measurement], A)
            end, Acc, Order)
        end, #{}, lists:seq(1, Samples)),
        io:format("~n~p (200 names, 2000 reads)~nvariant collect median_us median_reductions time_delta_pct~n", [Shape]),
        lists:foreach(fun({Label, Collect, _}) ->
            {Time, Red} = medians(maps:get({Label, Collect}, Results)),
            {Base, _} = medians(maps:get({eager, Collect}, Results)),
            io:format("~p ~p ~p ~p ~.2f~n", [Label, Collect, Time, Red, 100 * (Time / Base - 1)])
        end, Jobs)
    end, fixtures()).

fixtures() ->
    Vars = [{var, 1, list_to_atom("V" ++ integer_to_list(I))} || I <- lists:seq(1, 200)],
    Binds = [{match, 1, V, {integer, 1, 1}} || V <- Vars],
    Reads = lists:append(lists:duplicate(10, Vars)),
    Body = Binds ++ Reads,
    Branch = {'case', 1, {atom, 1, a}, [
        {clause, 1, [{atom, 1, a}], [], Binds},
        {clause, 1, [{atom, 1, b}], [], Binds}]},
    [{body, Body}, {clause, {clause, 1, [], [], Body}}, {branch_join, [Branch | Reads]}].

sample(F) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        erlang:garbage_collect(),
        {reductions, Before} = process_info(self(), reductions),
        Start = erlang:monotonic_time(microsecond),
        Value = F(),
        Stop = erlang:monotonic_time(microsecond),
        {reductions, After} = process_info(self(), reductions),
        true = is_tuple(Value),
        exit({sample, Stop - Start, After - Before})
    end),
    receive
        {'DOWN', Ref, process, Pid, {sample, Time, Red}} -> {Time, Red};
        {'DOWN', Ref, process, Pid, Reason} -> error({sample_failed, Reason})
    after 60000 -> exit(Pid, kill), error(sample_timeout)
    end.

medians(Values) ->
    {median([T || {T, _} <- Values]), median([R || {_, R} <- Values])}.
median(Values) -> lists:nth(length(Values) div 2 + 1, lists:sort(Values)).

build(Label, Source, SkipExported, SkipCounts) ->
    Module = list_to_atom("astranaut_vars_bench_" ++ atom_to_list(Label)),
    S0 = replace(Source, <<"-module(astranaut_vars).">>,
        iolist_to_binary(["-module(", atom_to_list(Module), ")."])),
    %% Re-enable the old unconditional work without maintaining a second engine.
    %% The eager reference shares the optimized driver's cached keys and gates.
    S1 = case SkipExported of
        true -> S0;
        false -> replace(S0, <<"collect_exported = lists:member(exported, Keys)">>,
            <<"collect_exported = true">>)
    end,
    S2 = case SkipCounts of
        true -> S1;
        false -> replace(S1, <<"count_bindings = lists:member(bindings, Keys)">>,
            <<"count_bindings = true">>)
    end,
    Path = filename:join(os:getenv("TMPDIR", "/tmp"), atom_to_list(Module) ++ "_" ++ os:getpid() ++ ".erl"),
    ok = file:write_file(Path, S2),
    Compiled = try compile:file(Path, [binary, return_errors, {i, "include"}])
        after file:delete(Path) end,
    {ok, Module, Beam} = Compiled,
    {module, Module} = code:load_binary(Module, Path, Beam),
    Module.

replace(Source, Old, New) ->
    %% Fail loudly if the implementation changes: never benchmark an unmodified variant.
    [_] = binary:matches(Source, Old),
    binary:replace(Source, Old, New).
