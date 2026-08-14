#!/usr/bin/env escript
%%! -noshell
-mode(compile).

-define(DEFAULT_ITERATIONS, 15).
-define(WARMUP_ITERATIONS, 3).

main(Args) ->
    Iterations = iterations(Args),
    Root = project_root(),
    ok = add_astranaut_path(Root),
    IncludeDir = filename:join(Root, "include"),
    Provider = filename:join([Root, "benchmark", "macro_bench_macros.erl"]),
    Workload = filename:join([Root, "benchmark", "macro_2000.erl"]),
    SourceLines = source_lines(Workload),
    CompileOpts = [
        binary,
        return_errors,
        return_warnings,
        nowarn_unused_function,
        {i, IncludeDir}
    ],
    ok = load_provider(Provider, CompileOpts),
    Forms = parse_forms(Workload, IncludeDir),
    DepthStats = validate_ast_depth(Forms),
    QuotedForms = expect_forms(
        astranaut_quote:parse_transform(Forms, CompileOpts)
    ),
    Transform = fun() ->
        expect_forms(
            astranaut_macro:parse_transform(
                QuotedForms, CompileOpts
            )
        )
    end,
    Compile = fun() ->
        expect_compile(
            compile:file(Workload, CompileOpts)
        )
    end,
    warmup(Transform),
    TransformTimes = measure(Transform, Iterations),
    warmup(Compile),
    CompileTimes = measure(Compile, Iterations),
    print_report(
        SourceLines,
        DepthStats,
        Iterations,
        TransformTimes,
        CompileTimes
    ).

iterations([]) ->
    ?DEFAULT_ITERATIONS;
iterations([Value]) ->
    case string:to_integer(Value) of
        {N, []} when N > 0 -> N;
        _ -> usage()
    end;
iterations(_) ->
    usage().

usage() ->
    io:format(
        standard_error,
        "usage: escript benchmark/macro_compile_bench.escript "
        "[positive_iterations]~n",
        []
    ),
    halt(2).

project_root() ->
    filename:dirname(filename:dirname(filename:absname(escript:script_name()))).

add_astranaut_path(Root) ->
    Candidates = [
        filename:join([
            Root,
            "_build",
            "default",
            "lib",
            "astranaut",
            "ebin"
        ]),
        filename:join([
            Root,
            "_build",
            "test",
            "lib",
            "astranaut",
            "ebin"
        ])
    ],
    case [Path || Path <- Candidates, filelib:is_dir(Path)] of
        [Ebin | _] ->
            true = code:add_patha(Ebin),
            ok;
        [] ->
            io:format(
                standard_error,
                "astranaut ebin not found; run `rebar3 compile` first~n",
                []
            ),
            halt(2)
    end.

load_provider(File, Opts) ->
    {Module, Binary} = expect_compile(compile:file(File, Opts)),
    case code:load_binary(Module, File, Binary) of
        {module, Module} -> ok;
        {error, Reason} -> erlang:error({load_provider_failed, Reason})
    end.

parse_forms(File, IncludeDir) ->
    case epp:parse_file(File, [IncludeDir], []) of
        {ok, Forms} ->
            case [Error || Error = {error, _Detail} <- Forms] of
                [] -> Forms;
                Errors -> erlang:error({parse_failed, Errors})
            end;
        {error, Reason} ->
            erlang:error({parse_failed, Reason})
    end.

source_lines(File) ->
    {ok, Binary} = file:read_file(File),
    length(binary:split(Binary, <<"\n">>, [global])) - 1.

validate_ast_depth(Forms) ->
    FunctionDepths = [
        {Name, ast_depth(Form)}
     || Form = {function, _Anno, Name, 1, _Clauses} <- Forms,
        Name =/= local_wrap
    ],
    DeepDepths = [
        Depth
     || {Name, Depth} <- FunctionDepths,
        lists:prefix("deep_", atom_to_list(Name))
    ],
    AllDepths = [Depth || {_Name, Depth} <- FunctionDepths],
    AllMin = lists:min(AllDepths),
    DeepMin = lists:min(DeepDepths),
    case {AllMin > 10, DeepMin > 10} of
        {true, true} ->
            ok;
        _ ->
            erlang:error(
                {ast_depth_too_shallow, #{all_min => AllMin, deep_min => DeepMin}}
            )
    end,
    #{
        all_min => AllMin,
        all_max => lists:max(AllDepths),
        deep_count => length(DeepDepths),
        deep_min => DeepMin,
        deep_max => lists:max(DeepDepths)
    }.

ast_depth(Term) when is_tuple(Term) ->
    1 + max_depth(tuple_to_list(Term));
ast_depth(Term) when is_list(Term) ->
    max_depth(Term);
ast_depth(_Term) ->
    0.

max_depth([]) ->
    0;
max_depth(Terms) ->
    lists:max([ast_depth(Term) || Term <- Terms]).

expect_forms(Forms) when is_list(Forms) ->
    Forms;
expect_forms(Other) ->
    erlang:error({transform_failed, Other}).

expect_compile({ok, Module, Binary}) ->
    {Module, Binary};
expect_compile({ok, Module, Binary, _Warnings}) ->
    {Module, Binary};
expect_compile(Other) ->
    erlang:error({compile_failed, Other}).

warmup(Fun) ->
    lists:foreach(
        fun(_) -> Fun() end,
        lists:seq(1, ?WARMUP_ITERATIONS)
    ).

measure(Fun, Iterations) ->
    [
        begin
            erlang:garbage_collect(),
            {Microseconds, _Result} = timer:tc(Fun),
            Microseconds
        end
     || _ <- lists:seq(1, Iterations)
    ].

print_report(
    SourceLines,
    DepthStats,
    Iterations,
    TransformTimes,
    CompileTimes
) ->
    io:format(
        "~nAstranaut macro workload~n"
        "  source lines:       ~p~n"
        "  workload functions: 61 "
        "(42 single, 7 double, 6 expanding, 5 local, 1 local definition)~n"
        "  deep functions:     ~p (approximately 50 lines each)~n"
        "  AST depth, all:     ~p..~p~n"
        "  AST depth, deep:    ~p..~p~n"
        "  source macro calls: 67~n"
        "  effective expansions: 73~n"
        "  warmups:            ~p~n"
        "  measured runs:      ~p~n~n",
        [
            SourceLines,
            maps:get(deep_count, DepthStats),
            maps:get(all_min, DepthStats),
            maps:get(all_max, DepthStats),
            maps:get(deep_min, DepthStats),
            maps:get(deep_max, DepthStats),
            ?WARMUP_ITERATIONS,
            Iterations
        ]
    ),
    print_stats("macro transform only", stats(TransformTimes)),
    print_stats("complete compile", stats(CompileTimes)).

stats(Times) ->
    Sorted = lists:sort(Times),
    Count = length(Sorted),
    #{
        min => hd(Sorted),
        median => percentile(Sorted, 0.50),
        p95 => percentile(Sorted, 0.95),
        max => lists:last(Sorted),
        mean => lists:sum(Sorted) / Count
    }.

percentile(Sorted, Fraction) ->
    Index = max(1, ceil(length(Sorted) * Fraction)),
    lists:nth(Index, Sorted).

print_stats(Label, Stats) ->
    io:format(
        "~-22s mean ~8.2f ms | median ~8.2f ms | "
        "p95 ~8.2f ms | min ~8.2f ms | max ~8.2f ms~n",
        [
            Label,
            milliseconds(maps:get(mean, Stats)),
            milliseconds(maps:get(median, Stats)),
            milliseconds(maps:get(p95, Stats)),
            milliseconds(maps:get(min, Stats)),
            milliseconds(maps:get(max, Stats))
        ]
    ).

milliseconds(Microseconds) ->
    Microseconds / 1000.
