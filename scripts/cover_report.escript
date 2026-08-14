#!/usr/bin/env escript
%% -*- erlang -*-

%% cover_report.escript -- generates agent-friendly JSON coverage report
%% from rebar3 ct output.
%%
%% Usage (from project root, after `rebar3 ct`):
%%   escript scripts/cover_report.escript
%% Output: _build/test/cover/cover_report.json

main(_Args) ->
    BaseDir = "_build/test",
    EbinDir = filename:join([BaseDir, "lib", "astranaut", "ebin"]),
    CoverData = filename:join([BaseDir, "cover", "ct.coverdata"]),
    OutFile = filename:join([BaseDir, "cover", "cover_report.json"]),

    {ok, _} = cover:start(),
    ok = cover:reset(),
    ok = cover:import(CoverData),
    code:add_patha(EbinDir),
    code:add_patha(filename:join([BaseDir, "lib", "astranaut", "test"])),

    Mods = lists:sort(cover:imported_modules()),

    Reports = lists:filtermap(
        fun(Mod) ->
            {ok, Result} = cover:analyse(Mod, coverage, line),
            case count_lines(Result) of
                {Cov, NotCov} when Cov + NotCov > 0 ->
                    {true, build_report(Mod, Cov, NotCov, Result)};
                _ ->
                    false
            end
        end,
        Mods
    ),

    Summary = #{modules => Reports, total_pct => total_pct(Reports)},
    Json = to_json(Summary),
    ok = file:write_file(OutFile, Json),
    io:format(
        "Coverage report -> ~s~n  ~p modules, ~.1f%~n",
        [OutFile, length(Reports), total_pct(Reports)]
    ).

build_report(Mod, Cov, NotCov, Result) ->
    SrcLines = src_lines(Mod),
    Total = Cov + NotCov,
    Pct = round(Cov * 10000 / Total) / 100,
    Missed = [{Line, SrcLines(Line)} || {{_Mod, Line}, {0, _NC}} <- Result, Line > 0],
    #{
        module => atom_to_list(Mod),
        coverage => Pct,
        covered => Cov,
        not_covered => NotCov,
        total => Total,
        missed => Missed
    }.

count_lines([]) ->
    {0, 0};
count_lines([{{_M, L}, {C, N}} | T]) when L > 0 ->
    {CT, NT} = count_lines(T),
    {C + CT, N + NT};
count_lines([_ | T]) ->
    count_lines(T).

src_lines(Mod) ->
    case find_src(Mod) of
        undefined ->
            fun(_Line) -> null end;
        Path ->
            {ok, Bin} = file:read_file(Path),
            Lines = binary:split(Bin, <<"\n">>, [global]),
            fun
                (Line) when Line =< length(Lines) ->
                    string:trim(binary_to_list(lists:nth(Line, Lines)));
                (_Line) ->
                    null
            end
    end.

find_src(Mod) ->
    Name = atom_to_list(Mod) ++ ".erl",
    find_src_1(["src/" ++ Name, "test/" ++ Name]).

find_src_1([]) ->
    undefined;
find_src_1([P | Ps]) ->
    case filelib:is_file(P) of
        true -> P;
        false -> find_src_1(Ps)
    end.

total_pct([]) ->
    0.0;
total_pct(Reports) ->
    {TC, TN} = lists:foldl(
        fun(#{covered := C, not_covered := N}, {CA, NA}) ->
            {CA + C, NA + N}
        end,
        {0, 0},
        Reports
    ),
    case TC + TN of
        0 -> 100.0;
        T -> round(TC * 10000 / T) / 100
    end.

%% -- simple JSON encoder --
to_json(#{modules := Mods, total_pct := Pct}) ->
    Items = [mod_to_json(M) || M <- Mods],
    <<"{\n  \"total_coverage\": ", (float_bin(Pct))/binary, ",\n", "  \"modules\": [\n",
        (join(<<",\n">>, Items))/binary, "\n  ]\n}\n">>.

mod_to_json(#{
    module := Mod,
    coverage := Pct,
    covered := Cov,
    not_covered := Not,
    total := Total,
    missed := Missed
}) ->
    Ms = [missed_to_json(L, C) || {L, C} <- Missed],
    <<"    { \"module\": \"", (list_to_binary(Mod))/binary, "\",\n", "      \"coverage\": ",
        (float_bin(Pct))/binary, ",\n", "      \"covered\": ", (integer_to_binary(Cov))/binary,
        ",\n", "      \"not_covered\": ", (integer_to_binary(Not))/binary, ",\n",
        "      \"total\": ", (integer_to_binary(Total))/binary, ",\n", "      \"missed\": [\n",
        (join(<<",\n">>, Ms))/binary, "\n      ]\n    }">>.

missed_to_json(Line, null) ->
    <<"        { \"line\": ", (integer_to_binary(Line))/binary, ", \"code\": null }">>;
missed_to_json(Line, Code) ->
    <<"        { \"line\": ", (integer_to_binary(Line))/binary, ", \"code\": \"",
        (json_escape(unicode:characters_to_binary(Code, utf8)))/binary, "\" }">>.

join(_, []) -> <<>>;
join(_, [X]) -> X;
join(S, [X | Xs]) -> <<X/binary, S/binary, (join(S, Xs))/binary>>.

float_bin(F) -> list_to_binary(io_lib:format("~.1f", [F])).

json_escape(B) -> json_escape(B, <<>>).
json_escape(<<>>, A) -> A;
json_escape(<<"\"", R/binary>>, A) -> json_escape(R, <<A/binary, "\\\"">>);
json_escape(<<"\\", R/binary>>, A) -> json_escape(R, <<A/binary, "\\\\">>);
json_escape(<<"\n", R/binary>>, A) -> json_escape(R, <<A/binary, "\\n">>);
json_escape(<<"\r", R/binary>>, A) -> json_escape(R, <<A/binary, "\\r">>);
json_escape(<<"\t", R/binary>>, A) -> json_escape(R, <<A/binary, "\\t">>);
json_escape(<<C/utf8, R/binary>>, A) -> json_escape(R, <<A/binary, C/utf8>>).
