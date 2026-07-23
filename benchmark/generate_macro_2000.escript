#!/usr/bin/env escript
%%! -noshell
-mode(compile).

-define(STANDARD_SINGLE_COUNT, 28).
-define(STANDARD_DOUBLE_COUNT, 4).
-define(STANDARD_EXPANDING_COUNT, 4).
-define(STANDARD_LOCAL_COUNT, 4).
-define(DEEP_SINGLE_COUNT, 14).
-define(DEEP_DOUBLE_COUNT, 3).
-define(DEEP_EXPANDING_COUNT, 2).
-define(DEEP_LOCAL_COUNT, 1).

main(Args) ->
    Output = output_path(Args),
    ok = filelib:ensure_dir(Output),
    Content = [
        "%%% Generated macro-expansion performance workload. Do not edit by hand.\n",
        "-module(macro_2000).\n",
        "\n",
        "-include(\"quote.hrl\").\n",
        "-include(\"macro.hrl\").\n",
        "\n",
        "-compile(nowarn_unused_function).\n",
        "-import_macro(macro_bench_macros).\n",
        "-local_macro([local_wrap/1]).\n",
        "\n",
        "local_wrap(Ast) ->\n",
        "    quote({local, unquote(Ast)}).\n",
        "\n",
        "%%% Standard functions: approximately 20 lines each.\n",
        functions(standard, single, ?STANDARD_SINGLE_COUNT),
        functions(standard, double, ?STANDARD_DOUBLE_COUNT),
        functions(standard, expanding, ?STANDARD_EXPANDING_COUNT),
        functions(standard, local, ?STANDARD_LOCAL_COUNT),
        "%%% Deep functions: approximately 50 lines with 16 nested maps.\n",
        functions(deep, single, ?DEEP_SINGLE_COUNT),
        functions(deep, double, ?DEEP_DOUBLE_COUNT),
        functions(deep, expanding, ?DEEP_EXPANDING_COUNT),
        functions(deep, local, ?DEEP_LOCAL_COUNT)
    ],
    ok = file:write_file(Output, unicode:characters_to_binary(Content)),
    io:format("generated ~s (~p functions)~n",
              [Output, total_function_count()]).

output_path([]) ->
    filename:join(filename:dirname(escript:script_name()), "macro_2000.erl");
output_path([Output]) ->
    Output;
output_path(_) ->
    usage().

usage() ->
    io:format(standard_error,
              "usage: escript benchmark/generate_macro_2000.escript [output]~n",
              []),
    halt(2).

functions(Shape, Kind, Count) ->
    [function(Shape, Kind, Index) || Index <- lists:seq(1, Count)].

function(standard, Kind, Index) ->
    Name = function_name(standard, Kind, Index),
    MacroExpression = macro_expression(Kind),
    io_lib:format(
      "~s(Input) ->~n"
      "    Index = ~B,~n"
      "    Seed = Input + Index,~n"
      "    Values = [Seed, Seed + 1, Seed + 2],~n"
      "    Total = lists:sum(Values),~n"
      "    Tagged =~n"
      "~s"
      "    Status =~n"
      "        case Total rem 2 of~n"
      "            0 -> even;~n"
      "            _ -> odd~n"
      "        end,~n"
      "    Summary = #{~n"
      "        index => Index,~n"
      "        input => Input,~n"
      "        tagged => Tagged,~n"
      "        total => Total,~n"
      "        status => Status~n"
      "    },~n"
      "    {ok, Summary}.~n~n",
      [Name, Index, MacroExpression]);
function(deep, Kind, Index) ->
    Name = function_name(deep, Kind, Index),
    MacroExpression = macro_expression(Kind, "DeepInput"),
    io_lib:format(
      "~s(Input) ->~n"
      "    Index = ~B,~n"
      "    Seed = Input + Index,~n"
      "    Values = [Seed, Seed + 1, Seed + 2],~n"
      "    Total = lists:sum(Values),~n"
      "    DeepInput =~n"
      "~s"
      "    Tagged =~n"
      "~s"
      "    Status =~n"
      "        case Total rem 2 of~n"
      "            0 -> even;~n"
      "            _ -> odd~n"
      "        end,~n"
      "    Summary = #{~n"
      "        index => Index,~n"
      "        input => Input,~n"
      "        tagged => Tagged,~n"
      "        total => Total,~n"
      "        status => Status~n"
      "    },~n"
      "    {ok, Summary}.~n~n",
      [Name, Index, deep_input(), MacroExpression]).

function_name(standard, Kind, Index) ->
    io_lib:format("~s_~3..0B", [atom_to_list(Kind), Index]);
function_name(deep, Kind, Index) ->
    io_lib:format("deep_~s_~3..0B", [atom_to_list(Kind), Index]).

macro_expression(single) ->
    macro_expression(
      single, "#{root => #{payload => #{data => {Index, Values}}}}");
macro_expression(double) ->
    macro_expression(
      double, "#{root => #{payload => #{data => {Index, Values}}}}");
macro_expression(expanding) ->
    macro_expression(
      expanding, "#{root => #{payload => #{data => {Index, Values}}}}");
macro_expression(local) ->
    macro_expression(
      local, "#{root => #{payload => #{data => {Index, Values}}}}").

macro_expression(single, Argument) ->
    io_lib:format(
      "        macro_bench_macros:single(~s),~n", [Argument]);
macro_expression(double, Argument) ->
    io_lib:format(
      "        macro_bench_macros:outer(~n"
      "          macro_bench_macros:inner(~s)),~n",
      [Argument]);
macro_expression(expanding, Argument) ->
    io_lib:format(
      "        macro_bench_macros:expands_to_single(~s),~n", [Argument]);
macro_expression(local, Argument) ->
    io_lib:format("        local_wrap(~s),~n", [Argument]).

deep_input() ->
    [
     "        #{level_01 =>\n",
     "              #{level_02 =>\n",
     "                    #{level_03 =>\n",
     "                          #{level_04 =>\n",
     "                                #{level_05 =>\n",
     "                                      #{level_06 =>\n",
     "                                            #{level_07 =>\n",
     "                                                  #{level_08 =>\n",
     "                                                        #{level_09 =>\n",
     "                                                              #{level_10 =>\n",
     "                                                                    #{level_11 =>\n",
     "                                                                          #{level_12 =>\n",
     "                                                                                #{level_13 =>\n",
     "                                                                                      #{level_14 =>\n",
     "                                                                                            #{level_15 =>\n",
     "                                                                                                  #{level_16 =>\n",
     "                                                                                                        #{index => Index,\n",
     "                                                                                                          values => Values}\n",
     "                                                                                                   }\n",
     "                                                                                             }\n",
     "                                                                                       }\n",
     "                                                                                 }\n",
     "                                                                           }\n",
     "                                                                     }\n",
     "                                                               }\n",
     "                                                         }\n",
     "                                                   }\n",
     "                                             }\n",
     "                                       }\n",
     "                                 }\n",
     "                           }\n",
     "                     }\n",
     "               }\n",
     "         },\n"
    ].

total_function_count() ->
    1 +
    ?STANDARD_SINGLE_COUNT + ?STANDARD_DOUBLE_COUNT +
    ?STANDARD_EXPANDING_COUNT + ?STANDARD_LOCAL_COUNT +
    ?DEEP_SINGLE_COUNT + ?DEEP_DOUBLE_COUNT +
    ?DEEP_EXPANDING_COUNT + ?DEEP_LOCAL_COUNT.
