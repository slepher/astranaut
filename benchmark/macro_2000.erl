%%% Generated macro-expansion performance workload. Do not edit by hand.
-module(macro_2000).

-include("quote.hrl").
-include("macro.hrl").

-compile(nowarn_unused_function).
-import_macro(macro_bench_macros).
-local_macro([local_wrap/1]).

local_wrap(Ast) ->
    quote({local, unquote(Ast)}).

%%% Standard functions: approximately 20 lines each.
single_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_002(Input) ->
    Index = 2,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_003(Input) ->
    Index = 3,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_004(Input) ->
    Index = 4,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_005(Input) ->
    Index = 5,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_006(Input) ->
    Index = 6,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_007(Input) ->
    Index = 7,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_008(Input) ->
    Index = 8,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_009(Input) ->
    Index = 9,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_010(Input) ->
    Index = 10,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_011(Input) ->
    Index = 11,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_012(Input) ->
    Index = 12,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_013(Input) ->
    Index = 13,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_014(Input) ->
    Index = 14,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_015(Input) ->
    Index = 15,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_016(Input) ->
    Index = 16,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_017(Input) ->
    Index = 17,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_018(Input) ->
    Index = 18,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_019(Input) ->
    Index = 19,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_020(Input) ->
    Index = 20,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_021(Input) ->
    Index = 21,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_022(Input) ->
    Index = 22,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_023(Input) ->
    Index = 23,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_024(Input) ->
    Index = 24,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_025(Input) ->
    Index = 25,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_026(Input) ->
    Index = 26,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_027(Input) ->
    Index = 27,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

single_028(Input) ->
    Index = 28,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

double_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:outer(
          macro_bench_macros:inner(#{root => #{payload => #{data => {Index, Values}}}})),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

double_002(Input) ->
    Index = 2,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:outer(
          macro_bench_macros:inner(#{root => #{payload => #{data => {Index, Values}}}})),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

double_003(Input) ->
    Index = 3,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:outer(
          macro_bench_macros:inner(#{root => #{payload => #{data => {Index, Values}}}})),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

double_004(Input) ->
    Index = 4,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:outer(
          macro_bench_macros:inner(#{root => #{payload => #{data => {Index, Values}}}})),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

expanding_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:expands_to_single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

expanding_002(Input) ->
    Index = 2,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:expands_to_single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

expanding_003(Input) ->
    Index = 3,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:expands_to_single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

expanding_004(Input) ->
    Index = 4,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        macro_bench_macros:expands_to_single(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

local_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        local_wrap(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

local_002(Input) ->
    Index = 2,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        local_wrap(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

local_003(Input) ->
    Index = 3,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        local_wrap(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

local_004(Input) ->
    Index = 4,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    Tagged =
        local_wrap(#{root => #{payload => #{data => {Index, Values}}}}),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

%%% Deep functions: approximately 50 lines with 16 nested maps.
deep_single_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_002(Input) ->
    Index = 2,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_003(Input) ->
    Index = 3,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_004(Input) ->
    Index = 4,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_005(Input) ->
    Index = 5,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_006(Input) ->
    Index = 6,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_007(Input) ->
    Index = 7,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_008(Input) ->
    Index = 8,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_009(Input) ->
    Index = 9,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_010(Input) ->
    Index = 10,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_011(Input) ->
    Index = 11,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_012(Input) ->
    Index = 12,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_013(Input) ->
    Index = 13,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_single_014(Input) ->
    Index = 14,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_double_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:outer(
          macro_bench_macros:inner(DeepInput)),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_double_002(Input) ->
    Index = 2,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:outer(
          macro_bench_macros:inner(DeepInput)),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_double_003(Input) ->
    Index = 3,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:outer(
          macro_bench_macros:inner(DeepInput)),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_expanding_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:expands_to_single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_expanding_002(Input) ->
    Index = 2,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        macro_bench_macros:expands_to_single(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

deep_local_001(Input) ->
    Index = 1,
    Seed = Input + Index,
    Values = [Seed, Seed + 1, Seed + 2],
    Total = lists:sum(Values),
    DeepInput =
        #{level_01 =>
              #{level_02 =>
                    #{level_03 =>
                          #{level_04 =>
                                #{level_05 =>
                                      #{level_06 =>
                                            #{level_07 =>
                                                  #{level_08 =>
                                                        #{level_09 =>
                                                              #{level_10 =>
                                                                    #{level_11 =>
                                                                          #{level_12 =>
                                                                                #{level_13 =>
                                                                                      #{level_14 =>
                                                                                            #{level_15 =>
                                                                                                  #{level_16 =>
                                                                                                        #{index => Index,
                                                                                                          values => Values}
                                                                                                   }
                                                                                             }
                                                                                       }
                                                                                 }
                                                                           }
                                                                     }
                                                               }
                                                         }
                                                   }
                                             }
                                       }
                                 }
                           }
                     }
               }
         },
    Tagged =
        local_wrap(DeepInput),
    Status =
        case Total rem 2 of
            0 -> even;
            _ -> odd
        end,
    Summary = #{
        index => Index,
        input => Input,
        tagged => Tagged,
        total => Total,
        status => Status
    },
    {ok, Summary}.

