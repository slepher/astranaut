%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_test_lib).

-include("compile_opts.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([get_baseline/2, realize_with_baseline/2, test_module_forms/2, compile_test_module/2, compile_test_forms/1, load_data_modules/2]).

%%%===================================================================
%%% API
%%%===================================================================
get_baseline(Mark, Forms) ->
    case astranaut_return:run(
           astranaut_lib:with_attribute(
             fun(Mark1, _Acc, #{line := Line}) when Mark == Mark1 ->
                     Line;
                (_Mark2, Acc, #{}) ->
                     Acc
             end, undefined, Forms, baseline, #{formatter => ?MODULE})) of
        {just, undefined} ->
            Msg = io_lib:format("attribute -baseline(~p) expected", [Mark]),
            io:format("~s", [Msg]),
            exit(list_to_binary(Msg));
        {just, Baseline} when is_integer(Baseline) ->
            Baseline;
        {just, {Baseline, _Column}} when is_integer(Baseline) ->
            Baseline
    end.

realize_with_baseline(Baseline, ErrorStruct) ->
    ErrorStruct1 =
        astranaut_error:with_all_formatted_failure(
            fun({Line, Formatter, Error}) when is_integer(Line) ->
                    {Line - Baseline, Formatter, Error};
               ({{Line, _Column}, Formatter, Error}) when is_integer(Line) ->
                    {Line - Baseline, Formatter, Error};
               (Error) ->
                    Error
            end, ErrorStruct),
    astranaut_error:realize(ErrorStruct1).

compile_test_module(Module, Config) ->
    Forms = test_module_forms(Module, Config),
    Opts = compile_opts(),
    astranaut_lib:load_forms(Forms, Opts).

compile_test_forms(Forms) ->
    Opts = compile_opts(),
    Opts1 = Opts -- [report_warnings, report_errors] ++ [return_warnings],
    astranaut_lib:load_forms(Forms, Opts1).

test_module_forms(Module, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, atom_to_list(Module) ++ ".erl"),
    Opts = compile_opts(),
    case astranaut_lib:parse_file(File, Opts) of
        {error, Errors, []} ->
            exit({compile_module_failed, Errors});
        Forms ->
            Forms
    end.

load_data_modules(Config, TestModules) ->
    lists:foreach(
      fun(TestModule) ->
              Return = compile_test_module(TestModule, Config),
              astranaut_return:with_error(
                fun(Error) ->
                        ?assertEqual(#{}, astranaut_error:printable(Error)),
                        Error
                end, Return)
      end, TestModules),
    Config.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
