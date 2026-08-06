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
-export([get_baseline/2, realize_with_baseline/2, test_module_forms/2,
         compile_test_module/2, compile_test_forms/1, load_data_modules/2,
         with_suite_data_dir/2]).
-export([assert_formatted_messages/1]).

%%%===================================================================
%%% API
%%%===================================================================
get_baseline(Mark, Forms) ->
    case astranaut_return:run(
           astranaut_lib:with_attribute(
             fun(Mark1, _Acc, #{pos := Pos}) when Mark =:= Mark1 ->
                     Pos;
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
            fun({Pos, Formatter, Error}) when is_integer(Pos) ->
                    {Pos - Baseline, Formatter, Error};
               ({{Line, _Column}, Formatter, Error}) when is_integer(Line) ->
                    {Line - Baseline, Formatter, Error};
               (Error) ->
                    Error
            end, ErrorStruct),
    astranaut_error:realize(ErrorStruct1).

compile_test_module(Module, Config) ->
    DataDir = configured_data_dir(Config),
    Forms = test_module_forms(Module, DataDir, Config),
    Opts = compile_opts(),
    Outfile = filename:join(filename:dirname(filename:absname(DataDir)), atom_to_list(Module) ++ ".beam"),
    file:delete(Outfile),
    astranaut_return:bind(
      astranaut_lib:load_forms(Forms, Opts),
      fun({Mod, Binary}) ->
              %% write beam file to make edts works.
              ok = file:write_file(Outfile, Binary, []),
              astranaut_return:return(Mod)
      end).

compile_test_forms(Forms) ->
    Opts = compile_opts(),
    Opts1 = Opts -- [report_warnings, report_errors] ++ [return_warnings],
    astranaut_lib:load_forms(Forms, Opts1).

test_module_forms(Module, Config) ->
    DataDir = configured_data_dir(Config),
    test_module_forms(Module, DataDir, Config).

test_module_forms(Module, DataDir, _Config) ->
    File = filename:join(DataDir, atom_to_list(Module) ++ ".erl"),
    Opts = compile_opts(),
    case filelib:is_file(File) of
        true ->
            case astranaut_lib:parse_file(File, Opts) of
                {error, Errors, []} ->
                    exit({compile_module_failed, Errors});
                Forms ->
                    Forms
            end;
        false ->
            exit({file_not_detected, File})
    end.

load_data_modules(Config, TestModules) ->
    lists:foreach(
      fun(TestModule) ->
              Return = compile_test_module(TestModule, Config),
              astranaut_return:with_error(
                fun(Error) ->
                        ?assertEqual(#{}, maps:without([warnings, formatted_warnings, file_warnings],
                                                       astranaut_error:printable(Error))),
                        Error
                end, Return)
      end, TestModules),
    Config.

with_suite_data_dir(Config, Suite) ->
    CurrentDataDir = proplists:get_value(data_dir, Config),
    SuiteDataDir =
        filename:join(
          filename:dirname(filename:absname(CurrentDataDir)),
          atom_to_list(Suite) ++ "_data"),
    [{fixture_data_dir, SuiteDataDir} | Config].

configured_data_dir(Config) ->
    proplists:get_value(
      fixture_data_dir, Config, proplists:get_value(data_dir, Config)).

assert_formatted_messages(Messages) ->
    lists:foreach(fun assert_formatted_message/1, Messages).

assert_formatted_message({_Line, Formatter, Error}) ->
    case formatter_protocol(Formatter) of
        strict ->
            assert_formatter_result(
              Formatter, Error,
              fun() -> Formatter:format_error(Error, #{default => throw}) end);
        legacy ->
            assert_formatter_result(
              Formatter, Error,
              fun() -> Formatter:format_error(Error) end);
        {invalid, Reason} ->
            ct:fail({invalid_formatter_protocol, Formatter, Reason})
    end.

formatter_protocol(Formatter) when is_atom(Formatter) ->
    case code:ensure_loaded(Formatter) of
        {module, Formatter} ->
            case {erlang:function_exported(Formatter, format_error, 1),
                  erlang:function_exported(Formatter, format_error, 2)} of
                {true, true} ->
                    strict;
                {true, false} ->
                    legacy;
                {false, _} ->
                    {invalid, missing_format_error_1}
            end;
        {error, Reason} ->
            {invalid, {module_not_loaded, Reason}}
    end;
formatter_protocol(_Formatter) ->
    {invalid, invalid_formatter_identifier}.

assert_formatter_result(Formatter, Error, FormatterFun) ->
    Result =
        try FormatterFun() of
            FormattedMessage ->
                {ok, FormattedMessage}
        catch
            Class:Reason:Stacktrace ->
                {error, {format_error_not_covered,
                         Formatter, Error, Class, Reason, Stacktrace}}
        end,
    case Result of
        {ok, Message} ->
            assert_formatted_message_result(Formatter, Error, Message);
        {error, Failure} ->
            ct:fail(Failure)
    end.

assert_formatted_message_result(Formatter, Error, Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            case lists:flatten(Message) of
                [] ->
                    ct:fail({invalid_formatted_message, Formatter, Error, Message});
                _ ->
                    ok
            end;
        false ->
            ct:fail({invalid_formatted_message, Formatter, Error, Message})
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
