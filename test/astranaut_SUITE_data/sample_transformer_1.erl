%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Feb 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_transformer_1).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    Return = 
        astranaut_return:lift_m(
            fun(_) -> Forms end,
            astranaut_lib:with_attribute(
                fun(mark_error_0, _Acc) ->
                       {warning, error_0};
                   (mark_0, _Acc) ->
                       {warning, warning_0};
                   (_Attr, Acc) ->
                       Acc
                end, ok, Forms, mark, #{formatter => ?MODULE})),
    astranaut_return:to_compiler(Return).

format_error(error_0) ->
    io_lib:format("get error 0", []);
format_error(warning_0) ->
    io_lib:format("get warning 0", []);
format_error(Error) ->
    astranaut:format_error(Error).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
