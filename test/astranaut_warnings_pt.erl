%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_warnings_pt).

-include("quote.hrl").

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================

parse_transform(Forms, Opts) ->
    {warning, Forms1, Warnings} = astranaut_macro:parse_transform(Forms, Opts),
    [{eof, Line}|Forms2] = lists:reverse(Forms1),
    Function = astranaut:function(
                 warnings,
                 quote(fun() ->
                               [{_File, Warnings}] = unquote(astranaut:abstract(Warnings)),
                               Warnings
                       end, Line)),
    Export = {attribute, Line, export, [{warnings, 0}]},
    Forms3 = astranaut:reorder_exports(lists:reverse([{eof, Line}, Function, Export|Forms2])),
    Forms3.

format_error(Error) ->
    astranaut_traverse:format_error(Error).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
