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

parse_transform(Forms0, Opts) ->
    {warning, Forms1, Warnings1} = astranaut_quote:parse_transform(Forms0, Opts),
    {warning, Forms2, Warnings2} = astranaut_macro:parse_transform(Forms1, Opts),
    [{eof, Line}|Forms3] = lists:reverse(Forms2),
    Warnings = 
        lists:foldl(
          fun({_File, Warnings}, Acc) ->
                  Warnings ++ Acc
          end, [], Warnings2 ++ Warnings1),
    Function = astranaut:function(
                 warnings,
                 quote(fun() ->
                               unquote(astranaut:abstract(Warnings))
                       end, Line)),
    Export = {attribute, Line, export, [{warnings, 0}]},
    Forms4 = astranaut:reorder_exports(lists:reverse([{eof, Line}, Function, Export|Forms3])),
    Forms4.

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
