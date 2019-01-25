%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_pt).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    Macro1 = {astranaut_example_macros, exported_macro, 0},
    Macro2 = {astranaut_example_macros, test_macro, 0},
    Opts = [auto_export, {formatter, ?MODULE}],
    Macros = [{Macro1, []}, {Macro2, Opts}],
    astranaut_macro:transform_macros(Macros, Forms).

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
