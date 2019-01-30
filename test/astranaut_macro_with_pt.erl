%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_with_pt).

-compile({parse_transform, macro_pt}).

%% API
-export([test/0]).

-exec_macro({astranaut_macro_example, macro_exported_function, [hello, world]}).

%%%===================================================================
%%% API
%%%===================================================================

test() ->
    astranaut_macro_example:quote_unquote(ok).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
