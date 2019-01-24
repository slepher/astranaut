%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_exports).

%% API
-export([macro_x/1]).

-include("quote.hrl").
-include("macro.hrl").

-export([hello/1]).

-use_macro({macro_x/1, [{as_attr, macro_x}, {auto_export, true}, {merge_function, head}]}).

%%%===================================================================
%%% API
%%%===================================================================

macro_x(Name) ->
    astranaut:function(
      Name,
      quote(
        fun(this) ->
                {this, _A@Name}
        end)).

-macro_x([hello]).

-macro_x([world]).

hello(that) ->
    that.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
