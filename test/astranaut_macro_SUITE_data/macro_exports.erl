%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(macro_exports).

%%-macro_options(debug_module).

%% API
-export_macro({[macro_x/1], [as_attr]}).

-include("quote.hrl").
-include("macro.hrl").

-export([hello/1]).

%%%===================================================================
%%% API
%%%===================================================================

macro_x(Name) ->
    macro_x_1(Name).

macro_x_1(Name) ->
    Fun =
        case Name of
            hello ->
              quote(
                fun(this) ->
                        {this, _A@Name};
                   (Default) ->
                       '__original__'(Default)
                end);
            _ ->
              quote(
                fun(this) ->
                        {this, _A@Name}
                end)
        end,
    astranaut_lib:gen_exported_function(Name, Fun).

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
