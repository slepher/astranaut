%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_struct_fail_0).

-include_lib("astranaut/include/compile_meta.hrl").
-include_lib("astranaut/include/struct.hrl").

-record(test, {name, value}).

-astranaut_compile_meta(silent).

-astranaut_struct({test, [{enforce_keys, [desc]}]}).

%% API
-export([new/0]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #test{}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
