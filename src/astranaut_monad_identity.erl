%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_monad_identity).

%% API
-export([run/1]).
-export([bind/2, return/1]).
%%%===================================================================
%%% API
%%%===================================================================
run({?MODULE, A}) ->
    A.

bind({?MODULE, IA}, KIB) ->
    KIB(IA).

return(A) ->
    {?MODULE, A}.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
