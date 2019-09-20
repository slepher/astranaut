%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(rebinding_test).

-compile({parse_transform, astranaut_rebinding}).

%% API
-export([test/1]).

%%%===================================================================
%%% API
%%%===================================================================
test(ABC) ->
    ABC = ABC + 1,
    ABC = ABC + 1,
    EFG = ABC + 1,
    F = fun Hello (0) -> 0; Hello (N) -> Hello(N - 1) end,
    ABC = 
        case EFG of
            ABC ->
                ABC = ABC + 1,
                ABC;
            EFG ->
                ABC = EFG + 2,
                ABC
        end,
    EFG = 
        case EFG of
            ABC ->
                ABC = ABC + 1,
                ABC;
            EFG ->
                ABC = EFG + 2,
                ABC
        end,
    Zero = 
        lists:map(
          fun(Zero) ->
                  ABC = Zero + 1,
                  Zero = ABC + 1,
                  Zero
          end, lists:seq(1, 10)),
    io:format("Zero is ~p~n", [Zero]),
    io:format("abc is ~p~n", [ABC]),
    io:format("efg is ~p~n", [EFG]),

    F(10).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
