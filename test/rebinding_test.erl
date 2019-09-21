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
    {ABC, ABC} = {ABC + 1, ABC + 1},
    {ABC, ABC} = {ABC + 1, ABC + 1},
    EFG = ABC + 1,
    ABC = ABC + 1,
    F = fun EFG (0, {EFG, EFG} = ABC, ABC) -> 0; EFG (N, ABC, ABC) -> EFG(N - 1) end,
    ABC = 
        case EFG of
            ABC ->
                ABC = ABC + 1,
                ABC;
            EFG ->
                ABC = EFG + 2,
                ABC
        end,
    %D = [ABC || ABC <- [ABC = ABC + 1]],
    EFG = 
        case EFG of
            ABC ->
                ABC = ABC + 1,
                ABC;
            EFG ->
                ABC = EFG + 2,
                ABC
        end,
    C = 
        try ABC of
            EFG ->
                ABC = ABC + 1,
                ABC;
            ABC ->
                ABC + 1
        catch
            _:ABC:_Stacks ->
                {ABC, ABC}
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
    %io:format("d is ~p~n", [D]),
    io:format("c is ~p~n", [C]),
    F(10).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
