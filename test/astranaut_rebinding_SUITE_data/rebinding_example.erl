%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(rebinding_example).

%% API
-export([]).

-include_lib("astranaut/include/rebinding.hrl").

-rebinding_all([debug]).

%%%===================================================================
%%% API
%%%===================================================================
-export([test/1, hello/2, hello_f/1]).

test(ABC) ->
    {ABC, ABC} = {ABC + 1, ABC + 1},
    {ABC, ABC} = {ABC + 1, ABC + 1},
    EFG = ABC + 1,
    +ABC = ABC + 1,
    F = fun EFG (0, {EFG, EFG} = ABC, ABC) -> 0; EFG (N, ABC, ABC) -> EFG(N - 1) end,
    ABC = 
        case EFG of
            +ABC ->
                ABC = ABC + 1,
                ABC;
            EFG ->
                ABC = EFG + 2,
                ABC
        end,
    D = <<ABC || ABC <- <<"123">> >>,
    io:format("d is ~p~n", [D]),
    D = [ABC || ABC <- [begin ABC = ABC + 1, ABC end]],
    EFG = 
        case EFG of
            +ABC ->
                ABC = ABC + 1,
                ABC;
            +EFG ->
                ABC = EFG + 2,
                ABC = ABC + 2,
                ABC
        end,
    C = 
        try ABC of
            +EFG ->
                ABC = ABC + 1,
                ABC;
            +ABC ->
                ABC + 1
        catch
            exception:eft ->
                efg;
            _:ABC when (ABC == 3) ->
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
    io:format("d is ~p~n", [D]),
    io:format("c is ~p~n", [C]),
    F(10).


hello(A, B) ->
  A = 
    case A of
        +B -> 
          B = A + B,
          A = A + B,
          B = A + B,
          B;
        +A ->
          B = A + B,
          B
    end,
  B = 
    case A of
        +B -> 
            A = 
                begin
                    B = A + B,
                    A = A + B,
                    A
                end,
            B = A + B,
            B;
        +A ->
          B = A + B,
          B
    end,
  {A, B}.

hello_f(A) ->
    A = A + 1,
    F = fun F (0) -> 0; F (A) -> A = F(A - 1), A end,
    A = F(A),
    F = fun F (0) -> 0; F (A) -> A = F(A - 1), A end,
    A = F(A),
    A.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
