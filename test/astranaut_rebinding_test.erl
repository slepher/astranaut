%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_rebinding_test).

-include("rebinding.hrl").

-rebinding_all([]).
-rebinding_fun({[test_lc, test_function], debug}).
-rebinding_fun({[test_lc_origin, test_function_origin], non_rebinding}).

%% API
-export([test/1, hello/2, hello_f/1]).
-export([test_lc/1, test_lc_origin/1]).
-export([test_function/1, test_function_origin/1]).

%%%===================================================================
%%% API
%%%===================================================================
test(ABC) ->
    {ABC, ABC} = {ABC + 1, ABC + 1},
    {ABC, ABC} = {ABC + 1, ABC + 1},
    EFG = ABC + 1,
    +ABC = ABC + 1,
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
    D = <<ABC || ABC <- <<"123">> >>,
    io:format("d is ~p~n", [D]),
    D = [ABC || ABC <- [begin ABC = ABC + 1, ABC end]],
    EFG = 
        case EFG of
            ABC ->
                ABC = ABC + 1,
                ABC;
            EFG ->
                ABC = EFG + 2,
                ABC = ABC + 2,
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
    io:format("d is ~p~n", [D]),
    io:format("c is ~p~n", [C]),
    F(10).

hello(A, B) ->
  A = 
    case A of
        B -> 
          B = A + B,
          A = A + B,
          B = A + B,
          B;
        A ->
          B = A + B,
          B
    end,
  B = 
    case A of
        B -> 
            A = 
                begin
                    B = A + B,
                    A = A + B,
                    A
                end,
            B = A + B,
            B;
        A ->
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

test_lc(A) ->
    A = [{A, B} || 
            A <- [begin A = A + 2, A end], 
            B <- [begin A = A + 1, A end]
        ],
    A.

test_lc_origin(A) ->
    A = [{A_1, B} || 
            A_1 <- [begin A_1 = A + 2, A_1 end], 
            B <- [begin A_2 = A_1 + 1, A_2 end]
        ],
    A.

test_function(A) ->
    B = 
        bind(begin A = A + 1, A end,
             begin A = A + 2, A end,
             fun(A) ->
                     A = A + 10,
                     A
             end),
    A = A + 1,
    {A, B}.

test_function_origin(A) ->
    A_4 = 
        bind(begin A_1 = A + 1, A_1 end,
             begin A_2 = A + 2, A_2 end,
             fun(A_1) ->
                     A_4 = A_1 + 10,
                     A_4
             end),
    A_5 = A_4 + 1,
    A_5.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
bind(A_1, _A_2, K) ->
    K(A_1 + 10).
