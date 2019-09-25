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
%-rebinding_fun({[test_lc, test_function], debug}).
-rebinding_fun({[test_lc_origin, test_function_origin], non_rebinding}).

%% API
-export([test_lc/1, test_lc_origin/1]).
-export([test_function/1, test_function_origin/1]).

%%%===================================================================
%%% API
%%%===================================================================

test_lc(A) ->
    A = [{A, B} || 
            B <- [begin A = A + 2, A end], 
            begin A = A + 3, A, true end,
            A <- [begin A = A + 1, A end],
            is_ok(begin A = A + 3, A end, begin A = A + 4, A end),
            true
        ],
    A.

test_lc_origin(A) ->
    A_1 = [{A_4, B}
	   || B <- [begin A_1 = A + 2, A_1 end],
	      begin A_1 = A + 3, A_1, true end,
	      A_2 <- [begin A_2 = A_1 + 1, A_2 end],
	      is_ok(begin A_3 = A_2 + 3, A_3 end,
		    begin A_4 = A_2 + 4, A_4 end),
	      true],
    A_1.
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
    A = 
        bind([],
             fun(_) ->
                     bind(begin A_1 = A + 1, A_1 end,
                          fun(A_1) ->
                                  bind(
                                    [begin A_2 = A_1 + 1, A_2 end],
                                    fun(B) ->
                                            {A_1, B}
                                    end)
                          end)
             end),
    A.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_ok(_A1, _A2) ->
    true.

bind(A, K) ->
    A(K).

bind(A_1, _A_2, K) ->
    K(A_1 + 10).
