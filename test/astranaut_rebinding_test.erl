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
-include("stacktrace.hrl").

-rebinding_fun({[test_lc, test_case], []}).
-rebinding_fun({[test_case_pinned], [clause_pinned]}).
-rebinding_fun({[test_function], [strict]}).
-rebinding_fun({[test_operator, test_tuple, test_list, test_try, test_function_guard], [strict]}).
-rebinding_fun({[test_map, test_map_update], [strict]}).
-rebinding_fun({[test_rec, test_rec_update], [strict]}).
-rebinding_fun({[test_lc_origin, test_function_origin, test_case_origin], non_rebinding}).
-rebinding_fun({[test_operator_origin, test_tuple_origin, test_list_origin], non_rebinding}).
-rebinding_fun({[test_map_origin, test_map_update_origin], non_rebinding}).
-rebinding_fun({[test_rec_origin, test_rec_update_origin], non_rebinding}).

-record(rec, {a, b, c, d}).

-compile(export_all).
-compile(nowarn_export_all).
%% API

%%%===================================================================
%%% API
%%%===================================================================

test_lc(A) ->
    A = [{A, B} || 
            B <- [begin A = A + 2, A end], 
            begin A = A + 3, A, true end,
            A <- [begin A = A + 1, A end],
            is_ok(A + 3, begin A = A + 4, A end),
            true
        ],
    A.

test_lc_origin(A) ->
    A_1 = [{A_3, B}
	   || B <- [begin A_1 = A + 2, A_1 end],
	      begin A_1 = A + 3, A_1, true end,
	      A_2 <- [begin A_2 = A_1 + 1, A_2 end],
              is_ok(A_2 + 3, begin A_3 = A_2 + 4, A_3 end),
	      true],
    A_1.

test_function(A) ->
    B = 10,
    B = ok(begin A = A + 1, A end, begin A = A + B, A end),
    B = bind(A + B,
             fun(A) -> A = A + B, A end),
    A = B + 1,
    A = A + 1,
    A.

test_function_origin(A) ->
    B = 10,
    B_1 = ok(begin A_1 = A + 1, A_1 end,
	     begin A_2 = A + B, A_2 end),
    B_2 = bind(begin A_3 = A_2 + B_1, A_3 end,
	       fun (A_4) -> A_5 = A_4 + B_1, A_5 end),
    A_4 = B_2 + 1,
    A_5 = A_4 + 1,
    A_5.

test_case(A) ->
    B = 15,
    {A, B} = {B, A},
    A = A + B,
    B = A + B,
    B = case A of
            10 -> A = A + 1, A = A + 1, A;
            +B ->  B = A + 1, A = B + 1, A;
            +A ->  B = A + B, B
        end,
    B = A + B,
    A = A + B,
    A.

test_case_pinned(A) ->
    B = 15,
    {A, B} = {B, A},
    A = A + B,
    B = A + B,
    B = case A of
            10 -> A = A + 1, A = A + 1, A;
            B -> B = A + 1, A = B + 1, A;
            A -> B = A + B, B
        end,
    B = A + B,
    A = A + B,
    A.

test_case_origin(A) ->
    B = 15,
    {A_1, B_1} = {B, A},
    A_2 = A_1 + B_1,
    B_2 = A_2 + B_1,
    B_4 = case A_2 of
	    10 ->  A_3 = A_2 + 1,   A_4 = A_3 + 1, A_4;
	    B_2 -> B_3 = A_2 + 1,   A_3 = B_3 + 1, A_3;
	    A_2 -> B_3 = A_2 + B_2, B_3
	  end,
    B_5 = A_2 + B_4,
    A_5 = A_2 + B_5,
    A_5.

test_try() ->
    A = 10,
    try 
        A
    catch
        Class:Exception?CAPTURE_STACKTRACE ->
            erlang:raise(Class, Exception, ?GET_STACKTRACE)
    end.

test_operator(A) ->
    A = (begin A = A + 1, A end + begin A = A + 1, A end),
    A.

test_tuple(A) ->
    {B, C} = {begin A = A + 1, A end, 
              begin A = A + 2, A end},
    {A, B, C}.

test_map(A) ->
    B = #{b => begin A = A + 1, A end, 
          c => begin A = A + 2, A end},
    B#{a => A}.

test_map_update(A) ->
    K = #{},
    B = (begin K = K#{d => begin A = A + 3, A end}, K end)#{b => begin A = A + 1, A end, c => begin A = A + 2, A end},
    B#{a => A}.

test_rec(A) ->
    B = #rec{b = begin A = A + 1, A end, 
             c = begin A = A + 2, A end},
    B#rec{a = A}.

test_rec_update(A) ->
    K = #rec{},
    B = (begin K = K#rec{d = begin A = A + 3, A end}, K end)#rec{b = begin A = A + 1, A end, c = begin A = A + 2, A end},
    B#rec{a = A}.

test_list(A) ->
    B = [begin A = A + 1, A end, 
         begin A = A + 2, A end],
    [A|B].

test_operator_origin(A) ->
    A_3 = begin A_1 = A + 1, A_1 end + begin A_2 = A + 1, A_2 end,
    A_3.

test_tuple_origin(A) ->
    {B, C} = {begin A_1 = A + 1, A_1 end,
	      begin A_2 = A + 2, A_2 end},
    {A_2, B, C}.

test_map_origin(A) ->
    B = #{b => begin A_1 = A + 1, A_1 end,
	  c => begin A_2 = A + 2, A_2 end},
    B#{a => A_2}.

test_map_update_origin(A) ->
    K = #{},
    B = begin
	  K_1 = K#{d => begin A_1 = A + 3, A_1 end}, K_1
	end#{b => begin A_2 = A + 1, A_2 end,
	     c => begin A_3 = A + 2, A_3 end},
    B#{a => A_3}.

test_rec_origin(A) ->
    B = #rec{b = begin A_1 = A + 1, A_1 end,
	     c = begin A_2 = A + 2, A_2 end},
    B#rec{a = A_2}.

test_rec_update_origin(A) ->
    K = #rec{},
    B = begin
	  K_1 = K#rec{d = begin A_1 = A + 3, A_1 end}, K_1
	end#rec{b = begin A_2 = A + 1, A_2 end,
		c = begin A_3 = A + 2, A_3 end},
    B#rec{a = A_3}.

test_list_origin(A) ->
    B = [begin A_1 = A + 1, A_1 end,
	 begin A_2 = A + 2, A_2 end],
    [A_2 | B].

test_function_guard(As) when is_list(As) ->
    As.

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

ok(A_1, A_2) ->
    A_1 + A_2.

bind(A_1, K) ->
    K(A_1 + 10).

%% test_errors_1(A) ->
%%     [begin A_1 = A + 1, A_1 end, begin A_2 = A_1 + 1, A_2 end].

%% test_errors_2(A) ->
%%     [begin A_1 = A + 1, A_1 end|begin A_2 = A_1 + 1, A_2 end].

%% test_errors_3(A) ->
%%     (begin A_1 = A + 1, A_1 end + begin A_2 = A_1 + 1, A_2 end).

%% test_errors_4(A) ->
%%     {begin A_1 = A + 1, A_1 end, begin A_2 = A_1 + 1, A_2 end}.

%% test_errors_5(A) ->
%%     #{a => begin A_1 = A + 1, A_1 end, b => begin A_2 = A_1 + 1, A_2 end}.

%% test_errors_5(A) ->
%%     X = #{},
%%     (begin X_2 = X#{a => A}, X end)#{a => begin A_1 = A + 1, A_1 end, b => begin A_2 = X, A_2 end},
%%     X_2.

%% test_errors_5(A) ->
%%     begin A_1 = A + 1, A_1 end,
%%     begin A_2 = A_1 + 1, A_2 end.

