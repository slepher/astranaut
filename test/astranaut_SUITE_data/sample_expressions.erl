-module(sample_expressions).

-compile(export_all).
-compile(nowarn_export_all).

-include("stacktrace.hrl").

-record(test, {a, b, c}).

list() ->
    [1, 2, 3].

tuple() ->
    {1, 2, 3}.

map() ->
    A = #{a => 1, b => 2},
    B = A#{a => 2, b => 3},
    #{a := C, b := D} = B,
    {B, C, D}.

record() ->
    A = #test{a = 1, b = 2},
    B = A#test{a = 2, b = 3},
    C = #test.a,
    D = A#test.a,
    #test{a = E} = A,
    {A, B, C, D, E}.

if_expr(A) ->
    if
        A == 2 -> a;
        A == 1 -> b;
        true -> c
    end.

case_expr(A) ->
    case A of
        10 -> a;
        20 -> b;
        C when (C =/= 10), (C =/= 20), (C == 30); (C == 40) ; (C == 50)->
            c;
        A ->
            d
    end.

try_catch_expr(A) ->
    try A of
        10 ->
            10;
        A ->
            A
    catch
        Error:badarg?CAPTURE_STACKTRACE ->
            erlang:raise(Error, badarg, ?GET_STACKTRACE);
        Error:Exception?CAPTURE_STACKTRACE ->
            erlang:raise(Error, Exception, ?GET_STACKTRACE)
    after
        ok
    end.
