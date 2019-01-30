-module(disable_tco_example).
-compile({parse_transform, astranaut_disable_tco}).
-export([f/1,a/1]).

% Auxilary function to print stack trace (by throwing an exception).

s(X) when X < 0 -> 0.
 
% Example 1: Stack trace in presence of a higher order function.
% > disable_tco_example:f(1).

% before disable tco
% ** exception error: no function clause matching disable_tco_example:s(1) (src/disable_tco_example.erl, line 9)
%     in function  disable_tco_example:g/2 (src/disable_tco_example.erl, line 28)

% after disable tco
% ** exception error: no function clause matching disable_tco_example:s(1) (src/disable_tco_example.erl, line 9)
%     in function  disable_tco_example:g/2 (src/disable_tco_example.erl, line 28)
%     in call from disable_tco_example:'-f/1-fun-0-'/2 (src/disable_tco_example.erl, line 25)
%     in call from disable_tco_example:f/1 (src/disable_tco_example.erl, line 26)

f(X) -> 
    H = fun (I, A) -> I(A, 3) end,
    H(fun g/2,X).
g(X,Y) -> 
    s(X) - Y.

% Example 2: Stack trace for chain of 1st order function applications.
% > test:a(1).           

% before disable tco
% ** exception error: no function clause matching disable_tco_example:s(7) (src/disable_tco_example.erl, line 9)

% after disable tco
% ** exception error: no function clause matching disable_tco_example:s(7) (src/disable_tco_example.erl, line 9)
%      in function  disable_tco_example:c/1 (src/disable_tco_example.erl, line 44)
%      in call from disable_tco_example:b/1 (src/disable_tco_example.erl, line 43)
%      in call from disable_tco_example:a/1 (src/disable_tco_example.erl, line 42)

a(X) -> b(X + 1).
b(X) -> c(X + 2).
c(X) -> s(X + 3).
