%%% Regression coverage for guarded functions selected for macro expansion.
-module(macro_guard_test).

-include("macro.hrl").

-export([simple/1, complex/1, macro_guard/1]).

-import_macro(macro_guard_macros).

simple(Value) when is_integer(Value) ->
    macro_guard_macros:identity(integer);
simple(_Value) ->
    other.

complex(Value)
  when is_integer(Value), Value > 10, Value < 20;
       is_float(Value), Value > 10.0, Value < 20.0 ->
    macro_guard_macros:identity(in_range);
complex(_Value) ->
    out_of_range.

macro_guard(Value) when macro_guard_macros:is_even(Value) ->
    even;
macro_guard(_Value) ->
    odd.
