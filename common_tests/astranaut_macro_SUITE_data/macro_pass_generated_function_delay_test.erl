%%%-------------------------------------------------------------------
%%% Spec scenario: an attribute macro generates a function form whose body
%%% contains a macro call.  The generated body is NOT recursively expanded
%%% during the unified attribute scan; the body is kept intact through the
%%% scan and only expanded later, by the function pass, against the final
%%% external environment.
%%%-------------------------------------------------------------------
-module(macro_pass_generated_function_delay_test).

-include("macro.hrl").

-export([delayed_value/0]).

-import_macro(macro_pass_external_helper).

-generate_delayed_macro_call(ok).

%% This import becomes visible only after the generated function form has
%% passed through the attribute scan.  Its macro call must therefore be
%% expanded by the final function pass, not by the scanner.
-import_macro(macro_uniform_a).
