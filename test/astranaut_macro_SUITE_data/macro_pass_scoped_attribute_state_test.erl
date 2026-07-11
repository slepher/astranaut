%%%-------------------------------------------------------------------
%%% A stateful attribute macro must not overwrite the unified scanner State.
%%% The following attribute expansion proves scanning can continue normally.
%%%-------------------------------------------------------------------
-module(macro_pass_scoped_attribute_state_test).

-include("macro.hrl").

-export([stateful_value/0, generated_helper/0]).

-import_macro(macro_pass_external_helper).

-stateful_attribute(ok).
-generate_helper(ok).
