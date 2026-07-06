%%%-------------------------------------------------------------------
%%% Test module importing macros from macro_node_roles.
%%%-------------------------------------------------------------------
-module(macro_node_role_test).

-include("quote.hrl").
-include("macro.hrl").

-export([test_node_roles/0]).

-import_macro(macro_node_roles).

test_node_roles() ->
    _A = macro_node_roles:gen_lc(),
    ok.

