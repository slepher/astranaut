%%% External providers without format_error/1 warn once and still expand.
-module(macro_missing_formatter_external_test).

-include("macro.hrl").

-export([value/0]).

-baseline(yep).

-import_macro(macro_missing_formatter_provider).
-import_macro(macro_missing_formatter_provider).
-import_macro(macro_only_v2_formatter_provider).

-use_macro({macro_missing_formatter_provider, emit/0, [alias]}).
-use_macro({macro_only_v2_formatter_provider, emit_v2/0, [alias]}).

value() ->
    {emit(), emit_v2(), emit()}.
