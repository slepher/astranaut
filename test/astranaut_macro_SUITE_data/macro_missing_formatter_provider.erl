%%% External macro provider deliberately without format_error/1.
-module(macro_missing_formatter_provider).

-include("quote.hrl").
-include("macro.hrl").

-export_macro([emit/0]).

emit() ->
    quote({missing_external, ok}).
