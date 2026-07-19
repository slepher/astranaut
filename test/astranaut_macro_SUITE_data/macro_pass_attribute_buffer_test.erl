-module(macro_pass_attribute_buffer_test).

-include("macro.hrl").

-export([value/0]).

-macro_options([{max_depth, 2}]).
-import_macro(macro_pass_depth).

-buffer_chain(ok).

value() ->
    {buffer_head(), buffer_tail()}.
