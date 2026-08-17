-module(macro_pass_attribute_buffer_cross_depth_error_test).

-include("macro.hrl").

-baseline(yep).
-macro_options([{max_depth, 1}]).
-import_macro(macro_pass_depth).

-buffer_chain(ok).
