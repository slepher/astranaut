-module(macro_export_local_options_warning_test).

-include("macro.hrl").

-baseline(yep).

-export_macro({exported/0,
               [{closure_roots, [{missing, 0}]},
                {internal_function, true}]}).

exported() ->
    ok.
