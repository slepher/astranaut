-ifndef(ASTRANAUT_COMPILE_META).
-define(ASTRANAUT_COMPILE_META, true).
-compile({parse_transform, astranaut_compile_meta_transformer}).
-endif.
