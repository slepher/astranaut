-ifndef(ASTRANAUT_DO).
-define(ASTRANAUT_DO, true).
-compile({parse_transform, astranaut_do_transformer}).
-endif.
