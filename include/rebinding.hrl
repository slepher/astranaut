-ifndef(ASTRANAUT_REBINDING).
-define(ASTRANAUT_REBINDING, true).
-compile({parse_transform, astranaut_rebinding}).
-endif.