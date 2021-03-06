%% just used in astranaut project, should not use outside.
-ifndef(ASTRANANUT_STRUCT_NAME).
-define(ASTRANANUT_STRUCT_NAME, true).
%% for '__struct__' in map
-define(STRUCT_KEY, '__struct__').
%% for astranaut_walk_return.
-define(WALK_RETURN, astranaut_walk_return).
%% for endo
-define(ENDO, astranaut_endo).
%% for astranaut_base_m.
-define(BASE_M, astranaut_base_m).
%% for astranaut_return_m.
-define(RETURN_OK, astranaut_return_ok).
-define(RETURN_FAIL, astranaut_return_fail).
%% for astranaut_traverse_m.
-define(TRAVERSE_M, astranaut_traverse_m).
%% for astranaut_error_state.
-define(ERROR_STATE, astranaut_error_state).
%% for astranaut_error_ctx.
-define(ERROR_CTX, astranaut_error_ctx).
-endif.
