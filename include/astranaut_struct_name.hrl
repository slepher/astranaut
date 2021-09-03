%% just used in astranaut project, should not use outside.
-ifndef(ASTRANANUT_STRUCT_NAME).
-define(ASTRANANUT_STRUCT_NAME, true).
%% for '__struct__' in map
-define(STRUCT_KEY, '__struct__').
%% for astranaut_walk_return.
-define(WALK_RETURN, astranaut_walk_return).
%% for astranaut_base.
-define(BASE_M, astranaut_base).
%% for astranaut_return_m.
-define(RETURN_OK, astranaut_return_ok).
-define(RETURN_FAIL, astranaut_return_fail).
%% for astranaut_traverse_m.
-define(TRAVERSE_M, astranaut_traverse).
%% for astranaut_error_state.
-define(ERROR_STATE, astranaut_error).
-endif.
