-module(astranaut_struct_invalid_name_fail).

-include_lib("astranaut/include/struct.hrl").

-record(test, {name}).
-astranaut_struct(42).
