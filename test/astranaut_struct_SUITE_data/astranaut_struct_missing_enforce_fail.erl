-module(astranaut_struct_missing_enforce_fail).

-include_lib("astranaut/include/struct.hrl").

-record(test, {name, value = default}).
-astranaut_struct({test, [{enforce_keys, [name]}]}).

-export([bad_new/0]).

bad_new() ->
    #test{}.
