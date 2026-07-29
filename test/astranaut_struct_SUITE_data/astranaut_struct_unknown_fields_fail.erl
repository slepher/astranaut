-module(astranaut_struct_unknown_fields_fail).

-include_lib("astranaut/include/struct.hrl").

-record(test, {name, value}).
-astranaut_struct(test).

-export([bad_new/0, bad_index/0, bad_access/1]).

-spec bad_new() -> #test{unknown :: atom()}.
bad_new() ->
    #test{unknown = value}.

bad_index() ->
    #test.unknown.

bad_access(Test) ->
    Test#test.unknown.
