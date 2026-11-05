-module(astranaut_struct_types).
-include_lib("astranaut/include/struct.hrl").
-record(full, {required :: integer(), optional :: integer(), defaulted = 1 :: integer(), untyped}).
-record(sparse, {
    required :: integer(), optional :: integer(), defaulted = 1 :: integer(), untyped
}).
-record(outer, {child :: #full{}}).
-record(plain, {value}).
-record(existing, {}).
-astranaut_struct({full, [export_type, {enforce_keys, [required]}]}).
-astranaut_struct(
    {sparse, [non_auto_fill, export_type, {type_name, sparse_type}, {enforce_keys, [required]}]}
).
-astranaut_struct([outer]).
-astranaut_struct({existing, [export_type]}).
-type existing() :: integer().
-type override() :: #sparse{optional :: binary()}.
-opaque nested() :: #outer{child :: #full{}}.
-type plain_type() :: #plain{}.
-export_type([override/0, nested/0, plain_type/0]).
-export([new/0]).
-spec new() -> #full{}.
new() -> #full{required = 1}.
-callback handle(#outer{}) -> #sparse{} | #existing{}.
