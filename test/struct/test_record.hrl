-record(test, {name, value = list_to_binary("world"), enable = true, desc}).
-record(test2, {name = hello, value}).
-record(test3, {name = hello, value}).

-astranaut_struct({test, [{enforce_keys, [name]}, non_auto_fill]}).
-astranaut_struct({test3, [{enforce_keys, [name]}, non_auto_fill]}).
