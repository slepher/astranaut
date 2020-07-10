-record(test, {name, value = list_to_binary("world"), enable = true, desc}).
-record(test2, {name = hello, value}).

-astranaut_struct({[test], [{enforce_keys, [name]}, non_auto_fill]}).
