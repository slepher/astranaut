-record(test, {name = hello, value = list_to_binary("world")}).
-record(test2, {name = hello, value}).

-astranaut_struct([test]).
