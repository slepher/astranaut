-module(astranaut_forms_tests).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

test_sort_forms_test() ->
    Function = function(foo, {atom, 1, ok}),
    Forms = [Function, module_form(), eof_form()],
    ?assertEqual(
        [module_form(), Function, eof_form()],
        astranaut_forms:sort_forms(Forms)
    ).

test_insert_forms_original_test() ->
    Original = function(foo, {atom, 1, original}),
    Replacement = function(foo, {call, 1, {atom, 1, '__original__'}, []}),
    Forms = astranaut_forms:insert_forms(
        [Replacement], [module_form(), Original, eof_form()]
    ),
    ?assertMatch(
        [
            {attribute, 1, module, example},
            {function, 1, foo_1, 0, _},
            {function, 1, foo, 0, [{clause, 1, [], [], [{call, 1, {atom, 1, foo_1}, []}]}]},
            {eof, 1}
        ],
        Forms
    ).

module_form() ->
    {attribute, 1, module, example}.

eof_form() ->
    {eof, 1}.

function(Name, Body) ->
    {function, 1, Name, 0, [{clause, 1, [], [], [Body]}]}.
