-module(astranaut_forms_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [test_sort_forms,
     test_insert_forms_original,
     test_syntax_compatibility_proxies].

test_sort_forms(_Config) ->
    Function = function(foo, {atom, 1, ok}),
    Forms = [Function, module_form(), eof_form()],
    ?assertEqual([module_form(), Function, eof_form()],
                 astranaut_forms:sort_forms(Forms)).

test_insert_forms_original(_Config) ->
    Original = function(foo, {atom, 1, original}),
    Replacement = function(foo, {call, 1, {atom, 1, '__original__'}, []}),
    Forms = astranaut_forms:insert_forms(
              [Replacement], [module_form(), Original, eof_form()]),
    ?assertMatch(
       [{attribute, 1, module, example},
        {function, 1, foo_1, 0, _},
        {function, 1, foo, 0,
         [{clause, 1, [], [], [{call, 1, {atom, 1, foo_1}, []}]}]},
        {eof, 1}],
       Forms).

test_syntax_compatibility_proxies(_Config) ->
    BaseForms = [module_form(), function(foo, {atom, 1, original}), eof_form()],
    NewForms = [function(bar, {atom, 1, generated})],
    ?assertEqual(astranaut_forms:sort_forms(BaseForms),
                 astranaut_syntax:sort_forms(BaseForms)),
    ?assertEqual(astranaut_forms:insert_forms(NewForms, BaseForms),
                 astranaut_syntax:insert_forms(NewForms, BaseForms)),
    UpdatedForms = [{updated, lists:nth(2, BaseForms), NewForms}, eof_form()],
    ?assertEqual(astranaut_forms:reorder_updated_forms(UpdatedForms),
                 astranaut_syntax:reorder_updated_forms(UpdatedForms)).

module_form() ->
    {attribute, 1, module, example}.

eof_form() ->
    {eof, 1}.

function(Name, Body) ->
    {function, 1, Name, 0, [{clause, 1, [], [], [Body]}]}.
