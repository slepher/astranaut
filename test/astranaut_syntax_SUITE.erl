%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2026, Chen Slepher
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(astranaut_syntax_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    erlang:system_flag(backtrace_depth, 20),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [test_validate_expression,
     test_validate_form_list,
     test_validate_root_role_error,
     test_validate_child_role_error,
     test_validate_invalid_node,
     %% form role
     test_validate_form_accept, test_validate_form_reject,
     %% expression role
     test_validate_expression_accept_more, test_validate_expression_reject,
     %% pattern role
     test_validate_pattern_accept, test_validate_pattern_reject,
     %% guard role
     test_validate_guard_accept, test_validate_guard_reject,
     %% type role
     test_validate_type_accept, test_validate_type_reject,
     %% clause role
     test_validate_clause_accept, test_validate_clause_reject,
     %% name role
     test_validate_name_accept, test_validate_name_reject,
     %% attribute_body role (always allowed)
     test_validate_attribute_body_accept,
     %% new types from node_roles additions (using erl_parse format)
     test_validate_list_comp,
     %% edge cases
     test_validate_nested_valid, test_validate_nested_invalid,
     test_validate_empty_list].

%%--------------------------------------------------------------------
%% original basic tests
%%--------------------------------------------------------------------

test_validate_expression(_Config) ->
    Expr = {call, 1, {atom, 1, foo}, [{integer, 1, 1}]},
    ?assertEqual(ok, astranaut_syntax:validate(Expr, expression)).

test_validate_form_list(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate([function_form()], form)).

test_validate_root_role_error(_Config) ->
    {error, Error} = astranaut_syntax:validate(function_form(), expression),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := expression,
                   actual_type := function,
                   slot := root},
                 Error).

test_validate_child_role_error(_Config) ->
    Expr = {match, 1, function_form(), {atom, 1, ok}},
    {error, Error} = astranaut_syntax:validate(Expr, expression),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := pattern,
                   actual_type := function,
                   slot := left,
                   parent_type := match_expr},
                 Error),
    #{path := Path} = Error,
    ?assertMatch([#{slot := left, index := 1, expected_role := pattern, type := function}], Path).

test_validate_invalid_node(_Config) ->
    {error, Error} = astranaut_syntax:validate({bad_ast}, expression),
    ?assertMatch(#{reason := invalid_node,
                   expected_role := expression,
                   slot := root},
                 Error).

%%--------------------------------------------------------------------
%% form role — function, attribute, attribute_body_role=spec
%%--------------------------------------------------------------------

test_validate_form_accept(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate(function_form(), form)),
    ?assertEqual(ok, astranaut_syntax:validate({attribute, 1, custom, [{atom, 1, val}]}, form)),
    ?assertEqual(ok, astranaut_syntax:validate({eof, 1}, form)).

test_validate_form_reject(_Config) ->
    {error, #{reason := invalid_role}} = astranaut_syntax:validate({atom, 1, ok}, form),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {call, 1, {atom, 1, f}, []}, form),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {clause, 1, [], [], [{atom, 1, ok}]}, form).

%%--------------------------------------------------------------------
%% expression role
%%--------------------------------------------------------------------

test_validate_expression_accept_more(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate({atom, 1, ok}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {call, 1, {atom, 1, f}, [{atom, 1, x}]}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {match, 1, {atom, 1, a}, {atom, 1, b}}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {'case', 1, {atom, 1, x}, [{clause, 1, [{atom, 1, x}], [], [{atom, 1, ok}]}]}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate({integer, 1, 42}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {'if', 1, [{clause, 1, [], [], [{atom, 1, true}]}]}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {'receive', 1, [{clause, 1, [{atom, 1, msg}], [], [{atom, 1, ok}]}]}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {'try', 1, [{atom, 1, body}], [], [], []}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {'catch', 1, {atom, 1, expr}}, expression)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {block, 1, [{atom, 1, a}, {atom, 1, b}]}, expression)).

test_validate_expression_reject(_Config) ->
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(function_form(), expression),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {clause, 1, [], [], [{atom, 1, ok}]}, expression).

%%--------------------------------------------------------------------
%% pattern role
%%--------------------------------------------------------------------

test_validate_pattern_accept(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate({atom, 1, ok}, pattern)),
    ?assertEqual(ok, astranaut_syntax:validate({var, 1, 'X'}, pattern)),
    ?assertEqual(ok, astranaut_syntax:validate({var, 1, '_'}, pattern)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {tuple, 1, [{atom, 1, a}, {var, 1, 'B'}]}, pattern)),
    ?assertEqual(ok, astranaut_syntax:validate({nil, 1}, pattern)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {cons, 1, {var, 1, 'H'}, {var, 1, 'T'}}, pattern)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {bin, 1, [{bin_element, 1, {var, 1, 'X'}, {integer, 1, 8}, default}]}, pattern)).

test_validate_pattern_reject(_Config) ->
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {call, 1, {atom, 1, f}, [{atom, 1, x}]}, pattern),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {match, 1, {atom, 1, a}, {atom, 1, b}}, pattern),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(function_form(), pattern).

%%--------------------------------------------------------------------
%% guard role
%%--------------------------------------------------------------------

test_validate_guard_accept(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate({atom, 1, true}, guard)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {call, 1, {atom, 1, is_integer}, [{var, 1, 'X'}]}, guard)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {op, 1, '>', {var, 1, 'X'}, {integer, 1, 0}}, guard)),
    ?assertEqual(ok, astranaut_syntax:validate({integer, 1, 42}, guard)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, '+'}}, [{var, 1, 'A'}, {var, 1, 'B'}]}, guard)).

test_validate_guard_reject(_Config) ->
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {match, 1, {atom, 1, a}, {atom, 1, b}}, guard),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(function_form(), guard).

%%--------------------------------------------------------------------
%% type role — use erl_parse format: {type, Pos, Name, Args}
%%--------------------------------------------------------------------

test_validate_type_accept(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate({nil, 1}, type)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {tuple, 1, [{atom, 1, a}, {atom, 1, b}]}, type)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {type, 1, integer, []}, type)).

test_validate_type_reject(_Config) ->
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(function_form(), type),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate({atom, 1, ok}, type),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {call, 1, {atom, 1, f}, []}, type).

%%--------------------------------------------------------------------
%% clause role
%%--------------------------------------------------------------------

test_validate_clause_accept(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate(
        {clause, 1, [{atom, 1, a}], [], [{atom, 1, ok}]}, clause)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {clause, 1, [{var, 1, 'X'}], [], [{atom, 1, ok}]}, clause)).

test_validate_clause_reject(_Config) ->
    {error, #{reason := invalid_role}} = astranaut_syntax:validate({atom, 1, ok}, clause),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {call, 1, {atom, 1, f}, []}, clause),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(function_form(), clause).

%%--------------------------------------------------------------------
%% name role — only atom is valid
%%--------------------------------------------------------------------

test_validate_name_accept(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate({atom, 1, foo}, name)).

test_validate_name_reject(_Config) ->
    {error, #{reason := invalid_role}} = astranaut_syntax:validate({var, 1, 'X'}, name),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate({integer, 1, 42}, name),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(function_form(), name).

%%--------------------------------------------------------------------
%% attribute_body role (always allowed regardless of node type)
%%--------------------------------------------------------------------

test_validate_attribute_body_accept(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate({atom, 1, ok}, attribute_body)),
    ?assertEqual(ok, astranaut_syntax:validate(function_form(), attribute_body)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {call, 1, {atom, 1, f}, []}, attribute_body)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {clause, 1, [], [], [{atom, 1, ok}]}, attribute_body)),
    ?assertEqual(ok, astranaut_syntax:validate(
        {type, 1, integer, []}, attribute_body)).

%%--------------------------------------------------------------------
%% list_comp — expression-only (from node_roles additions)
%%--------------------------------------------------------------------

test_validate_list_comp(_Config) ->
    Lc = {lc, 1, {atom, 1, ok}, [{generate, 1, {var, 1, 'X'}, {nil, 1}}]},
    ?assertEqual(ok, astranaut_syntax:validate(Lc, expression)),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(Lc, form),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(Lc, pattern).

%%--------------------------------------------------------------------
%% edge cases
%%--------------------------------------------------------------------

test_validate_nested_valid(_Config) ->
    Tree = {call, 1,
            {remote, 1, {atom, 1, m}, {atom, 1, f}},
            [{match, 1, {var, 1, 'X'}, {call, 1, {atom, 1, g}, [{integer, 1, 1}]}}]},
    ?assertEqual(ok, astranaut_syntax:validate(Tree, expression)).

test_validate_nested_invalid(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{clause, 1, [], [], [{atom, 1, ok}]}]},
    {error, Error} = astranaut_syntax:validate(Tree, expression),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := expression,
                   actual_type := clause,
                   slot := elements,
                   parent_type := application},
                 Error).

test_validate_empty_list(_Config) ->
    ?assertEqual(ok, astranaut_syntax:validate([], expression)),
    ?assertEqual(ok, astranaut_syntax:validate([], form)),
    ?assertEqual(ok, astranaut_syntax:validate([], pattern)).

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

function_form() ->
    {function, 1, foo, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
