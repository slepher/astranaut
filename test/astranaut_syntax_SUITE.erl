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
     test_validate_invalid_node].

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

function_form() ->
    {function, 1, foo, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.
