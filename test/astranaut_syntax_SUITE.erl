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
     test_validate_guard_uses_record_forms,
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
     %% validator metadata and validation scopes
     test_child_specs_include_slot_validators,
     test_all_child_specs_emit_expected_validators,
     test_all_child_spec_validators_accept_and_reject_ast,
     test_slot_validators_reject_wrong_structural_identity,
     test_slot_validators_reject_malformed_structural_nodes,
     test_validate_local_does_not_recurse_grandchildren,
     test_validate_recursive_recurse_grandchildren,
     test_traverse_validate_changed_node,
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
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(
        {call, 1, {atom, 1, helper}, []}, guard),
    {error, #{reason := invalid_role}} = astranaut_syntax:validate(function_form(), guard).

test_validate_guard_uses_record_forms(_Config) ->
    GuardRecord = {record, 1, guard_rec, []},
    ValidRecordForms = [{attribute, 1, record,
                         {guard_rec, [{record_field, 1, {atom, 1, field}, {integer, 1, 1}}]}}],
    InvalidRecordForms = [{attribute, 1, record,
                           {guard_rec, [{record_field, 1, {atom, 1, field},
                                         {call, 1, {atom, 1, helper}, []}}]}}],
    ?assertEqual(ok, astranaut_syntax:validate(GuardRecord, guard,
                                               #{forms => ValidRecordForms})),
    {error, Error} = astranaut_syntax:validate(GuardRecord, guard,
                                               #{forms => InvalidRecordForms}),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := guard,
                   actual_type := record_expr},
                 Error).

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
%% validator metadata and validation scopes
%%--------------------------------------------------------------------

test_child_specs_include_slot_validators(_Config) ->
    Pattern = {bin, 1, []},
    Body = {var, 1, 'Bin'},
    Specs = astranaut_syntax:child_specs(binary_generator, [[Pattern], [Body]], #{node => expression}),
    ?assertMatch(
       [#{slot := pattern,
          validator := {slot, binary_generator, pattern, pattern},
          attr := #{validator := {slot, binary_generator, pattern, pattern},
                    parent_type := binary_generator,
                    parent_slot := pattern}},
        #{slot := body,
          validator := {slot, binary_generator, body, expression},
          attr := #{validator := {slot, binary_generator, body, expression},
                    parent_type := binary_generator,
                    parent_slot := body}}],
       Specs).

test_all_child_specs_emit_expected_validators(_Config) ->
    lists:foreach(
      fun({ParentType, Subtrees, Attr, ExpectedSlots}) ->
              Specs = astranaut_syntax:child_specs(ParentType, Subtrees, Attr),
              ActualSlots = [{Slot, Role, Validator, maps:with([node, validator, parent_type, parent_slot], SpecAttr)}
                             || #{slot := Slot, role := Role, validator := Validator, attr := SpecAttr} <- Specs],
              Expected =
                  [{Slot, Role, {slot, ParentType, Slot, Role},
                    #{node => Role,
                      validator => {slot, ParentType, Slot, Role},
                      parent_type => ParentType,
                      parent_slot => Slot}}
                   || {Slot, Role} <- ExpectedSlots],
              ?assertEqual(Expected, ActualSlots)
      end, child_spec_cases()).

test_all_child_spec_validators_accept_and_reject_ast(_Config) ->
    Validators =
        lists:append(
          [[Validator || #{validator := Validator} <- astranaut_syntax:child_specs(ParentType, Subtrees, Attr)]
           || {ParentType, Subtrees, Attr, _ExpectedSlots} <- child_spec_cases()]),
    lists:foreach(
      fun(Validator) ->
              Role = validator_role(Validator),
              ?assertEqual(ok, astranaut_syntax:validate_local(valid_ast(Role), Validator)),
              case invalid_ast(Role) of
                  none ->
                      ok;
                  InvalidAst ->
                      {error, #{reason := invalid_role,
                                validator := Validator,
                                expected_role := Role}} =
                          astranaut_syntax:validate_local(InvalidAst, Validator)
              end
      end, Validators).

test_slot_validators_reject_wrong_structural_identity(_Config) ->
    BinElement = valid_ast(binary_field),
    MapField = {map_field_exact, 1, {atom, 1, key}, {atom, 1, value}},
    ?assertEqual(ok, astranaut_syntax:validate_local(BinElement, {slot, binary, elements, binary_field})),
    ?assertEqual(ok, astranaut_syntax:validate_local({atom, 1, value},
                                                     {slot, map_field_exact, map_field_exact_value, expression})),
    {error, BinError} = astranaut_syntax:validate_local(BinElement, {slot, tuple, elements, expression}),
    ?assertMatch(#{reason := invalid_role,
                   validator := {slot, tuple, elements, expression},
                   actual_type := binary_field},
                 BinError),
    {error, MapError} = astranaut_syntax:validate_local(MapField,
                                                        {slot, map_field_exact,
                                                         map_field_exact_value, expression}),
    ?assertMatch(#{reason := invalid_role,
                   validator := {slot, map_field_exact, map_field_exact_value, expression},
                   actual_type := map_field_exact},
                 MapError).

test_slot_validators_reject_malformed_structural_nodes(_Config) ->
    MalformedMapField = {map_field_exact, 1, {atom, 1, key}},
    MalformedBinElement = {bin_element, 1, {atom, 1, value}},
    {error, MapError} = astranaut_syntax:validate_local(MalformedMapField,
                                                        {slot, map_expr, fields, map_field}),
    ?assertMatch(#{reason := invalid_node,
                   validator := {slot, map_expr, fields, map_field}},
                 MapError),
    {error, BinError} = astranaut_syntax:validate_local(MalformedBinElement,
                                                        {slot, binary, elements, binary_field}),
    ?assertMatch(#{reason := invalid_node,
                   validator := {slot, binary, elements, binary_field}},
                 BinError).

test_validate_local_does_not_recurse_grandchildren(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{tuple, 1, [{clause, 1, [], [], [{atom, 1, ok}]}]}]},
    ?assertEqual(ok, astranaut_syntax:validate_local(Tree, {role, expression})).

test_validate_recursive_recurse_grandchildren(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{tuple, 1, [{clause, 1, [], [], [{atom, 1, ok}]}]}]},
    {error, Error} = astranaut_syntax:validate_recursive(Tree, {role, expression}),
    ?assertMatch(#{reason := invalid_role,
                   actual_type := clause,
                   parent_type := tuple},
                 Error).

test_traverse_validate_changed_node(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{atom, 1, bad}]},
    try astranaut:smap(
          fun({atom, 1, bad}) -> function_form();
             (Node) -> Node
          end, Tree, #{traverse => pre, validate => true}) of
        _ -> error(unexpected_ok)
    catch
        error:{invalid_transform_validation, #{reason := invalid_role,
                                               actual_type := function}} ->
            ok
    end.

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

child_spec_cases() ->
    Pattern = valid_ast(pattern),
    Expr = valid_ast(expression),
    Guard = valid_ast(guard),
    MapField = valid_ast(map_field),
    Clause = valid_ast(clause),
    Form = function_form(),
    Name = valid_ast(name),
    Type = valid_ast(type),
    AttributeBody = valid_ast(attribute_body),
    [{named_fun_expr, [[Name], [Clause]], #{node => expression},
      [{name, pattern}, {clauses, clause}]},
     {match_expr, [[Pattern], [Expr]], #{node => expression},
      [{left, pattern}, {right, expression}]},
     {maybe_match_expr, [[Pattern], [Expr]], #{node => expression},
      [{left, pattern}, {right, expression}]},
     {clause, [[Pattern], [Expr]], #{node => clause},
     [{patterns, pattern}, {body, expression}]},
     {clause, [[Pattern], [Guard], [Expr]], #{node => clause},
      [{patterns, pattern}, {guards, guard}, {body, expression}]},
     {binary, [[valid_ast(binary_field)]], #{node => expression},
      [{elements, binary_field}]},
     {map_expr, [[MapField]], #{node => expression},
      [{fields, map_field}]},
     {map_expr, [[Expr], [MapField]], #{node => expression},
      [{argument, expression}, {fields, map_field}]},
     {map_expr, [[MapField]], #{node => pattern},
      [{fields, map_field}]},
     {map_field_assoc, [[Expr], [Expr]], #{node => expression},
      [{map_field_assoc_key, expression}, {map_field_assoc_value, expression}]},
     {map_field_exact, [[Expr], [Expr]], #{node => expression},
      [{map_field_exact_key, expression}, {map_field_exact_value, expression}]},
     {map_field_exact, [[Expr], [Pattern]], #{node => pattern},
      [{map_field_exact_key, expression}, {map_field_exact_value, pattern}]},
     {generator, [[Pattern], [Expr]], #{node => expression},
      [{pattern, pattern}, {body, expression}]},
     {strict_generator, [[Pattern], [Expr]], #{node => expression},
      [{pattern, pattern}, {body, expression}]},
     {binary_generator, [[Pattern], [Expr]], #{node => expression},
      [{pattern, pattern}, {body, expression}]},
     {strict_binary_generator, [[Pattern], [Expr]], #{node => expression},
      [{pattern, pattern}, {body, expression}]},
     {map_generator, [[Pattern], [Expr]], #{node => expression},
      [{pattern, pattern}, {body, expression}]},
     {strict_map_generator, [[Pattern], [Expr]], #{node => expression},
      [{pattern, pattern}, {body, expression}]},
     {fun_expr, [[Clause]], #{node => expression},
      [{clauses, clause}]},
     {case_expr, [[Expr], [Clause]], #{node => expression},
      [{argument, expression}, {clauses, clause}]},
     {if_expr, [[Clause]], #{node => expression},
      [{clauses, clause}]},
     {receive_expr, [[Clause]], #{node => expression},
      [{clauses, clause}]},
     {receive_expr, [[Clause], [Expr], [Expr]], #{node => expression},
      [{clauses, clause}, {timeout, expression}, {action, expression}]},
     {try_expr, [[Expr], [Clause], [Clause], [Expr]], #{node => expression},
      [{body, expression}, {clauses, clause}, {handlers, clause}, {'after', expression}]},
     {try_expr, [[Expr], [Clause], [Clause]], #{node => expression},
      [{body, expression}, {clauses, clause}, {handlers, clause}]},
     {function, [[Name], [Clause]], #{node => form},
      [{name, name}, {clauses, clause}]},
     {function, [[Clause]], #{node => form},
      [{clauses, clause}]},
     {form_list, [[Form]], #{node => form},
      [{forms, form}]},
     {attribute, [[{atom, 1, custom}], [AttributeBody]], #{node => form},
      [{name, name}, {body, attribute_body}]},
     {attribute, [[{atom, 1, type}], [Name, Type]], #{node => form},
      [{name, name}, {body, type}]},
     {attribute, [[{atom, 1, opaque}], [Name, Type]], #{node => form},
      [{name, name}, {body, type}]},
     {attribute, [[{atom, 1, spec}], [Name, Type]], #{node => form},
      [{name, name}, {body, type}]},
     {attribute, [[{atom, 1, callback}], [Name, Type]], #{node => form},
      [{name, name}, {body, type}]},
     {list_comp, [[Expr], [Expr]], #{node => expression},
      [{template, expression}, {body, expression}]},
     {map_comp, [[Expr], [Expr]], #{node => expression},
      [{template, expression}, {body, expression}]},
     {binary_comp, [[Expr], [Expr]], #{node => expression},
      [{template, expression}, {body, expression}]},
     {maybe_expr, [[Expr]], #{node => expression},
      [{body, expression}]},
     {maybe_expr, [[Expr], [Clause]], #{node => expression},
      [{body, expression}, {else_clause, clause}]},
     {implicit_fun, [[Expr]], #{node => expression},
      [{name, expression}]},
     {record_access, [[Expr], [Expr], [Expr]], #{node => expression},
      [{argument, expression}, {field, expression}, {type, expression}]},
     {zip_generator, [[Expr]], #{node => expression},
      [{body, expression}]},
     {tuple, [[Pattern]], #{node => pattern},
      [{elements, pattern}]},
     {tuple, [[Expr]], #{node => expression},
      [{elements, expression}]},
     {tuple, [[Expr]], #{},
      [{elements, expression}]}].

validator_role({role, Role}) ->
    Role;
validator_role({slot, _ParentType, _Slot, Role}) ->
    Role.

valid_ast(expression) ->
    {atom, 1, ok};
valid_ast(pattern) ->
    {var, 1, 'X'};
valid_ast(guard) ->
    {op, 1, '>', {var, 1, 'X'}, {integer, 1, 0}};
valid_ast(map_field) ->
    {map_field_assoc, 1, {atom, 1, key}, {atom, 1, value}};
valid_ast(binary_field) ->
    {bin_element, 1, {atom, 1, value}, {integer, 1, 8}, default};
valid_ast(clause) ->
    {clause, 1, [{var, 1, 'X'}], [], [{atom, 1, ok}]};
valid_ast(form) ->
    function_form();
valid_ast(name) ->
    {atom, 1, foo};
valid_ast(type) ->
    {type, 1, integer, []};
valid_ast(attribute_body) ->
    {atom, 1, ok}.

invalid_ast(expression) ->
    function_form();
invalid_ast(pattern) ->
    {call, 1, {atom, 1, f}, []};
invalid_ast(guard) ->
    {match, 1, {var, 1, 'X'}, {integer, 1, 1}};
invalid_ast(map_field) ->
    {atom, 1, not_a_map_field};
invalid_ast(binary_field) ->
    {atom, 1, not_a_binary_field};
invalid_ast(clause) ->
    {atom, 1, ok};
invalid_ast(form) ->
    {atom, 1, ok};
invalid_ast(name) ->
    {var, 1, 'X'};
invalid_ast(type) ->
    {atom, 1, ok};
invalid_ast(attribute_body) ->
    none.
