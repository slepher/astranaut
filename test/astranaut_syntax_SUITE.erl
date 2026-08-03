%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2026, Chen Slepher
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(astranaut_syntax_SUITE).

-include("otp_vsn.hrl").

-ifdef(ASTRANAUT_OTP_AT_LEAST_25).
-feature(maybe_expr, enable).
-endif.

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
     test_validate_forms,
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
     test_validate_function_with_real_guard,
     test_validate_guard_uses_record_forms,
     %% type role
     test_validate_type_accept, test_validate_type_reject,
     test_validate_type_singletons,
     test_validate_type_and_spec_attributes,
     test_reject_malformed_type_and_spec_attributes,
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
     test_slot_roles_do_not_replace_node_role,
     test_binary_field_projection_slots,
     test_slot_validators_reject_wrong_structural_identity,
     test_slot_validators_reject_malformed_structural_nodes,
     test_validate_node_does_not_recurse_grandchildren,
     test_normalize_recurses_into_grandchildren,
     test_otp_vsn,
     test_generated_schema_proxy,
     test_try_handler_validation_uses_abstract_format,
     test_legacy_catch_handler,
     test_otp21_stacktrace_catch,
     test_legacy_map_pattern_key,
     test_otp23_map_pattern_key_expression,
     test_legacy_binary_size,
     test_otp23_binary_size_expression,
     test_otp25_maybe_expr,
     test_otp26_map_comprehension,
     test_otp28_strict_generator,
     test_otp29_multiple_comprehension_templates,
     test_otp29_native_record_forms,
     test_otp29_native_record_version,
     test_record_access_child_slot_order,
     test_traverse_validate_changed_node,
     test_traverse_validate_false_opt_out,
     test_traverse_validate_preserves_slot_attr,
     test_traverse_pre_validate_does_not_recurse_grandchildren,
     test_traverse_post_validate_recurse_grandchildren,
     test_traverse_input_validate_runs_before_post_walk,
     test_traverse_input_validate_attribute_body,
     test_traverse_validate_collect_returns_input,
     test_traverse_true_validate_is_output,
     test_traverse_both_validate_includes_input,
     test_read_only_traversals_skip_validation,
     test_map_m_validate_raises,
     %% edge cases
     test_set_pos_warning_keeps_shape,
     test_public_syntax_helpers,
     test_validate_nested_valid, test_validate_nested_invalid,
     test_validate_empty_list].

%%--------------------------------------------------------------------
%% original basic tests
%%--------------------------------------------------------------------

test_validate_expression(_Config) ->
    Expr = {call, 1, {atom, 1, foo}, [{integer, 1, 1}]},
    ?assertEqual(ok, validate(Expr, expression)).

test_validate_forms(_Config) ->
    ?assertEqual(ok, validate([function_form()], form)).

test_validate_root_role_error(_Config) ->
    {error, Error} = validate(function_form(), expression),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := expression,
                   actual_type := function,
                   slot := root},
                 Error).

test_validate_child_role_error(_Config) ->
    Expr = {match, 1, function_form(), {atom, 1, ok}},
    {error, Error} = validate(Expr, expression),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := pattern,
                   actual_type := function,
                   slot := left,
                   parent_type := match_expr},
                 Error),
    #{path := Path} = Error,
    ?assertMatch([#{slot := left, index := 1, expected_role := pattern, type := function}], Path).

test_validate_invalid_node(_Config) ->
    {error, Error} = validate({bad_ast}, expression),
    ?assertMatch(#{reason := invalid_node,
                   expected_role := expression,
                   slot := root},
                 Error).

%%--------------------------------------------------------------------
%% form role — function, attribute, attribute_body_role=spec
%%--------------------------------------------------------------------

test_validate_form_accept(_Config) ->
    ?assertEqual(ok, validate(function_form(), form)),
    ?assertEqual(ok, validate({attribute, 1, custom, [{atom, 1, val}]}, form)),
    ?assertEqual(ok, validate({eof, 1}, form)).

test_validate_form_reject(_Config) ->
    {error, #{reason := invalid_role}} = validate({atom, 1, ok}, form),
    {error, #{reason := invalid_role}} = validate(
        {call, 1, {atom, 1, f}, []}, form),
    {error, #{reason := invalid_role}} = validate(
        {clause, 1, [], [], [{atom, 1, ok}]}, form).

%%--------------------------------------------------------------------
%% expression role
%%--------------------------------------------------------------------

test_validate_expression_accept_more(_Config) ->
    ?assertEqual(ok, validate({atom, 1, ok}, expression)),
    ?assertEqual(ok, validate(
        {call, 1, {atom, 1, f}, [{atom, 1, x}]}, expression)),
    ?assertEqual(ok, validate(
        {match, 1, {atom, 1, a}, {atom, 1, b}}, expression)),
    ?assertEqual(ok, validate(
        {'case', 1, {atom, 1, x}, [{clause, 1, [{atom, 1, x}], [], [{atom, 1, ok}]}]}, expression)),
    ?assertEqual(ok, validate({integer, 1, 42}, expression)),
    ?assertEqual(ok, validate(
        {'if', 1, [{clause, 1, [], [], [{atom, 1, true}]}]}, expression)),
    ?assertEqual(ok, validate(
        {'receive', 1, [{clause, 1, [{atom, 1, msg}], [], [{atom, 1, ok}]}]}, expression)),
    ?assertEqual(ok, validate(
        {'try', 1, [{atom, 1, body}], [], [], []}, expression)),
    ?assertEqual(ok, validate(
        {'catch', 1, {atom, 1, expr}}, expression)),
    ?assertEqual(ok, validate(
        {block, 1, [{atom, 1, a}, {atom, 1, b}]}, expression)).

test_validate_expression_reject(_Config) ->
    {error, #{reason := invalid_role}} = validate(function_form(), expression),
    {error, #{reason := invalid_role}} = validate(
        {clause, 1, [], [], [{atom, 1, ok}]}, expression).

%%--------------------------------------------------------------------
%% pattern role
%%--------------------------------------------------------------------

test_validate_pattern_accept(_Config) ->
    ?assertEqual(ok, validate({atom, 1, ok}, pattern)),
    ?assertEqual(ok, validate({var, 1, 'X'}, pattern)),
    ?assertEqual(ok, validate({var, 1, '_'}, pattern)),
    ?assertEqual(ok, validate(
        {tuple, 1, [{atom, 1, a}, {var, 1, 'B'}]}, pattern)),
    ?assertEqual(ok, validate({nil, 1}, pattern)),
    ?assertEqual(ok, validate(
        {cons, 1, {var, 1, 'H'}, {var, 1, 'T'}}, pattern)),
    ?assertEqual(ok, validate(
        {bin, 1, [{bin_element, 1, {var, 1, 'X'}, {integer, 1, 8}, default}]}, pattern)).

test_validate_pattern_reject(_Config) ->
    {error, #{reason := invalid_role}} = validate(
        {call, 1, {atom, 1, f}, [{atom, 1, x}]}, pattern),
    ?assertEqual(ok, validate(
        {match, 1, {atom, 1, a}, {atom, 1, b}}, pattern)),
    {error, #{reason := invalid_role}} = validate(function_form(), pattern).

%%--------------------------------------------------------------------
%% guard role
%%--------------------------------------------------------------------

test_validate_guard_accept(_Config) ->
    ?assertEqual(ok, validate({atom, 1, true}, guard)),
    ?assertEqual(ok, validate(
        {call, 1, {atom, 1, is_integer}, [{var, 1, 'X'}]}, guard)),
    ?assertEqual(ok, validate(
        {op, 1, '>', {var, 1, 'X'}, {integer, 1, 0}}, guard)),
    ?assertEqual(ok, validate({integer, 1, 42}, guard)),
    ?assertEqual(ok, validate(
        {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, '+'}}, [{var, 1, 'A'}, {var, 1, 'B'}]}, guard)).

test_validate_guard_reject(_Config) ->
    {error, #{reason := invalid_role}} = validate(
        {match, 1, {atom, 1, a}, {atom, 1, b}}, guard),
    {error, #{reason := invalid_role}} = validate(
        {call, 1, {atom, 1, helper}, []}, guard),
    {error, #{reason := invalid_role}} = validate(function_form(), guard).

test_validate_function_with_real_guard(_Config) ->
    Form = parse_form("f(X) when X > 0 -> ok."),
    ?assertEqual(ok, validate(Form, form)).

test_validate_guard_uses_record_forms(_Config) ->
    GuardRecord = {record, 1, guard_rec, []},
    ValidRecordForms = [{attribute, 1, record,
                         {guard_rec, [{record_field, 1, {atom, 1, field}, {integer, 1, 1}}]}}],
    InvalidRecordForms = [{attribute, 1, record,
                           {guard_rec, [{record_field, 1, {atom, 1, field},
                                         {call, 1, {atom, 1, helper}, []}}]}}],
    ?assertEqual(ok, validate(GuardRecord, guard,
                                               #{forms => ValidRecordForms})),
    {error, Error} = validate(GuardRecord, guard,
                                               #{forms => InvalidRecordForms}),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := guard,
                   actual_type := record_expr},
                 Error).

%%--------------------------------------------------------------------
%% type role — use erl_parse format: {type, Pos, Name, Args}
%%--------------------------------------------------------------------

test_validate_type_accept(_Config) ->
    ?assertEqual(ok, validate({nil, 1}, type)),
    ?assertEqual(ok, validate(
        {tuple, 1, [{atom, 1, a}, {atom, 1, b}]}, type)),
    ?assertEqual(ok, validate(
        {type, 1, integer, []}, type)).

test_validate_type_reject(_Config) ->
    {error, #{reason := invalid_role}} = validate(function_form(), type),
    {error, #{reason := invalid_role}} = validate(
        {call, 1, {atom, 1, f}, []}, type).

test_validate_type_singletons(_Config) ->
    ?assertEqual(ok, validate({atom, 1, ok}, type)),
    ?assertEqual(ok, validate({integer, 1, 42}, type)),
    ?assertEqual(ok, validate({char, 1, $a}, type)),
    ?assertEqual(ok, validate({string, 1, "abc"}, type)).

test_validate_type_and_spec_attributes(_Config) ->
    ?assertEqual(ok, validate(parse_form("-type t() :: ok | 42 | atom()."), form)),
    ?assertEqual(ok, validate(parse_form("-type t(A) :: A."), form)),
    ?assertEqual(ok, validate(parse_form("-opaque opaque_t() :: ok | 42."), form)),
    ?assertEqual(ok, validate(parse_form("-spec foo() -> ok."), form)),
    ?assertEqual(ok, validate(parse_form("-callback foo() -> ok."), form)).

test_reject_malformed_type_and_spec_attributes(_Config) ->
    MalformedBodies =
        [{attribute, 1, type, []},
         {attribute, 1, type, [{atom, 1, t}]},
         {attribute, 1, opaque, []},
         {attribute, 1, spec, []},
         {attribute, 1, callback, []}],
    lists:foreach(
      fun(Form) ->
              {error, Error} = validate(Form, form),
              ?assertMatch(
                 #{reason := invalid_node,
                   exception := {error, {invalid_attribute_body, _, _}}},
                 Error)
      end, MalformedBodies),
    {error, InvalidTypeBody} =
        validate({attribute, 0, type, {t, ok, []}}, form),
    ?assertMatch(#{reason := invalid_node,
                   node := ok,
                   parent_type := attribute},
                 InvalidTypeBody),
    ?assertError(
       {invalid_attribute_body, type, []},
       astranaut_syntax:child_specs(
         attribute, [[{atom, 1, type}], []], #{node => form})).

%%--------------------------------------------------------------------
%% clause role
%%--------------------------------------------------------------------

test_validate_clause_accept(_Config) ->
    ?assertEqual(ok, validate(
        {clause, 1, [{atom, 1, a}], [], [{atom, 1, ok}]}, clause)),
    ?assertEqual(ok, validate(
        {clause, 1, [{var, 1, 'X'}], [], [{atom, 1, ok}]}, clause)).

test_validate_clause_reject(_Config) ->
    {error, #{reason := invalid_role}} = validate({atom, 1, ok}, clause),
    {error, #{reason := invalid_role}} = validate(
        {call, 1, {atom, 1, f}, []}, clause),
    {error, #{reason := invalid_role}} = validate(function_form(), clause).

%%--------------------------------------------------------------------
%% name role — only atom is valid
%%--------------------------------------------------------------------

test_validate_name_accept(_Config) ->
    ?assertEqual(ok, validate({atom, 1, foo}, name)).

test_validate_name_reject(_Config) ->
    {error, #{reason := invalid_role}} = validate({var, 1, 'X'}, name),
    {error, #{reason := invalid_role}} = validate({integer, 1, 42}, name),
    {error, #{reason := invalid_role}} = validate(function_form(), name).

%%--------------------------------------------------------------------
%% attribute_body role (always allowed regardless of node type)
%%--------------------------------------------------------------------

test_validate_attribute_body_accept(_Config) ->
    ?assertEqual(ok, validate({atom, 1, ok}, attribute_body)),
    ?assertEqual(ok, validate(function_form(), attribute_body)),
    ?assertEqual(ok, validate(
        {call, 1, {atom, 1, f}, []}, attribute_body)),
    ?assertEqual(ok, validate(
        {clause, 1, [], [], [{atom, 1, ok}]}, attribute_body)),
    ?assertEqual(ok, validate(
        {type, 1, integer, []}, attribute_body)).

%%--------------------------------------------------------------------
%% list_comp — expression-only (from node_roles additions)
%%--------------------------------------------------------------------

test_validate_list_comp(_Config) ->
    Lc = {lc, 1, {atom, 1, ok}, [{generate, 1, {var, 1, 'X'}, {nil, 1}}]},
    ?assertEqual(ok, validate(Lc, expression)),
    {error, #{reason := invalid_role}} = validate(Lc, form),
    {error, #{reason := invalid_role}} = validate(Lc, pattern).

test_generated_schema_proxy(_Config) ->
    ?assertEqual(astranaut_syntax_schema:node_roles(atom),
                 astranaut_syntax:node_roles(atom)),
    ?assertEqual(false,
                 astranaut_syntax_schema:node_available(maybe_expr, 24)),
    ?assertEqual(true,
                 astranaut_syntax_schema:node_available(maybe_expr, 25)),
    Operator = erl_syntax:atom(f),
    ?assertEqual(
       {ok, [{operator, expression, [Operator], nodes},
             {arguments, expression, [], nodes}]},
       astranaut_syntax_schema:child_layout(
         application, [[Operator], []], expression, 29)),
    ?assertEqual(
       false,
       astranaut_syntax_schema:slot_available(
         map_field_exact, map_field_exact_key, infix_expr,
         {op, 1, '+', {var, 1, 'K'}, {integer, 1, 1}}, 22)),
    ?assertEqual(
       true,
       astranaut_syntax_schema:slot_available(
         map_field_exact, map_field_exact_key, infix_expr,
         {op, 1, '+', {var, 1, 'K'}, {integer, 1, 1}}, 23)),
    ?assertEqual(
       true,
       astranaut_syntax_schema:slot_available(
         size_qualifier, size, default, default, 'pre-21')),
    ?assertEqual(
       true,
       astranaut_syntax_schema:slot_available(
         try_expr, handlers, clause, legacy_throw_handler_ast(), 20)),
    ?assertEqual(
       false,
       astranaut_syntax_schema:slot_available(
         try_expr, handlers, clause, stacktrace_throw_handler_ast(), 20)),
    ?assertEqual(
       true,
       astranaut_syntax_schema:slot_available(
         try_expr, handlers, clause, stacktrace_throw_handler_ast(), 21)).

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
          attr := #{validator := {slot, binary_generator, pattern, pattern}}},
        #{slot := body,
          validator := {slot, binary_generator, body, expression},
          attr := #{validator := {slot, binary_generator, body, expression}}}],
       Specs).

test_all_child_specs_emit_expected_validators(_Config) ->
    lists:foreach(
      fun({ParentType, Subtrees, Attr, ExpectedSlots}) ->
              Specs = astranaut_syntax:child_specs(ParentType, Subtrees, Attr),
              ActualSlots = [{Slot, Role, Validator,
                              maps:with([node, validator], SpecAttr)}
                             || #{slot := Slot, role := Role, validator := Validator, attr := SpecAttr} <- Specs],
              Expected =
                  [{Slot, Role, {slot, ParentType, Slot, Role},
                    expected_child_attr(Role, Attr, {slot, ParentType, Slot, Role}, ParentType, Slot)}
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
              ?assertEqual(ok, astranaut_syntax:validate_node(valid_slot_ast(Validator), Validator)),
              case invalid_ast(Role) of
                  none ->
                      ok;
                  InvalidAst ->
                      {error, #{reason := invalid_role,
                                validator := Validator,
                                expected_role := Role}} =
                          astranaut_syntax:validate_node(InvalidAst, Validator)
              end
      end, Validators).

test_slot_roles_do_not_replace_node_role(_Config) ->
    Pattern = valid_ast(pattern),
    Expr = valid_ast(expression),
    MapField = valid_ast(map_field),
    BinElement = valid_ast(binary_field),
    [MapSpec] = astranaut_syntax:child_specs(map_expr, [[MapField]], #{node => pattern}),
    ?assertMatch(#{role := map_field,
                   validator := {slot, map_expr, fields, map_field},
                   attr := #{node := pattern}},
                 MapSpec),
    [BinSpec] = astranaut_syntax:child_specs(binary, [[BinElement]], #{node => pattern}),
    ?assertMatch(#{role := binary_field,
                   validator := {slot, binary, elements, binary_field},
                   attr := #{node := pattern}},
                 BinSpec),
    MapFieldSpecs = astranaut_syntax:child_specs(
                      map_field_exact, [[Expr], [Pattern]],
                      #{node => pattern}),
    ?assertMatch([#{role := expression}, #{role := pattern}], MapFieldSpecs),
    [BinElementSpec] = astranaut_syntax:child_specs(
                         binary_field, [[Pattern]], #{node => pattern}),
    ?assertMatch(#{role := pattern}, BinElementSpec),
    [NameSpec|_] = astranaut_syntax:child_specs(function, [[valid_ast(name)], [valid_ast(clause)]],
                                                #{node => form}),
    ?assertNot(maps:is_key(node, maps:get(attr, NameSpec))).

test_binary_field_projection_slots(_Config) ->
    Value = {var, 1, 'X'},
    Size = {integer, 1, 8},
    DefaultField = {bin_element, 1, Value, default, default},
    SizedTypedField = {bin_element, 1, Value, Size, [integer, unsigned]},
    [DefaultValueSpec] =
        astranaut_syntax:child_specs(
          binary_field, astranaut_syntax:subtrees(DefaultField),
          #{node => pattern}),
    ?assertMatch(#{slot := value, role := pattern, subtrees := [Value]},
                 DefaultValueSpec),
    [QualifiedValueSpec, TypesSpec] =
        astranaut_syntax:child_specs(
          binary_field, astranaut_syntax:subtrees(SizedTypedField),
          #{node => pattern}),
    #{subtrees := [SizeQualifier]} = QualifiedValueSpec,
    ?assertMatch(#{slot := value, role := pattern}, QualifiedValueSpec),
    ?assertMatch(#{slot := types, role := attribute_body}, TypesSpec),
    [ValueSpec, SizeSpec] =
        astranaut_syntax:child_specs(
          size_qualifier, astranaut_syntax:subtrees(SizeQualifier),
          #{node => pattern}),
    ?assertMatch(#{slot := value, role := pattern, subtrees := [Value]},
                 ValueSpec),
    ?assertMatch(#{slot := size, role := binary_size, subtrees := [Size]},
                 SizeSpec).

test_slot_validators_reject_wrong_structural_identity(_Config) ->
    BinElement = valid_ast(binary_field),
    MapField = {map_field_exact, 1, {atom, 1, key}, {atom, 1, value}},
    ?assertEqual(ok, astranaut_syntax:validate_node(BinElement, {slot, binary, elements, binary_field})),
    ?assertEqual(ok, astranaut_syntax:validate_node({atom, 1, value},
                                                     {slot, map_field_exact, map_field_exact_value, expression})),
    {error, BinError} = astranaut_syntax:validate_node(BinElement, {slot, tuple, elements, expression}),
    ?assertMatch(#{reason := invalid_role,
                   validator := {slot, tuple, elements, expression},
                   actual_type := binary_field},
                 BinError),
    {error, MapError} = astranaut_syntax:validate_node(MapField,
                                                        {slot, map_field_exact,
                                                         map_field_exact_value, expression}),
    ?assertMatch(#{reason := invalid_role,
                   validator := {slot, map_field_exact, map_field_exact_value, expression},
                   actual_type := map_field_exact},
                 MapError).

test_slot_validators_reject_malformed_structural_nodes(_Config) ->
    MalformedMapField = {map_field_exact, 1, {atom, 1, key}},
    MalformedBinElement = {bin_element, 1, {atom, 1, value}},
    {error, MapError} = astranaut_syntax:validate_node(MalformedMapField,
                                                        {slot, map_expr, fields, map_field}),
    ?assertMatch(#{reason := invalid_node,
                   validator := {slot, map_expr, fields, map_field}},
                 MapError),
    {error, BinError} = astranaut_syntax:validate_node(MalformedBinElement,
                                                        {slot, binary, elements, binary_field}),
    ?assertMatch(#{reason := invalid_node,
                   validator := {slot, binary, elements, binary_field}},
                 BinError).

test_validate_node_does_not_recurse_grandchildren(_Config) ->
    DirectChildInvalid = {call, 1, {atom, 1, f}, [function_form()]},
    GrandchildInvalid = {call, 1, {atom, 1, f}, [{tuple, 1, [{clause, 1, [], [], [{atom, 1, ok}]}]}]},
    ?assertEqual(ok, astranaut_syntax:validate_node(DirectChildInvalid, {role, expression})),
    ?assertEqual(ok, astranaut_syntax:validate_node(GrandchildInvalid, {role, expression})).

test_normalize_recurses_into_grandchildren(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{tuple, 1, [{clause, 1, [], [], [{atom, 1, ok}]}]}]},
    {error, Error} = astranaut_syntax:normalize(Tree, {role, expression}),
    ?assertMatch(#{reason := invalid_role,
                   actual_type := clause,
                   parent_type := tuple},
                 Error).

test_otp_vsn(_Config) ->
    OtpVsn = astranaut_syntax:otp_vsn(),
    ?assert(OtpVsn =:= 'pre-21' orelse is_integer(OtpVsn)).

test_try_handler_validation_uses_abstract_format(_Config) ->
    LegacyHandler = parse_try_handler("try a catch throw:P -> P end"),
    assert_same_ast(legacy_throw_handler_ast(), LegacyHandler),
    StacktraceHandler = stacktrace_throw_handler_ast(),
    ?assertNotEqual(astranaut_syntax:subtrees(LegacyHandler),
                    astranaut_syntax:subtrees(StacktraceHandler)),
    HandlerSlot = {slot, try_expr, handlers, clause},
    ?assertEqual(ok, astranaut_syntax:validate_node(LegacyHandler, HandlerSlot,
                                                     #{otp_vsn => 'pre-21'})),
    {ok, LegacyHandler1} = astranaut_syntax:normalize(LegacyHandler, HandlerSlot,
                                                       #{otp_vsn => 'pre-21'}),
    assert_same_ast(LegacyHandler, LegacyHandler1),
    {error, #{reason := invalid_role,
              validator := HandlerSlot,
              actual_type := clause}} =
        astranaut_syntax:validate_node(StacktraceHandler, HandlerSlot,
                                        #{otp_vsn => 'pre-21'}),
    StacktraceHandlerWithInvalidBody =
        {clause, 1,
         [{tuple, 1, [{atom, 1, throw}, {var, 1, 'P'}, {var, 1, 'S'}]}],
         [],
         [function_form()]},
    ?assertEqual(ok, astranaut_syntax:validate_node(StacktraceHandlerWithInvalidBody,
                                                     HandlerSlot,
                                                     #{otp_vsn => 21})).

test_legacy_catch_handler(_Config) ->
    LegacyForm = parse_form("f() -> try body catch error:Reason -> Reason end."),
    LegacyTry = function_body_expr(LegacyForm),
    assert_same_ast(legacy_catch_try_ast(), LegacyTry),
    [Body, Clauses, Handlers, After] = astranaut_syntax:subtrees(LegacyTry),
    ?assertNotEqual([], Body),
    ?assertEqual([], Clauses),
    ?assertNotEqual([], Handlers),
    ?assertEqual([], After),
    ?assertEqual(ok, validate(LegacyTry, expression, #{otp_vsn => 'pre-21'})),
    ?assertEqual(ok, validate(LegacyTry, expression,
                                               #{otp_vsn => astranaut_syntax:otp_vsn()})).

test_otp21_stacktrace_catch(_Config) ->
    StacktraceTry = stacktrace_catch_try_ast(),
    case current_otp_at_least(21) of
        true ->
            StacktraceForm = parse_form("f() -> try body catch error:Reason:Stacktrace -> Reason end."),
            ParsedStacktraceTry = function_body_expr(StacktraceForm),
            assert_same_ast(StacktraceTry, ParsedStacktraceTry),
            ?assertEqual(ok, validate(ParsedStacktraceTry, expression,
                                                       #{otp_vsn => astranaut_syntax:otp_vsn()})),
            {error, #{reason := invalid_role,
                      actual_type := clause,
                      parent_type := try_expr}} =
                validate(ParsedStacktraceTry, expression, #{otp_vsn => 'pre-21'});
        false ->
            ?assertMatch({error, _}, validate(StacktraceTry, expression))
    end.

test_legacy_map_pattern_key(_Config) ->
    LegacyField = {map_field_exact, 1, {var, 1, 'K'}, {var, 1, 'V'}},
    LegacyForm = parse_form("f(#{K := V}) -> V."),
    LegacyPattern = function_first_pattern(LegacyForm),
    assert_same_ast({map, 1, [LegacyField]}, LegacyPattern),
    LegacyField1 = map_pattern_field(LegacyPattern),
    ?assertEqual(ok, validate(LegacyField1, pattern, #{otp_vsn => 22})),
    ?assertEqual(ok, validate(LegacyField1, pattern,
                                               #{otp_vsn => astranaut_syntax:otp_vsn()})),
    StaticKeyForm = parse_form("f(#{key := V}) -> V."),
    ?assertEqual(ok, validate(
                       map_pattern_field(function_first_pattern(StaticKeyForm)),
                       pattern, #{otp_vsn => 'pre-21'})).

test_otp23_map_pattern_key_expression(_Config) ->
    NewField = {map_field_exact, 1,
                {op, 1, '+', {var, 1, 'K'}, {integer, 1, 1}},
                {var, 1, 'V'}},
    NewCode = "f(#{(K + 1) := V}) -> V.",
    case current_otp_at_least(23) of
        true ->
            NewForm = parse_form(NewCode),
            NewPattern = function_first_pattern(NewForm),
            assert_same_ast({map, 1, [NewField]}, NewPattern),
            NewField1 = map_pattern_field(NewPattern),
            ?assertEqual(ok, validate(NewField1, pattern,
                                                       #{otp_vsn => astranaut_syntax:otp_vsn()})),
            {error, #{reason := invalid_role,
                      actual_type := infix_expr,
                      parent_type := map_field_exact}} =
                validate(NewField1, pattern, #{otp_vsn => 22});
        false ->
            ?assertMatch({error, _}, validate(NewField, pattern))
    end.

test_legacy_binary_size(_Config) ->
    LegacyBin = {bin, 1, [{bin_element, 1, {var, 1, 'X'}, {var, 1, 'N'}, default}]},
    LegacyForm = parse_form("f(<<X:N>>) -> X."),
    ParsedLegacyBin = function_first_pattern(LegacyForm),
    assert_same_ast(LegacyBin, ParsedLegacyBin),
    ?assertEqual(ok, validate(ParsedLegacyBin, pattern, #{otp_vsn => 22})),
    ?assertEqual(ok, validate(ParsedLegacyBin, pattern,
                                               #{otp_vsn => astranaut_syntax:otp_vsn()})),
    StaticSizeForm = parse_form("f(<<X:8>>) -> X."),
    ?assertEqual(ok, validate(
                       function_first_pattern(StaticSizeForm),
                       pattern, #{otp_vsn => 'pre-21'})).

test_otp23_binary_size_expression(_Config) ->
    NewBin = {bin, 1, [{bin_element, 1, {var, 1, 'X'},
                        {op, 1, '+', {var, 1, 'N'}, {integer, 1, 1}},
                        default}]},
    NewCode = "f(<<X:(N + 1)>>) -> X.",
    case current_otp_at_least(23) of
        true ->
            NewForm = parse_form(NewCode),
            ParsedNewBin = function_first_pattern(NewForm),
            assert_same_ast(NewBin, ParsedNewBin),
            ?assertEqual(ok, validate(ParsedNewBin, pattern,
                                                       #{otp_vsn => astranaut_syntax:otp_vsn()})),
            {error, #{reason := invalid_role,
                      actual_type := infix_expr,
                      parent_type := size_qualifier}} =
                validate(ParsedNewBin, pattern, #{otp_vsn => 22});
        false ->
            ?assertMatch({error, _}, validate(NewBin, pattern))
    end.

test_otp25_maybe_expr(_Config) ->
    MaybeExpr = {'maybe', 1,
                 [{maybe_match, 1,
                   {tuple, 1, [{atom, 1, ok}, {var, 1, 'B'}]},
                   {var, 1, 'A'}},
                  {var, 1, 'B'}]},
    Code = "f(A) -> maybe {ok, B} ?= A, B end.",
    case current_otp_at_least(25) of
        true ->
            Form = parse_form(Code),
            ParsedMaybeExpr = function_body_expr(Form),
            assert_same_ast(MaybeExpr, ParsedMaybeExpr),
            ?assertEqual(ok, validate(ParsedMaybeExpr, expression,
                                                       #{otp_vsn => astranaut_syntax:otp_vsn()})),
            {error, #{reason := invalid_role, actual_type := maybe_expr}} =
                validate(ParsedMaybeExpr, expression, #{otp_vsn => 24});
        false ->
            ?assertMatch({error, _}, validate(MaybeExpr, expression))
    end.

test_otp26_map_comprehension(_Config) ->
    MapComp = {mc, 1, {map_field_assoc, 1, {var, 1, 'K'}, {var, 1, 'V'}},
               [{m_generate, 1,
                 {map_field_exact, 1, {var, 1, 'K'}, {var, 1, 'V'}},
                 {var, 1, 'Map'}}]},
    Code = "f(Map) -> #{K => V || K := V <- Map}.",
    case current_otp_at_least(26) of
        true ->
            Form = parse_form(Code),
            ParsedMapComp = function_body_expr(Form),
            assert_same_ast(MapComp, ParsedMapComp),
            ?assertEqual(ok, validate(ParsedMapComp, expression,
                                                       #{otp_vsn => astranaut_syntax:otp_vsn()})),
            {error, #{reason := invalid_role, actual_type := map_comp}} =
                validate(ParsedMapComp, expression, #{otp_vsn => 25});
        false ->
            ?assertMatch({error, _}, validate(MapComp, expression))
    end.

test_otp28_strict_generator(_Config) ->
    StrictLc = {lc, 1, {var, 1, 'X'}, [{generate_strict, 1, {var, 1, 'X'}, {var, 1, 'A'}}]},
    Code = "f(A) -> [X || X <:- A].",
    case current_otp_at_least(28) of
        true ->
            Form = parse_form(Code),
            ParsedStrictLc = function_body_expr(Form),
            assert_same_ast(StrictLc, ParsedStrictLc),
            ?assertEqual(ok, validate(ParsedStrictLc, expression,
                                                       #{otp_vsn => astranaut_syntax:otp_vsn()})),
            {error, #{reason := invalid_role, actual_type := strict_generator}} =
                validate(ParsedStrictLc, expression, #{otp_vsn => 27});
        false ->
            ?assertMatch({error, _}, validate(StrictLc, expression))
    end.

test_otp29_multiple_comprehension_templates(_Config) ->
    Generator = {generate, 1, {var, 1, 'I'}, {var, 1, 'List'}},
    SingleListComp = {lc, 1, {var, 1, 'I'}, [Generator]},
    MultiListComp =
        {lc, 1, [{var, 1, 'I'}, {op, 1, '-', {var, 1, 'I'}}],
         [Generator]},
    SingleMapComp =
        {mc, 1,
         {map_field_assoc, 1, {var, 1, 'I'}, {var, 1, 'I'}},
         [Generator]},
    MultiMapComp =
        {mc, 1,
         [{map_field_assoc, 1, {var, 1, 'I'}, {var, 1, 'I'}},
          {map_field_assoc, 1, {op, 1, '+', {var, 1, 'I'}, {integer, 1, 1}},
           {var, 1, 'I'}}],
         [Generator]},
    ?assertEqual(ok, validate(SingleListComp, expression, #{otp_vsn => 28})),
    ?assertEqual(ok, validate(SingleMapComp, expression, #{otp_vsn => 28})),
    ?assertMatch({error, #{reason := invalid_role, actual_type := list_comp}},
                 validate(MultiListComp, expression, #{otp_vsn => 28})),
    ?assertMatch({error, #{reason := invalid_role, actual_type := map_comp}},
                 validate(MultiMapComp, expression, #{otp_vsn => 28})),
    case current_otp_at_least(29) of
        true ->
            ParsedMultiListComp =
                function_body_expr(parse_form("f(List) -> [I, -I || I <- List].")),
            ParsedMultiMapComp =
                function_body_expr(
                  parse_form(
                    "f(List) -> #{I => I, I + 1 => I || I <- List}.")),
            assert_same_ast(MultiListComp, ParsedMultiListComp),
            assert_same_ast(MultiMapComp, ParsedMultiMapComp),
            ?assertEqual(ok, validate(ParsedMultiListComp, expression,
                                      #{otp_vsn => 29})),
            ?assertEqual(ok, validate(ParsedMultiMapComp, expression,
                                      #{otp_vsn => 29}));
        false ->
            ok
    end.

test_traverse_validate_changed_node(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{atom, 1, bad}]},
    try astranaut:smap(
          fun({atom, 1, bad}) -> function_form();
             (Node) -> Node
          end, Tree, #{traverse => pre, role => expression, validate => true}) of
        _ -> error(unexpected_ok)
    catch
        error:{invalid_transform_normalization, #{reason := invalid_role,
                                               actual_type := function}} ->
            ok
    end.

test_traverse_validate_false_opt_out(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{atom, 1, bad}]},
    ?assertEqual(
       {call, 1, {atom, 1, f}, [function_form()]},
       astranaut:smap(
         fun({atom, 1, bad}) -> function_form();
            (Node) -> Node
         end, Tree, #{traverse => pre, validate => false})).

test_traverse_validate_preserves_slot_attr(_Config) ->
    Tree = {'case', 1, {var, 1, 'M'},
            [{clause, 1,
              [{map, 1, [{map_field_exact, 1, {atom, 1, key}, {var, 1, 'V'}}]}],
              [],
              [{atom, 1, ok}]}]},
    InvalidField = {atom, 1, not_a_map_field},
    try astranaut:smap(
          fun({map_field_exact, 1, {atom, 1, key}, {var, 1, 'V'}}, _Attr) ->
                  InvalidField;
             (Node, Attr) ->
                  Type = astranaut_syntax:type(Node),
                  astranaut_uniplate:with_subtrees(
                    fun(Subtrees) ->
                            astranaut_syntax:subtrees_pge(Type, Subtrees, Attr)
                    end, Node)
          end, Tree, #{traverse => pre, role => expression, validate => output}) of
        _ -> error(unexpected_ok)
    catch
        error:{invalid_transform_normalization, #{reason := invalid_role,
                                               validator := {slot, map_expr, fields, map_field},
                                               actual_type := atom}} ->
            ok
    end.

test_traverse_pre_validate_does_not_recurse_grandchildren(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{atom, 1, bad}]},
    Updated = {tuple, 1, [{tuple, 1, [{clause, 1, [], [], [{atom, 1, ok}]}]}]},
    ?assertEqual(
       {call, 1, {atom, 1, f}, [Updated]},
       astranaut:smap(
         fun({atom, 1, bad}) ->
                 Updated;
            (Node) ->
                 Node
         end, Tree, #{traverse => pre, role => expression, validate => output})).

test_traverse_post_validate_recurse_grandchildren(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{atom, 1, bad}]},
    Updated = {tuple, 1, [{tuple, 1, [{clause, 1, [], [], [{atom, 1, ok}]}]}]},
    try astranaut:smap(
          fun({atom, 1, bad}) ->
                  Updated;
             (Node) ->
                  Node
          end, Tree, #{traverse => post, role => expression, validate => output}) of
        _ -> error(unexpected_ok)
    catch
        error:{invalid_transform_normalization, #{reason := invalid_role,
                                               actual_type := clause}} ->
            ok
    end.

test_traverse_input_validate_runs_before_post_walk(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{clause, 1, [], [], [{atom, 1, ok}]}]},
    try astranaut:smap(
          fun(Node) -> Node end,
          Tree, #{traverse => post, role => expression, validate => input}) of
        _ -> error(unexpected_ok)
    catch
        error:{invalid_transform_normalization, #{reason := invalid_role,
                                                   actual_type := clause}} ->
            ok
    end.

test_traverse_input_validate_attribute_body(_Config) ->
    Attribute = {attribute, 1, custom, [{value, 1}]},
    ?assertEqual(
       Attribute,
       astranaut:smap(
         fun(Node) -> Node end,
         Attribute, #{traverse => post, role => form, validate => input})).

test_traverse_validate_collect_returns_input(_Config) ->
    Invalid = {clause, 1, [], [], [{atom, 1, ok}]},
    Tree = {call, 1, {atom, 1, f}, [Invalid]},
    Monad =
        astranaut:map_m(
          fun(Node) -> astranaut_traverse:return(Node) end,
          Tree, #{traverse => post,
                  role => expression,
                  validate => input,
                  validate_opts => #{fail => collect}}),
    Result = astranaut_traverse:eval(Monad, astranaut, #{}, ok),
    ?assertEqual({just, Tree}, astranaut_return:run(Result)),
    Errors = astranaut_error:errors(astranaut_return:run_error(Result)),
    ?assertMatch([{invalid_transform_normalization,
                   #{reason := invalid_role, actual_type := clause}}], Errors).

test_traverse_true_validate_is_output(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{clause, 1, [], [], [{atom, 1, ok}]}]},
    ?assertEqual(
       Tree,
       astranaut:smap(
         fun(Node) -> Node end,
         Tree, #{traverse => post, role => expression, validate => true})).

test_traverse_both_validate_includes_input(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{clause, 1, [], [], [{atom, 1, ok}]}]},
    try astranaut:smap(
          fun(Node) -> Node end,
          Tree, #{traverse => post, role => expression, validate => both}) of
        _ -> error(unexpected_ok)
    catch
        error:{invalid_transform_normalization, #{reason := invalid_role,
                                                   actual_type := clause}} ->
            ok
    end.

test_read_only_traversals_skip_validation(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{clause, 1, [], [], [{atom, 1, ok}]}]},
    ?assertEqual(4,
                 astranaut:sreduce(fun(_, Acc) -> Acc + 1 end, 0, Tree, #{traverse => pre})),
    ?assertEqual(false,
                 astranaut:search(fun({atom, 1, missing}) -> true; (_) -> false end,
                                  Tree, #{traverse => pre})),
    ?assertMatch(#{return := 4},
                 astranaut:reduce(fun(_, Acc) -> Acc + 1 end, 0, Tree, #{traverse => pre})).

test_map_m_validate_raises(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{atom, 1, bad}]},
    Monad =
        astranaut:map_m(
          fun({atom, 1, bad}) ->
                  astranaut_traverse:return(function_form());
             (Node) ->
                  astranaut_traverse:return(Node)
          end, Tree, #{traverse => pre, role => expression, validate => true}),
    try astranaut_traverse:eval(Monad, astranaut, #{}, ok) of
        _ -> error(unexpected_ok)
    catch
        error:{invalid_transform_normalization, #{reason := invalid_role,
                                               pos := 1,
                                               actual_type := function}} ->
            ok
    end.

%%--------------------------------------------------------------------
%% edge cases
%%--------------------------------------------------------------------

test_set_pos_warning_keeps_shape(_Config) ->
    ?assertEqual({warning, {2, erl_lint, bad}},
                 astranaut_syntax:set_pos({warning, {1, erl_lint, bad}}, 2)).

test_public_syntax_helpers(_Config) ->
    Node = {atom, 1, ok},
    ?assertEqual(
       {error, {2, erl_lint, bad}},
       astranaut_syntax:set_pos(
         {error, {1, erl_lint, bad}}, 2)),
    ?assert(astranaut_syntax:is_leaf(erl_syntax:atom(ok))),
    ?assertNot(
       astranaut_syntax:is_leaf(
         erl_syntax:tuple([erl_syntax:atom(ok)]))),

    RoleContexts =
        [{pattern,
          astranaut_syntax:pattern_node(Node)},
         {guard,
          astranaut_syntax:guard_node(Node)},
         {expression,
          astranaut_syntax:expression_node(Node)},
         {clause,
          astranaut_syntax:update_node(clause, Node)}],
    lists:foreach(
      fun({Role, Context}) ->
              ?assertEqual(Node, context_node(Context)),
              ?assertEqual(
                 #{node => Role},
                 context_attrs(Context))
      end, RoleContexts),

    RecordName = erl_syntax:atom(sample_record),
    RecordField = erl_syntax:atom(sample_field),
    [[_], [RecordNameContext, RecordFieldContext]] =
        attribute_contexts(
          record, [RecordName, RecordField]),
    ?assertEqual(
       #{attribute => record},
       context_attrs(RecordNameContext)),
    ?assertEqual(
       #{attribute => record},
       context_attrs(RecordFieldContext)),

    TypeName = erl_syntax:atom(sample_type),
    TypeBody = erl_syntax:atom(ok),
    TypeParam = erl_syntax:variable('T'),
    lists:foreach(
      fun(Attribute) ->
              [[_], [TypeNameContext, TypeBodyContext,
                     TypeParamContext]] =
                  attribute_contexts(
                    Attribute,
                    [TypeName, TypeBody, TypeParam]),
              ?assertEqual(
                 #{attribute => Attribute},
                 context_attrs(TypeNameContext)),
              ?assertEqual(
                 #{attribute => Attribute, node => type},
                 context_attrs(TypeBodyContext)),
              ?assertEqual(
                 #{attribute => Attribute},
                 context_attrs(TypeParamContext))
      end, [type, opaque]),

    SpecName = erl_syntax:tuple(
                 [erl_syntax:atom(sample),
                  erl_syntax:integer(0)]),
    SpecBody = erl_syntax:atom(ok),
    lists:foreach(
      fun(Attribute) ->
              [[_], [SpecNameContext, SpecBodyContext]] =
                  attribute_contexts(
                    Attribute, [SpecName, SpecBody]),
              ?assertEqual(
                 #{attribute => Attribute},
                 context_attrs(SpecNameContext)),
              ?assertEqual(
                 #{attribute => Attribute, node => type},
                 context_attrs(SpecBodyContext))
      end, [spec, callback]),

    CustomBody = erl_syntax:atom(value),
    [[_], [CustomContext]] =
        attribute_contexts(custom, [CustomBody]),
    ?assertEqual(
       #{attribute => custom},
       context_attrs(CustomContext)),
    Subtrees = [[erl_syntax:atom(value)]],
    ?assertEqual(
       Subtrees,
       astranaut_syntax:attribute_subtrees_type(
         tuple, Subtrees, #{})).

test_validate_nested_valid(_Config) ->
    Tree = {call, 1,
            {remote, 1, {atom, 1, m}, {atom, 1, f}},
            [{match, 1, {var, 1, 'X'}, {call, 1, {atom, 1, g}, [{integer, 1, 1}]}}]},
    ?assertEqual(ok, validate(Tree, expression)).

test_validate_nested_invalid(_Config) ->
    Tree = {call, 1, {atom, 1, f}, [{clause, 1, [], [], [{atom, 1, ok}]}]},
    {error, Error} = validate(Tree, expression),
    ?assertMatch(#{reason := invalid_role,
                   expected_role := expression,
                   actual_type := clause,
                   slot := arguments,
                   parent_type := application},
                 Error).

test_validate_empty_list(_Config) ->
    ?assertEqual(ok, validate([], expression)),
    ?assertEqual(ok, validate([], form)),
    ?assertEqual(ok, validate([], pattern)).

-ifdef(ASTRANAUT_OTP_AT_LEAST_29).
test_otp29_native_record_forms(_Config) ->
    Field = {record_field, 1, {atom, 1, x}, {integer, 1, 1}},
    QualifiedCreate = {record, 1, {mod, rec}, [Field]},
    QualifiedUpdate = {record, 1, {var, 1, 'R'}, {mod, rec}, [Field]},
    AnonymousPattern = {record, 1, [], [Field]},
    AnonymousUpdate = {record, 1, {var, 1, 'R'}, [], [Field]},
    QualifiedAccess = {record_field, 1, {var, 1, 'R'}, {mod, rec}, {atom, 1, x}},
    AnonymousAccess = {record_field, 1, {var, 1, 'R'}, [], {atom, 1, x}},
    NativeRecord = {attribute, 1, native_record,
                    {rec, [{record_field, 1, {atom, 1, x}}]}},
    ExportRecord = {attribute, 1, export_record, [rec]},
    ImportRecord = {attribute, 1, import_record, {mod, [rec]}},
    ?assertEqual(ok, validate(QualifiedCreate, expression)),
    ?assertEqual(ok, validate(QualifiedUpdate, expression)),
    ?assertEqual(ok, validate(AnonymousPattern, pattern)),
    ?assertEqual(ok, validate(AnonymousUpdate, expression)),
    ?assertEqual(ok, validate(QualifiedAccess, expression)),
    ?assertEqual(ok, validate(AnonymousAccess, expression)),
    ?assertEqual(ok, validate(NativeRecord, form)),
    ?assertEqual(ok, validate(ExportRecord, form)),
    ?assertEqual(ok, validate(ImportRecord, form)),
    [ArgumentSpec, TypeSpec, FieldsSpec] =
        astranaut_syntax:child_specs(
          record_expr, astranaut_syntax:subtrees(QualifiedUpdate),
          #{node => expression}),
    ?assertMatch(#{slot := argument,
                   validator := {slot, record_expr, argument, expression}},
                 ArgumentSpec),
    ?assertMatch(#{slot := type,
                   validator := {slot, record_expr, type, expression}},
                 TypeSpec),
    ?assertMatch(#{slot := fields,
                   validator := {slot, record_expr, fields, expression}},
                 FieldsSpec),
    lists:foreach(
      fun({Node, Role}) ->
              ?assertEqual({ok, Node}, astranaut_syntax:normalize(Node, {role, Role}))
      end,
      [{QualifiedCreate, expression},
       {QualifiedUpdate, expression},
       {AnonymousPattern, pattern},
       {AnonymousUpdate, expression},
       {QualifiedAccess, expression},
       {AnonymousAccess, expression},
       {NativeRecord, form},
       {ExportRecord, form},
       {ImportRecord, form}]).

test_otp29_native_record_version(_Config) ->
    Field = {record_field, 1, {atom, 1, x}, {integer, 1, 1}},
    QualifiedCreate = {record, 1, {mod, rec}, [Field]},
    AnonymousUpdate = {record, 1, {var, 1, 'R'}, [], [Field]},
    NativeRecord = {attribute, 1, native_record,
                    {rec, [{record_field, 1, {atom, 1, x}}]}},
    ExportRecord = {attribute, 1, export_record, [rec]},
    ImportRecord = {attribute, 1, import_record, {mod, [rec]}},
    ?assertMatch({error, #{reason := invalid_role}},
                 astranaut_syntax:normalize(
                   QualifiedCreate, {role, expression}, #{otp_vsn => 28})),
    ?assertMatch({error, #{reason := invalid_role}},
                 astranaut_syntax:normalize(
                   AnonymousUpdate, {role, expression}, #{otp_vsn => 28})),
    %% These names were valid wild attributes before OTP 29, so their tuple
    %% shape alone cannot be version-gated as native-record syntax.
    ?assertEqual({ok, NativeRecord},
                 astranaut_syntax:normalize(
                   NativeRecord, {role, form}, #{otp_vsn => 28})),
    ?assertEqual({ok, ExportRecord},
                 astranaut_syntax:normalize(
                   ExportRecord, {role, form}, #{otp_vsn => 28})),
    ?assertEqual({ok, ImportRecord},
                 astranaut_syntax:normalize(
                   ImportRecord, {role, form}, #{otp_vsn => 28})).
-else.
test_otp29_native_record_forms(_Config) ->
    ok.

test_otp29_native_record_version(_Config) ->
    ok.
-endif.

test_record_access_child_slot_order(_Config) ->
    Argument = [{var, 1, 'R'}],
    Type = [{atom, 1, rec}],
    Field = [{atom, 1, x}],
    [ArgumentSpec, TypeSpec, FieldSpec] =
        astranaut_syntax:child_specs(
          record_access, [Argument, Type, Field], #{node => expression}),
    ?assertMatch(#{slot := argument, subtrees := Argument,
                   validator := {slot, record_access, argument, expression}},
                 ArgumentSpec),
    ?assertMatch(#{slot := type, subtrees := Type,
                   validator := {slot, record_access, type, expression}},
                 TypeSpec),
    ?assertMatch(#{slot := field, subtrees := Field,
                   validator := {slot, record_access, field, expression}},
                 FieldSpec).

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

validate(NodeOrNodes, Role) ->
    validate(NodeOrNodes, Role, #{}).

validate(NodeOrNodes, Role, Opts) ->
    case astranaut_syntax:normalize(NodeOrNodes, {role, Role}, Opts) of
        {ok, _NodeOrNodes1} -> ok;
        {error, _Error} = Error -> Error
    end.

function_form() ->
    {function, 1, foo, 0, [{clause, 1, [], [], [{atom, 1, ok}]}]}.

parse_form(Code) ->
    {ok, Tokens, _EndLine} = erl_scan:string(Code),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.

parse_form_result(Code) ->
    case erl_scan:string(Code) of
        {ok, Tokens, _EndLine} ->
            erl_parse:parse_form(Tokens);
        {error, ErrorInfo, _EndLine} ->
            {error, ErrorInfo}
    end.

attribute_contexts(Attribute, BodyTrees) ->
    astranaut_syntax:attribute_subtrees_type(
      attribute,
      [[erl_syntax:atom(Attribute)], BodyTrees],
      #{}).

context_node(
  {uniplate_node_context, Node, _Withs, _Reduces, _Skip,
   _UpAttrs, _Entries, _Exits}) ->
    Node.

context_attrs(
  {uniplate_node_context, _Node, _Withs, _Reduces, _Skip,
   UpAttrs, _Entries, _Exits}) ->
    lists:foldl(
      fun(Attr, Acc) ->
              maps:merge(Acc, Attr)
      end, #{}, lists:reverse(UpAttrs)).

assert_parser_or_constructed_ast_rejected(Code, ConstructedAst, Role) ->
    case parse_form_result(Code) of
        {ok, Form} ->
            ?assertMatch({error, _}, validate(extract_feature_ast(Role, Form), Role));
        {error, _ErrorInfo} ->
            ?assertMatch({error, _}, validate(ConstructedAst, Role))
    end.

assert_same_ast(Expected, Actual) ->
    ?assertEqual(astranaut_lib:replace_pos(Expected, 0),
                 astranaut_lib:replace_pos(Actual, 0)).

current_otp_at_least(Min) ->
    case astranaut_syntax:otp_vsn() of
        OtpVsn when is_integer(OtpVsn) ->
            OtpVsn >= Min;
        'pre-21' ->
            false
    end.

function_body_expr({function, _Pos, _Name, _Arity,
                    [{clause, _ClausePos, _Patterns, _Guards, [Expr]}]}) ->
    Expr.

parse_try_handler(TryCode) ->
    {'try', _TryPos, _Body, _Clauses, [Handler], _After} =
        function_body_expr(parse_form("f() -> " ++ TryCode ++ ".")),
    Handler.

function_first_pattern({function, _Pos, _Name, _Arity,
                        [{clause, _ClausePos, [Pattern|_Patterns], _Guards, _Body}]}) ->
    Pattern.

map_pattern_field({map, _Pos, [Field]}) ->
    Field.

extract_feature_ast(pattern, Form) ->
    function_first_pattern(Form);
extract_feature_ast(expression, Form) ->
    function_body_expr(Form).

stacktrace_catch_try_ast() ->
    Handler =
        {clause, 1,
         [{tuple, 1, [{atom, 1, error}, {var, 1, 'Reason'}, {var, 1, 'Stacktrace'}]}],
         [],
         [{var, 1, 'Reason'}]},
    {'try', 1, [{atom, 1, body}], [], [Handler], []}.

legacy_throw_handler_ast() ->
    {clause, 1,
     [{tuple, 1, [{atom, 1, throw}, {var, 1, 'P'}, {var, 1, '_'}]}],
     [],
     [{var, 1, 'P'}]}.

stacktrace_throw_handler_ast() ->
    {clause, 1,
     [{tuple, 1, [{atom, 1, throw}, {var, 1, 'P'}, {var, 1, 'S'}]}],
     [],
     [{var, 1, 'P'}]}.

legacy_catch_try_ast() ->
    Handler =
        {clause, 1,
         [{tuple, 1, [{atom, 1, error}, {var, 1, 'Reason'}, {var, 1, '_'}]}],
         [],
         [{var, 1, 'Reason'}]},
    {'try', 1, [{atom, 1, body}], [], [Handler], []}.

child_spec_cases() ->
    Pattern = valid_ast(pattern),
    Expr = valid_ast(expression),
    Guard = valid_ast(guard),
    MapField = valid_ast(map_field),
    Clause = valid_ast(clause),
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
     {function, [[Name], [Clause]], #{node => form},
      [{name, name}, {clauses, clause}]},
     {function, [[Clause]], #{node => form},
      [{clauses, clause}]},
     {attribute, [[{atom, 1, custom}], [AttributeBody]], #{node => form},
      [{name, name}, {body, attribute_body}]},
     {attribute, [[{atom, 1, type}], [Name, Type]], #{node => form},
      [{name, name}, {type_name, name}, {type_body, type}, {type_params, type_param}]},
     {attribute, [[{atom, 1, opaque}], [Name, Type]], #{node => form},
      [{name, name}, {type_name, name}, {type_body, type}, {type_params, type_param}]},
     {attribute, [[{atom, 1, spec}], [Name, Type]], #{node => form},
      [{name, name}, {spec_mfa, attribute_body}, {specs, type}]},
     {attribute, [[{atom, 1, callback}], [Name, Type]], #{node => form},
      [{name, name}, {spec_mfa, attribute_body}, {specs, type}]},
     {list_comp, [[Expr], [Expr]], #{node => expression},
      [{template, expression}, {body, expression}]},
     {map_comp, [[Expr], [Expr]], #{node => expression},
      [{template, map_field}, {body, expression}]},
     {binary_comp, [[Expr], [Expr]], #{node => expression},
      [{template, expression}, {body, expression}]},
     {maybe_expr, [[Expr]], #{node => expression},
      [{body, expression}]},
     {maybe_expr, [[Expr], [Clause]], #{node => expression},
      [{body, expression}, {else_clause, clause}]},
     {implicit_fun, [[Expr]], #{node => expression},
      [{name, expression}]},
     {record_access, [[Expr], [Expr], [Expr]], #{node => expression},
      [{argument, expression}, {type, expression}, {field, expression}]},
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

expected_child_attr(Role, Attr, Validator, _ParentType, _Slot) ->
    Base = #{validator => Validator},
    case expected_child_node(Role, Attr) of
        none -> Base;
        Node -> Base#{node => Node}
    end.

expected_child_node(Role, Attr) ->
    case Role of
        map_field ->
            maps:get(node, Attr, expression);
        binary_field ->
            maps:get(node, Attr, expression);
        expression ->
            expression;
        pattern ->
            pattern;
        guard ->
            guard;
        form ->
            form;
        type ->
            type;
        clause ->
            clause;
        _ ->
            none
    end.

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
valid_ast(type_param) ->
    {var, 1, 'A'};
valid_ast(attribute_body) ->
    {atom, 1, ok}.

valid_slot_ast({slot, try_expr, handlers, clause}) ->
    {clause, 1,
     [{tuple, 1, [{atom, 1, error}, {var, 1, 'Reason'}, {var, 1, '_'}]}],
     [],
     [{atom, 1, ok}]};
valid_slot_ast(Validator) ->
    valid_ast(validator_role(Validator)).

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
    {call, 1, {atom, 1, f}, []};
invalid_ast(type_param) ->
    {atom, 1, ok};
invalid_ast(attribute_body) ->
    none.
