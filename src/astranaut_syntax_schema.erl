%%%-------------------------------------------------------------------
%%% @doc Direct dispatch generated from src/syntax.term.
%%%
%%% GENERATED FILE. DO NOT EDIT.
%%% AST projection and reconstruction remain owned by erl_syntax.
%%%-------------------------------------------------------------------
-module(astranaut_syntax_schema).
-export([node_roles/1,
         role_available/2,
         traverse_transparent/1,
         node_available/2,
         format_available/3,
         slot_available/5,
         child_layout/4]).
node_roles(annotated_type) ->
    [type];
node_roles(application) ->
    [expression, guard];
node_roles(arity_qualifier) ->
    [expression];
node_roles(atom) ->
    [expression, guard, name, pattern, type];
node_roles(attribute) ->
    [form];
node_roles(binary) ->
    [expression, guard, pattern, type];
node_roles(binary_comp) ->
    [expression];
node_roles(binary_field) ->
    [expression, guard, pattern];
node_roles(binary_generator) ->
    [expression];
node_roles(bitstring_type) ->
    [type];
node_roles(block_expr) ->
    [expression];
node_roles(case_expr) ->
    [expression];
node_roles(catch_expr) ->
    [expression];
node_roles(char) ->
    [expression, guard, pattern, type];
node_roles(class_qualifier) ->
    [pattern];
node_roles(clause) ->
    [clause];
node_roles(comment) ->
    [form];
node_roles(cond_expr) ->
    [expression];
node_roles(conjunction) ->
    [expression, guard];
node_roles(cons) ->
    [expression, guard, pattern, type];
node_roles(constrained_function_type) ->
    [type];
node_roles(constraint) ->
    [type];
node_roles(disjunction) ->
    [expression, guard];
node_roles(else_expr) ->
    [expression];
node_roles(eof_marker) ->
    [form];
node_roles(error_marker) ->
    [form];
node_roles(float) ->
    [expression, guard, pattern];
node_roles(fun_expr) ->
    [expression];
node_roles(fun_type) ->
    [type];
node_roles(function) ->
    [form];
node_roles(function_type) ->
    [type];
node_roles(generator) ->
    [expression];
node_roles(if_expr) ->
    [expression];
node_roles(implicit_fun) ->
    [expression];
node_roles(infix_expr) ->
    [expression, guard];
node_roles(integer) ->
    [expression, guard, pattern, type];
node_roles(integer_range_type) ->
    [type];
node_roles(list) ->
    [expression, guard, pattern, type];
node_roles(list_comp) ->
    [expression];
node_roles(map_comp) ->
    [expression];
node_roles(map_expr) ->
    [expression, guard, pattern];
node_roles(map_field_assoc) ->
    [expression, guard, pattern];
node_roles(map_field_exact) ->
    [expression, guard, pattern];
node_roles(map_generator) ->
    [expression];
node_roles(map_type) ->
    [type];
node_roles(map_type_assoc) ->
    [type];
node_roles(map_type_exact) ->
    [type];
node_roles(match_expr) ->
    [expression, pattern];
node_roles(maybe_expr) ->
    [expression];
node_roles(maybe_match_expr) ->
    [expression];
node_roles(module_qualifier) ->
    [expression, guard];
node_roles(named_fun_expr) ->
    [expression];
node_roles(nil) ->
    [expression, guard, pattern, type];
node_roles(operator) ->
    [expression, guard];
node_roles(parentheses) ->
    [expression, guard, pattern, type];
node_roles(prefix_expr) ->
    [expression, guard];
node_roles(receive_expr) ->
    [expression];
node_roles(record_access) ->
    [expression, guard];
node_roles(record_expr) ->
    [expression, guard, pattern];
node_roles(record_field) ->
    [expression, guard, pattern];
node_roles(record_index_expr) ->
    [expression, guard];
node_roles(record_type) ->
    [type];
node_roles(record_type_field) ->
    [type];
node_roles(remote_type) ->
    [type];
node_roles(size_qualifier) ->
    [expression, guard, pattern];
node_roles(strict_binary_generator) ->
    [expression];
node_roles(strict_generator) ->
    [expression];
node_roles(strict_map_generator) ->
    [expression];
node_roles(string) ->
    [expression, guard, pattern, type];
node_roles(text) ->
    [form];
node_roles(try_expr) ->
    [expression];
node_roles(tuple) ->
    [expression, guard, pattern, type];
node_roles(tuple_type) ->
    [type];
node_roles(type_application) ->
    [type];
node_roles(type_binary) ->
    [type];
node_roles(type_fun) ->
    [type];
node_roles(type_integer_range) ->
    [type];
node_roles(type_map) ->
    [type];
node_roles(type_map_field) ->
    [type];
node_roles(type_record) ->
    [type];
node_roles(type_tuple) ->
    [type];
node_roles(type_union) ->
    [type];
node_roles(typed_record_field) ->
    [type];
node_roles(underscore) ->
    [pattern];
node_roles(user_type_application) ->
    [type];
node_roles(variable) ->
    [expression, guard, pattern, type];
node_roles(warning_marker) ->
    [form];
node_roles(zip_generator) ->
    [expression];
node_roles(_Type) ->
    [expression, pattern, guard].
role_available(binary_field, binary_field) ->
    true;
role_available(map_field_assoc, map_field) ->
    true;
role_available(map_field_exact, map_field) ->
    true;
role_available(variable, type_param) ->
    true;
role_available(Type, Role) ->
    lists:member(Role, node_roles(Type)).
traverse_transparent(arity_qualifier) ->
    true;
traverse_transparent(class_qualifier) ->
    true;
traverse_transparent(conjunction) ->
    true;
traverse_transparent(disjunction) ->
    true;
traverse_transparent(operator) ->
    true;
traverse_transparent(size_qualifier) ->
    true;
traverse_transparent(_Type) ->
    false.
node_available(Type, OtpVsn) ->
    case node_bounds(Type) of
        {Since, Until} ->
            OtpVsn >= Since
            andalso
            OtpVsn =< Until;
        unknown ->
            true
    end.
node_bounds(annotated_type) ->
    {19, 29};
node_bounds(application) ->
    {19, 29};
node_bounds(arity_qualifier) ->
    {19, 29};
node_bounds(atom) ->
    {19, 29};
node_bounds(attribute) ->
    {19, 29};
node_bounds(binary) ->
    {19, 29};
node_bounds(binary_comp) ->
    {19, 29};
node_bounds(binary_field) ->
    {19, 29};
node_bounds(binary_generator) ->
    {19, 29};
node_bounds(bitstring_type) ->
    {19, 29};
node_bounds(block_expr) ->
    {19, 29};
node_bounds(case_expr) ->
    {19, 29};
node_bounds(catch_expr) ->
    {19, 29};
node_bounds(char) ->
    {19, 29};
node_bounds(class_qualifier) ->
    {19, 29};
node_bounds(clause) ->
    {19, 29};
node_bounds(comment) ->
    {19, 29};
node_bounds(cond_expr) ->
    {19, 22};
node_bounds(conjunction) ->
    {19, 29};
node_bounds(cons) ->
    {19, 29};
node_bounds(constrained_function_type) ->
    {19, 29};
node_bounds(constraint) ->
    {19, 29};
node_bounds(disjunction) ->
    {19, 29};
node_bounds(else_expr) ->
    {25, 29};
node_bounds(eof_marker) ->
    {19, 29};
node_bounds(error_marker) ->
    {19, 29};
node_bounds(float) ->
    {19, 29};
node_bounds(fun_expr) ->
    {19, 29};
node_bounds(fun_type) ->
    {19, 29};
node_bounds(function) ->
    {19, 29};
node_bounds(function_type) ->
    {19, 29};
node_bounds(generator) ->
    {19, 29};
node_bounds(if_expr) ->
    {19, 29};
node_bounds(implicit_fun) ->
    {19, 29};
node_bounds(infix_expr) ->
    {19, 29};
node_bounds(integer) ->
    {19, 29};
node_bounds(integer_range_type) ->
    {19, 29};
node_bounds(list) ->
    {19, 29};
node_bounds(list_comp) ->
    {19, 29};
node_bounds(map_comp) ->
    {26, 29};
node_bounds(map_expr) ->
    {19, 29};
node_bounds(map_field_assoc) ->
    {19, 29};
node_bounds(map_field_exact) ->
    {19, 29};
node_bounds(map_generator) ->
    {26, 29};
node_bounds(map_type) ->
    {19, 29};
node_bounds(map_type_assoc) ->
    {19, 29};
node_bounds(map_type_exact) ->
    {19, 29};
node_bounds(match_expr) ->
    {19, 29};
node_bounds(maybe_expr) ->
    {25, 29};
node_bounds(maybe_match_expr) ->
    {25, 29};
node_bounds(module_qualifier) ->
    {19, 29};
node_bounds(named_fun_expr) ->
    {19, 29};
node_bounds(nil) ->
    {19, 29};
node_bounds(operator) ->
    {19, 29};
node_bounds(parentheses) ->
    {19, 29};
node_bounds(prefix_expr) ->
    {19, 29};
node_bounds(receive_expr) ->
    {19, 29};
node_bounds(record_access) ->
    {19, 29};
node_bounds(record_expr) ->
    {19, 29};
node_bounds(record_field) ->
    {19, 29};
node_bounds(record_index_expr) ->
    {19, 29};
node_bounds(record_type) ->
    {19, 29};
node_bounds(record_type_field) ->
    {19, 29};
node_bounds(remote_type) ->
    {19, 29};
node_bounds(size_qualifier) ->
    {19, 29};
node_bounds(strict_binary_generator) ->
    {28, 29};
node_bounds(strict_generator) ->
    {28, 29};
node_bounds(strict_map_generator) ->
    {28, 29};
node_bounds(string) ->
    {19, 29};
node_bounds(text) ->
    {19, 29};
node_bounds(try_expr) ->
    {19, 29};
node_bounds(tuple) ->
    {19, 29};
node_bounds(tuple_type) ->
    {19, 29};
node_bounds(type_application) ->
    {19, 29};
node_bounds(type_binary) ->
    {19, 29};
node_bounds(type_fun) ->
    {19, 29};
node_bounds(type_integer_range) ->
    {19, 29};
node_bounds(type_map) ->
    {19, 29};
node_bounds(type_map_field) ->
    {19, 29};
node_bounds(type_record) ->
    {19, 29};
node_bounds(type_tuple) ->
    {19, 29};
node_bounds(type_union) ->
    {19, 29};
node_bounds(typed_record_field) ->
    {19, 29};
node_bounds(underscore) ->
    {19, 29};
node_bounds(user_type_application) ->
    {19, 29};
node_bounds(variable) ->
    {19, 29};
node_bounds(warning_marker) ->
    {19, 29};
node_bounds(zip_generator) ->
    {28, 29};
node_bounds(_Type) ->
    unknown.
format_available(Type, Node, OtpVsn) ->
    node_available(Type, OtpVsn)
    andalso
    case erl_syntax:is_tree(Node) of
        true ->
            true;
        false ->
            case format_bounds(Type, Node) of
                {Since, Until} ->
                    OtpVsn >= Since
                    andalso
                    OtpVsn =< Until;
                unknown ->
                    not format_required(Type)
            end
    end.
format_bounds(annotated_type, {ann_type, _, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(application, {call, _, Node1, Nodes2})
    when not is_list(Node1), is_list(Nodes2) ->
    {19, 29};
format_bounds(atom, {atom, _, _}) ->
    {19, 29};
format_bounds(attribute, {attribute, _, _, _}) ->
    {19, 29};
format_bounds(binary, {bin, _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(binary_comp, {bc, _, Node1, Nodes2})
    when not is_list(Node1), is_list(Nodes2) ->
    {19, 29};
format_bounds(binary_field, {bin_element, _, Node1, default, default})
    when not is_list(Node1) ->
    {19, 29};
format_bounds(binary_field, {bin_element, _, Node1, Node2, default})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(binary_field, {bin_element, _, Node1, default, Values2})
    when not is_list(Node1), is_list(Values2) ->
    {19, 29};
format_bounds(binary_field, {bin_element, _, Node1, Node2, Values3})
    when not is_list(Node1), not is_list(Node2), is_list(Values3) ->
    {19, 29};
format_bounds(binary_generator, {b_generate, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(bitstring_type, {type, _, binary, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(block_expr, {block, _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(case_expr, {'case', _, Node1, Nodes2})
    when not is_list(Node1), is_list(Nodes2) ->
    {19, 29};
format_bounds(catch_expr, {'catch', _, Node1}) when not is_list(Node1) ->
    {19, 29};
format_bounds(char, {char, _, _}) ->
    {19, 29};
format_bounds(clause, {clause, _, Nodes1, Nodes2, Nodes3})
    when is_list(Nodes1), is_list(Nodes2), is_list(Nodes3) ->
    {19, 29};
format_bounds(cons, {cons, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(constrained_function_type,
              {type, _, bounded_fun, [Node1, Nodes2]})
    when is_list(Nodes2), not is_list(Node1) ->
    {19, 29};
format_bounds(constraint, {type, _, constraint, [Node1, Nodes2]})
    when is_list(Nodes2), not is_list(Node1) ->
    {19, 29};
format_bounds(else_expr, {'else', _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(eof_marker, {eof, _}) ->
    {19, 29};
format_bounds(error_marker, {error, _}) ->
    {19, 29};
format_bounds(float, {float, _, _}) ->
    {19, 29};
format_bounds(fun_expr, {'fun', _, {clauses, Nodes1}})
    when is_list(Nodes1) ->
    {19, 29};
format_bounds(fun_type, {type, _, 'fun', []}) ->
    {19, 29};
format_bounds(function, {function, _, _, _, Nodes1})
    when is_list(Nodes1) ->
    {19, 29};
format_bounds(function_type,
              {type, _, 'fun', [{type, _, product, Nodes1}, Node2]})
    when not is_list(Node2), is_list(Nodes1) ->
    {19, 29};
format_bounds(function_type, {type, _, 'fun', [{type, _, any}, Node1]})
    when not is_list(Node1) ->
    {19, 29};
format_bounds(generator, {generate, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(if_expr, {'if', _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(implicit_fun, {'fun', _, {function, _, _}}) ->
    {19, 29};
format_bounds(implicit_fun, {'fun', _, {function, Node1, Node2, Node3}})
    when not is_list(Node3), not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(infix_expr, {op, _, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(integer, {integer, _, _}) ->
    {19, 29};
format_bounds(integer_range_type, {type, _, range, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(list, {cons, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(list_comp, {lc, _, Node1, Nodes2})
    when not is_list(Node1), is_list(Nodes2) ->
    {19, 29};
format_bounds(list_comp, {lc, _, Nodes1, Nodes2})
    when is_list(Nodes1), is_list(Nodes2) ->
    {29, 29};
format_bounds(map_comp, {mc, _, Node1, Nodes2})
    when not is_list(Node1), is_list(Nodes2) ->
    {19, 29};
format_bounds(map_comp, {mc, _, Nodes1, Nodes2})
    when is_list(Nodes1), is_list(Nodes2) ->
    {29, 29};
format_bounds(map_expr, {map, _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(map_expr, {map, _, Node1, Nodes2})
    when not is_list(Node1), is_list(Nodes2) ->
    {19, 29};
format_bounds(map_field_assoc, {map_field_assoc, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(map_field_exact, {map_field_exact, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(map_generator, {m_generate, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(map_type, {type, _, map, any}) ->
    {19, 29};
format_bounds(map_type, {type, _, map, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(map_type_assoc,
              {type, _, map_field_assoc, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(map_type_exact,
              {type, _, map_field_exact, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(match_expr, {match, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(maybe_expr, {'maybe', _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(maybe_expr, {'maybe', _, Nodes1, {'else', _, Nodes2}})
    when is_list(Nodes1), is_list(Nodes2) ->
    {19, 29};
format_bounds(maybe_match_expr, {maybe_match, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(module_qualifier, {remote, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(named_fun_expr, {named_fun, _, _, Nodes1})
    when is_list(Nodes1) ->
    {19, 29};
format_bounds(nil, {nil, _}) ->
    {19, 29};
format_bounds(prefix_expr, {op, _, _, Node1}) when not is_list(Node1) ->
    {19, 29};
format_bounds(receive_expr, {'receive', _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(receive_expr, {'receive', _, Nodes1, Node2, Nodes3})
    when is_list(Nodes1), not is_list(Node2), is_list(Nodes3) ->
    {19, 29};
format_bounds(record_access,
              {record_field, _, Node1, LocalRecord2, Node3})
    when not is_list(Node1), is_atom(LocalRecord2), not is_list(Node3) ->
    {19, 29};
format_bounds(record_access,
              {record_field, _, Node1, {Module2, Record3}, Node4})
    when
        not is_list(Node1),
        is_atom(Record3),
        is_atom(Module2),
        not is_list(Node4) ->
    {29, 29};
format_bounds(record_access, {record_field, _, Node1, [], Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {29, 29};
format_bounds(record_expr, {record, _, LocalRecord1, Nodes2})
    when is_atom(LocalRecord1), is_list(Nodes2) ->
    {19, 29};
format_bounds(record_expr, {record, _, Node1, LocalRecord2, Nodes3})
    when not is_list(Node1), is_atom(LocalRecord2), is_list(Nodes3) ->
    {19, 29};
format_bounds(record_expr, {record, _, {Module1, Record2}, Nodes3})
    when is_atom(Record2), is_atom(Module1), is_list(Nodes3) ->
    {29, 29};
format_bounds(record_expr, {record, _, [], Nodes1}) when is_list(Nodes1) ->
    {29, 29};
format_bounds(record_expr,
              {record, _, Node1, {Module2, Record3}, Nodes4})
    when
        not is_list(Node1),
        is_atom(Record3),
        is_atom(Module2),
        is_list(Nodes4) ->
    {29, 29};
format_bounds(record_expr, {record, _, Node1, [], Nodes2})
    when not is_list(Node1), is_list(Nodes2) ->
    {29, 29};
format_bounds(record_field, {record_field, _, Node1})
    when not is_list(Node1) ->
    {19, 29};
format_bounds(record_field, {record_field, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(record_index_expr, {record_index, _, _, Node1})
    when not is_list(Node1) ->
    {19, 29};
format_bounds(record_type, {type, _, record, [Node1 | Nodes2]})
    when is_list(Nodes2), not is_list(Node1) ->
    {19, 29};
format_bounds(record_type_field, {type, _, field_type, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(remote_type, {type, _, _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(remote_type, {remote_type, _, [Node1, Node2, Nodes3]})
    when is_list(Nodes3), not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(strict_binary_generator,
              {b_generate_strict, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(strict_generator, {generate_strict, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(strict_map_generator,
              {m_generate_strict, _, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(string, {string, _, _}) ->
    {19, 29};
format_bounds(try_expr, {'try', _, Nodes1, Nodes2, Nodes3, Nodes4})
    when
        is_list(Nodes1),
        is_list(Nodes2),
        is_list(Nodes3),
        is_list(Nodes4) ->
    {19, 29};
format_bounds(tuple, {tuple, _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(tuple_type, {type, _, tuple, any}) ->
    {19, 29};
format_bounds(tuple_type, {type, _, tuple, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(type_application, {type, _, _, Nodes1})
    when is_list(Nodes1) ->
    {19, 29};
format_bounds(type_application,
              {remote_type, _, [Node1, Node2, Nodes3]})
    when is_list(Nodes3), not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(type_binary, {type, _, binary, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(type_fun, {type, _, 'fun', []}) ->
    {19, 29};
format_bounds(type_integer_range, {type, _, range, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(type_map, {type, _, map, any}) ->
    {19, 29};
format_bounds(type_map, {type, _, map, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(type_map_field,
              {type, _, map_field_assoc, [Node1, Node2]})
    when not is_list(Node2), not is_list(Node1) ->
    {19, 29};
format_bounds(type_record, {type, _, record, [Node1 | Nodes2]})
    when is_list(Nodes2), not is_list(Node1) ->
    {19, 29};
format_bounds(type_tuple, {type, _, tuple, any}) ->
    {19, 29};
format_bounds(type_tuple, {type, _, tuple, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(type_union, {type, _, union, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(typed_record_field, {typed_record_field, Node1, Node2})
    when not is_list(Node1), not is_list(Node2) ->
    {19, 29};
format_bounds(underscore, {var, _, '_'}) ->
    {19, 29};
format_bounds(user_type_application, {user_type, _, _, Nodes1})
    when is_list(Nodes1) ->
    {19, 29};
format_bounds(variable, {var, _, _}) ->
    {19, 29};
format_bounds(warning_marker, {warning, _}) ->
    {19, 29};
format_bounds(zip_generator, {zip, _, Nodes1}) when is_list(Nodes1) ->
    {19, 29};
format_bounds(_Type, _Node) ->
    unknown.
format_required(annotated_type) ->
    true;
format_required(application) ->
    true;
format_required(atom) ->
    true;
format_required(attribute) ->
    true;
format_required(binary) ->
    true;
format_required(binary_comp) ->
    true;
format_required(binary_field) ->
    true;
format_required(binary_generator) ->
    true;
format_required(bitstring_type) ->
    true;
format_required(block_expr) ->
    true;
format_required(case_expr) ->
    true;
format_required(catch_expr) ->
    true;
format_required(char) ->
    true;
format_required(clause) ->
    true;
format_required(cons) ->
    true;
format_required(constrained_function_type) ->
    true;
format_required(constraint) ->
    true;
format_required(else_expr) ->
    true;
format_required(eof_marker) ->
    true;
format_required(error_marker) ->
    true;
format_required(float) ->
    true;
format_required(fun_expr) ->
    true;
format_required(fun_type) ->
    true;
format_required(function) ->
    true;
format_required(function_type) ->
    true;
format_required(generator) ->
    true;
format_required(if_expr) ->
    true;
format_required(implicit_fun) ->
    true;
format_required(infix_expr) ->
    true;
format_required(integer) ->
    true;
format_required(integer_range_type) ->
    true;
format_required(list) ->
    true;
format_required(list_comp) ->
    true;
format_required(map_comp) ->
    true;
format_required(map_expr) ->
    true;
format_required(map_field_assoc) ->
    true;
format_required(map_field_exact) ->
    true;
format_required(map_generator) ->
    true;
format_required(map_type) ->
    true;
format_required(map_type_assoc) ->
    true;
format_required(map_type_exact) ->
    true;
format_required(match_expr) ->
    true;
format_required(maybe_expr) ->
    true;
format_required(maybe_match_expr) ->
    true;
format_required(module_qualifier) ->
    true;
format_required(named_fun_expr) ->
    true;
format_required(nil) ->
    true;
format_required(prefix_expr) ->
    true;
format_required(receive_expr) ->
    true;
format_required(record_access) ->
    true;
format_required(record_expr) ->
    true;
format_required(record_field) ->
    true;
format_required(record_index_expr) ->
    true;
format_required(record_type) ->
    true;
format_required(record_type_field) ->
    true;
format_required(remote_type) ->
    true;
format_required(strict_binary_generator) ->
    true;
format_required(strict_generator) ->
    true;
format_required(strict_map_generator) ->
    true;
format_required(string) ->
    true;
format_required(try_expr) ->
    true;
format_required(tuple) ->
    true;
format_required(tuple_type) ->
    true;
format_required(type_application) ->
    true;
format_required(type_binary) ->
    true;
format_required(type_fun) ->
    true;
format_required(type_integer_range) ->
    true;
format_required(type_map) ->
    true;
format_required(type_map_field) ->
    true;
format_required(type_record) ->
    true;
format_required(type_tuple) ->
    true;
format_required(type_union) ->
    true;
format_required(typed_record_field) ->
    true;
format_required(underscore) ->
    true;
format_required(user_type_application) ->
    true;
format_required(variable) ->
    true;
format_required(warning_marker) ->
    true;
format_required(zip_generator) ->
    true;
format_required(_Type) ->
    false.
slot_available(map_field_exact, map_field_exact_key, atom, _ChildNode,
               OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(map_field_exact, map_field_exact_key, char, _ChildNode,
               OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(map_field_exact, map_field_exact_key, float, _ChildNode,
               OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(map_field_exact, map_field_exact_key, integer,
               _ChildNode, OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(map_field_exact, map_field_exact_key, nil, _ChildNode,
               OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(map_field_exact, map_field_exact_key, string, _ChildNode,
               OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(map_field_exact, map_field_exact_key, variable,
               _ChildNode, OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(map_field_exact, map_field_exact_key, _ChildType,
               _ChildNode, OtpVsn)
    when OtpVsn =< 22 ->
    false;
slot_available(size_qualifier, size, default, _ChildNode, OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(size_qualifier, size, integer, _ChildNode, OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(size_qualifier, size, variable, _ChildNode, OtpVsn)
    when OtpVsn =< 22 ->
    true;
slot_available(size_qualifier, size, _ChildType, _ChildNode, OtpVsn)
    when OtpVsn =< 22 ->
    false;
slot_available(try_expr, handlers, _ChildType,
               {clause, _, [], Nodes1, Nodes2},
               OtpVsn)
    when OtpVsn =< 20, is_list(Nodes1), is_list(Nodes2) ->
    true;
slot_available(try_expr, handlers, _ChildType,
               {clause, _, [Node1, Node2], Nodes3, Nodes4},
               OtpVsn)
    when
        OtpVsn =< 20,
        not is_list(Node2),
        not is_list(Node1),
        is_list(Nodes3),
        is_list(Nodes4) ->
    true;
slot_available(try_expr, handlers, _ChildType,
               {clause, _, [{tuple, _, [Node1, Node2]}], Nodes3, Nodes4},
               OtpVsn)
    when
        OtpVsn =< 20,
        not is_list(Node1),
        not is_list(Node2),
        is_list(Nodes3),
        is_list(Nodes4) ->
    true;
slot_available(try_expr, handlers, _ChildType,
               {clause, _,
                [{tuple, _, [Node1, Node2, {var, _, '_'}]}],
                Nodes3, Nodes4},
               OtpVsn)
    when
        OtpVsn =< 20,
        not is_list(Node1),
        not is_list(Node2),
        is_list(Nodes3),
        is_list(Nodes4) ->
    true;
slot_available(try_expr, handlers, _ChildType,
               {clause, _,
                [{tree, class_qualifier, _,
                  {class_qualifier, Node1, Node2}}],
                Nodes3, Nodes4},
               OtpVsn)
    when
        OtpVsn =< 20,
        not is_list(Node1),
        not is_list(Node2),
        is_list(Nodes3),
        is_list(Nodes4) ->
    true;
slot_available(try_expr, handlers, _ChildType,
               {clause, _,
                [{tree, class_qualifier, _,
                  {class_qualifier, Node1, Node2, {var, _, '_'}}}],
                Nodes3, Nodes4},
               OtpVsn)
    when
        OtpVsn =< 20,
        not is_list(Node1),
        not is_list(Node2),
        is_list(Nodes3),
        is_list(Nodes4) ->
    true;
slot_available(try_expr, handlers, _ChildType, _ChildNode, OtpVsn)
    when OtpVsn =< 20 ->
    false;
slot_available(binary, elements, binary_field, _ChildNode, _OtpVsn) ->
    true;
slot_available(map_comp, template, map_field_assoc, _ChildNode, _OtpVsn) ->
    true;
slot_available(map_comp, template, map_field_exact, _ChildNode, _OtpVsn) ->
    true;
slot_available(map_expr, fields, map_field_assoc, _ChildNode, _OtpVsn) ->
    true;
slot_available(map_expr, fields, map_field_exact, _ChildNode, _OtpVsn) ->
    true;
slot_available(map_generator, pattern, map_field_assoc, _ChildNode,
               _OtpVsn) ->
    true;
slot_available(map_generator, pattern, map_field_exact, _ChildNode,
               _OtpVsn) ->
    true;
slot_available(strict_map_generator, pattern, map_field_assoc,
               _ChildNode, _OtpVsn) ->
    true;
slot_available(strict_map_generator, pattern, map_field_exact,
               _ChildNode, _OtpVsn) ->
    true;
slot_available(_ParentType, _Slot, binary_field, _ChildNode, _OtpVsn) ->
    false;
slot_available(_ParentType, _Slot, map_field_assoc, _ChildNode, _OtpVsn) ->
    false;
slot_available(_ParentType, _Slot, map_field_exact, _ChildNode, _OtpVsn) ->
    false;
slot_available(_ParentType, _Slot, _ChildType, _ChildNode, _OtpVsn) ->
    true.
child_layout(attribute, [[NameTree], BodyTrees], _ParentRole, OtpVsn)
    when is_list(BodyTrees) ->
    try erl_syntax:atom_value(NameTree) of
        Name ->
            attribute_layout(Name, NameTree, BodyTrees, OtpVsn)
    catch
        _:_ ->
            {error, {invalid_attribute_body, invalid_name, BodyTrees}}
    end;
child_layout(annotated_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(application, [Group1 = [_], Group2], _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{operator, expression, Group1, nodes},
      {arguments, expression, Group2, nodes}]};
child_layout(arity_qualifier, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(atom, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(binary, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{elements, binary_field, Group1, nodes}]};
child_layout(binary_comp, [Group1 = [_], Group2], _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{template, expression, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(binary_field, [Group1 = [_]], ParentRole, _OtpVsn) ->
    {ok, [{value, ParentRole, Group1, nodes}]};
child_layout(binary_field, [Group1 = [_], Group2], ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{value, ParentRole, Group1, nodes},
      {types, attribute_body, Group2, nodes}]};
child_layout(binary_generator,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{pattern, pattern, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(bitstring_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(block_expr, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(case_expr, [Group1 = [_], Group2], _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{argument, expression, Group1, nodes},
      {clauses, clause, Group2, nodes}]};
child_layout(catch_expr, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(char, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(class_qualifier, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(clause, [Group1, Group2], _ParentRole, _OtpVsn)
    when is_list(Group1), is_list(Group2) ->
    {ok,
     [{patterns, pattern, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(clause, [Group1, Group2, Group3], _ParentRole, _OtpVsn)
    when is_list(Group1), is_list(Group2), is_list(Group3) ->
    {ok,
     [{patterns, pattern, Group1, nodes},
      {guards, guard, Group2, nodes},
      {body, expression, Group3, nodes}]};
child_layout(comment, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(cond_expr, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{clauses, clause, Group1, nodes}]};
child_layout(conjunction, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(cons, Subtrees, ParentRole, _OtpVsn) when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(constrained_function_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(constraint, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(disjunction, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(else_expr, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{clauses, clause, Group1, nodes}]};
child_layout(eof_marker, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(error_marker, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(float, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(fun_expr, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{clauses, clause, Group1, nodes}]};
child_layout(fun_type, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(function, [Group1 = [_], Group2], _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{name, name, Group1, nodes}, {clauses, clause, Group2, nodes}]};
child_layout(function_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(generator,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{pattern, pattern, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(if_expr, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{clauses, clause, Group1, nodes}]};
child_layout(implicit_fun, [Group1 = [_]], _ParentRole, _OtpVsn) ->
    {ok, [{name, expression, Group1, nodes}]};
child_layout(infix_expr, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(integer, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(integer_range_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(list, Subtrees, ParentRole, _OtpVsn) when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(list_comp, [Group1 = [_], Group2], _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{template, expression, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(list_comp, [Group1, Group2], _ParentRole, OtpVsn)
    when is_list(Group1), is_list(Group2), OtpVsn >= 29 ->
    {ok,
     [{template, expression, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(map_comp, [Group1 = [_], Group2], _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{template, map_field, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(map_comp, [Group1, Group2], _ParentRole, OtpVsn)
    when is_list(Group1), is_list(Group2), OtpVsn >= 29 ->
    {ok,
     [{template, map_field, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(map_expr, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{fields, map_field, Group1, nodes}]};
child_layout(map_expr, [Group1 = [_], Group2], _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{argument, expression, Group1, nodes},
      {fields, map_field, Group2, nodes}]};
child_layout(map_field_assoc,
             [Group1 = [_], Group2 = [_]],
             pattern, _OtpVsn) ->
    {ok,
     [{map_field_assoc_key, expression, Group1, nodes},
      {map_field_assoc_value, pattern, Group2, nodes}]};
child_layout(map_field_assoc,
             [Group1 = [_], Group2 = [_]],
             ParentRole, _OtpVsn) ->
    {ok,
     [{map_field_assoc_key, ParentRole, Group1, nodes},
      {map_field_assoc_value, ParentRole, Group2, nodes}]};
child_layout(map_field_exact,
             [Group1 = [_], Group2 = [_]],
             pattern, _OtpVsn) ->
    {ok,
     [{map_field_exact_key, expression, Group1, nodes},
      {map_field_exact_value, pattern, Group2, nodes}]};
child_layout(map_field_exact,
             [Group1 = [_], Group2 = [_]],
             ParentRole, _OtpVsn) ->
    {ok,
     [{map_field_exact_key, ParentRole, Group1, nodes},
      {map_field_exact_value, ParentRole, Group2, nodes}]};
child_layout(map_generator,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{pattern, pattern, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(map_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(map_type_assoc, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(map_type_exact, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(match_expr, [Group1 = [_], Group2 = [_]], pattern, _OtpVsn) ->
    {ok,
     [{left, pattern, Group1, nodes}, {right, pattern, Group2, nodes}]};
child_layout(match_expr,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{left, pattern, Group1, nodes},
      {right, expression, Group2, nodes}]};
child_layout(maybe_expr, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{body, expression, Group1, nodes}]};
child_layout(maybe_expr, [Group1, Group2], _ParentRole, _OtpVsn)
    when is_list(Group1), is_list(Group2) ->
    {ok,
     [{body, expression, Group1, nodes},
      {else_clause, clause, Group2, nodes}]};
child_layout(maybe_match_expr,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{left, pattern, Group1, nodes},
      {right, expression, Group2, nodes}]};
child_layout(module_qualifier, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(named_fun_expr,
             [Group1 = [_], Group2],
             _ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{name, pattern, Group1, nodes}, {clauses, clause, Group2, nodes}]};
child_layout(nil, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(operator, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(parentheses, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(prefix_expr, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(receive_expr, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{clauses, clause, Group1, nodes}]};
child_layout(receive_expr,
             [Group1, Group2 = [_], Group3],
             _ParentRole, _OtpVsn)
    when is_list(Group1), is_list(Group3) ->
    {ok,
     [{clauses, clause, Group1, nodes},
      {timeout, expression, Group2, nodes},
      {action, expression, Group3, nodes}]};
child_layout(record_access,
             [Group1 = [_], Group2 = [_], Group3 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{argument, expression, Group1, nodes},
      {type, expression, Group2, nodes},
      {field, expression, Group3, nodes}]};
child_layout(record_expr,
             [Group1 = [_ | _], Group2],
             ParentRole, _OtpVsn)
    when is_list(Group2) ->
    {ok,
     [{type, expression, Group1, nodes},
      {fields, ParentRole, Group2, nodes}]};
child_layout(record_expr,
             [Group1 = [_], Group2 = [_ | _], Group3],
             ParentRole, _OtpVsn)
    when is_list(Group3) ->
    {ok,
     [{argument, expression, Group1, nodes},
      {type, expression, Group2, nodes},
      {fields, ParentRole, Group3, nodes}]};
child_layout(record_field, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(record_index_expr, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(record_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(record_type_field, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(remote_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(size_qualifier,
             [Group1 = [_], Group2 = [_]],
             ParentRole, _OtpVsn) ->
    {ok,
     [{value, ParentRole, Group1, nodes},
      {size, binary_size, Group2, nodes}]};
child_layout(strict_binary_generator,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{pattern, pattern, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(strict_generator,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{pattern, pattern, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(strict_map_generator,
             [Group1 = [_], Group2 = [_]],
             _ParentRole, _OtpVsn) ->
    {ok,
     [{pattern, pattern, Group1, nodes},
      {body, expression, Group2, nodes}]};
child_layout(string, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(text, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(try_expr,
             [Group1, Group2, Group3, Group4],
             _ParentRole, _OtpVsn)
    when
        is_list(Group1),
        is_list(Group2),
        is_list(Group3),
        is_list(Group4) ->
    {ok,
     [{body, expression, Group1, nodes},
      {clauses, clause, Group2, nodes},
      {handlers, clause, Group3, nodes},
      {'after', expression, Group4, nodes}]};
child_layout(tuple, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(tuple_type, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_application, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_binary, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_fun, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(type_integer_range, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_map, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_map_field, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_record, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_tuple, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(type_union, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(typed_record_field, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(underscore, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(user_type_application, Subtrees, ParentRole, _OtpVsn)
    when is_list(Subtrees) ->
    {ok, [{elements, ParentRole, Subtrees, groups}]};
child_layout(variable, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(warning_marker, [], _ParentRole, _OtpVsn) ->
    {ok, []};
child_layout(zip_generator, [Group1], _ParentRole, _OtpVsn)
    when is_list(Group1) ->
    {ok, [{body, expression, Group1, nodes}]};
child_layout(Type, Subtrees, _ParentRole, _OtpVsn) ->
    {error, {invalid_syntax_layout, Type, Subtrees}}.
attribute_layout(callback, NameTree, [Group1 | Group2], _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {spec_mfa, attribute_body, [Group1], nodes},
      {specs, type, Group2, nodes}]};
attribute_layout(callback, _NameTree, BodyTrees, _OtpVsn) ->
    {error, {invalid_attribute_body, callback, BodyTrees}};
attribute_layout(export_record, NameTree, Group1, OtpVsn)
    when OtpVsn >= 29 ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, Group1, nodes}]};
attribute_layout(export_record, NameTree, BodyTrees, _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, BodyTrees, nodes}]};
attribute_layout(import_record, NameTree, Group1, OtpVsn)
    when OtpVsn >= 29 ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, Group1, nodes}]};
attribute_layout(import_record, NameTree, BodyTrees, _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, BodyTrees, nodes}]};
attribute_layout(native_record, NameTree, Group1, OtpVsn)
    when OtpVsn >= 29 ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, Group1, nodes}]};
attribute_layout(native_record, NameTree, BodyTrees, _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, BodyTrees, nodes}]};
attribute_layout(opaque, NameTree, [Group1, Group2 | Group3], _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {type_name, name, [Group1], nodes},
      {type_body, type, [Group2], nodes},
      {type_params, type_param, Group3, nodes}]};
attribute_layout(opaque, _NameTree, BodyTrees, _OtpVsn) ->
    {error, {invalid_attribute_body, opaque, BodyTrees}};
attribute_layout(record, NameTree, Group1, _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, Group1, nodes}]};
attribute_layout(spec, NameTree, [Group1 | Group2], _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {spec_mfa, attribute_body, [Group1], nodes},
      {specs, type, Group2, nodes}]};
attribute_layout(spec, _NameTree, BodyTrees, _OtpVsn) ->
    {error, {invalid_attribute_body, spec, BodyTrees}};
attribute_layout(type, NameTree, [Group1, Group2 | Group3], _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {type_name, name, [Group1], nodes},
      {type_body, type, [Group2], nodes},
      {type_params, type_param, Group3, nodes}]};
attribute_layout(type, _NameTree, BodyTrees, _OtpVsn) ->
    {error, {invalid_attribute_body, type, BodyTrees}};
attribute_layout(_Name, NameTree, BodyTrees, _OtpVsn) ->
    {ok,
     [{name, name, [NameTree], nodes},
      {body, attribute_body, BodyTrees, nodes}]}.
