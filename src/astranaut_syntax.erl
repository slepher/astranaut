%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc OTP-compatible syntax-tree helpers, validation, and normalization.
%%%
%%% Form ordering APIs remain as compatibility proxies to astranaut_forms.
%%% @end
%%% Created : 18 Mar 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_syntax).

-include("otp_vsn.hrl").

-type form() :: erl_parse:abstract_form().
-type node_role() :: expression | pattern | guard | form | type | clause |
                     name | type_param | attribute_body | binary_size |
                     map_field | binary_field.
-type validator() :: {role, node_role()} |
                     {slot, atom(), atom(), node_role()}.
-type validation_opts() ::
        #{attr => map(),
          forms => [form()],
          record_defs => [form()],
          otp_vsn => integer() | 'pre-21',
          fail => raise | collect}.
-type validation_error() :: map().
-type child_spec() ::
        #{slot := atom(),
          role := node_role(),
          validator := validator(),
          subtrees := term(),
          attr := map()}.

-export_type([node_role/0, validator/0, validation_opts/0,
              validation_error/0, child_spec/0]).

%% API
-export([type/1, otp_vsn/0, get_pos/1, set_pos/2, is_pos/1, is_leaf/1]).
-export([subtrees/1, update_tree/2, revert/1]).
-export([subtrees_pge/3, attribute_subtrees_type/3]).
-export([validate_node/2, validate_node/3, normalize/2, normalize/3,
         child_specs/3, node_roles/1]).
-export([pattern_node/1, guard_node/1, expression_node/1, update_node/2]).
-export([reorder_updated_forms/1, sort_forms/1, insert_forms/2]).

-spec type(term()) -> atom().
type(Node) ->
    erl_syntax:type(Node).

-spec otp_vsn() -> integer() | 'pre-21'.
otp_vsn() ->
    ?ASTRANAUT_OTP_VSN.

-spec get_pos(term()) -> term().
get_pos(Node) ->
    erl_syntax:get_pos(Node).

-spec set_pos(term(), term()) -> term().
set_pos(Node, Pos) ->
    case erl_syntax:is_tree(Node) of
        true ->
            erl_syntax:set_pos(Node, Pos);
        false ->
            %% unwrap node if node is a wrapper.
            Node1 = erl_syntax:revert(Node),
            set_pos_1(Node1, Pos)
    end.

set_pos_1({error, {_, Formatter, Error}}, Pos) ->
    {error, {Pos, Formatter, Error}};
set_pos_1({warning, {_, Formatter, Warning}}, Pos) ->
    {warning, {Pos, Formatter, Warning}};
set_pos_1(Node, Pos) ->
    setelement(2, Node, Pos).

-spec is_pos(term()) -> boolean().
is_pos(Pos) ->
    case Pos of
        Pos when is_integer(Pos) ->
            true;
        {Line, Column} when is_integer(Line), is_integer(Column) ->
            true;
        Pos ->
            false
    end.

-spec is_leaf(term()) -> boolean().
is_leaf(Node) ->
    erl_syntax:is_leaf(Node).

%% as this issue mentioned, it's a bug, but will cause compatibility issue
%% https://github.com/erlang/otp/issues/4529
%% the goal of revert/1 is to fix this without cause compatibility issues.
%% just use astranaut_syntax:subtrees/1 replace of erl_syntax:subtress/1,
%% astranaut_syntax:revert/1 replace of erl_syntax:revert/1.
-spec subtrees(erl_syntax:syntaxTree()) -> [[erl_syntax:syntaxTree()]].
subtrees({attribute, Pos, Name, {TypeName, TypeBody, TypeParams}}) when Name =:= type; Name =:= opaque ->
    NameTree = name_arity_tree(Name, Pos),
    TypeNameTree = name_arity_tree(TypeName, Pos),
    [[NameTree], [TypeNameTree, TypeBody|TypeParams]];
subtrees({attribute, Pos, Name, {MFA, Specs}}) when Name =:= spec; Name =:= callback ->
    NameTree = name_arity_tree(Name, Pos),
    MFATree = mfa_tree(MFA, Pos),
    [[NameTree], [MFATree|Specs]];
subtrees({attribute, _Pos, Name, Body})
  when Name =:= type; Name =:= opaque; Name =:= spec; Name =:= callback ->
    erlang:error({invalid_attribute_body, Name, Body});
subtrees({'try', _Pos, Body, Clauses, Handlers, After}) ->
    [Body, Clauses, Handlers, After];
subtrees(Node) ->
    erl_syntax:subtrees(Node).

mfa_tree(MFA, Pos) ->
    erl_syntax:set_pos(erl_syntax:tuple(lists:map(fun(Element) -> name_arity_tree(Element, Pos) end, tuple_to_list(MFA))), Pos).

name_arity_tree(Name, Pos) when is_atom(Name) ->
    erl_syntax:set_pos(erl_syntax:atom(Name), Pos);
name_arity_tree(Arity, Pos) when is_integer(Arity) ->
    erl_syntax:set_pos(erl_syntax:integer(Arity), Pos).

-spec update_tree(erl_syntax:syntaxTree(), [[erl_syntax:syntaxTree()]]) -> erl_syntax:syntaxTree().
update_tree(Node, Subtrees) ->
    erl_syntax:update_tree(Node, Subtrees).

-spec revert(term()) -> term().
revert(Node) ->
    case erl_syntax:is_tree(Node) of
        false ->
            erl_syntax:revert(Node);
        true ->
            case erl_syntax:type(Node) of
                attribute ->
                    Name = erl_syntax:concrete(erl_syntax:attribute_name(Node)),
                    Args = erl_syntax:attribute_arguments(Node),
                    Pos = erl_syntax:get_pos(Node),
                    revert_attribute(Name, Args, Pos, Node);
                _ ->
                    erl_syntax:revert(Node)
            end
    end.

revert_attribute(Name, [TypeNameTree, TypeTree|TypeParamTrees], Pos, _Node) when Name =:= type; Name =:= opaque ->
    TypeName = erl_syntax:atom_value(TypeNameTree),
    {attribute, Pos, Name, {TypeName, TypeTree, TypeParamTrees}};
revert_attribute(Name, [MFATree|SpecTrees], Pos, _Node) when Name =:= spec; Name =:= callback ->
    MFA = mfa_value(MFATree),
    {attribute, Pos, Name, {MFA, SpecTrees}};
revert_attribute(_Name, _Subtrees, _Pos, Node) ->
    erl_syntax:revert(Node).

mfa_value(MFATree) ->
    tuple = erl_syntax:type(MFATree),
    list_to_tuple(
      lists:map(
        fun(MFA) -> 
                Type = erl_syntax:type(MFA),
                name_arity_value(Type, MFA) 
        end, erl_syntax:tuple_elements(MFATree))).

name_arity_value(atom, NameTree) ->
    erl_syntax:atom_value(NameTree);
name_arity_value(integer, ArityTree) ->
    erl_syntax:integer_value(ArityTree).

-spec subtrees_pge(atom(), term(), map()) -> term().
subtrees_pge(Type, Subtrees, Attr) ->
    child_specs_annotated_subtrees(child_specs(Type, Subtrees, Attr)).

-spec attribute_subtrees_type(atom(), term(), map()) -> term().
attribute_subtrees_type(attribute, [[NameTree], BodyTrees], #{}) ->
    Name = erl_syntax:atom_value(NameTree),
    [[NameTree], update_attribute_body_trees(Name, BodyTrees)];
attribute_subtrees_type(_Type, Subtrees, #{}) ->
    Subtrees.

update_attribute_body_trees(record = Name, [RecordNameTree|RecordBodyTrees]) ->
    attribute(Name, [name_node(RecordNameTree)|RecordBodyTrees]);
update_attribute_body_trees(Name, [TypeNameTree, TypeTree|TypeParamTrees]) when Name =:= type; Name =:= opaque ->
    attribute(Name, [name_node(TypeNameTree), type_node(TypeTree)|type_param_node(TypeParamTrees)]);
update_attribute_body_trees(Name, [SpecMFATree|SpecTrees]) when Name =:= spec; Name =:= callback ->
    attribute(Name, [name_node(SpecMFATree)|type_node(SpecTrees)]);
update_attribute_body_trees(Name, BodyTrees) ->
    attribute(Name, BodyTrees).

-spec pattern_node(term()) -> term().
pattern_node(Subtree) ->
    update_node(pattern, Subtree).

-spec guard_node(term()) -> term().
guard_node(Subtree) ->
    update_node(guard, Subtree).

-spec expression_node(term()) -> term().
expression_node(Subtree) ->
    update_node(expression, Subtree).

name_node(Subtree) ->
    Subtree.

type_node(Subtree) ->
    update_node(type, Subtree).

type_param_node(Subtree) ->
    Subtree.

attribute(Attribute, Subtree) ->
    astranaut_uniplate:up_attr(#{attribute => Attribute}, Subtree).

-spec update_node(node_role(), term()) -> term().
update_node(Node, Subtree) ->
    astranaut_uniplate:up_attr(#{node => Node}, Subtree).

%%===================================================================
%% syntax validation functions
%%===================================================================
-spec validate_node(erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()], validator()) ->
          ok | {error, validation_error()}.
validate_node(NodeOrNodes, Validator) ->
    validate_node(NodeOrNodes, Validator, #{}).

-spec validate_node(erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()], validator(),
                    validation_opts()) -> ok | {error, validation_error()}.
validate_node(NodeOrNodes, Validator, Opts) ->
    Env = validation_env(Opts),
    validate_node(NodeOrNodes, Validator, root, Env, []).

-spec normalize(erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()], validator()) ->
          {ok, erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()]} |
          {error, validation_error()}.
normalize(NodeOrNodes, Validator) ->
    normalize(NodeOrNodes, Validator, #{}).

-spec normalize(erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()], validator(),
                validation_opts()) ->
          {ok, erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()]} |
          {error, validation_error()}.
normalize(NodeOrNodes, Validator, Opts) ->
    Attr = validation_attr(Opts, validator_role(Validator)),
    Env = validation_env(Opts),
    normalize(NodeOrNodes, Validator, root, Attr, Env, []).

validation_attr(Opts, Role) ->
    Attr = maps:get(attr, Opts, #{}),
    case maps:is_key(node, Attr) orelse not node_role(Role) of
        true ->
            Attr;
        false ->
            Attr#{node => Role}
    end.

node_role(Role) ->
    lists:member(Role, [expression, pattern, guard, form, type, clause]).

slot_role(Role) ->
    lists:member(Role, [name, type_param, attribute_body, binary_size, map_field, binary_field]).

validation_env(Opts) ->
    #{forms => maps:get(record_defs, Opts, maps:get(forms, Opts, [])),
      otp_vsn => maps:get(otp_vsn, Opts, otp_vsn())}.

validate_node([], _Validator, _Slot, _Env, _Path) ->
    ok;
validate_node(Nodes, Validator, Slot, Env, Path) when is_list(Nodes) ->
    validate_node_list(Nodes, Validator, Slot, Env, Path, 1);
validate_node(Node, Validator, Slot, Env, Path) ->
    case validate_node_info(Node, Validator, Slot, Env, Path) of
        {ok, _NodeInfo} -> ok;
        {error, _Error} = Error -> Error
    end.

validate_node_info(Node, Validator, Slot, Env, Path) ->
    case node_info(Node) of
        {ok, Type, Pos, _Subtrees} = NodeInfo ->
            case role_allowed(Validator, Type, Node, Env) of
                true -> {ok, NodeInfo};
                false ->
                    ExpectedRole = validator_role(Validator),
                    {error, invalid_role_error(Validator, ExpectedRole, Slot, Type, Pos, Node, Path)}
            end;
        {error, Exception} ->
            {error, invalid_node_error(Validator, validator_role(Validator), Slot, Node, Exception, Path)}
    end.

normalize([], _Validator, _Slot, _Attr, _Env, _Path) ->
    {ok, []};
normalize(Nodes, Validator, Slot, Attr, Env, Path) when is_list(Nodes) ->
    normalize_node_list(Nodes, Validator, Slot, Attr, Env, Path, 1, []);
normalize(Node0, Validator, Slot, Attr, Env, Path) ->
    Node = unwrap_node_context(Node0),
    case validate_node_info(Node, Validator, Slot, Env, Path) of
        {ok, NodeInfo} ->
            normalize_node(Node, NodeInfo, Validator, Slot, Attr, Env, Path);
        {error, _Error} = Error ->
            Error
    end.

normalize_node(Node, {ok, Type, Pos, []}, Validator, Slot, _Attr, Env, Path) ->
    normalize_revert_node(Node, Validator, Slot, Type, Pos, Env, Path);
normalize_node(Node, {ok, Type, Pos, Subtrees}, Validator, Slot, Attr, Env, Path) ->
    case normalize_child_specs(child_specs(Type, Subtrees, Attr), Env, Path, []) of
        {ok, Specs1} ->
            normalize_rebuild_node(Node, Validator, Slot, Type, Pos, Specs1, Env, Path);
        {error, Error} ->
            {error, add_parent_error(Type, Pos, Error)}
    end.

normalize_node_list([Node|Nodes], Validator, Slot, Attr, Env, Path, Index, Acc) ->
    Path1 = Path ++ [path_item(Slot, Index, Validator, Node)],
    case normalize(Node, Validator, Slot, Attr, Env, Path1) of
        {ok, Node1} ->
            normalize_node_list(Nodes, Validator, Slot, Attr, Env, Path, Index + 1, [Node1|Acc]);
        {error, Error} ->
            {error, Error}
    end;
normalize_node_list([], _Validator, _Slot, _Attr, _Env, _Path, _Index, Acc) ->
    {ok, lists:reverse(Acc)}.

normalize_child_specs([#{slot := Slot, validator := Validator,
                         subtrees := Subtrees, attr := Attr} = Spec|Specs],
                      Env, Path, Acc) ->
    case normalize_child_subtrees(Subtrees, Validator, Slot, Attr, Env, Path, 1, []) of
        {ok, Subtrees1} ->
            Spec1 = Spec#{subtrees => Subtrees1},
            normalize_child_specs(Specs, Env, Path, [Spec1|Acc]);
        {error, Error} ->
            {error, Error}
    end;
normalize_child_specs([], _Env, _Path, Acc) ->
    {ok, lists:reverse(Acc)}.

normalize_child_subtrees([Subtree|Subtrees], Validator, Slot, Attr, Env, Path, Index, Acc)
  when is_list(Subtree) ->
    case normalize_child_subtrees(Subtree, Validator, Slot, Attr, Env, Path, 1, []) of
        {ok, Subtree1} ->
            normalize_child_subtrees(Subtrees, Validator, Slot, Attr, Env, Path, Index + 1, [Subtree1|Acc]);
        {error, Error} ->
            {error, Error}
    end;
normalize_child_subtrees([Node|Nodes], Validator, Slot, Attr, Env, Path, Index, Acc) ->
    Path1 = Path ++ [path_item(Slot, Index, Validator, Node)],
    case normalize(Node, Validator, Slot, Attr, Env, Path1) of
        {ok, Node1} ->
            normalize_child_subtrees(Nodes, Validator, Slot, Attr, Env, Path, Index + 1, [Node1|Acc]);
        {error, Error} ->
            {error, Error}
    end;
normalize_child_subtrees([], _Validator, _Slot, _Attr, _Env, _Path, _Index, Acc) ->
    {ok, lists:reverse(Acc)}.

normalize_rebuild_node(Node, Validator, Slot, Type, Pos, Specs, Env, Path) ->
    Subtrees1 = child_specs_plain_subtrees(Specs),
    try
        Node1 = revert(update_tree(Node, Subtrees1)),
        case validate_node(Node1, Validator, Slot, Env, Path) of
            ok ->
                {ok, Node1};
            {error, Error} ->
                {error, add_parent_error(Type, Pos, Error)}
        end
    catch
        Class:Reason ->
            {error, invalid_node_error(Validator, validator_role(Validator), Slot,
                                       Node, {Class, Reason}, Path)}
    end.

normalize_revert_node(Node, Validator, Slot, Type, Pos, Env, Path) ->
    try
        Node1 = revert(Node),
        case validate_node(Node1, Validator, Slot, Env, Path) of
            ok ->
                {ok, Node1};
            {error, Error} ->
                {error, add_parent_error(Type, Pos, Error)}
        end
    catch
        Class:Reason ->
            {error, invalid_node_error(Validator, validator_role(Validator), Slot,
                                       Node, {Class, Reason}, Path)}
    end.

validate_node_list([Node|Nodes], Validator, Slot, Env, Path, Index) ->
    Path1 = Path ++ [path_item(Slot, Index, Validator, Node)],
    case validate_node(Node, Validator, Slot, Env, Path1) of
        ok ->
            validate_node_list(Nodes, Validator, Slot, Env, Path, Index + 1);
        {error, Error} ->
            {error, Error}
    end;
validate_node_list([], _Validator, _Slot, _Env, _Path, _Index) ->
    ok.

node_info(Node) ->
    node_info_1(unwrap_node_context(Node)).

node_info_1(default) ->
    {ok, default, none, []};
node_info_1(Node) ->
    case is_tuple(Node) orelse erl_syntax:is_tree(Node) of
        true ->
            try
                Type = type(Node),
                Pos = get_pos(Node),
                Subtrees = subtrees(Node),
                ok = validate_subtrees_shape(Type, Subtrees),
                _Reverted = revert(Node),
                {ok, Type, Pos, Subtrees}
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end;
        false ->
            {error, {bad_syntax_tree, Node}}
    end.

validate_subtrees_shape(attribute, [[NameTree], BodyTrees]) ->
    validate_attribute_body_shape(erl_syntax:atom_value(NameTree), BodyTrees);
validate_subtrees_shape(_Type, _Subtrees) ->
    ok.

node_type_info(Node) ->
    case unwrap_node_context(Node) of
        default ->
            {ok, default, none};
        Node1 ->
            node_type_info_1(Node1)
    end.

unwrap_node_context({uniplate_node_context, Node, _Withs, _Reduces, _Skip,
                     _UpAttrs, _Entries, _Exits}) ->
    Node;
unwrap_node_context(Node) ->
    Node.

node_type_info_1(Node) ->
    case is_tuple(Node) orelse erl_syntax:is_tree(Node) of
        true ->
            try
                {ok, type(Node), get_pos(Node)}
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end;
        false ->
            {error, {bad_syntax_tree, Node}}
    end.

add_parent_error(ParentType, ParentPos, Error) ->
    case maps:is_key(parent_type, Error) of
        true -> Error;
        false -> Error#{parent_type => ParentType, parent_pos => ParentPos}
    end.

invalid_role_error(Validator, ExpectedRole, Slot, Type, Pos, Node, Path) ->
    #{reason => invalid_role,
      validator => Validator,
      expected_role => ExpectedRole,
      slot => Slot,
      actual_type => Type,
      pos => Pos,
      node => Node,
      path => Path}.

invalid_node_error(Validator, ExpectedRole, Slot, Node, Exception, Path) ->
    #{reason => invalid_node,
      validator => Validator,
      expected_role => ExpectedRole,
      slot => Slot,
      node => Node,
      exception => Exception,
      path => Path}.

path_item(Slot, Index, Validator, Node) ->
    Item0 = #{slot => Slot, index => Index, validator => Validator,
              expected_role => validator_role(Validator)},
    case node_type_info(Node) of
        {ok, Type, Pos} ->
            Item0#{type => Type, pos => Pos};
        {error, _Exception} ->
            Item0#{type => invalid}
    end.

role_allowed(Validator, Type, Node, Env) ->
    ExpectedRole = validator_role(Validator),
    syntax_allowed(Validator, Type, Node, Env) andalso
        role_allowed_1(Type, ExpectedRole, Node, Env) andalso
        slot_type_allowed(Validator, Type).

syntax_allowed(Validator, Type, Node, Env) ->
    OtpVsn = maps:get(otp_vsn, Env, otp_vsn()),
    syntax_node_allowed(Type, Node, OtpVsn) andalso
        syntax_slot_allowed(Validator, Type, Node, OtpVsn).

syntax_node_allowed(Type, Node, OtpVsn) ->
    syntax_node_type_allowed(Type, OtpVsn) andalso
        (not otp29_node_shape(Type, Node) orelse otp_at_least(OtpVsn, 29)).

otp29_node_shape(Type, Node) ->
    otp29_native_record_syntax(Type, Node) orelse
        otp29_multiple_comprehension_templates(Type, Node).

syntax_node_type_allowed(maybe_expr, OtpVsn) ->
    otp_at_least(OtpVsn, 25);
syntax_node_type_allowed(maybe_match_expr, OtpVsn) ->
    otp_at_least(OtpVsn, 25);
syntax_node_type_allowed(map_comp, OtpVsn) ->
    otp_at_least(OtpVsn, 26);
syntax_node_type_allowed(map_generator, OtpVsn) ->
    otp_at_least(OtpVsn, 26);
syntax_node_type_allowed(strict_generator, OtpVsn) ->
    otp_at_least(OtpVsn, 28);
syntax_node_type_allowed(strict_binary_generator, OtpVsn) ->
    otp_at_least(OtpVsn, 28);
syntax_node_type_allowed(strict_map_generator, OtpVsn) ->
    otp_at_least(OtpVsn, 28);
syntax_node_type_allowed(zip_generator, OtpVsn) ->
    otp_at_least(OtpVsn, 28);
syntax_node_type_allowed(_Type, _OtpVsn) ->
    true.

otp29_native_record_syntax(record_expr, Node) ->
    case revert(Node) of
        {record, _Pos, Type, _Fields} -> native_record_type(Type);
        {record, _Pos, _Argument, Type, _Fields} -> native_record_type(Type);
        _ -> false
    end;
otp29_native_record_syntax(record_access, Node) ->
    case revert(Node) of
        {record_field, _Pos, _Argument, Type, _Field} -> native_record_type(Type);
        _ -> false
    end;
otp29_native_record_syntax(_Type, _Node) ->
    false.

otp29_multiple_comprehension_templates(list_comp, Node) ->
    case revert(Node) of
        {lc, _Pos, Templates, _Body} -> is_list(Templates);
        _ -> false
    end;
otp29_multiple_comprehension_templates(map_comp, Node) ->
    case revert(Node) of
        {mc, _Pos, Templates, _Body} -> is_list(Templates);
        _ -> false
    end;
otp29_multiple_comprehension_templates(_Type, _Node) ->
    false.

native_record_type([]) ->
    true;
native_record_type({Module, Name}) when is_atom(Module), is_atom(Name) ->
    true;
native_record_type(_Type) ->
    false.

syntax_slot_allowed({slot, map_field_exact, map_field_exact_key, _Role}, Type, Node, OtpVsn) ->
    otp_at_least(OtpVsn, 23) orelse legacy_map_pattern_key_allowed(Type, Node);
syntax_slot_allowed({slot, try_expr, handlers, clause}, clause, Node, OtpVsn) ->
    otp_at_least(OtpVsn, 21) orelse legacy_try_handler_allowed(Node);
syntax_slot_allowed({slot, size_qualifier, Slot, binary_size}, _Type, Node, OtpVsn)
  when Slot =:= elements; Slot =:= size ->
    otp_at_least(OtpVsn, 23) orelse legacy_binary_size_allowed(Node);
syntax_slot_allowed(_Validator, _Type, _Node, _OtpVsn) ->
    true.

otp_at_least(OtpVsn, Min) when is_integer(OtpVsn) ->
    OtpVsn >= Min;
otp_at_least(_OtpVsn, _Min) ->
    false.

legacy_map_pattern_key_allowed(Type, Node) ->
    (Type =:= variable) orelse literal_node(Node).

legacy_try_handler_allowed(Node) ->
    try legacy_try_handler_patterns_allowed(erl_syntax:clause_patterns(Node))
    catch
        error:_Reason ->
            false
    end.

legacy_try_handler_patterns_allowed([Pattern]) ->
    legacy_try_handler_pattern_allowed(erl_syntax:revert(Pattern));
legacy_try_handler_patterns_allowed(Patterns) ->
    length(Patterns) =< 2.

legacy_try_handler_pattern_allowed({tuple, _Pos, [_Class, _Reason]}) ->
    true;
legacy_try_handler_pattern_allowed({tuple, _Pos, [_Class, _Reason, {var, _VarPos, '_'}]}) ->
    true;
legacy_try_handler_pattern_allowed({tree, class_qualifier, _Attr,
                                    {class_qualifier, _Class, _Reason, {var, _VarPos, '_'}}}) ->
    true;
legacy_try_handler_pattern_allowed(_Pattern) ->
    false.

legacy_binary_size_allowed({integer, _Pos, _Value}) ->
    true;
legacy_binary_size_allowed({var, _Pos, _Name}) ->
    true;
legacy_binary_size_allowed(default) ->
    true;
legacy_binary_size_allowed(_Node) ->
    false.

literal_node({atom, _Pos, _Value}) -> true;
literal_node({char, _Pos, _Value}) -> true;
literal_node({float, _Pos, _Value}) -> true;
literal_node({integer, _Pos, _Value}) -> true;
literal_node({nil, _Pos}) -> true;
literal_node({string, _Pos, _Value}) -> true;
literal_node(_Node) -> false.

role_allowed_1(Type, guard, _Node, _Env)
  when Type =:= conjunction; Type =:= disjunction; Type =:= operator ->
    true;
role_allowed_1(_Type, guard, Node, Env) ->
    try erl_lint:is_guard_test(revert(Node), maps:get(forms, Env, [])) of
        Result -> Result
    catch
        error:badarg -> false
    end;
role_allowed_1(Type, map_field, _Node, _Env) ->
    lists:member(Type, [map_field_assoc, map_field_exact]);
role_allowed_1(Type, binary_field, _Node, _Env) ->
    Type =:= binary_field;
role_allowed_1(_Type, binary_size, default, _Env) ->
    true;
role_allowed_1(Type, binary_size, _Node, _Env)
  when Type =:= conjunction; Type =:= disjunction; Type =:= infix_expr;
       Type =:= operator; Type =:= prefix_expr ->
    true;
role_allowed_1(Type, binary_size, Node, Env) ->
    role_allowed_1(Type, guard, Node, Env);
role_allowed_1(Type, type_param, _Node, _Env) ->
    Type =:= variable;
role_allowed_1(Type, ExpectedRole, _Node, _Env) ->
    (ExpectedRole =:= attribute_body) orelse lists:member(ExpectedRole, node_roles(Type)).

slot_type_allowed({slot, map_expr, fields, map_field}, Type) ->
    lists:member(Type, [map_field_assoc, map_field_exact]);
slot_type_allowed({slot, ParentType, pattern, pattern}, _Type)
  when ParentType =:= map_generator; ParentType =:= strict_map_generator ->
    true;
slot_type_allowed({slot, binary, elements, binary_field}, Type) ->
    Type =:= binary_field;
slot_type_allowed({slot, _ParentType, _Slot, Role}, Type)
  when Role =/= map_field, Role =/= binary_field ->
    not lists:member(Type, [map_field_assoc, map_field_exact, binary_field]);
slot_type_allowed(_Validator, _Type) ->
    true.

validator_role({role, Role}) ->
    Role;
validator_role({slot, _ParentType, _Slot, Role}) ->
    Role.

-define(EXPR_PAT_GUARD, [atom, char, float, integer, nil, string, variable,
                          tuple, list, cons, binary, binary_field, size_qualifier,
                          map_expr, map_field_exact, record_expr, record_field,
                          parentheses]).

-define(EXPR_GUARD, [application, module_qualifier, infix_expr, prefix_expr,
                     operator, record_index_expr, map_field_assoc,
                     record_access, conjunction, disjunction]).

-define(EXPR_PAT, [match_expr]).
-define(EXPR_ONLY, [maybe_match_expr, case_expr, if_expr,
                    receive_expr, fun_expr, named_fun_expr, try_expr,
                    catch_expr, block_expr, generator, strict_generator,
                    binary_generator, strict_binary_generator,
                    map_generator, strict_map_generator,
                    implicit_fun, list_comp, binary_comp, map_comp,
                    zip_generator, maybe_expr,
                    arity_qualifier]).

-define(PAT_ONLY, [underscore, class_qualifier]).
-define(CLAUSE_ONLY, [clause]).
-define(NAME_ALSO, [atom]).
-define(TYPE_ALSO, [atom, char, integer, string, variable,
                    nil, parentheses, tuple, list, cons, binary]).
-define(TYPE_ONLY, [fun_type, type_application, type_union, type_fun, type_tuple,
                    type_record, typed_record_field, type_binary,
                    type_integer_range, type_map, map_type, type_map_field,
                    user_type_application, remote_type,
                    annotated_type, bitstring_type, constrained_function_type,
                    function_type, constraint, map_type_assoc, map_type_exact,
                    record_type, record_type_field]).
-define(FORM_ONLY, [function, attribute, eof_marker, error_marker,
                    warning_marker, comment, text, form_list]).

-define(ROLE_ORDER, [{?FORM_ONLY, [form]},
                     {?TYPE_ONLY, [type]},
                     {?CLAUSE_ONLY, [clause]},
                     {?PAT_ONLY, [pattern]},
                     {?EXPR_PAT, [expression, pattern]},
                     {?EXPR_ONLY, [expression]},
                     {?EXPR_GUARD, [expression, guard]},
                     {?EXPR_PAT_GUARD, [expression, pattern, guard]}]).

-spec node_roles(atom()) -> [node_role()].
node_roles(Type) ->
    Roles = find_roles(Type, ?ROLE_ORDER, [expression, pattern, guard]),
    Extra = lists:append([add_if(Type, ?TYPE_ALSO, [type]),
                          add_if(Type, ?NAME_ALSO, [name])]),
    lists:usort(Roles ++ Extra).

find_roles(_Type, [], Default) -> Default;
find_roles(Type, [{Set, Roles}|T], Default) ->
    case lists:member(Type, Set) of
        true -> Roles;
        false -> find_roles(Type, T, Default)
    end.

add_if(Type, Set, Roles) ->
    case lists:member(Type, Set) of true -> Roles; false -> [] end.

-spec child_specs(atom(), [[erl_syntax:syntaxTree()]], map()) -> [child_spec()].
child_specs(Type, Subtrees, Attr) ->
    add_child_validators(Type, child_specs_1(Type, Subtrees, Attr)).

child_specs_1(map_expr, [Fields], Attr) ->
    [map_field_child_spec(fields, Fields, Attr)];
child_specs_1(map_expr, [Argument, Fields], Attr) ->
    [child_spec(argument, expression, Argument, Attr),
     map_field_child_spec(fields, Fields, Attr)];
child_specs_1(binary, [Elements], Attr) ->
    [binary_field_child_spec(elements, [Elements], Attr)];
child_specs_1(application, [Operator, Arguments], Attr) ->
    [child_spec(operator, expression, Operator, Attr),
     child_spec(arguments, expression, Arguments, Attr)];
child_specs_1(binary_field, [Values], Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(value, Role, Values, Attr)];
child_specs_1(binary_field, [Values, Types], Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(value, Role, Values, Attr),
     child_spec(types, attribute_body, Types, Attr)];
child_specs_1(binary_field, [Values, Sizes, Types], Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(value, Role, Values, Attr),
     child_spec(size, binary_size, Sizes, Attr),
     child_spec(types, attribute_body, Types, Attr)];
child_specs_1(size_qualifier, [[Value], [Size]], Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(value, Role, [Value], Attr),
     child_spec(size, binary_size, [Size], Attr)];
child_specs_1(size_qualifier, [[Value, Size]], Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(value, Role, [Value], Attr),
     child_spec(size, binary_size, [Size], Attr)];
child_specs_1(size_qualifier, Subtrees, Attr) ->
    [child_spec(elements, binary_size, Subtrees, Attr)];
child_specs_1(binary_field, Subtrees, Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(elements, Role, Subtrees, Attr)];
child_specs_1(Type, [Keys, Values], #{node := pattern} = Attr)
  when Type =:= map_field_assoc; Type =:= map_field_exact ->
    {KeySlot, ValueSlot} = map_field_slots(Type),
    [child_spec(KeySlot, expression, Keys, Attr),
     child_spec(ValueSlot, pattern, Values, Attr)];
child_specs_1(Type, [Keys, Values], #{node := Role} = Attr)
  when Type =:= map_field_assoc; Type =:= map_field_exact ->
    {KeySlot, ValueSlot} = map_field_slots(Type),
    [child_spec(KeySlot, map_field_child_role(Role), Keys, Attr),
     child_spec(ValueSlot, Role, Values, Attr)];
child_specs_1(_Type, Subtrees, #{node := pattern} = Attr) ->
    [child_spec(elements, pattern, Subtrees, Attr)];
child_specs_1(named_fun_expr, [Names, Clauses], Attr) ->
    [child_spec(name, pattern, Names, Attr),
     child_spec(clauses, clause, Clauses, Attr)];
child_specs_1(Type, [Patterns, Expressions], Attr) when Type =:= match_expr; Type =:= maybe_match_expr ->
    [child_spec(left, pattern, Patterns, Attr),
     child_spec(right, expression, Expressions, Attr)];
child_specs_1(clause, [Patterns, Expressions], Attr) ->
    [child_spec(patterns, pattern, Patterns, Attr),
     child_spec(body, expression, Expressions, Attr)];
child_specs_1(clause, [Patterns, Guards, Expressions], Attr) ->
    [child_spec(patterns, pattern, Patterns, Attr),
     child_spec(guards, guard, Guards, Attr),
     child_spec(body, expression, Expressions, Attr)];
child_specs_1(Type, [Patterns, Expressions], Attr) when Type =:= generator; Type =:= strict_generator;
                                                       Type =:= binary_generator; Type =:= strict_binary_generator;
                                                       Type =:= map_generator; Type =:= strict_map_generator ->
    [child_spec(pattern, pattern, Patterns, Attr),
     child_spec(body, expression, Expressions, Attr)];
child_specs_1(fun_expr, [Clauses], Attr) ->
    [child_spec(clauses, clause, Clauses, Attr)];
child_specs_1(case_expr, [Argument, Clauses], Attr) ->
    [child_spec(argument, expression, Argument, Attr),
     child_spec(clauses, clause, Clauses, Attr)];
child_specs_1(if_expr, [Clauses], Attr) ->
    [child_spec(clauses, clause, Clauses, Attr)];
child_specs_1(receive_expr, [Clauses], Attr) ->
    [child_spec(clauses, clause, Clauses, Attr)];
child_specs_1(receive_expr, [Clauses, Timeout, Action], Attr) ->
    [child_spec(clauses, clause, Clauses, Attr),
     child_spec(timeout, expression, Timeout, Attr),
     child_spec(action, expression, Action, Attr)];
child_specs_1(try_expr, [Body, Clauses, Handlers, After], Attr) ->
    [child_spec(body, expression, Body, Attr),
     child_spec(clauses, clause, Clauses, Attr),
     child_spec(handlers, clause, Handlers, Attr),
     child_spec('after', expression, After, Attr)];
child_specs_1(try_expr, [Body, Clauses, Handlers], Attr) ->
    [child_spec(body, expression, Body, Attr),
     child_spec(clauses, clause, Clauses, Attr),
     child_spec(handlers, clause, Handlers, Attr)];
child_specs_1(function, [Name, Clauses], Attr) ->
    [child_spec(name, name, Name, Attr),
     child_spec(clauses, clause, Clauses, Attr)];
child_specs_1(function, [Clauses], Attr) ->
    [child_spec(clauses, clause, Clauses, Attr)];
child_specs_1(form_list, Subtrees, Attr) ->
    [child_spec(forms, form, Subtrees, Attr)];
child_specs_1(attribute, [[NameTree], BodyTrees], Attr) ->
    Attribute = erl_syntax:atom_value(NameTree),
    [child_spec(name, name, [NameTree], Attr)|
     attribute_body_specs(Attribute, BodyTrees, Attr#{attribute => Attribute})];
child_specs_1(list_comp, [Template, Body], Attr) ->
    [child_spec(template, expression, Template, Attr),
     child_spec(body, expression, Body, Attr)];
child_specs_1(map_comp, [Template, Body], Attr) ->
    [map_field_child_spec(template, Template, Attr),
     child_spec(body, expression, Body, Attr)];
child_specs_1(binary_comp, [Template, Body], Attr) ->
    [child_spec(template, expression, Template, Attr),
     child_spec(body, expression, Body, Attr)];
child_specs_1(maybe_expr, [Body], Attr) ->
    [child_spec(body, expression, Body, Attr)];
child_specs_1(maybe_expr, [Body, Else], Attr) ->
    [child_spec(body, expression, Body, Attr),
     child_spec(else_clause, clause, Else, Attr)];
child_specs_1(implicit_fun, [Name], Attr) ->
    [child_spec(name, expression, Name, Attr)];
child_specs_1(record_expr, [Type, Fields], Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(type, expression, Type, Attr),
     child_spec(fields, Role, Fields, Attr)];
child_specs_1(record_expr, [Argument, Type, Fields], Attr) ->
    Role = maps:get(node, Attr, expression),
    [child_spec(argument, expression, Argument, Attr),
     child_spec(type, expression, Type, Attr),
     child_spec(fields, Role, Fields, Attr)];
child_specs_1(record_access, [Argument, Type, Field], Attr) ->
    [child_spec(argument, expression, Argument, Attr),
     child_spec(type, expression, Type, Attr),
     child_spec(field, expression, Field, Attr)];
child_specs_1(zip_generator, [Body], Attr) ->
    [child_spec(body, expression, Body, Attr)];
child_specs_1(_Type, Subtrees, #{node := Role} = Attr) ->
    [child_spec(elements, Role, Subtrees, Attr)];
child_specs_1(_Type, Subtrees, Attr) ->
    [child_spec(elements, expression, Subtrees, Attr)].

add_child_validators(ParentType, Specs) ->
    lists:map(fun(Spec) -> add_child_validator(ParentType, Spec) end, Specs).

add_child_validator(ParentType, #{slot := Slot, role := Role, attr := Attr} = Spec) ->
    Validator = {slot, ParentType, Slot, Role},
    ChildAttr = Attr#{validator => Validator},
    Spec#{validator => Validator, attr => ChildAttr}.

child_spec(Slot, Role, Subtrees, Attr) ->
    #{slot => Slot,
      role => Role,
      subtrees => Subtrees,
      attr => child_attr(Role, Attr)}.

child_attr(Role, Attr) ->
    case node_role(Role) of
        true ->
            Attr#{node => Role};
        false ->
            case Role of
                map_field ->
                    Attr;
                binary_field ->
                    Attr;
                _ ->
                    case slot_role(Role) of
                        true -> maps:remove(node, Attr);
                        false -> Attr#{node => Role}
                    end
            end
    end.

attribute_body_specs(Attribute, BodyTrees, Attr) when Attribute =:= type; Attribute =:= opaque ->
    ok = validate_attribute_body_shape(Attribute, BodyTrees),
    [TypeNameTree, TypeTree|TypeParamTrees] = BodyTrees,
    [child_spec(type_name, name, [TypeNameTree], Attr),
     child_spec(type_body, type, [TypeTree], Attr),
     child_spec(type_params, type_param, TypeParamTrees, Attr)];
attribute_body_specs(Attribute, BodyTrees, Attr) when Attribute =:= spec; Attribute =:= callback ->
    ok = validate_attribute_body_shape(Attribute, BodyTrees),
    [MFATree|SpecTrees] = BodyTrees,
    [child_spec(spec_mfa, attribute_body, [MFATree], Attr),
     child_spec(specs, type, SpecTrees, Attr)];
attribute_body_specs(Attribute, BodyTrees, Attr) ->
    [child_spec(body, attribute_body_role(Attribute), BodyTrees, Attr)].

validate_attribute_body_shape(type, [_, _|_] = BodyTrees) when is_list(BodyTrees) ->
    ok;
validate_attribute_body_shape(opaque, [_, _|_] = BodyTrees) when is_list(BodyTrees) ->
    ok;
validate_attribute_body_shape(spec, [_|_] = BodyTrees) when is_list(BodyTrees) ->
    ok;
validate_attribute_body_shape(callback, [_|_] = BodyTrees) when is_list(BodyTrees) ->
    ok;
validate_attribute_body_shape(Attribute, BodyTrees)
  when Attribute =:= type; Attribute =:= opaque;
       Attribute =:= spec; Attribute =:= callback ->
    erlang:error({invalid_attribute_body, Attribute, BodyTrees});
validate_attribute_body_shape(_Attribute, _BodyTrees) ->
    ok.

map_field_child_spec(Slot, Nodes, Attr) ->
    child_spec(Slot, map_field, Nodes, Attr).

binary_field_child_spec(Slot, Nodes, Attr) ->
    child_spec(Slot, binary_field, Nodes, Attr).

map_field_child_role(pattern) -> expression;
map_field_child_role(Role) -> Role.

map_field_slots(map_field_assoc) -> {map_field_assoc_key, map_field_assoc_value};
map_field_slots(map_field_exact) -> {map_field_exact_key, map_field_exact_value}.

attribute_body_role(record) -> attribute_body;
attribute_body_role(type) -> type;
attribute_body_role(opaque) -> type;
attribute_body_role(spec) -> type;
attribute_body_role(callback) -> type;
attribute_body_role(_) -> attribute_body.

child_specs_annotated_subtrees(Specs) ->
    build_child_specs_subtrees(Specs, fun validator_node/2).

child_specs_plain_subtrees(Specs) ->
    build_child_specs_subtrees(Specs, fun(_Spec, Nodes) -> Nodes end).

build_child_specs_subtrees([#{validator := {slot, attribute, name, name}} = NameSpec|BodySpecs], Wrap) ->
    [NameSubtrees] = build_child_spec_subtreess(NameSpec, Wrap),
    BodySubtrees = lists:append(lists:map(fun(Spec) -> attribute_child_spec_subtrees(Spec, Wrap) end,
                                          BodySpecs)),
    [NameSubtrees, BodySubtrees];
build_child_specs_subtrees(Specs, Wrap) ->
    lists:append(lists:map(fun(Spec) -> build_child_spec_subtreess(Spec, Wrap) end, Specs)).

attribute_child_spec_subtrees(Spec, Wrap) ->
    lists:append(build_child_spec_subtreess(Spec, Wrap)).

build_child_spec_subtreess(#{slot := Slot, subtrees := Subtrees} = Spec, Wrap)
  when Slot =:= elements; Slot =:= forms ->
    Wrap(Spec, Subtrees);
build_child_spec_subtreess(#{subtrees := Subtrees} = Spec, Wrap) ->
    [Wrap(Spec, Subtrees)].

validator_node(#{attr := Attr}, Nodes) ->
    ChildAttr = maps:with([node, validator], Attr),
    astranaut_uniplate:up_attr(
      fun(ParentAttr) ->
              maps:merge(maps:without([node, validator], ParentAttr), ChildAttr)
      end, Nodes).

%%===================================================================
%% update forms related functions
%%===================================================================
-spec reorder_updated_forms([form() | {updated, form(), [form()]}]) -> [form()].
reorder_updated_forms(Forms) ->
    astranaut_forms:reorder_updated_forms(Forms).

-spec sort_forms([erl_parse:abstract_form()]) -> [erl_parse:abstract_form()].
sort_forms(Forms) ->
    astranaut_forms:sort_forms(Forms).

-spec insert_forms([erl_parse:abstract_form()], [erl_parse:abstract_form()]) -> [erl_parse:abstract_form()].
insert_forms(NewForms, Forms) ->
    astranaut_forms:insert_forms(NewForms, Forms).
