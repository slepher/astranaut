%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc OTP-compatible syntax-tree helpers, validation, and normalization.
%%%
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
          group_mode := nodes | groups,
          attr := map()}.

-export_type([node_role/0, validator/0, validation_opts/0,
              validation_error/0, child_spec/0]).

%% API
-export([type/1, otp_vsn/0, get_pos/1, set_pos/2, is_pos/1, is_leaf/1]).
-export([subtrees/1, update_tree/2, revert/1]).
-export([subtrees_pge/3]).
-export([validate_node/2, validate_node/3, normalize/2, normalize/3,
         child_specs/3, node_roles/1]).

-spec type(term()) -> atom().
%% ERLANG AST SEMANTICS
%% Record fields and typed record fields are real abstract-format nodes, but
%% erl_syntax only accepts them standalone through syntax-tree wrappers. Keep
%% callbacks on ordinary tuples while delegating projection and reconstruction
%% to those OTP wrappers.
%% ASTRANAUT OTP ADAPTER
type({record_field, _Pos, _Name}) ->
    record_field;
type({record_field, _Pos, _Name, _Value}) ->
    record_field;
type({typed_record_field, _Field, _FieldType}) ->
    typed_record_field;
type(Node) ->
    erl_syntax:type(Node).

-spec otp_vsn() -> integer() | 'pre-21'.
otp_vsn() ->
    ?ASTRANAUT_OTP_VSN.

-spec get_pos(term()) -> term().
%% ASTRANAUT OTP ADAPTER
%% typed_record_field has no annotation field of its own in abstract format;
%% its source position is the position of the contained record_field.
get_pos({typed_record_field, Field, _FieldType}) ->
    get_pos(Field);
get_pos(Node) ->
    erl_syntax:get_pos(Node).

-spec set_pos(term(), term()) -> term().
%% ASTRANAUT OTP ADAPTER: raw record-field boundary; see type/1.
set_pos({record_field, _OldPos, _Name} = Node, Pos) ->
    setelement(2, Node, Pos);
set_pos({record_field, _OldPos, _Name, _Value} = Node, Pos) ->
    setelement(2, Node, Pos);
set_pos({typed_record_field, Field, FieldType}, Pos) ->
    {typed_record_field, set_pos(Field, Pos), FieldType};
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
%% ASTRANAUT OTP ADAPTER: raw record-field boundary; see type/1.
is_leaf({record_field, _Pos, _Name}) ->
    false;
is_leaf({record_field, _Pos, _Name, _Value}) ->
    false;
is_leaf({typed_record_field, _Field, _FieldType}) ->
    false;
is_leaf(Node) ->
    erl_syntax:is_leaf(Node).

%% as this issue mentioned, it's a bug, but will cause compatibility issue
%% https://github.com/erlang/otp/issues/4529
%% the goal of revert/1 is to fix this without cause compatibility issues.
%% just use astranaut_syntax:subtrees/1 replace of erl_syntax:subtress/1,
%% astranaut_syntax:revert/1 replace of erl_syntax:revert/1.
-spec subtrees(erl_syntax:syntaxTree()) -> [[erl_syntax:syntaxTree()]].
%% ASTRANAUT OTP SPECIAL CASE
%% OTP erl_syntax does not project type/opaque/spec/callback attributes
%% symmetrically with the abstract format. Keep this compatibility projection
%% local and let every ordinary node fall through to erl_syntax:subtrees/1.
%% Scope: supported OTP releases 19-29. Regression coverage lives in
%% astranaut_syntax_SUITE's type/spec attribute tests.
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
%% ASTRANAUT OTP ADAPTER: delegate raw record-field nodes through their
%% OTP syntax-tree wrappers; child grouping remains owned by erl_syntax.
subtrees({record_field, _Pos, _Name} = Node) ->
    erl_syntax:subtrees(record_field_tree(Node));
subtrees({record_field, _Pos, _Name, _Value} = Node) ->
    erl_syntax:subtrees(record_field_tree(Node));
subtrees({typed_record_field, _Field, _FieldType} = Node) ->
    erl_syntax:subtrees(typed_record_field_tree(Node));
subtrees(Node) ->
    erl_syntax:subtrees(Node).

record_field_tree({record_field, Pos, Name}) ->
    erl_syntax:set_pos(erl_syntax:record_field(Name), Pos);
record_field_tree({record_field, Pos, Name, Value}) ->
    erl_syntax:set_pos(erl_syntax:record_field(Name, Value), Pos).

typed_record_field_tree({typed_record_field, Field, FieldType}) ->
    FieldTree = otp_tree(Field),
    Tree = erl_syntax:typed_record_field(FieldTree, otp_tree(FieldType)),
    erl_syntax:set_pos(Tree, erl_syntax:get_pos(FieldTree)).

mfa_tree(MFA, Pos) ->
    erl_syntax:set_pos(erl_syntax:tuple(lists:map(fun(Element) -> name_arity_tree(Element, Pos) end, tuple_to_list(MFA))), Pos).

name_arity_tree(Name, Pos) when is_atom(Name) ->
    erl_syntax:set_pos(erl_syntax:atom(Name), Pos);
name_arity_tree(Arity, Pos) when is_integer(Arity) ->
    erl_syntax:set_pos(erl_syntax:integer(Arity), Pos).

-spec update_tree(erl_syntax:syntaxTree(), [[erl_syntax:syntaxTree()]]) -> erl_syntax:syntaxTree().
%% ASTRANAUT OTP ADAPTER: reconstruct through the same OTP wrapper and
%% return the standard abstract-format tuple expected by callbacks.
update_tree({record_field, _Pos, _Name} = Node, Subtrees) ->
    revert_record_field(
      erl_syntax:update_tree(record_field_tree(Node), otp_subtrees(Subtrees)));
update_tree({record_field, _Pos, _Name, _Value} = Node, Subtrees) ->
    revert_record_field(
      erl_syntax:update_tree(record_field_tree(Node), otp_subtrees(Subtrees)));
update_tree({typed_record_field, _Field, _FieldType} = Node, Subtrees) ->
    revert_typed_record_field(
      erl_syntax:update_tree(typed_record_field_tree(Node),
                             otp_subtrees(Subtrees)));
update_tree(Node, Subtrees) ->
    %% OTP AUTHORITY: reconstruction is deliberately not schema-generated.
    erl_syntax:update_tree(Node, otp_subtrees(Subtrees)).

otp_subtrees(Subtrees) ->
    [[otp_tree(Node) || Node <- Group] || Group <- Subtrees].

otp_tree({record_field, _Pos, _Name} = Node) ->
    record_field_tree(Node);
otp_tree({record_field, _Pos, _Name, _Value} = Node) ->
    record_field_tree(Node);
otp_tree({typed_record_field, _Field, _FieldType} = Node) ->
    typed_record_field_tree(Node);
otp_tree(Node) ->
    Node.

-spec revert(term()) -> term().
revert(Node) ->
    case erl_syntax:is_tree(Node) of
        false ->
            erl_syntax:revert(Node);
        true ->
            case erl_syntax:type(Node) of
                attribute ->
                    %% ASTRANAUT OTP ADAPTER
                    %% Revert the attribute projection paired with the marked
                    %% subtrees/1 compatibility clauses above.
                    Name = erl_syntax:concrete(erl_syntax:attribute_name(Node)),
                    Args = erl_syntax:attribute_arguments(Node),
                    Pos = erl_syntax:get_pos(Node),
                    revert_attribute(Name, Args, Pos, Node);
                record_field ->
                    %% ASTRANAUT OTP ADAPTER
                    %% erl_syntax:subtrees/1 projects record fields as #tree{},
                    %% but erl_syntax:revert/1 leaves that standalone projection
                    %% wrapped. Record fields are real abstract-format nodes and
                    %% must therefore reach traversal callbacks as such.
                    revert_record_field(Node);
                typed_record_field ->
                    %% ASTRANAUT OTP SPECIAL CASE
                    %% Like record_field, this is a real abstract-format node
                    %% whose standalone OTP projection does not revert to raw.
                    revert_typed_record_field(Node);
                _ ->
                    erl_syntax:revert(Node)
            end
    end.

revert_record_field(Node) ->
    Pos = erl_syntax:get_pos(Node),
    Name = revert(erl_syntax:record_field_name(Node)),
    case erl_syntax:record_field_value(Node) of
        none -> {record_field, Pos, Name};
        Value -> {record_field, Pos, Name, revert(Value)}
    end.

revert_typed_record_field(Node) ->
    {typed_record_field,
     revert(erl_syntax:typed_record_field_body(Node)),
     revert(erl_syntax:typed_record_field_type(Node))}.

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
        {ok, Type, Pos, Subtrees} = NodeInfo ->
            ExpectedRole = validator_role(Validator),
            OtpVsn = maps:get(otp_vsn, Env, otp_vsn()),
            case role_allowed(Validator, Type, Node, Env) of
                false ->
                    {error, invalid_role_error(
                              Validator, ExpectedRole, Slot,
                              Type, Pos, Node, Path)};
                true ->
                    case syntax_child_layout(
                           Type, Subtrees, ExpectedRole, OtpVsn) of
                        {ok, _Layout} -> {ok, NodeInfo};
                        {error, Reason} ->
                            {error, invalid_node_error(
                                      Validator, ExpectedRole, Slot, Node,
                                      {error, Reason}, Path)}
                    end
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
                _Reverted = revert(Node),
                {ok, Type, Pos, Subtrees}
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end;
        false ->
            {error, {bad_syntax_tree, Node}}
    end.

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
        role_allowed_1(Type, ExpectedRole, Node, Env).

syntax_allowed(Validator, Type, Node, Env) ->
    OtpVsn = maps:get(otp_vsn, Env, otp_vsn()),
    astranaut_syntax_schema:format_available(
      Type, unwrap_node_context(Node), OtpVsn) andalso
        case Validator of
            {slot, ParentType, Slot, _Role} ->
                ChildNode = case unwrap_node_context(Node) of
                                default -> default;
                                Node1 -> revert(Node1)
                            end,
                astranaut_syntax_schema:slot_available(
                  ParentType, Slot, Type, ChildNode, OtpVsn);
            {role, _Role} ->
                true
        end.

role_allowed_1(Type, guard, Node, Env) ->
    case astranaut_syntax_schema:traverse_transparent(Type) of
        true ->
            %% ASTRANAUT TRAVERSAL SPECIAL CASE
            %% syntax-tools projection nodes are traversed only as transparent
            %% OTP reconstruction containers and never reach a callback.
            astranaut_syntax_schema:role_available(Type, guard);
        false ->
            %% ERLANG SEMANTICS
            %% Guard legality depends on the concrete expression and record
            %% environment, so it cannot be reduced to a static schema role.
            try erl_lint:is_guard_test(revert(Node), maps:get(forms, Env, [])) of
                Result -> Result
            catch
                error:badarg -> false
            end
    end;
role_allowed_1(_Type, binary_size, default, _Env) ->
    %% ERLANG SEMANTICS: `default` is the non-node size marker.
    true;
role_allowed_1(Type, binary_size, Node, Env) ->
    %% ERLANG SEMANTICS: an explicit binary size follows guard-expression
    %% legality; slot-level OTP version restrictions are generated separately.
    role_allowed_1(Type, guard, Node, Env);
role_allowed_1(_Type, attribute_body, _Node, _Env) ->
    %% ASTRANAUT SEMANTICS: unknown attribute bodies are deliberately opaque.
    true;
role_allowed_1(Type, ExpectedRole, _Node, _Env) ->
    astranaut_syntax_schema:role_available(Type, ExpectedRole).

validator_role({role, Role}) ->
    Role;
validator_role({slot, _ParentType, _Slot, Role}) ->
    Role.

-spec node_roles(atom()) -> [node_role()].
node_roles(Type) ->
    astranaut_syntax_schema:node_roles(Type).

-spec child_specs(atom(), [[erl_syntax:syntaxTree()]], map()) -> [child_spec()].
child_specs(Type, Subtrees, Attr) ->
    ParentRole = maps:get(node, Attr, expression),
    case syntax_child_layout(Type, Subtrees, ParentRole, otp_vsn()) of
        {ok, Children} ->
            Attr1 = child_specs_attr(Type, Subtrees, Attr),
            [child_spec(Type, Slot, Role, ChildSubtrees, GroupMode, Attr1)
             || {Slot, Role, ChildSubtrees, GroupMode} <- Children];
        {error, Reason} ->
            erlang:error(Reason)
    end.

%% ASTRANAUT TRAVERSAL SPECIAL CASE
%% Some Astranaut form traversal paths expose only a function's clauses after
%% handling its name separately. This is not an erl_syntax:subtrees/1 layout.
syntax_child_layout(function, [Clauses], _ParentRole, _OtpVsn)
  when is_list(Clauses) ->
    {ok, [{clauses, clause, Clauses, nodes}]};
%% ASTRANAUT OTP SPECIAL CASE
%% erl_syntax:subtrees/1 returns [] for an empty structured map_expr.
syntax_child_layout(map_expr, [], _ParentRole, _OtpVsn) ->
    {ok, []};
syntax_child_layout(Type, Subtrees, ParentRole, OtpVsn) ->
    astranaut_syntax_schema:child_layout(Type, Subtrees, ParentRole, OtpVsn).

%% ASTRANAUT OTP SPECIAL CASE
%% Attribute traversal carries its name as context because OTP subtrees do not.
child_specs_attr(attribute, [[NameTree], _BodyTrees], Attr) ->
    Attr#{attribute => erl_syntax:atom_value(NameTree)};
child_specs_attr(_Type, _Subtrees, Attr) ->
    Attr.

child_spec(ParentType, Slot, Role, Subtrees, GroupMode, Attr) ->
    Validator = {slot, ParentType, Slot, Role},
    #{slot => Slot,
      role => Role,
      validator => Validator,
      subtrees => Subtrees,
      group_mode => GroupMode,
      attr => (child_attr(Role, Attr))#{validator => Validator}}.

child_attr(Role, Attr) ->
    case node_role(Role) of
        true ->
            Attr#{node => Role};
        false ->
            case Role of
                StructuralRole when StructuralRole =:= map_field;
                                         StructuralRole =:= binary_field ->
                    Attr;
                _ ->
                    case slot_role(Role) of
                        true -> maps:remove(node, Attr);
                        false -> Attr#{node => Role}
                    end
            end
    end.

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

build_child_spec_subtreess(#{group_mode := groups, subtrees := Subtrees} = Spec, Wrap) ->
    Wrap(Spec, Subtrees);
build_child_spec_subtreess(#{subtrees := Subtrees} = Spec, Wrap) ->
    [Wrap(Spec, Subtrees)].

validator_node(#{attr := Attr}, Nodes) ->
    ChildAttr = maps:with([node, validator], Attr),
    astranaut_uniplate:up_attr(
      fun(ParentAttr) ->
              maps:merge(maps:without([node, validator], ParentAttr), ChildAttr)
      end, Nodes).
