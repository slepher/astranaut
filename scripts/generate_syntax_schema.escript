#!/usr/bin/env escript
%% -*- erlang -*-

%% Compile src/syntax.term into direct Erlang dispatch clauses.

-mode(compile).

main(Args) ->
    try
        Root = project_root(),
        SchemaFile = filename:join([Root, "src", "syntax.term"]),
        OutputFile = filename:join([Root, "src", "astranaut_syntax_schema.erl"]),
        Schema = read_schema(SchemaFile),
        Forms = generate_forms(Schema),
        ok = validate_generated_forms(Forms),
        Output = render_forms(Forms),
        run(Args, OutputFile, Forms, Output)
    catch
        throw:{usage, Message} ->
            io:format(standard_error, "~s~n", [Message]),
            halt(2);
        Class:Reason:Stacktrace ->
            io:format(standard_error, "syntax schema generation failed: ~p:~tp~n~tp~n",
                      [Class, Reason, Stacktrace]),
            halt(2)
    end.

run([], OutputFile, _Forms, Output) ->
    ok = file:write_file(OutputFile, Output),
    io:format("generated ~s~n", [OutputFile]);
run(["--check"], OutputFile, Forms, _Output) ->
    case read_generated_forms(OutputFile) of
        {ok, Forms} ->
            io:format("syntax schema generated module is current~n", []);
        {ok, _Other} ->
            io:format(standard_error,
                      "~s is stale; run scripts/generate_syntax_schema.escript~n",
                      [OutputFile]),
            halt(1);
        {error, enoent} ->
            io:format(standard_error,
                      "~s is missing; run scripts/generate_syntax_schema.escript~n",
                      [OutputFile]),
            halt(1);
        {error, Reason} ->
            erlang:error({cannot_read_generated_module, OutputFile, Reason})
    end;
run(_, _OutputFile, _Forms, _Output) ->
    throw({usage, "usage: scripts/generate_syntax_schema.escript [--check]"}).

project_root() ->
    Script = filename:absname(escript:script_name()),
    filename:dirname(filename:dirname(Script)).

read_schema(File) ->
    case file:consult(File) of
        {ok, [Schema]} when is_map(Schema) -> Schema;
        {ok, Terms} -> erlang:error({invalid_syntax_schema, Terms});
        {error, Reason} -> erlang:error({cannot_read_syntax_schema, File, Reason})
    end.

generate_forms(Schema) ->
    Nodes = lists:sort(
              fun(A, B) -> maps:get(type, A) < maps:get(type, B) end,
              maps:get(nodes, Schema)),
    RoleEntries = role_entries(Nodes, maps:get(excluded_nodes, Schema, [])),
    [{attribute, 0, module, astranaut_syntax_schema},
     {attribute, 0, export,
      [{node_roles, 1}, {role_available, 2}, {traverse_transparent, 1},
       {node_available, 2}, {format_available, 3}, {slot_available, 5},
       {child_layout, 4}]},
     function_form(node_roles, role_clauses(RoleEntries)),
     function_form(role_available, role_available_clauses(Nodes)),
     function_form(traverse_transparent, traverse_clauses(Nodes)),
     function_form(node_available, node_available_clauses()),
     function_form(node_bounds, node_bounds_clauses(Nodes, Schema)),
     function_form(format_available, format_available_clauses()),
     function_form(format_bounds, format_bounds_clauses(Nodes, Schema)),
     function_form(format_required, format_required_clauses(Nodes, Schema)),
     function_form(slot_available, slot_available_clauses(Nodes, Schema)),
     function_form(child_layout, child_layout_clauses(Nodes, Schema)),
     function_form(attribute_layout,
                   attribute_layout_clauses(maps:get(attribute_layouts, Schema),
                                            Schema))].

validate_generated_forms(Forms) ->
    case compile:forms(Forms, [return_errors, return_warnings]) of
        {ok, astranaut_syntax_schema, _Beam} -> ok;
        {ok, astranaut_syntax_schema, _Beam, _Warnings} -> ok;
        {error, Errors, Warnings} ->
            erlang:error({invalid_generated_forms, Errors, Warnings})
    end.

render_forms(Forms) ->
    iolist_to_binary([header(), [erl_pp:form(Form) || Form <- Forms]]).

header() ->
    "%%%-------------------------------------------------------------------\n"
    "%%% @doc Direct dispatch generated from src/syntax.term.\n"
    "%%%\n"
    "%%% GENERATED FILE. DO NOT EDIT.\n"
    "%%% AST projection and reconstruction remain owned by erl_syntax.\n"
    "%%%-------------------------------------------------------------------\n".

read_generated_forms(File) ->
    case epp:parse_file(File, [], []) of
        {ok, Forms} ->
            {ok, [normalize_form(Form) || Form <- Forms,
                                         keep_generated_form(Form)]};
        {error, enoent} ->
            {error, enoent};
        {error, Reason} ->
            {error, Reason}
    end.

keep_generated_form({attribute, _Anno, file, _File}) -> false;
keep_generated_form({eof, _Anno}) -> false;
keep_generated_form(_Form) -> true.

normalize_form(Form) ->
    erl_parse:map_anno(fun(_Anno) -> 0 end, Form).

role_entries(Nodes, Excluded) ->
    Entries = Nodes ++ [E || E <- Excluded, maps:is_key(roles, E)],
    lists:sort([{maps:get(type, E), lists:usort(maps:get(roles, E))}
                || E <- Entries]).

role_clauses(Entries) ->
    [clause([literal(Type)], [], [literal(Roles)]) || {Type, Roles} <- Entries] ++
        [clause([variable('_Type')], [], [literal([expression, pattern, guard])])].

role_available_clauses(Nodes) ->
    Entries = lists:sort(
                [{maps:get(type, Node), Role}
                 || Node <- Nodes,
                    Role <- lists:usort(maps:get(slot_roles, Node, []))]),
    [clause([literal(Type), literal(Role)], [], [literal(true)])
     || {Type, Role} <- Entries] ++
        [clause([variable('Type'), variable('Role')], [],
                [remote_call(lists, member,
                             [variable('Role'),
                              local_call(node_roles, [variable('Type')])])])].

traverse_clauses(Nodes) ->
    Transparent = lists:sort(
                    [maps:get(type, Node)
                     || Node <- Nodes,
                        maps:get(traverse, Node, normal) =:= transparent]),
    [clause([literal(Type)], [], [literal(true)]) || Type <- Transparent] ++
        [clause([variable('_Type')], [], [literal(false)])].

node_available_clauses() ->
    Type = variable('Type'),
    OtpVsn = variable('OtpVsn'),
    Since = variable('Since'),
    Until = variable('Until'),
    [clause([Type, OtpVsn], [],
            [case_expr(
               local_call(node_bounds, [Type]),
               [clause([tuple_ast([Since, Until])], [],
                       [andalso_expr(op_expr('>=', OtpVsn, Since),
                                     op_expr('=<', OtpVsn, Until))]),
                clause([literal(unknown)], [], [literal(true)])])])].

node_bounds_clauses(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Bounds = [{maps:get(type, Node),
               maps:get(since, Node, SchemaMin),
               maps:get(until, Node, SchemaMax)} || Node <- Nodes],
    [clause([literal(Type)], [], [literal({Since, Until})])
     || {Type, Since, Until} <- Bounds] ++
        [clause([variable('_Type')], [], [literal(unknown)])].

all_format_entries(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    lists:append([format_entries(Node, Nodes, SchemaMin, SchemaMax)
                  || Node <- Nodes]).

format_available_clauses() ->
    Type = variable('Type'),
    Node = variable('Node'),
    OtpVsn = variable('OtpVsn'),
    Since = variable('Since'),
    Until = variable('Until'),
    BoundsCase =
        case_expr(
          local_call(format_bounds, [Type, Node]),
          [clause([tuple_ast([Since, Until])], [],
                  [andalso_expr(op_expr('>=', OtpVsn, Since),
                                op_expr('=<', OtpVsn, Until))]),
           clause([literal(unknown)], [],
                  [not_expr(local_call(format_required, [Type]))])]),
    TreeCase =
        case_expr(
          remote_call(erl_syntax, is_tree, [Node]),
          [clause([literal(true)], [], [literal(true)]),
           clause([literal(false)], [], [BoundsCase])]),
    [clause([Type, Node, OtpVsn], [],
            [andalso_expr(local_call(node_available, [Type, OtpVsn]), TreeCase)])].

format_bounds_clauses(Nodes, Schema) ->
    [format_clause(Entry) || Entry <- all_format_entries(Nodes, Schema)] ++
        [clause([variable('_Type'), variable('_Node')], [], [literal(unknown)])].

format_required_clauses(Nodes, Schema) ->
    Entries = all_format_entries(Nodes, Schema),
    KnownTypes = lists:usort([Type || {Type, _Pattern, _Guards, _Bounds} <- Entries]),
    [clause([literal(Type)], [], [literal(true)]) || Type <- KnownTypes] ++
        [clause([variable('_Type')], [], [literal(false)])].

format_entries(Node, Nodes, SchemaMin, SchemaMax) ->
    Type = maps:get(type, Node),
    Formats = resolve_formats(Node, Nodes),
    [{Type, Pattern, Guards,
      {maps:get(since, Format, SchemaMin), maps:get(until, Format, SchemaMax)}}
     || Format <- Formats,
        {Pattern, Guards} <- [shape_pattern(maps:get(shape, Format))]].

format_clause({Type, Pattern, Guards, Bounds}) ->
    clause([literal(Type), Pattern], Guards, [literal(Bounds)]).

slot_available_clauses(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Entries = slot_validation_entries(Nodes, Schema, SchemaMin, SchemaMax),
    StructuralTypes = lists:sort(
                        [maps:get(type, Node)
                         || Node <- Nodes, maps:get(structural, Node, false)]),
    StructuralSlots = structural_slot_entries(Nodes, Schema, StructuralTypes),
    lists:append([slot_validation_clauses(Entry, SchemaMin, SchemaMax)
                  || Entry <- Entries]) ++
        [clause([literal(ParentType), literal(Slot), literal(ChildType),
                 variable('_ChildNode'), variable('_OtpVsn')], [], [literal(true)])
         || {ParentType, Slot, ChildType} <- StructuralSlots] ++
        [clause([variable('_ParentType'), variable('_Slot'), literal(ChildType),
                 variable('_ChildNode'), variable('_OtpVsn')], [], [literal(false)])
         || ChildType <- StructuralTypes] ++
        [clause([variable('_ParentType'), variable('_Slot'), variable('_ChildType'),
                 variable('_ChildNode'), variable('_OtpVsn')], [], [literal(true)])].

structural_slot_entries(Nodes, Schema, StructuralTypes) ->
    Entries = lists:usort(
                lists:append(
                  [begin
                       ParentType = maps:get(type, Node),
                       Layouts = resolve_layouts(Node, Schema, Nodes),
                       lists:append(
                         [[{ParentType, maps:get(slot, Child), ChildType}
                           || ChildType <- structural_child_types(
                                             Child, Nodes)]
                          || Layout <- Layouts,
                             Child <- layout_children(Layout)])
                   end || Node <- Nodes])),
    case [Entry || {_ParentType, _Slot, ChildType} = Entry <- Entries,
                   not lists:member(ChildType, StructuralTypes)] of
        [] -> Entries;
        Invalid -> erlang:error({non_structural_slot_types, Invalid})
    end.

structural_child_types(Child, Nodes) ->
    RoleTypes = [maps:get(type, Node)
                 || Node <- Nodes,
                    lists:member(maps:get(role, Child),
                                 maps:get(slot_roles, Node, []))],
    lists:usort(RoleTypes ++ maps:get(allowed_structural_types, Child, [])).

slot_validation_entries(Nodes, Schema, SchemaMin, SchemaMax) ->
    lists:usort(
      lists:append(
        [begin
             ParentType = maps:get(type, Node),
             Layouts = resolve_layouts(Node, Schema, Nodes),
             [{ParentType, maps:get(slot, Child),
               normalize_slot_rule(ParentType, maps:get(slot, Child), Rule,
                                   SchemaMin, SchemaMax)}
              || Layout <- Layouts,
                 Child <- layout_children(Layout),
                 Rule <- maps:get(validation, Child, [])]
         end || Node <- Nodes])).

layout_children(#{context := Context}) ->
    lists:append([maps:get(children, Variant) || Variant <- Context]);
layout_children(#{children := Children}) ->
    Children.

normalize_slot_rule(ParentType, Slot, Rule, SchemaMin, SchemaMax) ->
    Since = maps:get(since, Rule, SchemaMin),
    Until = maps:get(until, Rule, SchemaMax),
    Allowed = [{Key, maps:get(Key, Rule)}
               || Key <- [allowed_types, allowed_formats], maps:is_key(Key, Rule)],
    case {Since >= SchemaMin, Since =< Until, Until =< SchemaMax, Allowed} of
        {true, true, true, [{Key, Values}]} when is_list(Values), Values =/= [] ->
            #{since => Since, until => Until, Key => Values};
        _ ->
            erlang:error({invalid_slot_validation, ParentType, Slot, Rule})
    end.

slot_validation_clauses({ParentType, Slot,
                         #{since := Since, until := Until,
                           allowed_types := Types}}, SchemaMin, SchemaMax) ->
    Guards = slot_rule_guards(Since, Until, SchemaMin, SchemaMax),
    OtpVsn = version_variable(Guards),
    [clause([literal(ParentType), literal(Slot), literal(Type),
             variable('_ChildNode'), OtpVsn], Guards, [literal(true)])
     || Type <- lists:usort(Types)] ++
        [slot_rejection_clause(ParentType, Slot, Since, Until, SchemaMin, SchemaMax)];
slot_validation_clauses({ParentType, Slot,
                         #{since := Since, until := Until,
                           allowed_formats := Formats}}, SchemaMin, SchemaMax) ->
    VersionGuards = slot_rule_guards(Since, Until, SchemaMin, SchemaMax),
    [begin
          {Pattern, ShapeGuards} = shape_pattern(Format),
          Guards = VersionGuards ++ ShapeGuards,
          clause([literal(ParentType), literal(Slot), variable('_ChildType'),
                  Pattern, version_variable(VersionGuards)], Guards, [literal(true)])
      end || Format <- Formats] ++
        [slot_rejection_clause(ParentType, Slot, Since, Until, SchemaMin, SchemaMax)].

slot_rejection_clause(ParentType, Slot, Since, Until, SchemaMin, SchemaMax) ->
    Guards = slot_rule_guards(Since, Until, SchemaMin, SchemaMax),
    clause([literal(ParentType), literal(Slot), variable('_ChildType'),
            variable('_ChildNode'), version_variable(Guards)], Guards, [literal(false)]).

slot_rule_guards(Since, Until, SchemaMin, SchemaMax) ->
    version_guards(Since, Until, SchemaMin, SchemaMax).

shape_pattern(Shape) ->
    {Pattern, Guards, _Next} = shape_pattern(Shape, 1),
    {Pattern, Guards}.

shape_pattern(anno, Next) -> {variable('_'), [], Next};
shape_pattern({value, local_record_name}, Next) ->
    variable_pattern("LocalRecord", Next, is_atom);
shape_pattern({value, native_record_name}, Next) ->
    Module = variable("Module", Next),
    Name = variable("Record", Next + 1),
    {tuple_ast([Module, Name]),
     [local_call(is_atom, [Module]), local_call(is_atom, [Name])], Next + 2};
shape_pattern({value, anonymous_record_name}, Next) -> {literal([]), [], Next};
shape_pattern({value, _Name}, Next) -> {variable('_'), [], Next};
shape_pattern({values, _Name}, Next) ->
    variable_pattern("Values", Next, is_list);
shape_pattern({node, _Name}, Next) ->
    Var = variable("Node", Next),
    {Var, [not_expr(local_call(is_list, [Var]))], Next + 1};
shape_pattern({nodes, _Name}, Next) ->
    variable_pattern("Nodes", Next, is_list);
shape_pattern({optional, Shape}, Next) -> shape_pattern(Shape, Next);
shape_pattern([Head|Tail], Next0) ->
    {HeadPattern, HeadGuards, Next1} = shape_pattern(Head, Next0),
    {TailPattern, TailGuards, Next2} = shape_pattern(Tail, Next1),
    {cons_ast(HeadPattern, TailPattern), HeadGuards ++ TailGuards, Next2};
shape_pattern([], Next) -> {literal([]), [], Next};
shape_pattern(Tuple, Next0) when is_tuple(Tuple) ->
    {Patterns, Guards, Next1} = shape_patterns(tuple_to_list(Tuple), Next0, [], []),
    {tuple_ast(Patterns), Guards, Next1};
shape_pattern(Value, Next) ->
    {literal(Value), [], Next}.

shape_patterns([Shape|Shapes], Next0, PatternAcc, GuardAcc) ->
    {Pattern, Guards, Next1} = shape_pattern(Shape, Next0),
    shape_patterns(Shapes, Next1, [Pattern|PatternAcc], Guards ++ GuardAcc);
shape_patterns([], Next, PatternAcc, GuardAcc) ->
    {lists:reverse(PatternAcc), lists:reverse(GuardAcc), Next}.

variable_pattern(Prefix, Next, GuardName) ->
    Var = variable(Prefix, Next),
    {Var, [local_call(GuardName, [Var])], Next + 1}.

variable(Prefix, Next) ->
    variable(list_to_atom(lists:concat([Prefix, Next]))).

child_layout_clauses(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Clauses = lists:append(
                [node_layout_clauses(Node, Nodes, Schema, SchemaMin, SchemaMax)
                 || Node <- Nodes, maps:get(type, Node) =/= attribute]),
    [attribute_child_layout_clause()] ++ Clauses ++
        [clause([variable('Type'), variable('Subtrees'), variable('_ParentRole'),
                 variable('_OtpVsn')], [],
                [error_tuple(invalid_syntax_layout,
                             [variable('Type'), variable('Subtrees')])])].

attribute_child_layout_clause() ->
    NameTree = variable('NameTree'),
    BodyTrees = variable('BodyTrees'),
    OtpVsn = variable('OtpVsn'),
    Try = {'try', 0,
           [remote_call(erl_syntax, atom_value, [NameTree])],
           [clause([variable('Name')], [],
                   [local_call(attribute_layout,
                               [variable('Name'), NameTree, BodyTrees, OtpVsn])])],
           [clause([tuple_ast([variable('_'), variable('_'), variable('_')])], [],
                   [error_tuple(invalid_attribute_body,
                                [literal(invalid_name), BodyTrees])])],
           []},
    clause([literal(attribute),
            list_ast([list_ast([NameTree]), BodyTrees]),
            variable('_ParentRole'), OtpVsn],
           [local_call(is_list, [BodyTrees])],
           [Try]).

node_layout_clauses(Node, Nodes, Schema, SchemaMin, SchemaMax) ->
    Type = maps:get(type, Node),
    Layouts = resolve_layouts(Node, Schema, Nodes),
    case Layouts of
        [] ->
            [clause([literal(Type), literal([]), variable('_ParentRole'),
                     variable('_OtpVsn')], [], [ok_tuple([])])];
        _ ->
            lists:append(
              [layout_clauses(Type, Layout, SchemaMin, SchemaMax)
               || Layout <- Layouts])
    end.

layout_clauses(Type, #{context := Context} = Layout, SchemaMin, SchemaMax) ->
    Exact = [Variant || Variant <- Context,
                        maps:get(when_role, Variant) =/= any],
    Fallback = [Variant || Variant <- Context,
                           maps:get(when_role, Variant) =:= any],
    [layout_clause(Type, Layout, Variant, SchemaMin, SchemaMax)
     || Variant <- Exact ++ Fallback];
layout_clauses(Type, Layout, SchemaMin, SchemaMax) ->
    [layout_clause(Type, Layout, #{children => maps:get(children, Layout)},
                   SchemaMin, SchemaMax)].

layout_clause(Type, Layout, Variant, SchemaMin, SchemaMax) ->
    Children = maps:get(children, Variant),
    ParentRole = case maps:get(when_role, Variant, any) of
                     any -> parent_role_variable(Children);
                     Role -> literal(Role)
                 end,
    {SubtreesPattern, GroupVars, ShapeGuards} = subtree_pattern(Layout, Children),
    Since = maps:get(since, Layout, SchemaMin),
    Until = maps:get(until, Layout, SchemaMax),
    VersionGuards = version_guards(Since, Until, SchemaMin, SchemaMax),
    Guards = ShapeGuards ++ VersionGuards,
    GroupMode = case maps:get(groups, Layout) of any -> groups; _ -> nodes end,
    Descriptors = descriptors(Children, GroupVars, ParentRole, GroupMode),
    clause([literal(Type), SubtreesPattern, ParentRole,
            version_variable(VersionGuards)],
           Guards, [ok_tuple(Descriptors)]).

subtree_pattern(#{groups := any}, [_Child]) ->
    Subtrees = variable('Subtrees'),
    {Subtrees, [Subtrees], [local_call(is_list, [Subtrees])]};
subtree_pattern(_Layout, Children) ->
    Patterns = [group_pattern(Index, maps:get(cardinality, Child))
                || {Index, Child} <- lists:zip(lists:seq(1, length(Children)), Children)],
    Groups = [variable("Group", I) || I <- lists:seq(1, length(Children))],
    Guards = [local_call(is_list, [Group])
              || {Group, Child} <- lists:zip(Groups, Children),
                 maps:get(cardinality, Child) =/= one,
                 maps:get(cardinality, Child) =/= one_or_many],
    {list_ast(Patterns), Groups, Guards}.

group_pattern(Index, one) ->
    match_expr(variable("Group", Index), list_ast([variable('_')]));
group_pattern(Index, one_or_many) ->
    match_expr(variable("Group", Index), cons_ast(variable('_'), variable('_')));
group_pattern(Index, many) -> variable("Group", Index);
group_pattern(Index, deep_many) -> variable("Group", Index).

descriptors(Children, Groups, ParentRole, GroupMode) ->
    [tuple_ast([literal(maps:get(slot, Child)),
                role_ast(maps:get(role, Child), ParentRole), Group,
                literal(GroupMode)])
     || {Child, Group} <- lists:zip(Children, Groups)].

role_ast(inherit, ParentRole) -> ParentRole;
role_ast(Role, _ParentRole) -> literal(Role).

parent_role_variable(Children) ->
    case lists:any(fun(Child) -> maps:get(role, Child) =:= inherit end, Children) of
        true -> variable('ParentRole');
        false -> variable('_ParentRole')
    end.

version_guards(Since, Until, SchemaMin, SchemaMax) ->
    Lower = case Since =:= SchemaMin of
                true -> [];
                false -> [op_expr('>=', variable('OtpVsn'), literal(Since))]
            end,
    Upper = case Until =:= SchemaMax of
                true -> [];
                false -> [op_expr('=<', variable('OtpVsn'), literal(Until))]
            end,
    Lower ++ Upper.

version_variable([]) -> variable('_OtpVsn');
version_variable(_Guards) -> variable('OtpVsn').

attribute_layout_clauses(Layouts, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Entries = lists:sort(maps:to_list(Layouts)),
    lists:append([attribute_clauses(Name, Layout, SchemaMin, SchemaMax)
                  || {Name, Layout} <- Entries, Name =/= default]) ++
        [attribute_default_clause(maps:get(default, Layouts))].

attribute_clauses(Name, Layout, SchemaMin, SchemaMax) ->
    Children = maps:get(children, Layout),
    {BodyPattern, Groups} = attribute_body_pattern(Children),
    Since = maps:get(since, Layout, SchemaMin),
    Until = maps:get(until, Layout, SchemaMax),
    VersionGuards = version_guards(Since, Until, SchemaMin, SchemaMax),
    Descriptors = descriptors(Children, Groups, variable('_ParentRole'), nodes),
    NameDescriptor = tuple_ast([literal(name), literal(name),
                                list_ast([variable('NameTree')]), literal(nodes)]),
    [clause([literal(Name), variable('NameTree'), BodyPattern,
             version_variable(VersionGuards)],
            VersionGuards, [ok_tuple([NameDescriptor|Descriptors])])] ++
        attribute_fallback_clauses(Name, BodyPattern, Since, Until,
                                   SchemaMin, SchemaMax).

attribute_fallback_clauses(_Name, {var, _Anno, _Variable},
                           SchemaMin, SchemaMax, SchemaMin, SchemaMax) ->
    [];
attribute_fallback_clauses(Name, _BodyPattern,
                           SchemaMin, SchemaMax, SchemaMin, SchemaMax) ->
    [clause([literal(Name), variable('_NameTree'), variable('BodyTrees'),
             variable('_OtpVsn')], [],
            [error_tuple(invalid_attribute_body,
                         [literal(Name), variable('BodyTrees')])])];
attribute_fallback_clauses(Name, _BodyPattern,
                           _Since, _Until, _SchemaMin, _SchemaMax) ->
    [clause([literal(Name), variable('NameTree'), variable('BodyTrees'),
             variable('_OtpVsn')], [],
            [ok_tuple([
               tuple_ast([literal(name), literal(name),
                          list_ast([variable('NameTree')]), literal(nodes)]),
               tuple_ast([literal(body), literal(attribute_body),
                          variable('BodyTrees'), literal(nodes)])])])].

attribute_default_clause(#{children := [Child]}) ->
    Slot = maps:get(slot, Child),
    Role = maps:get(role, Child),
    clause([variable('_Name'), variable('NameTree'), variable('BodyTrees'),
            variable('_OtpVsn')], [],
           [ok_tuple([
              tuple_ast([literal(name), literal(name),
                         list_ast([variable('NameTree')]), literal(nodes)]),
              tuple_ast([literal(Slot), literal(Role),
                         variable('BodyTrees'), literal(nodes)])])]).

attribute_body_pattern(Children) ->
    attribute_body_pattern(Children, 1, [], []).

attribute_body_pattern([Child|Children], Index, PatternAcc, GroupAcc) ->
    Group = variable("Group", Index),
    case maps:get(cardinality, Child) of
        one ->
            attribute_body_pattern(Children, Index + 1,
                                   [Group|PatternAcc], [list_ast([Group])|GroupAcc]);
        many when Children =:= [] ->
            Prefix = lists:reverse(PatternAcc),
            Pattern = case Prefix of
                          [] -> Group;
                          _ -> list_with_tail(Prefix, Group)
                      end,
            {Pattern, lists:reverse([Group|GroupAcc])}
    end.

resolve_layouts(#{alias_of := Target}, Schema, Nodes) ->
    resolve_layouts(find_node(Target, Nodes), Schema, Nodes);
resolve_layouts(#{layouts := Layouts}, _Schema, _Nodes) -> Layouts;
resolve_layouts(#{layout := attribute}, _Schema, _Nodes) -> [];
resolve_layouts(#{layout := Name}, Schema, _Nodes) ->
    maps:get(Name, maps:get(layouts, Schema));
resolve_layouts(_Node, _Schema, _Nodes) -> [].

resolve_formats(#{alias_of := Target}, Nodes) ->
    resolve_formats(find_node(Target, Nodes), Nodes);
resolve_formats(Node, _Nodes) -> maps:get(formats, Node, []).

find_node(Type, Nodes) ->
    case [Node || Node <- Nodes, maps:get(type, Node) =:= Type] of
        [Node] -> Node;
        [] -> erlang:error({missing_alias_target, Type})
    end.

function_form(Name, [{clause, _Anno, Patterns, _Guards, _Body}|_] = Clauses) ->
    {function, 0, Name, length(Patterns), Clauses}.

clause(Patterns, [], Body) -> {clause, 0, Patterns, [], Body};
clause(Patterns, Guards, Body) -> {clause, 0, Patterns, [Guards], Body}.

literal(Value) -> erl_parse:abstract(Value).
variable(Name) -> {var, 0, Name}.
tuple_ast(Elements) -> {tuple, 0, Elements}.
list_ast(Elements) -> list_with_tail(Elements, {nil, 0}).
list_with_tail([Head|Tail], Last) -> cons_ast(Head, list_with_tail(Tail, Last));
list_with_tail([], Last) -> Last.
cons_ast(Head, Tail) -> {cons, 0, Head, Tail}.
match_expr(Pattern, Value) -> {match, 0, Pattern, Value}.
local_call(Name, Args) -> {call, 0, literal(Name), Args}.
remote_call(Module, Name, Args) ->
    {call, 0, {remote, 0, literal(Module), literal(Name)}, Args}.
op_expr(Name, Arg) -> {op, 0, Name, Arg}.
op_expr(Name, Left, Right) -> {op, 0, Name, Left, Right}.
andalso_expr(Left, Right) -> op_expr('andalso', Left, Right).
not_expr(Arg) -> op_expr('not', Arg).
case_expr(Argument, Clauses) -> {'case', 0, Argument, Clauses}.
ok_tuple(Descriptors) -> tuple_ast([literal(ok), list_ast(Descriptors)]).
error_tuple(Reason, Arguments) ->
    tuple_ast([literal(error), tuple_ast([literal(Reason)|Arguments])]).
