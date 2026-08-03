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
        Output = iolist_to_binary(generate(Schema)),
        run(Args, OutputFile, Output)
    catch
        throw:{usage, Message} ->
            io:format(standard_error, "~s~n", [Message]),
            halt(2);
        Class:Reason:Stacktrace ->
            io:format(standard_error, "syntax schema generation failed: ~p:~tp~n~tp~n",
                      [Class, Reason, Stacktrace]),
            halt(2)
    end.

run([], OutputFile, Output) ->
    ok = file:write_file(OutputFile, Output),
    io:format("generated ~s~n", [OutputFile]);
run(["--check"], OutputFile, Output) ->
    case file:read_file(OutputFile) of
        {ok, Output} ->
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
run(_, _OutputFile, _Output) ->
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

generate(Schema) ->
    Nodes = lists:sort(
              fun(A, B) -> maps:get(type, A) < maps:get(type, B) end,
              maps:get(nodes, Schema)),
    RoleEntries = role_entries(Nodes, maps:get(excluded_nodes, Schema, [])),
    ["%%%-------------------------------------------------------------------\n",
     "%%% @doc Direct dispatch generated from src/syntax.term.\n",
     "%%%\n",
     "%%% GENERATED FILE. DO NOT EDIT.\n",
     "%%% AST projection and reconstruction remain owned by erl_syntax.\n",
     "%%%-------------------------------------------------------------------\n",
     "-module(astranaut_syntax_schema).\n\n",
     "-export([node_roles/1, role_available/2, traverse_transparent/1,\n",
     "         node_available/2, format_available/3, slot_available/5,\n",
     "         child_layout/4]).\n\n",
     role_function(RoleEntries),
     role_available_function(Nodes),
     traverse_function(Nodes),
     availability_function(Nodes, Schema),
     format_functions(Nodes, Schema),
     slot_validation_function(Nodes, Schema),
     child_layout_function(Nodes, Schema),
     attribute_layout_function(maps:get(attribute_layouts, Schema), Schema)].

role_entries(Nodes, Excluded) ->
    Entries = Nodes ++ [E || E <- Excluded, maps:is_key(roles, E)],
    lists:sort([{maps:get(type, E), lists:usort(maps:get(roles, E))}
                || E <- Entries]).

role_function(Entries) ->
    [function_clause(node_roles, Type, Roles) || {Type, Roles} <- Entries] ++
        ["node_roles(_Type) ->\n    [expression, pattern, guard].\n\n"].

role_available_function(Nodes) ->
    Entries = lists:sort(
                [{maps:get(type, Node), Role}
                 || Node <- Nodes,
                    Role <- lists:usort(maps:get(slot_roles, Node, []))]),
    [[io_lib:format("role_available(~p, ~p) -> true;~n", [Type, Role])
      || {Type, Role} <- Entries],
     "role_available(Type, Role) ->\n"
     "    lists:member(Role, node_roles(Type)).\n\n"].

traverse_function(Nodes) ->
    Transparent = lists:sort(
                    [maps:get(type, Node)
                     || Node <- Nodes,
                        maps:get(traverse, Node, normal) =:= transparent]),
    [[io_lib:format("traverse_transparent(~p) -> true;~n", [Type])
      || Type <- Transparent],
     "traverse_transparent(_Type) -> false.\n\n"].

availability_function(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Bounds = [{maps:get(type, Node),
               maps:get(since, Node, SchemaMin),
               maps:get(until, Node, SchemaMax)} || Node <- Nodes],
    ["node_available(Type, 'pre-21') ->\n",
     "    node_available(Type, 19);\n",
     "node_available(Type, OtpVsn) when is_integer(OtpVsn) ->\n",
     "    case node_bounds(Type) of\n",
     "        {Since, Until} -> OtpVsn >= Since andalso OtpVsn =< Until;\n",
     "        unknown -> true\n",
     "    end;\n",
     "node_available(_Type, _OtpVsn) ->\n",
     "    false.\n\n",
     [io_lib:format("node_bounds(~p) -> {~B, ~B};~n", [Type, Since, Until])
      || {Type, Since, Until} <- Bounds],
     "node_bounds(_Type) -> unknown.\n\n"].

format_functions(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Entries = lists:append(
                [format_entries(Node, Nodes, SchemaMin, SchemaMax)
                 || Node <- Nodes]),
    KnownTypes = lists:usort([Type || {Type, _Pattern, _Guards, _Bounds} <- Entries]),
    ["format_available(Type, Node, 'pre-21') ->\n",
     "    format_available(Type, Node, 19);\n",
     "format_available(Type, Node, OtpVsn) when is_integer(OtpVsn) ->\n",
     "    node_available(Type, OtpVsn) andalso\n",
     "        case erl_syntax:is_tree(Node) of\n",
     "            true -> true;\n",
     "            false ->\n",
     "                case format_bounds(Type, Node) of\n",
     "                    {Since, Until} -> OtpVsn >= Since andalso OtpVsn =< Until;\n",
     "                    unknown -> not format_required(Type)\n",
     "                end\n",
     "        end;\n",
     "format_available(_Type, _Node, _OtpVsn) ->\n",
     "    false.\n\n",
     [format_clause(Entry) || Entry <- Entries],
     "format_bounds(_Type, _Node) -> unknown.\n\n",
     [io_lib:format("format_required(~p) -> true;~n", [Type])
      || Type <- KnownTypes],
     "format_required(_Type) -> false.\n\n"].

format_entries(Node, Nodes, SchemaMin, SchemaMax) ->
    Type = maps:get(type, Node),
    Formats = resolve_formats(Node, Nodes),
    [{Type, Pattern, Guards,
      {maps:get(since, Format, SchemaMin), maps:get(until, Format, SchemaMax)}}
     || Format <- Formats,
        {Pattern, Guards} <- [shape_pattern(maps:get(shape, Format))]].

format_clause({Type, Pattern, [], {Since, Until}}) ->
    io_lib:format("format_bounds(~p, ~s) -> {~B, ~B};~n",
                  [Type, Pattern, Since, Until]);
format_clause({Type, Pattern, Guards, {Since, Until}}) ->
    io_lib:format("format_bounds(~p, ~s) when ~s -> {~B, ~B};~n",
                  [Type, Pattern, join(" andalso ", Guards), Since, Until]).

slot_validation_function(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Entries = slot_validation_entries(Nodes, Schema, SchemaMin, SchemaMax),
    StructuralTypes = lists:sort(
                        [maps:get(type, Node)
                         || Node <- Nodes, maps:get(structural, Node, false)]),
    StructuralSlots = structural_slot_entries(Nodes, Schema, StructuralTypes),
    ["slot_available(ParentType, Slot, ChildType, ChildNode, 'pre-21') ->\n",
     "    slot_available(ParentType, Slot, ChildType, ChildNode, 19);\n",
     [slot_validation_clauses(Entry, SchemaMin, SchemaMax)
      || Entry <- Entries],
     [io_lib:format(
        "slot_available(~p, ~p, ~p, _ChildNode, _OtpVsn) -> true;~n",
        [ParentType, Slot, ChildType])
      || {ParentType, Slot, ChildType} <- StructuralSlots],
     [io_lib:format(
        "slot_available(_ParentType, _Slot, ~p, _ChildNode, _OtpVsn) -> false;~n",
        [ChildType])
      || ChildType <- StructuralTypes],
     "slot_available(_ParentType, _Slot, _ChildType, _ChildNode, _OtpVsn) ->\n",
     "    true.\n\n"].

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
    [[io_lib:format(
        "slot_available(~p, ~p, ~p, _ChildNode, OtpVsn) when ~s -> true;~n",
        [ParentType, Slot, Type, join(" andalso ", Guards)])
      || Type <- lists:usort(Types)],
     slot_rejection_clause(ParentType, Slot, Since, Until, SchemaMin, SchemaMax)];
slot_validation_clauses({ParentType, Slot,
                         #{since := Since, until := Until,
                           allowed_formats := Formats}}, SchemaMin, SchemaMax) ->
    VersionGuards = slot_rule_guards(Since, Until, SchemaMin, SchemaMax),
    [[begin
          {Pattern, ShapeGuards} = shape_pattern(Format),
          Guards = VersionGuards ++ ShapeGuards,
          io_lib:format(
            "slot_available(~p, ~p, _ChildType, ~s, OtpVsn) when ~s -> true;~n",
            [ParentType, Slot, Pattern, join(" andalso ", Guards)])
      end || Format <- Formats],
     slot_rejection_clause(ParentType, Slot, Since, Until, SchemaMin, SchemaMax)].

slot_rejection_clause(ParentType, Slot, Since, Until, SchemaMin, SchemaMax) ->
    Guards = slot_rule_guards(Since, Until, SchemaMin, SchemaMax),
    io_lib:format(
      "slot_available(~p, ~p, _ChildType, _ChildNode, OtpVsn) when ~s -> false;~n",
      [ParentType, Slot, join(" andalso ", Guards)]).

slot_rule_guards(Since, Until, SchemaMin, SchemaMax) ->
    ["is_integer(OtpVsn)"|
     version_guards(Since, Until, SchemaMin, SchemaMax)].

shape_pattern(Shape) ->
    {Pattern, Guards, _Next} = shape_pattern(Shape, 1),
    {Pattern, Guards}.

shape_pattern(anno, Next) -> {"_", [], Next};
shape_pattern({value, local_record_name}, Next) ->
    variable_pattern("LocalRecord", Next, fun(V) -> "is_atom(" ++ V ++ ")" end);
shape_pattern({value, native_record_name}, Next) ->
    Module = variable("Module", Next),
    Name = variable("Record", Next + 1),
    {"{" ++ Module ++ ", " ++ Name ++ "}",
     ["is_atom(" ++ Module ++ ")", "is_atom(" ++ Name ++ ")"], Next + 2};
shape_pattern({value, anonymous_record_name}, Next) -> {"[]", [], Next};
shape_pattern({value, _Name}, Next) -> {"_", [], Next};
shape_pattern({values, _Name}, Next) ->
    variable_pattern("Values", Next, fun(V) -> "is_list(" ++ V ++ ")" end);
shape_pattern({node, _Name}, Next) ->
    variable_pattern("Node", Next, fun(V) -> "not is_list(" ++ V ++ ")" end);
shape_pattern({nodes, _Name}, Next) ->
    variable_pattern("Nodes", Next, fun(V) -> "is_list(" ++ V ++ ")" end);
shape_pattern({optional, Shape}, Next) -> shape_pattern(Shape, Next);
shape_pattern([Head|Tail], Next0) ->
    {HeadPattern, HeadGuards, Next1} = shape_pattern(Head, Next0),
    {TailPattern, TailGuards, Next2} = shape_pattern(Tail, Next1),
    {"[" ++ HeadPattern ++ "|" ++ TailPattern ++ "]",
     HeadGuards ++ TailGuards, Next2};
shape_pattern([], Next) -> {"[]", [], Next};
shape_pattern(Tuple, Next0) when is_tuple(Tuple) ->
    {Patterns, Guards, Next1} = shape_patterns(tuple_to_list(Tuple), Next0, [], []),
    {"{" ++ join(", ", Patterns) ++ "}", Guards, Next1};
shape_pattern(Value, Next) ->
    {lists:flatten(io_lib:format("~p", [Value])), [], Next}.

shape_patterns([Shape|Shapes], Next0, PatternAcc, GuardAcc) ->
    {Pattern, Guards, Next1} = shape_pattern(Shape, Next0),
    shape_patterns(Shapes, Next1, [Pattern|PatternAcc], Guards ++ GuardAcc);
shape_patterns([], Next, PatternAcc, GuardAcc) ->
    {lists:reverse(PatternAcc), lists:reverse(GuardAcc), Next}.

variable_pattern(Prefix, Next, GuardFun) ->
    Var = variable(Prefix, Next),
    {Var, [GuardFun(Var)], Next + 1}.

variable(Prefix, Next) -> Prefix ++ integer_to_list(Next).

child_layout_function(Nodes, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Clauses = lists:append(
                [node_layout_clauses(Node, Nodes, Schema, SchemaMin, SchemaMax)
                 || Node <- Nodes, maps:get(type, Node) =/= attribute]),
    ["child_layout(Type, Subtrees, ParentRole, 'pre-21') ->\n",
     "    child_layout(Type, Subtrees, ParentRole, 19);\n",
     "child_layout(attribute, [[NameTree], BodyTrees], _ParentRole, OtpVsn) "
     "when is_integer(OtpVsn), is_list(BodyTrees) ->\n",
     "    try erl_syntax:atom_value(NameTree) of\n",
     "        Name -> attribute_layout(Name, NameTree, BodyTrees, OtpVsn)\n",
     "    catch\n",
     "        _:_ -> {error, {invalid_attribute_body, invalid_name, BodyTrees}}\n",
     "    end;\n",
     Clauses,
     "child_layout(Type, Subtrees, _ParentRole, _OtpVsn) ->\n",
     "    {error, {invalid_syntax_layout, Type, Subtrees}}.\n\n"].

node_layout_clauses(Node, Nodes, Schema, SchemaMin, SchemaMax) ->
    Type = maps:get(type, Node),
    Layouts = resolve_layouts(Node, Schema, Nodes),
    case Layouts of
        [] ->
            [io_lib:format("child_layout(~p, [], _ParentRole, OtpVsn) "
                           "when is_integer(OtpVsn) -> {ok, []};~n", [Type])];
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
                     Role -> lists:flatten(io_lib:format("~p", [Role]))
                 end,
    {SubtreesPattern, GroupVars, ShapeGuards} = subtree_pattern(Layout, Children),
    Since = maps:get(since, Layout, SchemaMin),
    Until = maps:get(until, Layout, SchemaMax),
    Guards = ["is_integer(OtpVsn)"|ShapeGuards ++
                                     version_guards(Since, Until, SchemaMin, SchemaMax)],
    GroupMode = case maps:get(groups, Layout) of any -> groups; _ -> nodes end,
    Descriptors = descriptors(Children, GroupVars, ParentRole, GroupMode),
    GuardText = case Guards of [] -> ""; _ -> " when " ++ join(" andalso ", Guards) end,
    io_lib:format("child_layout(~p, ~s, ~s, OtpVsn)~s ->~n"
                  "    {ok, [~s]};~n",
                  [Type, SubtreesPattern, ParentRole, GuardText,
                   join(", ", Descriptors)]).

subtree_pattern(#{groups := any}, [_Child]) ->
    {"Subtrees", ["Subtrees"], ["is_list(Subtrees)"]};
subtree_pattern(_Layout, Children) ->
    Patterns = [group_pattern(Index, maps:get(cardinality, Child))
                || {Index, Child} <- lists:zip(lists:seq(1, length(Children)), Children)],
    Groups = ["Group" ++ integer_to_list(I) || I <- lists:seq(1, length(Children))],
    Guards = ["is_list(" ++ Group ++ ")"
              || {Group, Child} <- lists:zip(Groups, Children),
                 maps:get(cardinality, Child) =/= one,
                 maps:get(cardinality, Child) =/= one_or_many],
    {"[" ++ join(", ", Patterns) ++ "]", Groups, Guards}.

group_pattern(Index, one) -> "Group" ++ integer_to_list(Index) ++ " = [_]";
group_pattern(Index, one_or_many) -> "Group" ++ integer_to_list(Index) ++ " = [_|_]";
group_pattern(Index, many) -> "Group" ++ integer_to_list(Index);
group_pattern(Index, deep_many) -> "Group" ++ integer_to_list(Index).

descriptors(Children, Groups, ParentRole, GroupMode) ->
    [lists:flatten(
       io_lib:format("{~p, ~s, ~s, ~p}",
                     [maps:get(slot, Child),
                      role_text(maps:get(role, Child), ParentRole), Group,
                      GroupMode]))
     || {Child, Group} <- lists:zip(Children, Groups)].

role_text(inherit, ParentRole) -> ParentRole;
role_text(Role, _ParentRole) -> lists:flatten(io_lib:format("~p", [Role])).

parent_role_variable(Children) ->
    case lists:any(fun(Child) -> maps:get(role, Child) =:= inherit end, Children) of
        true -> "ParentRole";
        false -> "_ParentRole"
    end.

version_guards(Since, Until, SchemaMin, SchemaMax) ->
    Lower = case Since =:= SchemaMin of true -> []; false -> ["OtpVsn >= " ++ integer_to_list(Since)] end,
    Upper = case Until =:= SchemaMax of true -> []; false -> ["OtpVsn =< " ++ integer_to_list(Until)] end,
    Lower ++ Upper.

attribute_layout_function(Layouts, Schema) ->
    #{min := SchemaMin, max := SchemaMax} = maps:get(otp_versions, Schema),
    Entries = lists:sort(maps:to_list(Layouts)),
    [[attribute_clauses(Name, Layout, SchemaMin, SchemaMax)
      || {Name, Layout} <- Entries, Name =/= default],
     attribute_default_clause(maps:get(default, Layouts))].

attribute_clauses(Name, Layout, SchemaMin, SchemaMax) ->
    Children = maps:get(children, Layout),
    {BodyPattern, Groups} = attribute_body_pattern(Children),
    Since = maps:get(since, Layout, SchemaMin),
    Until = maps:get(until, Layout, SchemaMax),
    Guards = ["is_integer(OtpVsn)"|
              version_guards(Since, Until, SchemaMin, SchemaMax)],
    GuardText = " when " ++ join(" andalso ", Guards),
    Descriptors = descriptors(Children, Groups, "_ParentRole", nodes),
    [io_lib:format("attribute_layout(~p, NameTree, ~s, OtpVsn)~s ->~n"
                   "    {ok, [{name, name, [NameTree], nodes}, ~s]};~n",
                   [Name, BodyPattern, GuardText, join(", ", Descriptors)]),
     attribute_fallback_clause(Name, Since, Until, SchemaMin, SchemaMax)].

attribute_fallback_clause(Name, SchemaMin, SchemaMax, SchemaMin, SchemaMax) ->
    io_lib:format("attribute_layout(~p, _NameTree, BodyTrees, _OtpVsn) ->~n"
                  "    {error, {invalid_attribute_body, ~p, BodyTrees}};~n",
                  [Name, Name]);
attribute_fallback_clause(Name, _Since, _Until, _SchemaMin, _SchemaMax) ->
    io_lib:format("attribute_layout(~p, NameTree, BodyTrees, _OtpVsn) ->~n"
                  "    {ok, [{name, name, [NameTree], nodes}, "
                  "{body, attribute_body, BodyTrees, nodes}]};~n",
                  [Name]).

attribute_default_clause(#{children := [Child]}) ->
    Slot = maps:get(slot, Child),
    Role = maps:get(role, Child),
    io_lib:format("attribute_layout(_Name, NameTree, BodyTrees, _OtpVsn) ->~n"
                  "    {ok, [{name, name, [NameTree], nodes}, "
                  "{~p, ~p, BodyTrees, nodes}]}.~n",
                  [Slot, Role]).

attribute_body_pattern(Children) ->
    attribute_body_pattern(Children, 1, [], []).

attribute_body_pattern([Child|Children], Index, PatternAcc, GroupAcc) ->
    Group = "Group" ++ integer_to_list(Index),
    case maps:get(cardinality, Child) of
        one ->
            attribute_body_pattern(Children, Index + 1,
                                   [Group|PatternAcc], ["[" ++ Group ++ "]"|GroupAcc]);
        many when Children =:= [] ->
            Prefix = lists:reverse(PatternAcc),
            Pattern = case Prefix of
                          [] -> Group;
                          _ -> "[" ++ join(", ", Prefix) ++ "|" ++ Group ++ "]"
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

function_clause(Name, Key, Value) ->
    io_lib:format("~p(~p) ->~n    ~tp;~n", [Name, Key, Value]).

join(_Separator, []) -> "";
join(Separator, [Head|Tail]) ->
    lists:flatten([Head, [[Separator, Item] || Item <- Tail]]).
