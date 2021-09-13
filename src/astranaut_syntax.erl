%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_syntax).

-type form() :: erl_parse:abstract_form().

%% API
-export([type/1, get_pos/1, set_pos/2, is_pos/1, is_leaf/1]).
-export([subtrees/1, update_tree/2, revert/1]).
-export([subtrees_pge/3, attribute_subtrees_type/3]).
-export([pattern_node/1, guard_node/1, expression_node/1, update_node/2]).
-export([reorder_updated_forms/1, sort_forms/1, insert_forms/2]).

type(Node) ->
    erl_syntax:type(Node).

get_pos(Node) ->
    erl_syntax:get_pos(Node).

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
    {warnings, {Pos, Formatter, Warning}};
set_pos_1(Node, Pos) ->
    setelement(2, Node, Pos).

is_pos(Pos) ->
    case Pos of
        Pos when is_integer(Pos) ->
            true;
        {Line, Column} when is_integer(Line), is_integer(Column) ->
            true;
        Pos ->
            false
    end.

is_leaf(Node) ->
    erl_syntax:is_leaf(Node).

-spec subtrees(erl_syntax:syntaxTree()) -> [[erl_syntax:syntaxTree()]].
subtrees({attribute, Pos, Name, {TypeName, TypeBody, TypeParams}}) when Name == type; Name == opaque ->
    NameTree = name_arity_tree(Name, Pos),
    TypeNameTree = name_arity_tree(TypeName, Pos),
    [[NameTree], [TypeNameTree, TypeBody|TypeParams]];
subtrees({attribute, Pos, Name, {MFA, Specs}}) when Name == spec; Name == callback ->
    NameTree = name_arity_tree(Name, Pos),
    MFATree = mfa_tree(MFA, Pos),
    [[NameTree], [MFATree|Specs]];
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

%% as this issue mentioned, it's a bug, but will cause compatibility issue
%% https://github.com/erlang/otp/issues/4529
%% the goal of module is to fix this without cause compatibility issues.
%% just use astranaut_syntax:subtrees/1 replace of erl_syntax:subtress/1,
%% astranaut_syntax:revert/1 replace of erl_syntax:revert/1.
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

revert_attribute(Name, [TypeNameTree, TypeTree|TypeParamTrees], Pos, _Node) when Name == type; Name == opaque ->
    TypeName = erl_syntax:atom_value(TypeNameTree),
    {attribute, Pos, Name, {TypeName, TypeTree, TypeParamTrees}};
revert_attribute(Name, [MFATree|SpecTrees], Pos, _Node) when Name == spec; Name == callback ->
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

subtrees_pge(_Type, Subtrees, #{node := pattern}) ->
    Subtrees;
subtrees_pge(named_fun_expr, [Names, Clauses], #{}) ->
    [pattern_node(Names), Clauses];
subtrees_pge(Type, [Patterns, Expressions], #{}) when Type == match_expr; Type == clause ->
    [pattern_node(Patterns), expression_node(Expressions)];
subtrees_pge(clause, [Patterns, Guards, Expressions], #{}) ->
    [pattern_node(Patterns), guard_node(Guards), expression_node(Expressions)];
subtrees_pge(Type, [Patterns, Expressions], #{}) when Type == generator; Type == binary_generator ->
    [pattern_node(Patterns), expression_node(Expressions)];
subtrees_pge(_Type, Subtrees, #{}) ->
    Subtrees.

attribute_subtrees_type(attribute, [[NameTree], BodyTrees], #{}) ->
    Name = erl_syntax:atom_value(NameTree),
    [[NameTree], update_attribute_body_trees(Name, BodyTrees)];
attribute_subtrees_type(_Type, Subtrees, #{}) ->
    Subtrees.

update_attribute_body_trees(record = Name, [RecordNameTree|RecordBodyTrees]) ->
    attribute(Name, [name_node(RecordNameTree)|RecordBodyTrees]);
update_attribute_body_trees(Name, [TypeNameTree, TypeTree|TypeParamTrees]) when Name == type; Name == opaque ->
    attribute(Name, [name_node(TypeNameTree), type_node(TypeTree)|type_param_node(TypeParamTrees)]);
update_attribute_body_trees(Name, [SpecMFATree|SpecTrees]) when Name == spec; Name == callback ->
    attribute(Name, [name_node(SpecMFATree)|type_node(SpecTrees)]);
update_attribute_body_trees(Name, BodyTrees) ->
    attribute(Name, BodyTrees).

pattern_node(Subtree) ->
    update_node(pattern, Subtree).

guard_node(Subtree) ->
    update_node(guard, Subtree).

expression_node(Subtree) ->
    update_node(expression, Subtree).

name_node(Subtree) ->
    update_node(name, Subtree).

type_node(Subtree) ->
    update_node(type, Subtree).

type_param_node(Subtree) ->
    update_node(type_param, Subtree).

attribute(Attribute, Subtree) ->
    astranaut_uniplate:up_attr(#{attribute => Attribute}, Subtree).

update_node(Node, Subtree) ->
    astranaut_uniplate:up_attr(#{node => Node}, Subtree).

%%===================================================================
%% update forms related functions
%%===================================================================
-spec reorder_updated_forms([form()]) -> [form()].
reorder_updated_forms(Forms) ->
    Functions = forms_functions(Forms),
    reorder_updated_forms(Forms, Functions, grforms_new()).

reorder_updated_forms([{updated, Form, NewForms}|Tails], Functions0, GRForms) ->
    %% get new functions after transformed.
    FormFunctions = forms_functions([Form]),
    NewFormsFunctions = forms_functions(NewForms),
    NewFormsFunctions1 = ordsets:subtract(NewFormsFunctions, FormFunctions),
    {Functions1, GRForms1, Tails2} =
        insert_forms(NewForms, NewFormsFunctions1, Functions0, GRForms, Tails),
    reorder_updated_forms(Tails2, Functions1, GRForms1);
reorder_updated_forms([Form|Tails], Functions, GRForms) ->
    reorder_updated_forms(Tails, Functions, grforms_append(Form, GRForms));
reorder_updated_forms([], _Functions, GRForms) ->
    grforms_to_forms(GRForms).

forms_functions(Forms) ->
    forms_functions(Forms, ordsets:new()).

forms_functions(Forms, Functions0) ->
    lists:foldl(
      fun({function, _Pos, Name, Arity, _Clauses}, Acc) ->
              ordsets:add_element({Name, Arity}, Acc);
         (_Node, Acc) ->
              Acc
      end, Functions0, Forms).

grforms_new() ->
    %% [Eof], [Function...], [Attribute...], [Module...]}.
    %% ERForms, FRForms, ARForms, MForms.
    {[], [], [], []}.

grforms_to_forms({ERForms, FRForms, ARForms, MRForms}) ->
    lists:reverse(MRForms) ++ lists:reverse(ARForms) ++ lists:reverse(FRForms) ++ ERForms.

grforms_append({attribute, _Pos, module, _ModuleName} = Module, {[], [], [], MForms}) ->
    {[], [], [], [Module|MForms]};
grforms_append({attribute, _Pos, file, _FileName} = File, {[], [], [], MForms}) ->
    {[], [], [], [File|MForms]};
grforms_append({attribute, _Pos, file, _FileName} = File, {[], [], ARForms, MForms}) ->
    {[], [], [File|ARForms], MForms};
grforms_append({attribute, _Pos, file, _FileName} = File, {[], FRForms, ARForms, MForms}) ->
    {[], [File|FRForms], ARForms, MForms};
grforms_append({attribute, _Pos, export, _Exports} = Export, {[], FRForms, ARForms, MForms}) ->
    {[], FRForms, [Export|ARForms], MForms};
grforms_append({attribute, _Pos, export_type, _Exports} = ExportType, {[], FRForms, ARForms, MForms}) ->
    {[], FRForms, [ExportType|ARForms], MForms};
grforms_append({attribute, _Pos, spec, _SpecValue} = Spec, {ERForms, FRForms, ARForms, MRForms}) ->
    {ERForms, [Spec|FRForms], ARForms, MRForms};
grforms_append({function, _Pos, _Name, _Arity, _Clauses} = Function, {ERForms, FRForms, ARForms, MRForms}) ->
    {ERForms, [Function|FRForms], ARForms, MRForms};
grforms_append({eof, _Pos} = Eof, {[], FRForms, ARForms, MRForms}) ->
    {[Eof], FRForms, ARForms, MRForms};
grforms_append(Form, {[], [], ARForms, MRForms}) ->
    {[], [], [Form|ARForms], MRForms};
grforms_append(Form, {[], FRForms, ARForms, MRForms}) ->
    {[], [Form|FRForms], ARForms, MRForms};
grforms_append(Form, GRForms) ->
    erlang:exit({insert_form_failed, Form, GRForms}).

grforms_insert(Form, GRForms) ->
    case erl_syntax:type(Form) of
	attribute ->
            Name = erl_syntax:concrete(erl_syntax:attribute_name(Form)),
            grforms_insert_attribute(Name, Form, GRForms);
	comment ->
            grforms_insert_comment(Form, GRForms);
	function ->
            grforms_insert_function(Form, GRForms);
	eof_marker ->
            grforms_insert_eof(Form, GRForms);
	error_marker ->
            grforms_insert_error_marker(Form, GRForms);
	form_list ->
            grforms_insert_form_list(Form, GRForms);
	warning_marker ->
            grforms_insert_warning_marker(Form, GRForms);
	text ->
            grforms_insert_text(Form, GRForms);
	_ ->
            grforms_insert_default(Form, GRForms)
    end.

grforms_insert_attribute(file, File, {ERForms, FRForms, ARForms, []}) ->
    {ERForms, FRForms, ARForms, [File]};
grforms_insert_attribute(file, File, GRForms) ->
    grforms_append(File, GRForms);
grforms_insert_attribute(module, Module, {ERForms, FRForms, ARForms, [{attribute, _Pos2, module, _ModuleName1}|MForms]}) ->
    {ERForms, FRForms, ARForms, [Module|MForms]};
grforms_insert_attribute(module, Module, {ERForms, FRForms, ARForms, MForms}) ->
    {ERForms, FRForms, ARForms, [Module|MForms]};
grforms_insert_attribute(export, {attribute, Pos, export, Exports}, {ERForms, FRForms, ARForms, MRForms}) ->
    Exports1 = remove_duplicated_exports(Exports, FRForms),
    Exports2 = remove_duplicated_exports(Exports1, ARForms),
    case Exports2 of
        [] ->
            {ERForms, FRForms, ARForms, MRForms};
        _ ->
            Export = {attribute, Pos, export, Exports2},
            {ERForms, FRForms, [Export|ARForms], MRForms}
    end;
grforms_insert_attribute(spec, Spec, {ERForms, FRForms, ARForms, MRForms}) ->
    {ERForms, [Spec|FRForms], ARForms, MRForms};
grforms_insert_attribute(_Name, Attribute, {ERForms, FRForms, ARForms, MRForms}) ->
    {ERForms, FRForms, [Attribute|ARForms], MRForms}.

grforms_insert_comment(Comment, GRForms) ->
    grforms_append(Comment, GRForms).

grforms_insert_function(Function, {ERForms, FRForms, ARForms, MRForms}) ->
    {ERForms, [Function|FRForms], ARForms, MRForms}.

grforms_insert_eof(_Eof1, {[Eof], FRForms, ARForms, MRForms}) ->
    {[Eof], FRForms, ARForms, MRForms};
grforms_insert_eof(Eof, {[], FRForms, ARForms, MRForms}) ->
    {[Eof], FRForms, ARForms, MRForms}.

grforms_insert_error_marker( Error, GRForms) ->
    grforms_append(Error, GRForms).

grforms_insert_warning_marker(Warning, GRForms) ->
    grforms_append(Warning, GRForms).

grforms_insert_form_list(Form, GRForms) ->
    lists:foldl(fun grforms_insert/2, GRForms, erl_syntax:form_list_elements(erl_syntax:flatten_form_list(Form))).

grforms_insert_text(Text, GRForms) ->
    grforms_append(Text, GRForms).

grforms_insert_default(Form, GRForms) ->
    grforms_append(Form, GRForms).

remove_duplicated_exports(Exports1, [{attribute, _Pos, export, Exports}|T]) ->
    Exports2 = Exports1 -- Exports,
    remove_duplicated_exports(Exports2, T);
remove_duplicated_exports(Exports, [_Form|T]) ->
    remove_duplicated_exports(Exports, T);
remove_duplicated_exports([], _Forms) ->
    [];
remove_duplicated_exports(Exports, []) ->
    Exports.

grforms_with_functions(Fun, {ERForms, FRForms, ARForms, MRForms}) ->
    FRForms1 = Fun(FRForms),
    {ERForms, FRForms1, ARForms, MRForms}.

-spec sort_forms([erl_parse:abstract_form()]) -> [erl_parse:abstract_form()].
%% @doc sort forms to valid order, same as insert_forms(Forms, []).
%% @see insert_forms/2.
sort_forms(Forms) ->
    insert_forms(Forms, []).

-spec insert_forms([erl_parse:abstract_form()], [erl_parse:abstract_form()]) -> [erl_parse:abstract_form()].
%% @doc insert new forms to froms with order fillow these rules
%% <ul>
%% <li>rename functions in forms which function has '__original__' call in it with same name and arity in new forms.</li>
%% <li>'__original__'(Args1, Args2, ...) will be transformed to RenamedFunction(Args1, Args2, ...).</li>
%% <li>after rename, it dose not matter function or spec with duplicated name and arity, lint will get these errors.</li>
%% <li>attribute in new forms will insert before first spec or function or eof.</li>
%% <li>spec in new forms will insert before function with same name and arity or eof.</li>
%% <li>function in new forms will insert after spec with same name and arity or insert before eof.</li>
%% <li>eof_marker in new forms will be dropped if there is an eof_marker already exists in forms.</li>
%% <li>eof_marker in new forms will insert at the end of forms if there is no eof_market in forms.</li>
%% <li>if form is marked from other file (between -file(file1) and -file(file2)), do not change this mark.(not implemented)</li>
%% <li>insert_forms does not changes the original forms order, but with some rules check.</li>
%% <li>eof_marker should be the last element of original forms</li>
%% <li>except file attribute, module should be the first element of original forms</li>
%% <li>an error {insert_form_failed, Form, GRForms} is throwed when original forms check failed</li>
%% </ul>
%% @end
insert_forms(NewForms, Forms) ->
    Functions = forms_functions(Forms),
    NewFormsFunctions = forms_functions(NewForms),
    GRForms = lists:foldl(fun grforms_append/2, grforms_new(), Forms),
    {_Functions, GRForms1, []} = insert_forms(NewForms, NewFormsFunctions, Functions, GRForms, []),
    grforms_to_forms(GRForms1).

%% merge forms rule
%% 1. rename functions which generated functions has '__original__' call in it with same name and arity.
%% 2. rename spec if new spec is generated
%% 3. for map_forms/2, code does not know how forms will change in tails
%$ 4. if it's need to adjust new generated form order, only forms in heads should be affeted
%% 5. new generated attribute should insert before first -spec in heads
%% 6. new generated spec should insert before function with same name and arity in heads
%% 7. new generated function should insert after spec with same name and arity in heads.
%% 8. if file of new generated form is different from oldone, file attribute should be created to mark file.
%% new_forms, heads, tails is original order.
%% after merge forms, Heads is reversed order, tails is original order.
insert_forms(NewForms, NewFormsFucntions, Functions, GRForms, Tails) ->
    {Functions1, NewForms1, GRForms1, Tails1} = merge_functions(NewForms, NewFormsFucntions, Functions, GRForms, Tails),
    GRForms2 = lists:foldl(fun grforms_insert/2, GRForms1, NewForms1),
    {Functions1, GRForms2, Tails1}.

%% =====================================================================
%% merge functions
%% =====================================================================
merge_functions(NewForms, NewFormsFucntions, Functions, GRForms, Tails) ->
    ExistsNewFunctions =
        ordsets:from_list(
          lists:filter(
            fun(NameArity) ->
                    ordsets:is_element(NameArity, Functions)
            end, ordsets:to_list(NewFormsFucntions))),
    Functions1 = ordsets:union(Functions, NewFormsFucntions),
    {Functions2, NewFormsR2, GRForms1, Tails1} =
        lists:foldl(
          fun({function, _Pos, Name, Arity, _Clauses} = Form,
              {FunctionsAcc, NewFormsAcc, GRFormsAcc, TailsAcc}) ->
                  case ordsets:is_element({Name, Arity}, ExistsNewFunctions) andalso is_renamed(Arity, Form) of
                      true ->
                          NewName = new_function_name(Name, Arity, FunctionsAcc),
                          Form1 = update_call_name('__original__', NewName, Arity, Form),
                          GRFormsAcc1 =
                              grforms_with_functions(
                                fun(FRForms) ->
                                        update_function_name(Name, Arity, NewName, FRForms)
                                end, GRFormsAcc),
                          TailsAcc1 = update_function_name(Name, Arity, NewName, TailsAcc),
                          {ordsets:add_element({NewName, Arity}, FunctionsAcc), [Form1|NewFormsAcc], GRFormsAcc1, TailsAcc1};
                      false ->
                          {FunctionsAcc, [Form|NewFormsAcc], GRFormsAcc, TailsAcc}
                  end;
             (Form, {FunctionsAcc, NewFormsAcc, HeadsAcc, TailsAcc}) ->
                  {FunctionsAcc, [Form|NewFormsAcc], HeadsAcc, TailsAcc}
          end, {Functions1, [], GRForms, Tails}, NewForms),
    {Functions2, lists:reverse(NewFormsR2), GRForms1, Tails1}.

-spec is_renamed(integer(), erl_parse:abstract_form()) -> boolean().
is_renamed(Arity, Form) ->
    Either =
        astranaut_uniplate:map_m_static(
          fun({call, _Pos1, {atom, _Pos2, '__original__'}, Arguments} = Node) ->
                  case length(Arguments) == Arity of
                      true ->
                          {left, renamed};
                      false ->
                          {right, Node}
                  end;
             (Node) ->
                  {right, Node}
          end, Form, fun astranaut:uniplate/1, either, #{traverse => pre}),
    case Either of
        {left, renamed} ->
            true;
        {right, _Form} ->
            false
    end.
  
new_function_name(FName, Arity, Functions) ->
    new_function_name(FName, Arity, Functions, 1).

new_function_name(FName, Arity, Functions, Counter) ->
    FName1 = list_to_atom(atom_to_list(FName) ++ "_" ++ integer_to_list(Counter)),
    case ordsets:is_element({FName1, Arity}, Functions) of
        true ->
            new_function_name(FName, Arity, Functions, Counter + 1);
        false ->
            FName1
    end.

update_function_name(Name, Arity, NewName, Forms) ->
    lists:map(
      fun({function, Pos, FName, FArity, Clauses})
            when (FName == Name) andalso (FArity == Arity) ->
              Clauses1 =
                  lists:map(
                    fun(Clause) ->
                            update_call_name(Name, NewName, Arity, Clause)
                    end, Clauses),
              {function, Pos, NewName, Arity, Clauses1};
         (Form) ->
              Form
      end, Forms).

update_call_name(OrignalName, NewName, Arity, Function) ->
    astranaut:smap(
      fun({call, Pos, {atom, Pos2, Name}, Arguments})
            when (Name == OrignalName) andalso (length(Arguments) == Arity) ->
              {call, Pos, {atom, Pos2, NewName}, Arguments};
         (Node) ->
              Node
      end, Function, #{traverse => pre}).
