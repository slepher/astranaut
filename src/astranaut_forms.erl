%%%-------------------------------------------------------------------
%%% @doc Form ordering, insertion, and generated-function merging.
%%%-------------------------------------------------------------------
-module(astranaut_forms).

-type form() :: astranaut:form().
-type updated_form() :: form() | {updated, form(), [form()]}.
-type function_id() :: {atom(), non_neg_integer()}.

-record(grouped_forms,
        {eof = [] :: [form()],
         functions = [] :: [form()],
         attributes = [] :: [form()],
         module = [] :: [form()]}).

-export([reorder_updated_forms/1, sort_forms/1, insert_forms/2]).

-spec reorder_updated_forms([updated_form()]) -> [form()].
reorder_updated_forms(Forms) ->
    Functions = function_ids(Forms),
    reorder_updated_forms(Forms, Functions, #grouped_forms{}).

reorder_updated_forms([{updated, Form, NewForms}|Tails], Functions0, Groups) ->
    FormFunctions = function_ids([Form]),
    NewFormsFunctions = function_ids(NewForms),
    AddedFunctions = ordsets:subtract(NewFormsFunctions, FormFunctions),
    {Functions1, Groups1, Tails1} =
        insert_forms(NewForms, AddedFunctions, Functions0, Groups, Tails),
    reorder_updated_forms(Tails1, Functions1, Groups1);
reorder_updated_forms([Form|Tails], Functions, Groups) ->
    reorder_updated_forms(Tails, Functions, append_form(Form, Groups));
reorder_updated_forms([], _Functions, Groups) ->
    groups_to_forms(Groups).

-spec function_ids([updated_form()]) -> ordsets:ordset(function_id()).
function_ids(Forms) ->
    function_ids(Forms, ordsets:new()).

function_ids(Forms, Functions0) ->
    lists:foldl(
      fun({function, _Pos, Name, Arity, _Clauses}, Functions) ->
              ordsets:add_element({Name, Arity}, Functions);
         (_Form, Functions) ->
              Functions
      end, Functions0, Forms).

groups_to_forms(#grouped_forms{eof = EofForms,
                               functions = FunctionForms,
                               attributes = AttributeForms,
                               module = ModuleForms}) ->
    lists:reverse(ModuleForms) ++
        lists:reverse(AttributeForms) ++
        lists:reverse(FunctionForms) ++
        EofForms.

append_form({attribute, _Pos, module, _Name} = Module,
            #grouped_forms{eof = [], functions = [], attributes = [],
                           module = ModuleForms} = Groups) ->
    Groups#grouped_forms{module = [Module|ModuleForms]};
append_form({attribute, _Pos, file, _Value} = File,
            #grouped_forms{eof = [], functions = [], attributes = [],
                           module = ModuleForms} = Groups) ->
    Groups#grouped_forms{module = [File|ModuleForms]};
append_form({attribute, _Pos, file, _Value} = File,
            #grouped_forms{eof = [], functions = [],
                           attributes = AttributeForms} = Groups) ->
    Groups#grouped_forms{attributes = [File|AttributeForms]};
append_form({attribute, _Pos, file, _Value} = File,
            #grouped_forms{eof = [], functions = FunctionForms} = Groups) ->
    Groups#grouped_forms{functions = [File|FunctionForms]};
append_form({attribute, _Pos, export, _Exports} = Export,
            #grouped_forms{eof = [], attributes = AttributeForms} = Groups) ->
    Groups#grouped_forms{attributes = [Export|AttributeForms]};
append_form({attribute, _Pos, export_type, _Exports} = ExportType,
            #grouped_forms{eof = [], attributes = AttributeForms} = Groups) ->
    Groups#grouped_forms{attributes = [ExportType|AttributeForms]};
append_form({attribute, _Pos, spec, _Value} = Spec,
            #grouped_forms{functions = FunctionForms} = Groups) ->
    Groups#grouped_forms{functions = [Spec|FunctionForms]};
append_form({function, _Pos, _Name, _Arity, _Clauses} = Function,
            #grouped_forms{functions = FunctionForms} = Groups) ->
    Groups#grouped_forms{functions = [Function|FunctionForms]};
append_form({eof, _Pos} = Eof, #grouped_forms{eof = []} = Groups) ->
    Groups#grouped_forms{eof = [Eof]};
append_form(Form,
            #grouped_forms{eof = [], functions = [],
                           attributes = AttributeForms} = Groups) ->
    Groups#grouped_forms{attributes = [Form|AttributeForms]};
append_form(Form,
            #grouped_forms{eof = [], functions = FunctionForms} = Groups) ->
    Groups#grouped_forms{functions = [Form|FunctionForms]};
append_form(Form, Groups) ->
    erlang:exit({insert_form_failed, Form, legacy_groups(Groups)}).

legacy_groups(#grouped_forms{eof = EofForms,
                             functions = FunctionForms,
                             attributes = AttributeForms,
                             module = ModuleForms}) ->
    {EofForms, FunctionForms, AttributeForms, ModuleForms}.

insert_form(Form, Groups) ->
    case erl_syntax:type(Form) of
        attribute ->
            Name = erl_syntax:concrete(erl_syntax:attribute_name(Form)),
            insert_attribute(Name, Form, Groups);
        function ->
            add_function(Form, Groups);
        eof_marker ->
            insert_eof(Form, Groups);
        form_list ->
            insert_form_list(Form, Groups);
        _ ->
            append_form(Form, Groups)
    end.

insert_attribute(file, File, #grouped_forms{module = []} = Groups) ->
    Groups#grouped_forms{module = [File]};
insert_attribute(file, File, Groups) ->
    append_form(File, Groups);
insert_attribute(module, Module,
                 #grouped_forms{module =
                                    [{attribute, _Pos, module, _Name}|ModuleForms]} = Groups) ->
    Groups#grouped_forms{module = [Module|ModuleForms]};
insert_attribute(module, Module,
                 #grouped_forms{module = ModuleForms} = Groups) ->
    Groups#grouped_forms{module = [Module|ModuleForms]};
insert_attribute(export, {attribute, Pos, export, Exports},
                 #grouped_forms{functions = FunctionForms,
                                attributes = AttributeForms} = Groups) ->
    UniqueExports = remove_duplicated_exports(
                      remove_duplicated_exports(Exports, FunctionForms),
                      AttributeForms),
    case UniqueExports of
        [] ->
            Groups;
        _ ->
            Export = {attribute, Pos, export, UniqueExports},
            Groups#grouped_forms{attributes = [Export|AttributeForms]}
    end;
insert_attribute(spec, Spec,
                 #grouped_forms{functions = FunctionForms} = Groups) ->
    Groups#grouped_forms{functions = [Spec|FunctionForms]};
insert_attribute(_Name, Attribute,
                 #grouped_forms{attributes = AttributeForms} = Groups) ->
    Groups#grouped_forms{attributes = [Attribute|AttributeForms]}.

add_function(Function,
             #grouped_forms{functions = FunctionForms} = Groups) ->
    Groups#grouped_forms{functions = [Function|FunctionForms]}.

insert_eof(_Eof, #grouped_forms{eof = [_ExistingEof]} = Groups) ->
    Groups;
insert_eof(Eof, #grouped_forms{eof = []} = Groups) ->
    Groups#grouped_forms{eof = [Eof]}.

insert_form_list(Form, Groups) ->
    Elements = erl_syntax:form_list_elements(erl_syntax:flatten_form_list(Form)),
    lists:foldl(fun insert_form/2, Groups, Elements).

remove_duplicated_exports(Exports1, [{attribute, _Pos, export, Exports}|Forms]) ->
    remove_duplicated_exports(Exports1 -- Exports, Forms);
remove_duplicated_exports(Exports, [_Form|Forms]) ->
    remove_duplicated_exports(Exports, Forms);
remove_duplicated_exports([], _Forms) ->
    [];
remove_duplicated_exports(Exports, []) ->
    Exports.

map_functions(Fun,
              #grouped_forms{functions = FunctionForms} = Groups) ->
    Groups#grouped_forms{functions = Fun(FunctionForms)}.

%% @doc Sort forms into the same valid order used by insert_forms/2.
-spec sort_forms([form()]) -> [form()].
sort_forms(Forms) ->
    insert_forms(Forms, []).

%% @doc Insert generated forms while preserving form order and __original__ semantics.
-spec insert_forms([form()], [form()]) -> [form()].
insert_forms(NewForms, Forms) ->
    Functions = function_ids(Forms),
    NewFunctions = function_ids(NewForms),
    Groups = lists:foldl(fun append_form/2, #grouped_forms{}, Forms),
    {_Functions, Groups1, []} =
        insert_forms(NewForms, NewFunctions, Functions, Groups, []),
    groups_to_forms(Groups1).

insert_forms(NewForms, NewFunctions, Functions, Groups, Tails) ->
    {Functions1, NewForms1, Groups1, Tails1} =
        merge_functions(NewForms, NewFunctions, Functions, Groups, Tails),
    Groups2 = lists:foldl(fun insert_form/2, Groups1, NewForms1),
    {Functions1, Groups2, Tails1}.

merge_functions(NewForms, NewFunctions, Functions, Groups, Tails) ->
    ExistingNewFunctions = ordsets:intersection(NewFunctions, Functions),
    Functions1 = ordsets:union(Functions, NewFunctions),
    {Functions2, NewFormsReversed, Groups1, Tails1} =
        lists:foldl(
          fun({function, _Pos, Name, Arity, _Clauses} = Form,
              {FunctionsAcc, NewFormsAcc, GroupsAcc, TailsAcc}) ->
                  case ordsets:is_element({Name, Arity}, ExistingNewFunctions)
                       andalso is_renamed(Arity, Form) of
                      true ->
                          NewName = new_function_name(Name, Arity, FunctionsAcc),
                          Form1 = update_call_name('__original__', NewName, Arity, Form),
                          GroupsAcc1 =
                              map_functions(
                                fun(FunctionForms) ->
                                        update_function_name(Name, Arity, NewName,
                                                             FunctionForms)
                                end, GroupsAcc),
                          TailsAcc1 = update_function_name(Name, Arity, NewName, TailsAcc),
                          {ordsets:add_element({NewName, Arity}, FunctionsAcc),
                           [Form1|NewFormsAcc], GroupsAcc1, TailsAcc1};
                      false ->
                          {FunctionsAcc, [Form|NewFormsAcc], GroupsAcc, TailsAcc}
                  end;
             (Form, {FunctionsAcc, NewFormsAcc, GroupsAcc, TailsAcc}) ->
                  {FunctionsAcc, [Form|NewFormsAcc], GroupsAcc, TailsAcc}
          end, {Functions1, [], Groups, Tails}, NewForms),
    {Functions2, lists:reverse(NewFormsReversed), Groups1, Tails1}.

is_renamed(Arity, Form) ->
    astranaut:search(
      fun({call, _Pos1, {atom, _Pos2, '__original__'}, Arguments}) ->
              length(Arguments) =:= Arity;
         (_Node) ->
              false
      end, Form, #{traverse => pre}).

new_function_name(Name, Arity, Functions) ->
    new_function_name(Name, Arity, Functions, 1).

new_function_name(Name, Arity, Functions, Counter) ->
    NewName = list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Counter)),
    case ordsets:is_element({NewName, Arity}, Functions) of
        true -> new_function_name(Name, Arity, Functions, Counter + 1);
        false -> NewName
    end.

update_function_name(Name, Arity, NewName, Forms) ->
    lists:map(
      fun({function, Pos, FunctionName, FunctionArity, Clauses})
            when FunctionName =:= Name, FunctionArity =:= Arity ->
              Clauses1 = update_call_name(Name, NewName, Arity, Clauses),
              {function, Pos, NewName, Arity, Clauses1};
         (Form) ->
              Form
      end, Forms).

update_call_name(OriginalName, NewName, Arity, Tree) ->
    astranaut:smap(
      fun({call, Pos, {atom, NamePos, Name}, Arguments})
            when Name =:= OriginalName, length(Arguments) =:= Arity ->
              {call, Pos, {atom, NamePos, NewName}, Arguments};
         (Node) ->
              Node
      end, Tree, #{traverse => pre}).
