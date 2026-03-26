%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 15 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_struct_transformer).

-include("quote.hrl").

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    Module = astranaut_lib:analyze_forms_module(Forms),
    Return =
        astranaut_return:bind(
          init_struct_defs(Forms, init_records(Module, Forms)),
          fun({Forms1, StructInitMap}) ->
                  Forms2 = ensure_import_macro(Forms1),
                  astranaut_return:bind(
                    astranaut:map(
                      fun(Node, Attrs) ->
                              walk(Node, StructInitMap, Attrs)
                      end, Forms2, #{traverse => pre, formatter => ?MODULE}),
                    fun(Forms3) ->
                            remove_used_struct_records(Forms, Forms3)
                    end)
          end),
    astranaut_return:to_compiler(Return).

format_error({undefined_record, Record}) ->
    io_lib:format("record ~p in is not defined", [Record]);
format_error({invalid_struct_name, Struct}) ->
    io_lib:format("~p is not a valid struct name", [Struct]);
format_error({enforce_keys_not_in_struct, RecordName, Keys}) ->
    io_lib:format("the enforce keys must be defined in record ~p: ~p", [RecordName, Keys]);
format_error({missing_enforce_keys, RecordName, Keys}) ->
    io_lib:format("the following keys must also be given when building struct ~p: ~p", [RecordName, Keys]);
format_error({undefined_record_field, RecordName, FieldName}) ->
    io_lib:format("field ~p undefined in record ~p", [FieldName, RecordName]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _ -> io_lib:write(Message)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Record abstract format is transformed to Map abstract format when
%% RecordName is mentioned in -astranaut_struct([RecordName...]).
walk({call, _Line1,
      {remote, _Line2, {atom, _Line2a, astranaut_struct}, {atom, _Line3, record}, [Node]}},
     _StructInitMap, #{}) ->
    astranaut:walk_return(#{return => Node, continue => true});

%% transform record creation like #RecordName{Field1 = Value1, Field2 = Value2...}
%% in pattern and expression.
walk({record, Line, Name, Fields} = Node, StructInitMap, #{node := NodeType}) ->
    update_record(Name, Line, Fields, StructInitMap, #{type => NodeType, node => Node});

%% transform record update like Record#RecordName{Field1 = Value1, Field2 = Value2...}
walk({record, Line, Update, Name, Fields} = Node, StructInitMap, #{node := NodeType}) ->
    update_record(Name, Line, Fields, StructInitMap, #{type => NodeType, update => Update, node => Node});

%% transform record index like #RecordName.Field -> Field.
walk({record_index, Line, RecordName, {atom, _Line2, FieldName}} = Node, StructInitMap, _Attr) ->
    case maps:find(RecordName, StructInitMap) of
        {ok, StructDef} ->
            Fields = astranaut_struct_record:fields(StructDef),
            case lists:member(FieldName, Fields) of
                true ->
                    {atom, Line, FieldName};
                false ->
                    {error, {undefined_record_field, RecordName, FieldName}}
            end;
        error ->
            Node
    end;

%% transform record field access.
walk({record_field, Line, Struct, RecordName, {atom, _Line2, FieldName} = Field} = Node, StructInitMap, _Attr) ->
    case maps:find(RecordName, StructInitMap) of
        {ok, StructDef} ->
            StructDefFields = astranaut_struct_record:fields(StructDef),
            FieldInitValues = astranaut_struct_record:init_values(StructDef),
            EnforceKeys = astranaut_struct_record:enforce_keys(StructDef),
            case lists:member(FieldName, StructDefFields) of
                true ->
                    case lists:member(FieldName, EnforceKeys) of
                        true ->
                            quote(maps:get(unquote(Field), unquote(Struct)));
                        false ->
                            FieldInit = maps:get(FieldName, FieldInitValues, astranaut_lib:abstract_form(undefined)),
                            Init = astranaut_lib:replace_pos(FieldInit, Line),
                            quote(maps:get(unquote(Field), unquote(Struct), unquote(Init)))
                    end;
                false ->
                    {error, {undefined_record_field, RecordName, FieldName}}
            end;
        error ->
            Node
    end;

%% transform record type like #Record{Field1 :: FieldType1 ...}.
walk({type, Line, record, [{atom, _Line, RecordName}|Fields]} = Node, StructInitMap, _Attr) ->
    case maps:find(RecordName, StructInitMap) of
        {ok, StructDef} ->
            case record_field_types(RecordName, Line, Fields, StructDef) of
                {ok, FieldTypes} ->
                    {type, Line, map, FieldTypes};
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            Node
    end;

%% record type with astranaut_struct:record(#Record{}) will not be transformed.
walk({remote_type, _Line1, [{atom, _Line2, astranaut_struct}, {atom, _Line3, record}, [TypeNode]]},
     _StructInitMap, _Attr) ->
    astranaut:walk_return(#{return => TypeNode, continue => true});

walk(Node, _StructInitMap, Attr) ->
    Type = erl_syntax:type(Node),
    astranaut_uniplate:with_subtrees(
      fun(Subtrees) ->
              astranaut_syntax:subtrees_pge(Type, Subtrees, Attr)
      end, Node).

record_opts(#{update := Update}) ->
    #{update => Update, append_struct => false, append_init => false, field_type => map_field_assoc};
record_opts(#{type := pattern}) ->
    #{append_struct => true, append_init => false, field_type => map_field_exact};
record_opts(#{}) ->
    #{append_struct => true, append_init => true, field_type => map_field_assoc}.

update_record(Name, Line, RecordFields, RecordInitMap, #{node := Node} = Opts) ->
    RecordOpts = record_opts(Opts),
    case maps:find(Name, RecordInitMap) of
        {ok, StructDef} ->
            case update_record_fields(Name, Line, RecordFields, StructDef, RecordOpts) of
                {ok, StructFields} ->
                    update_record_main(Line, StructFields, RecordOpts);
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            Node
    end.

update_record_main(Line, Fields, #{update := Update}) ->
    {map, Line, Update, Fields};
update_record_main(Line, Fields, #{}) ->
    {map, Line, Fields}.

update_record_fields(RecordName, Line, Fields, StructDef, #{field_type := MapFieldType} = Opts) ->
    StructDefFields = astranaut_struct_record:fields(StructDef),
    case traverse_fields(
           fun(FieldLine, FieldName, FieldValue) ->
                   {MapFieldType, FieldLine, FieldName, FieldValue}
           end, RecordName, Fields, StructDefFields) of
        {ok, FieldNodes, FieldNames} ->
            case append_init(RecordName, FieldNames, StructDef, Line, Opts) of
                {ok, InitFields} ->
                    StructFieldNodes = append_struct_name(RecordName, Line, Opts),
                    {ok, StructFieldNodes ++ FieldNodes ++ InitFields};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

append_init(RecordName, FieldNames, StructDef, Line, #{append_init := true, field_type := MapFieldType}) ->
    EnforceKeys = astranaut_struct_record:enforce_keys(StructDef),
    MissingKeys = EnforceKeys -- FieldNames,
    case MissingKeys of
        [] ->
            FieldInitValues = astranaut_struct_record:init_values(StructDef),
            FieldInitValues1 = maps:without(FieldNames, FieldInitValues),
            Fields =
                lists:map(
                  fun({FieldName, FieldInit}) ->
                          {MapFieldType, Line, {atom, Line, FieldName}, astranaut_lib:replace_pos(FieldInit, Line)}
                  end, maps:to_list(FieldInitValues1)),
            {ok, Fields};
        _ ->
            {error, {missing_enforce_keys, RecordName, MissingKeys}}
    end;
append_init(_RecordName, _FieldNames, _StructDef, _Line, _Opts) ->
    {ok, []}.

append_struct_name(RecordName, Line, #{append_struct := true, field_type := MapFieldType}) ->
    [{MapFieldType, Line, {atom, Line, '__struct__'}, {atom, Line, RecordName}}];
append_struct_name(_RecordName, _Line, #{}) ->
    [].

record_field_types(RecordName, Line, Fields, StructDef) ->
    StructDefFields = astranaut_struct_record:fields(StructDef),
    case traverse_fields(
           fun(FieldLine, FieldName, FieldType) ->
                   {atom, _FieldNamePos, FieldNameAtom} = FieldName,
                   IsMandatory = astranaut_struct_record:mandatory_field(FieldNameAtom, StructDef),
                   AssocType = struct_field_assoc_type(IsMandatory),
                   struct_field_type(AssocType, FieldLine, FieldName, FieldType)
           end, RecordName, Fields, StructDefFields) of
        {ok, FieldTypeNodes, FieldNames} ->
            case append_init_types(FieldNames, StructDef, Line) of
                {ok, InitTypeNodes} ->
                    {ok, [{type, Line, map_field_exact, struct_name_field(RecordName, Line)}|FieldTypeNodes] ++ InitTypeNodes};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

append_init_types(FieldNames, StructDef, Line) ->
    StructFields = astranaut_struct_record:fields(StructDef),
    FieldInitTypes = maps:without(FieldNames, astranaut_struct_record:types(StructDef)),
    Fields =
        lists:reverse(
          lists:foldl(
            fun(FieldName, Acc) ->
                    case maps:find(FieldName, FieldInitTypes) of
                        {ok, FieldType} ->
                            IsMandatory = astranaut_struct_record:mandatory_field(FieldName, StructDef),
                            AssocType = struct_field_assoc_type(IsMandatory),
                            AtomFieldName = {atom, Line, FieldName},
                            FieldType1 = astranaut_lib:replace_pos(FieldType, Line),
                            [struct_field_type(AssocType, Line, AtomFieldName, FieldType1)|Acc];
                        error ->
                            Acc
                    end
            end, [], StructFields)),
    {ok, Fields}.

struct_name_field(StructName, Line) ->
    [{atom, Line, '__struct__'}, {atom, Line, StructName}].

struct_field_assoc_type(true) ->
    map_field_exact;
struct_field_assoc_type(false) ->
    map_field_assoc.

struct_field_type(Type, Line, FieldName, FieldType) ->
    {type, Line, Type, [FieldName, FieldType]}.

traverse_fields(Fun, RecordName, Fields, StructFields) ->
    traverse_fields(Fun, RecordName, Fields, StructFields, [], []).

traverse_fields(_Fun, _RecordName, [], _StructFields, AccNodes, AccNames) ->
    {ok, lists:reverse(AccNodes), lists:reverse(AccNames)};
traverse_fields(Fun, RecordName,
                [{record_field, Line, {atom, _Line2, FieldName} = FieldNameAtom, FieldValue}|T],
                StructFields, AccNodes, AccNames) ->
    apply_field(Fun, RecordName, FieldName, Line, FieldNameAtom, FieldValue, StructFields, T, AccNodes, AccNames);
traverse_fields(Fun, RecordName,
                [{type, Line, field_type, [{atom, _Line2, FieldName} = FieldNameAtom, FieldType]}|T],
                StructFields, AccNodes, AccNames) ->
    apply_field(Fun, RecordName, FieldName, Line, FieldNameAtom, FieldType, StructFields, T, AccNodes, AccNames);
traverse_fields(Fun, RecordName, [_Other|T], StructFields, AccNodes, AccNames) ->
    traverse_fields(Fun, RecordName, T, StructFields, AccNodes, AccNames).

apply_field(Fun, RecordName, FieldName, Line, FieldNameAtom, FieldValue, StructFields, T, AccNodes, AccNames) ->
    case lists:member(FieldName, StructFields) of
        true ->
            Node = Fun(Line, FieldNameAtom, FieldValue),
            traverse_fields(Fun, RecordName, T, StructFields, [Node|AccNodes], [FieldName|AccNames]);
        false ->
            {error, {undefined_record_field, RecordName, FieldName}}
    end.

%%%===================================================================
%%% Initialize Structs with attribute astranaut_struct.
%%%===================================================================
init_struct_defs(Forms, RecordDefMap) ->
    TraverseOpts = #{formatter => ?MODULE, simplify_return => false, deep_attr => true},
    Validator = #{non_auto_fill => boolean,
                  auto_fill => boolean,
                  enforce_keys => {list_of, atom}},
    astranaut_lib:forms_with_attribute(
      fun({Struct, Opts}, Acc, Attr) ->
              astranaut_return:bind(
                astranaut_lib:validate(Validator, Opts),
                fun(Opts1) ->
                        add_struct(Struct, Opts1, RecordDefMap, Acc, Attr)
                end);
         (Struct, Acc, Attr) ->
              add_struct(Struct, #{}, RecordDefMap, Acc, Attr)
      end, maps:new(), Forms, astranaut_struct, TraverseOpts).

add_struct(Struct, Opts, RecordMap, StructDefs, #{pos := Line}) ->
    case new_struct(Struct, Opts, RecordMap) of
        {ok, NewStruct} ->
            Node = astranaut_lib:gen_attribute_node(astranaut_struct_def, Line, NewStruct),
            astranaut_return:return({[Node], maps:put(Struct, NewStruct, StructDefs)});
        {errors, Reasons} ->
            astranaut_return:errors_ok(Reasons, {[], StructDefs});
        {error, Reason} ->
            astranaut_return:error_ok(Reason, {[], StructDefs})
    end.

new_struct(Struct, Opts, RecordMap) when is_atom(Struct) ->
    case maps:find(Struct, RecordMap) of
        {ok, RecordDef} ->
            RecordDef1 = update_record_def(RecordDef, Opts),
            FieldErrors = astranaut_struct_record:warnings(RecordDef1),
            case FieldErrors of
                [] ->
                    {ok, RecordDef1};
                _ ->
                    {errors, FieldErrors}
            end;
        error ->
            {error, {undefined_record, Struct}}
    end;
new_struct(Struct, _Opts, _RecordMap) ->
    {error, {invalid_struct_name, Struct}}.

init_records(Module, Forms) ->
    lists:foldl(
      fun({attribute, Line, record, {RecordName, _RecordFields} = Record}, RecordMapAcc) ->
              RecordDef = astranaut_struct_record:record_def(Module, Record, Line),
              maps:put(RecordName, RecordDef, RecordMapAcc);
         (_Node, RecordMapAcc) ->
              RecordMapAcc
      end, maps:new(), Forms).

update_record_def(RecordDef, #{} = StructDef) ->
    AutoFill = get_boolean(auto_fill, non_auto_fill, StructDef, true),
    EnforceKeys = maps:get(enforce_keys, StructDef, []),
    RecordDef1 = astranaut_struct_record:set_auto_fill(AutoFill, RecordDef),
    RecordDef2 = astranaut_struct_record:update_enforce_keys(EnforceKeys, RecordDef1),
    astranaut_struct_record:update_init_values(RecordDef2).

get_boolean(Key, ReverseKey, Opts, Default) ->
    case maps:find(Key, Opts) of
        {ok, Value} ->
            Value;
        error ->
            case maps:find(ReverseKey, Opts) of
                {ok, Value1} ->
                    not Value1;
                error ->
                    Default
            end
    end.

remove_used_structs(Forms, UsedStructs) ->
    lists:filter(
      fun({attribute, _Line, record, {RecordName, _Fields}}) ->
              not ordsets:is_element(RecordName, UsedStructs);
         (_Node) ->
              true
      end, Forms).

remove_used_struct_records(Forms0, Forms1) ->
    UsedRecords0 = used_records(Forms0),
    UsedRecords1 = used_records(Forms1),
    Forms = remove_used_structs(Forms1, ordsets:subtract(UsedRecords0, UsedRecords1)),
    astranaut_return:return(Forms).

ensure_import_macro(Forms) ->
    case has_import_macro(astranaut_struct, Forms) of
        true ->
            Forms;
        false ->
            Import = astranaut_lib:gen_attribute_node(import_macro, 0, astranaut_struct),
            astranaut_syntax:insert_forms([Import], Forms)
    end.

has_import_macro(Module, Forms) ->
    lists:any(
      fun({attribute, _Line, import_macro, ImportedModule}) when ImportedModule =:= Module ->
              true;
         ({attribute, _Line, import_macro, {ImportedModule, _Spec}}) when ImportedModule =:= Module ->
              true;
         (_Node) ->
              false
      end, Forms).

used_records(Forms) ->
    astranaut:sreduce(
      fun(Node, Acc) ->
              case walk_record_name(Node) of
                  {ok, Name} ->
                      ordsets:add_element(Name, Acc);
                  error ->
                      Acc
              end
      end, ordsets:new(), Forms, #{traverse => pre}).

walk_record_name({call, _Line1, {atom, _Line2, record_info}, [{atom, _Line3, fields}, {atom, _Line4, Name}]}) ->
    {ok, Name};
walk_record_name({record, _Line, Name, _Fields}) ->
    {ok, Name};
walk_record_name({record, _Line, _Update, Name, _Fields}) ->
    {ok, Name};
walk_record_name({record_index, _Line, Name, _Field}) ->
    {ok, Name};
walk_record_name({record_field, _Line, _Record, Name, _Field}) ->
    {ok, Name};
walk_record_name({type, _Line1, record, [{atom, _Line2, Name}|_Fields]}) ->
    {ok, Name};
walk_record_name(_Node) ->
    error.
