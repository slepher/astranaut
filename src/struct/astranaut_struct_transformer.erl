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
-include("rebinding.hrl").
-include("astranaut_do.hrl").

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    Module = astranaut:module(Forms),
    ReturnM =
        do([astranaut_return_m ||
               Forms <- astranaut_traverse:without_errors(Forms),
               Forms0 = Forms,
               RecordDefMap = init_records(Module, Forms),
               {Forms, StructInitMap} <-
                   init_struct_defs(Forms, RecordDefMap),
               Forms <- astranaut_traverse:map(
                 fun(Node, Attrs) ->
                         walk(Node, StructInitMap, Attrs)
                 end, Forms, #{traverse => pre, formatter => ?MODULE, simplify_return => false}),
               Forms <- transform_struct_macros(Forms),
               remove_used_struct_records(Forms0, Forms)
           ]),
    astranaut_return_m:to_compiler(ReturnM).

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
        _    -> io_lib:write(Message)
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Record abstract format is transformed to Map abstract format when RecordName is mentioned in -astranaut_struct([RecordName...]).

walk({call, _Line1, {remote, _Line2, {atom, _Line2, astranaut_struct}, {atom, _Line3, record}, [Node]}}, _StructInitMap, #{}) ->
    astranaut_walk_return:new(#{node => Node, continue => true});

%% transform record creation like #RecordName{Field1 = Value1, Field2 = Value2...} in pattern and expression
%% if record creation is in pattern, transformed to #{'__struct__' := RecordName, Field1 := Value1, Field2 := Value2}.
%% if record creation is in expression, transformed to #{'__struct__' => RecordName, Field1 => Value1, Field2 => Value2, ... RestFieldDefaultValueInitialize}.
%% compile will fail when FieldX is undefined in Record.
walk({record, Line, Name, Fields} = Node, StructInitMap, #{node := NodeType}) ->
    update_record(Name, Line, Fields, StructInitMap, #{type => NodeType, node => Node});

%% transform record update like Record#RecordName{Field1 = Value1, Field2 = Value2...} in expression.
%% transformed to Record#{'__struct__' => RecordName, Field1 => Value1, Field2 => Value2}.
%% compile will fail when FieldX is undefined in Record.
walk({record, Line, Update, Name, Fields} = Node, StructInitMap, #{node := NodeType}) ->
    update_record(Name, Line, Fields, StructInitMap, #{type => NodeType, update => Update, node => Node});

%% transform record index like #RecordName.Field
%% transformed to Field.
%% compile will fail when Field is undefined in Record.
walk({record_index, Line, RecordName, {atom, _Line2, FieldName}} = Node, StructIntMap, #{}) ->
    case maps:find(RecordName, StructIntMap) of
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

%% transform record index access like Record#RecordName.Field in expression.
%% if Field is in enforce key, transformed to maps:get(Field, Record).
%% else, transformed to maps:get(Field, Record, FieldDefaultValue).
walk({record_field, Line, Struct, RecordName, {atom, _Line2, FieldName} = Field} = Node, StructInitMap, #{}) ->
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
                            FieldInit = maps:get(FieldName, FieldInitValues, astranaut:abstract(undefined)),
                            Init = astranaut:replace_line(FieldInit, Line),
                            quote(maps:get(unquote(Field), unquote(Struct), unquote(Init)))
                    end;
                false ->
                    Error = {undefined_record_field, RecordName, FieldName},
                    {error, Error}
            end;
        error ->
            Node
    end;

%% transform record type like #Record{Field1 :: FieldType1, Field2 :; FieldType2}.
%% transformed to #{'__struct__' => RecordName, Field1 => FieldType1, Field2 => FieldType2, ... RestFieldTypes}.
walk({type, Line, record, [{atom, _Line, RecordName}|Fields]} = Node, StructInitMap, #{}) ->
    case maps:find(RecordName, StructInitMap) of
        {ok, StructDef} ->
            do([astranaut_traverse_m ||
                   FieldTypes <-
                       astranaut_traverse_m:pop_nodes(record_field_types(RecordName, Line, Fields, StructDef)),
                   astranaut_traverse_m:node({type, Line, map, FieldTypes})
               ]);
        error ->
            Node
    end;

%% record type with astranaut_struct:record(#Record{}) will not be transformed.
walk({remote_type, _Line1, [{atom, _Line2, astranaut_struct}, {atom, _Line3, record}, [TypeNode]]}, _StructIntMap, #{}) ->
    astranaut_walk_return:new(#{node => TypeNode, continue => true});

walk(Node, _StructInitMap, #{}) ->
    Node.

record_opts(#{update := Update}) ->
    #{update => Update, append_struct => false, append_init => false, field_type => map_field_assoc};
record_opts(#{type := pattern}) ->
    #{append_struct => true, append_init => false, field_type => map_field_exact};
record_opts(#{}) ->
    #{append_struct => true, append_init => true, field_type => map_field_assoc}.

update_record(Name, Line, RecordFields, RecordInitMap, #{node := Node} = Opts) ->
    RecordOpts = record_opts(Opts),
    case maps:find(Name, RecordInitMap) of
        {ok, SturctDef} ->
            do([astranaut_traverse_m ||
                   StructFields <-
                       astranaut_traverse_m:pop_nodes(
                         update_record_fields(Name, Line, RecordFields, SturctDef, RecordOpts)),
                   astranaut_traverse_m:node(update_record_main(Line, StructFields, RecordOpts))
               ]);
        error ->
            Node
    end.

update_record_main(Line, Fields, #{update := Update}) ->
    {map, Line, Update, Fields};
update_record_main(Line, Fields, #{}) ->
    {map, Line, Fields}.

update_record_fields(RecordName, Line, Fields, StructDef, #{field_type := MapFieldType} = Opts) ->
    StructDefFields = astranaut_struct_record:fields(StructDef),
    do([astranaut_traverse_m ||
           append_struct_name(RecordName, Line, Opts),
           FieldNames <-
               traverse_fields(
                 fun(Line, FieldName, FieldValue) ->
                         {MapFieldType, Line, FieldName, FieldValue}
                 end, RecordName, Fields, StructDefFields),
           append_init(RecordName, FieldNames, StructDef, Line, Opts)
       ]).

append_init(RecordName, FieldNames, StructDef, Line, #{append_init := true, field_type := MapFieldType}) ->
    EnforceKeys = astranaut_struct_record:enforce_keys(StructDef),
    MissingKeys = EnforceKeys -- FieldNames,
    FieldInitValues = astranaut_struct_record:init_values(StructDef),
    FieldInitValues1 = maps:without(FieldNames, FieldInitValues),
    Fields =
        lists:map(
          fun({FieldName, FieldInit}) ->
                  {MapFieldType, Line, {atom, Line, FieldName}, astranaut:replace_line(FieldInit, Line)}
          end, maps:to_list(FieldInitValues1)),
    do([astranaut_traverse_m ||
           check_missing_keys(RecordName, MissingKeys),
           astranaut_traverse_m:nodes(Fields)
       ]);
append_init(_RecordName, _FieldNames, _FieldInitValues, _Line, _Opts) ->
    astranaut_traverse_m:return(ok).

check_missing_keys(_RecordName, []) ->
    astranaut_traverse_m:return(ok);
check_missing_keys(RecordName, MissingKeys) ->
    astranaut_traverse_m:error({missing_enforce_keys, RecordName, MissingKeys}).

append_struct_name(RecordName, Line, #{append_struct := true, field_type := MapFieldType}) ->
    astranaut_traverse_m:node({MapFieldType, Line, {atom, Line, '__struct__'}, {atom, Line, RecordName}});
append_struct_name(_RecordName, _Line, #{}) ->
    astranaut_traverse_m:return(ok).

record_field_types(RecordName, Line, Fields, StructDef) ->
    StructDefFields = astranaut_struct_record:fields(StructDef),
    do([astranaut_traverse_m ||
           astranaut_traverse_m:node({type, Line, map_field_exact, struct_name_field(RecordName, Line)}),
           FieldNames <- traverse_fields(
                           fun(Line, FieldName, FieldType) ->
                                   IsMandatory = astranaut_struct_record:mandatory_field(FieldName, StructDef),
                                   AssocType = struct_field_assoc_type(IsMandatory),
                                   struct_field_type(AssocType, Line, FieldName, FieldType)
                           end, RecordName, Fields, StructDefFields),
           append_init_types(FieldNames, StructDef, Line)
      ]).

append_init_types(FieldNames, StructDef, Line) ->
    StructFields = astranaut_struct_record:fields(StructDef),
    FieldInitTypes = astranaut_struct_record:types(StructDef),
    FieldInitTypes = maps:without(FieldNames, FieldInitTypes),
    Fields =
        lists:reverse(
          lists:foldl(
            fun(FieldName, Acc) ->
                    case maps:find(FieldName, FieldInitTypes) of
                        {ok, FieldType} ->
                            IsMandatory = astranaut_struct_record:mandatory_field(FieldName, StructDef),
                            AssocType = struct_field_assoc_type(IsMandatory),
                            AtomFieldName = {atom, Line, FieldName},
                            FieldType = astranaut:replace_line(FieldType, Line),
                            Field = struct_field_type(AssocType, Line, AtomFieldName, FieldType),
                            [Field|Acc];
                        error ->
                            Acc
                    end
            end, [], StructFields)),
    astranaut_traverse_m:nodes(Fields).

struct_name_field(StructName, Line) ->
    [{atom, Line, '__struct__'}, {atom, Line, StructName}].

struct_field_assoc_type(true) ->
    map_field_exact;
struct_field_assoc_type(false) ->
    map_field_assoc.

struct_field_type(Type, Line, FieldName, FieldType) ->
    {type, Line, Type, [FieldName, FieldType]}.

traverse_fields(Fun, RecordName, Fields, StructFields) ->
    astranaut_monad:foldl_m(
      fun({record_field, Line1, {atom, _Line2, FieldName} = FieldNameAtom, FieldValue}, Acc) ->
              apply_field(Fun, RecordName, FieldName, Line1, FieldNameAtom, FieldValue, StructFields, Acc);
         ({type, Line1, field_type, [{atom, _Line2, FieldName} = FieldNameAtom, FieldType]}, Acc) ->
              apply_field(Fun, RecordName, FieldName, Line1, FieldNameAtom, FieldType, StructFields, Acc)
      end, [], Fields, astranaut_traverse_m).

apply_field(Fun, RecordName, FieldName, Line, FieldNameAtom, FieldValue, StructFields, Acc) ->
    case lists:member(FieldName, StructFields) of
        true ->
            Node = Fun(Line, FieldNameAtom, FieldValue),
            do([astranaut_traverse_m ||
                   astranaut_traverse_m:node(Node),
                   return([FieldName|Acc])
               ]);
        false ->
            do([astranaut_traverse_m ||
                   astranaut_traverse_m:error({undefined_record_field, RecordName, FieldName}),
                   return(Acc)
               ])
    end.

%%%===================================================================
%%% Initialize Structs with attribute astranaut_struct.
%%%===================================================================
init_struct_defs(Forms, RecordDefMap) ->
    TraverseOpts = #{formatter => ?MODULE, simplify_return => false, deep_attr => true},
    Validator = #{non_auto_fill => boolean,
                  auto_fill => boolean,
                  enforce_keys => {list_of, atom}},
    astranaut_options:forms_with_attribute(
      fun({Struct, Opts}, Acc, Attr) ->
              do([astranaut_base_m ||
                     Opts1 <- astranaut_options:validate(Validator, Opts, #{}),
                     add_struct(Struct, Opts1, RecordDefMap, Acc, Attr)
                 ]);
         (Struct, Acc, Attr) ->
              add_struct(Struct, #{}, RecordDefMap, Acc, Attr)
      end, maps:new(), Forms, astranaut_struct, TraverseOpts).

add_struct(Struct, Opts, RecordMap, StructDefs, #{line := Line}) ->
    case new_struct(Struct, Opts, RecordMap) of
        {ok, NewStruct} ->
            Node = astranaut:attribute_node(astranaut_struct_def, Line, NewStruct),
            {[Node], maps:put(Struct, NewStruct, StructDefs)};
        {errors, Reason} ->
            {errors, {[], StructDefs}, Reason};
        {error, Reason} ->
            {error, {[], StructDefs}, Reason}
    end.

new_struct(Struct, Opts, RecordMap) when is_atom(Struct) ->
    case maps:find(Struct, RecordMap) of
        {ok, RecordDef} ->
            RecordDef1 = update_record_def(RecordDef, Opts),
            FieldErrors = astranaut_struct_record:warnings(RecordDef1),
            case FieldErrors of
                [] ->
                    {ok, RecordDef1};
                FieldErrors ->
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
    AutoFill = astranaut_options:get_boolean(auto_fill, non_auto_fill, StructDef, true),
    EnforceKeys = maps:get(enforce_keys, StructDef, []),
    RecordDef1 = astranaut_struct_record:set_auto_fill(AutoFill, RecordDef),
    RecordDef2 = astranaut_struct_record:update_enforce_keys(EnforceKeys, RecordDef1),
    RecordDef3 = astranaut_struct_record:update_init_values(RecordDef2),
    RecordDef3.

remove_used_structs(Forms, UsedStructs) ->
    lists:filter(
      fun({attribute, _Line, record, {RecordName, _Fields}}) ->
              not ordsets:is_element(RecordName, UsedStructs);
         (_Node) ->
              true
      end, Forms).

transform_struct_macros(Forms) ->
    Macros = [{astranaut_struct, from_record, 2, []}, {astranaut_struct, to_record, 2, []},
              {astranaut_struct, from_map, 3, []}, {astranaut_struct, update, 3, []},
              {astranaut_struct, from_other_record, 4, []}],
    astranaut_macro:transform_macros(Macros, Forms).

remove_used_struct_records(Forms0, Forms1) ->
    UsedRecords0 = used_records(Forms0),
    UsedRecords1 = used_records(Forms1),
    Forms = remove_used_structs(Forms1, ordsets:subtract(UsedRecords0, UsedRecords1)),
    astranaut_return_m:return(Forms).
    
used_records(Forms) ->
    astranaut_traverse:reduce(
      fun(Node, Acc, #{}) ->
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
