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

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    File = astranaut:file(Forms),
    Module = astranaut:module(Forms),
    StructAttributes = astranaut:attributes_with_line(astranaut_struct, Forms),
    TraverseState = init_struct_defs(Module, StructAttributes),
    #{errors := Errors, state := State} = init_structs(Module, Forms, TraverseState),
    #{structs := StructInitMap, forms := Forms1} = State,
    Forms2 = lists:reverse(Forms1),
    case Errors of
        [] ->
            Return = 
                astranaut_traverse:map(
                  fun(Node, Attrs) ->
                          walk(Node, StructInitMap, Attrs)
                  end, Forms2, #{traverse => pre, formatter => ?MODULE, parse_transform => File}),
            astranaut_traverse:map_traverse_return(
              fun(Forms3) ->
                      io:format("~s~n", [astranaut:to_string(Forms3)]),
                      Forms3
              end, Return);
        _ ->
            astranaut_traverse:parse_transform_return({error, Errors, []}, File)
    end.

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
    #{node => Node, continue => true};

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
                            FieldInit = maps:get(FieldName, FieldInitValues, astranaut_quote:quote(undefined)),
                            Init = astranaut:replace_line(FieldInit, Line),
                            quote(maps:get(unquote(Field), unquote(Struct), unquote(Init)))
                    end;
                error ->
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
            FieldTypes = update_record_field_types(RecordName, Line, Fields, StructDef),
            {type, Line, map, FieldTypes};
        error ->
            Node
    end;

%% record type with astranaut_struct:record(#Record{}) will not be transformed.
walk({remote_type, _Line1, [{atom, _Line2, astranaut_struct}, {atom, _Line3, record}, [TypeNode]]}, _StructIntMap, #{}) ->
    astranaut_traverse:traverse_fun_return(#{node => TypeNode, continue => true});

walk(Node, _StructInitMap, #{}) ->
    Node.

record_opts(#{update := Update}) ->
    #{update => Update, append_struct => false, append_init => false, field_type => map_field_assoc};
record_opts(#{type := pattern}) ->
    #{append_struct => true, append_init => false, field_type => map_field_exact};
record_opts(#{}) ->
    #{append_struct => true, append_init => true, field_type => map_field_assoc}.

update_record(Name, Line, Fields, RecordInitMap, #{node := Node} = Opts) ->
    RecordOpts = record_opts(Opts),
    case maps:find(Name, RecordInitMap) of
        {ok, SturctDef} ->
            ReturnFields = update_record_fields(Name, Line, Fields, SturctDef, RecordOpts),
            astranaut_traverse:map_traverse_fun_return(
              fun(Fields1) ->
                      update_record_main(Line, Fields1, RecordOpts)
              end, ReturnFields);
        error ->
            Node
    end.

update_record_main(Line, Fields, #{update := Update}) ->
    {map, Line, Update, Fields};
update_record_main(Line, Fields, #{}) ->
    {map, Line, Fields}.

update_record_fields(RecordName, Line, Fields, StructDef, #{field_type := MapFieldType} = Opts) ->
    StructDefFields = astranaut_struct_record:fields(StructDef),
    Return = 
        astranaut_traverse:mapfold(
          fun({record_field, Line1, {atom, _Line2, FieldName} = FieldNameAtom, FieldValue}, Acc, #{}) ->
                  MapFieldNode = {MapFieldType, Line1, FieldNameAtom, FieldValue},
                  case lists:member(FieldName, StructDefFields) of
                      true ->
                          {MapFieldNode, [FieldName|Acc]};
                      false ->
                          Error = {undefined_record_field, RecordName, FieldName},
                          astranaut_traverse:traverse_fun_return(#{node => MapFieldNode, state => Acc, error => Error})
                  end
          end, [], Fields, #{traverse => list, formatter => ?MODULE}),
    Return1 = 
        astranaut_traverse:map_traverse_return(
          fun({Fields1, FieldNames}) ->
                  Fields2 = append_init(RecordName, FieldNames, StructDef, Fields1, Line, Opts),
                  astranaut_traverse:map_traverse_return(
                    fun(Fields3) ->
                            append_struct(RecordName, Fields3, Line, Opts)
                    end, Fields2)
          end, Return),
    Return2 = astranaut_traverse:reply_to_traverse_fun_return(Return1, Fields),
    Return2.


append_init(RecordName, FieldNames, StructDef, Fields, Line, #{append_init := true, field_type := MapFieldType}) ->
    EnforceKeys = astranaut_struct_record:enforce_keys(StructDef),
    MissingKeys = EnforceKeys -- FieldNames,
    FieldInitValues = astranaut_struct_record:init_values(StructDef),
    FieldInitValues1 = maps:without(FieldNames, FieldInitValues),
    Fields1 = 
        lists:reverse(
          maps:fold(
            fun(FieldName, FieldInit, Acc) ->
                [{MapFieldType, Line, {atom, Line, FieldName}, astranaut:replace_line(FieldInit, Line)}|Acc]
            end, Fields, FieldInitValues1)),
    case MissingKeys of
        [] ->
            Fields1;
        _ ->
            {error, Fields1, {missing_enforce_keys, RecordName, MissingKeys}}
    end;
append_init(_RecordName, _FieldNames, _FieldInitValues, Fields, _Line, _Opts) ->
    Fields.

append_struct(RecordName, Fields, Line, #{append_struct := true, field_type := MapFieldType}) ->
    [{MapFieldType, Line, {atom, Line, '__struct__'}, {atom, Line, RecordName}}|Fields];
append_struct(_RecordName, Fields, _Line, #{}) ->
    Fields.

update_record_field_types(RecordName, Line, Fields, StructDef) ->
    StructDefFields = astranaut_struct_record:fields(StructDef),
    Return = 
        astranaut_traverse:mapfold(
          fun({type, Line1, field_type, [{atom, _Line2, AtomFieldName} = FieldName, FieldType]}, Acc, #{}) ->
                  MapFieldTypeNode = {type, Line1, map_field_exact, [FieldName, FieldType]},
                  case lists:member(AtomFieldName, StructDefFields) of
                      true ->
                          {MapFieldTypeNode, [FieldName|Acc]};
                      false ->
                          Error = {undefined_recorid_field, RecordName, FieldName},
                          astranaut_traverse:traverse_fun_return(#{node => MapFieldTypeNode, state => Acc, error => Error})
                  end
          end, [], Fields, #{traverse => list, formatter => ?MODULE}),
    Return1 = astranaut_traverse:reply_to_traverse_fun_return(Return, {Fields, []}),
    astranaut_traverse:map_traverse_fun_return(
      fun({Fields1, _FieldNames}) ->
              append_struct_type(RecordName, Fields1, Line)
      end, Return1).

append_struct_type(RecordName, Fields, Line) ->
    [{type, Line, map_field_exact, [{atom, Line, '__struct__'}, {atom, Line, RecordName}]}|Fields].

init_struct_defs(Module, StructDefs) ->
    astranaut:init_attributes(
      fun({error, {invalid_attribute, Structs}}, Line, _Opts, Acc) ->
              add_struct_errors(Structs, Module, Line, Acc);
         (Structs, Line, Opts, Acc) ->
              add_struct_defs(Structs, Line, Opts, Acc)
      end, astranaut_traverse:new_state(maps:new()), StructDefs).

add_struct_defs(Structs, Line, Opts, #{state := StructMap} = TraverseState) ->
    {Opts1, Errors1} = astranaut:validate_options(fun struct_options_validator/2, Opts),
    StructMap1 =
        lists:foldl(
          fun(Struct, Acc) ->
              maps:put(Struct, Opts1#{line => Line}, Acc)
          end, StructMap, Structs),
    State = astranaut_traverse:traverse_fun_return(#{state => StructMap1, errors => Errors1}),
    astranaut_traverse:merge_state(State, TraverseState).

add_struct_errors(Struct, Module, Line, {StructMap, Errors}) ->
    Error = {invalid_struct_def, Struct},
    {StructMap, [{Line, Module, Error}|Errors]}.

struct_options_validator(non_auto_fill, Boolean) when is_boolean(Boolean) ->
    ok;
struct_options_validator(auto_fill, Boolean) when is_boolean(Boolean) ->
    ok;
struct_options_validator(enforce_keys, Keys) ->
    case lists:filter(
           fun(Key) ->
                   not is_atom(Key)
           end, Keys) of
        [] ->
            ok;
        NonAtomKeys ->
            {error, {invalid_enforce_keys, NonAtomKeys}}
    end;
struct_options_validator(_Key, _Value) ->
    error.

init_structs(Module, Forms, #{state := StructDefs} = TraverseState) ->
    TraverseState1 = TraverseState#{state => #{structs => maps:new(), forms => []}},
    lists:foldl(
      fun({attribute, Line, record, {RecordName, _RecordFields} = Record} = Node, 
          #{state := #{structs := RecordMapAcc, forms := FormsAcc} = StateAcc} = Acc) ->
              case maps:find(RecordName, StructDefs) of
                  {ok, StructDef} ->
                      RecordDef = astranaut_struct_record:record_def(Module, Record, Line),
                      RecordDef1 = update_record_def(RecordDef, StructDef),
                      FieldsErrors = astranaut_struct_record:warnings(RecordDef1),
                      RecordMapAcc1 = maps:put(RecordName, RecordDef1, RecordMapAcc),
                      Node1 = {attribute, Line, astranaut_struct_def, RecordDef1},
                      StateAcc1 = StateAcc#{structs => RecordMapAcc1, forms := [Node1, Node|FormsAcc]},
                      State = astranaut_traverse:traverse_fun_return(#{state => StateAcc1, errors => FieldsErrors}),
                      astranaut_traverse:merge_state(State, Acc);
                  error ->
                      Acc#{state => StateAcc#{forms => [Node|FormsAcc]}}
              end;
         (Node, #{state := #{forms := FormsAcc} = StateAcc} = Acc) ->
              Acc#{state => StateAcc#{forms => [Node|FormsAcc]}}
      end, TraverseState1, Forms).

update_record_def(RecordDef, #{line := Line} = StructDef) ->
    AutoFill = not maps:get(non_auto_fill, StructDef, false),
    EnforceKeys = maps:get(enforce_keys, StructDef, []),
    RecordDef1 = astranaut_struct_record:set_auto_fill(AutoFill, RecordDef),
    RecordDef2 = astranaut_struct_record:update_enforce_keys(EnforceKeys, ?MODULE, Line, RecordDef1),
    RecordDef3 = astranaut_struct_record:update_init_values(RecordDef2),
    RecordDef3.
