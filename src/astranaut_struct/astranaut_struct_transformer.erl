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
    Records = astranaut:attributes(record, Forms),
    StructAttributes = astranaut:attributes(astranaut_struct, Forms),
    TraverseState = init_struct_defs(Module, StructAttributes),
    #{errors := Errors, state := StructInitMap} = init_structs(Module, Records, TraverseState),
    case Errors of
        [] ->
            Return = 
                astranaut_traverse:map(
                  fun(Node, Attrs) ->
                          walk(Node, StructInitMap, Attrs)
                  end, Forms, #{traverse => pre, formatter => ?MODULE, parse_transform => File}),
            astranaut_traverse:map_traverse_return(
              fun(Forms) ->
                      io:format("~s~n", [astranaut:to_string(Forms)]),
                      Forms
              end, Return);
        _ ->
            astranaut_traverse:parse_transform_return({error, Errors, []}, File)
    end.

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
        {ok, StructFieldsMap} ->
            case maps:is_key(FieldName, StructFieldsMap) of
                true ->
                    {atom, Line, FieldName};
                false ->
                    {error, {undefined_record_field, RecordName, FieldName}}
            end;
        error ->
            Node
    end;

%% transform record index access like Record#RecordName.Field in expression.
%% transformed to maps:get(Field, Record, FieldDefaultValue),
walk({record_field, Line, Struct, RecordName, {atom, _Line2, FieldName} = Field} = Node, StructInitMap, #{}) ->
    case maps:find(RecordName, StructInitMap) of
        {ok, StructFieldsMap} ->
            case maps:find(FieldName, StructFieldsMap) of
                {ok, FieldInit} ->
                    Init = astranaut:replace_line(FieldInit, Line),
                    quote(maps:get(unquote(Field), unquote(Struct), unquote(Init)));
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
        {ok, StructFieldsMap} ->
            FieldTypes = update_record_field_types(RecordName, Line, Fields, StructFieldsMap),
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
        {ok, FieldsMap} ->
            ReturnFields = update_record_fields(Name, Line, Fields, FieldsMap, RecordOpts),
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

update_record_fields(RecordName, Line, Fields, FieldsMap, #{field_type := MapFieldType} = Opts) ->
    Return = 
        astranaut_traverse:mapfold(
          fun({record_field, Line1, {atom, _Line2, FieldName} = FieldNameAtom, FieldValue}, Acc, #{}) ->
                  MapFieldNode = {MapFieldType, Line1, FieldNameAtom, FieldValue},
                  case maps:is_key(FieldName, FieldsMap) of
                      true ->
                          {MapFieldNode, [FieldName|Acc]};
                      false ->
                          Error = {undefined_record_field, RecordName, FieldName},
                          astranaut_traverse:traverse_fun_return(#{node => MapFieldNode, state => Acc, error => Error})
                  end
          end, [], Fields, #{traverse => list, formatter => ?MODULE}),
    Return1 = astranaut_traverse:reply_to_traverse_fun_return(Return, {Fields, []}),
    astranaut_traverse:map_traverse_fun_return(
      fun({Fields1, FieldNames}) ->
              Fields2 = append_init(FieldNames, FieldsMap, Fields1, Line, Opts),
              append_struct(RecordName, Fields2, Line, Opts)
      end, Return1).

append_init(FieldNames, FieldsMap, Fields, Line, #{append_init := true, field_type := MapFieldType}) ->
    FieldsMap1 = maps:without(FieldNames, FieldsMap),
    lists:reverse(
      maps:fold(
        fun(FieldName, FieldInit, Acc) ->
                [{MapFieldType, Line, {atom, Line, FieldName}, astranaut:replace_line(FieldInit, Line)}|Acc]
        end, Fields, FieldsMap1));
append_init(_FieldNames, _FieldsMap, Fields, _Line, _Opts) ->
    Fields.

append_struct(RecordName, Fields, Line, #{append_struct := true, field_type := MapFieldType}) ->
    [{MapFieldType, Line, {atom, Line, '__struct__'}, {atom, Line, RecordName}}|Fields];
append_struct(_RecordName, Fields, _Line, #{}) ->
    Fields.

update_record_field_types(RecordName, Line, Fields, FieldsMap) ->
    Return = 
        astranaut_traverse:mapfold(
          fun({type, Line1, field_type, [{atom, _Line2, AtomFieldName} = FieldName, FieldType]}, Acc, #{}) ->
                  MapFieldTypeNode = {type, Line1, map_field_exact, [FieldName, FieldType]},
                  case maps:is_key(AtomFieldName, FieldsMap) of
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

init_struct_defs(_Module, StructDefs) ->
    lists:foldl(
      fun({Struct, Opts}, Acc) when is_atom(Struct) ->
              add_struct_defs([Struct], Opts, Acc);
         ({Structs, Opts}, Acc) when is_list(Structs) ->
              add_struct_defs(Structs, Opts, Acc);
         (Struct, Acc) when is_atom(Struct) ->
              add_struct_defs([Struct], [], Acc);
         (Struct, Acc) ->
              add_struct_errors(Struct, Acc)
      end, astranaut_traverse:new_state(maps:new()), StructDefs).

add_struct_defs(Structs, Opts, #{state := StructMap} = TraverseState) ->
    {Opts1, Errors1} = astranaut:validate_options(fun struct_options_validator/2, Opts),
    StructMap1 =
        lists:foldl(
          fun(Struct, Acc) ->
              maps:put(Struct, Opts1, Acc)
          end, StructMap, Structs),
    State = astranaut_traverse:traverse_fun_return(#{state => StructMap1, errors => Errors1}),
    astranaut_traverse:merge_state(State, TraverseState).

add_struct_errors(Struct, {StructMap, Errors}) ->
    {StructMap, [{invalid_struct_def, Struct}|Errors]}.

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

init_structs(Module, Records, #{state := Defs} = TraverseState) ->
    MapRecords1 = maps:keys(Defs),
    TraverseState1 = TraverseState#{state => maps:new()},
    lists:foldl(
      fun({RecordName, _Fields} = Record, #{state := RecordMapAcc} = StateAcc) ->
              case lists:member(RecordName, MapRecords1) of
                  true ->
                      RecordDef = astranaut_struct_record:record_def(Module, Record),
                      FieldsErrors = astranaut_struct_record:warnings(RecordDef),
                      InitMap = astranaut_struct_record:full_init_values(RecordDef),
                      RecordMapAcc1 = maps:put(RecordName, InitMap, RecordMapAcc),
                      State = astranaut_traverse:traverse_fun_return(#{state => RecordMapAcc1, errors => FieldsErrors}),
                      astranaut_traverse:merge_state(State, StateAcc);
                  false ->
                      StateAcc
              end
      end, TraverseState1, Records).
