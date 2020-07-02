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
    Module = astranaut:module(Forms),
    Return = 
        astranaut_traverse:bind(
          astranaut_traverse:bind(
            astranaut_traverse:without_errors(Forms),
            fun(Forms) ->
                    RecordDefMap = init_records(Module, Forms),
                    astranaut_traverse:bind(
                      init_struct_defs(Forms, RecordDefMap),
                      fun(#{struct_defs := StructInitMap, forms := Forms}) ->
                              Forms = lists:reverse(Forms),
                              astranaut_traverse:map(
                                fun(Node, Attrs) ->
                                        walk(Node, StructInitMap, Attrs)
                                end, Forms, #{traverse => pre, formatter => ?MODULE})
                      end)
            end),
          fun(Forms) ->
                  %% io:format("forms is ~ts~n", [astranaut:to_string(Forms)]),
                  Forms
          end),
    astranaut_traverse:parse_transform_return(Return).

format_error({undefined_record, Record, Attribute}) ->
    io_lib:format("[~s]: record ~p in is not defined", [astranaut:to_string(Attribute), Record]);
format_error({invalid_struct_def, Struct, Attribute}) ->
    io_lib:format("[~s]: ~p is not a valid struct name in", [astranaut:to_string(Attribute), Struct]);
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

init_struct_defs(Forms, RecordDefMap) ->
    State = #{struct_defs => maps:new(), forms => []},
    astranaut_traverse:reduce(
      fun({attribute, Line, astranaut_struct, Struct} = Node, #{forms := FormsAcc} = StateAcc, #{}) ->
              StateAcc1 = StateAcc#{forms := [Node|FormsAcc]},
              #{state := State, errors := Errors} = add_struct(Struct, Line, Node, StateAcc1, RecordDefMap),
              Errors1 = lists:map(fun(Error) -> {Line, ?MODULE, Error} end, Errors),
              astranaut_traverse:traverse_fun_return(#{state => State, errors => Errors1});
         (Node, #{forms := FormsAcc} = StateAcc, #{}) ->
              StateAcc#{forms => [Node|FormsAcc]}
      end, State, Forms, #{traverse => list, formatter => ?MODULE}).

add_struct({Structs, Opts} = Rec, Line, Node, Acc, StructDefMap) ->
    case astranaut:is_opts(Opts) of
        true ->
            add_struct_defs(Structs, Line, Node, Opts, Acc, StructDefMap);
        false ->
            {error, {invalid_struct_def, Rec}}
    end;
add_struct(Structs, Line, Node,  Acc, StructDefMap) ->
    add_struct_defs(Structs, Line, Node, [], Acc, StructDefMap).

add_struct_defs(Structs, Line, Node, Opts, State, StructDefMap) when is_list(Structs) ->
    add_struct_defs_1(Structs, Line, Node, Opts, State, StructDefMap);
add_struct_defs(Struct, Line, Node, Opts, State, StructDefMap) ->
    add_struct_defs_1([Struct], Line, Node, Opts, State, StructDefMap).

add_struct_defs_1(Structs, Line, Node, Opts, TraverseState, RecordDefMap) ->
    {Opts1, Errors1} = astranaut:validate_options(fun struct_options_validator/2, Opts),
    astranaut_traverse:then_return(
      #{state => ok, errors => Errors1},
      astranaut_traverse:fold_bind_returns(
        fun(Struct, #{struct_defs := StructDefs, forms := Forms} = StateAcc) when is_atom(Struct) ->
                case maps:find(Struct, RecordDefMap) of
                    {ok, RecordDef} ->
                        RecordDef1 = update_record_def(RecordDef, Opts1),
                        FieldErrors = astranaut_struct_record:warnings(RecordDef1),
                        case FieldErrors of
                            [] ->
                                Node = astranaut:attribute_node(astranaut_struct_def, Line, RecordDef1),
                                Forms1 = [Node|Forms],
                                StructDefs1 = maps:put(Struct, RecordDef1, StructDefs),
                                StateAcc1 = StateAcc#{struct_defs => StructDefs1, forms => Forms1},
                                {ok, StateAcc1};
                            FieldErrors ->
                                {errors, StateAcc, FieldErrors}
                        end;
                    error ->
                        {error, StateAcc, {undefined_record, Struct, Node}}
                end;
           (Struct, StateAcc) ->
                {error, StateAcc, {invalid_struct_def, Struct, Node}}
        end, TraverseState, Structs)).

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

init_records(Module, Forms) ->
    lists:foldl(
      fun({attribute, Line, record, {RecordName, _RecordFields} = Record}, RecordMapAcc) ->
              RecordDef = astranaut_struct_record:record_def(Module, Record, Line),
              maps:put(RecordName, RecordDef, RecordMapAcc);
         (_Node, RecordMapAcc) ->
              RecordMapAcc
      end, maps:new(), Forms).

update_record_def(RecordDef, #{} = StructDef) ->
    AutoFill = not maps:get(non_auto_fill, StructDef, false),
    EnforceKeys = maps:get(enforce_keys, StructDef, []),
    RecordDef1 = astranaut_struct_record:set_auto_fill(AutoFill, RecordDef),
    RecordDef2 = astranaut_struct_record:update_enforce_keys(EnforceKeys, RecordDef1),
    RecordDef3 = astranaut_struct_record:update_init_values(RecordDef2),
    RecordDef3.
