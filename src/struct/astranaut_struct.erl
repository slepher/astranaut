%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_struct).

-include("quote.hrl").
-include("macro.hrl").
-include("astranaut_struct_name.hrl").

-export_macro({[from_record/2, to_record/2], []}).
-export_macro({[from_map/3, update/3], [{attrs, [astranaut_struct_def]}]}).

%% API
-export([to_map/2]).
-export([from_record/2, to_record/2, update/3, from_map/3]).
-export([from_record_impl/3, to_record_impl/3, update_impl/5, from_map_impl/5]).
%%%===================================================================
%%% API
%%%===================================================================
from_record(RecordName, Record) ->
    do_record_function(quote(from_record_impl), RecordName, Record).

to_record(RecordName, Record) ->
    do_record_function(quote(to_record_impl), RecordName, Record).

from_map(StructName, Struct, #{} = Attrs) ->
    do_struct_function(quote(from_map_impl), StructName, Struct, Attrs).

update(StructName, Struct, Attrs) ->
    do_struct_function(quote(update_impl), StructName, Struct, Attrs).

to_map(StructName, #{?STRUCT_KEY := StructName} = Struct) ->
    maps:remove(?STRUCT_KEY, Struct).

from_record_impl(RecordName, RecordFields, Record) when is_tuple(Record) ->
    StructSize = length(RecordFields) + 1,
    case tuple_size(Record) of
        StructSize ->
            maps:from_list(lists:zip(['__struct__'|RecordFields], tuple_to_list(Record)));
        _ ->
            exit({invalid_record, RecordName, Record})
    end;
from_record_impl(RecordName, _RecordFields, Record) ->
    exit({invalid_record, RecordName, Record}).

to_record_impl(RecordName, RecordFields, #{'__struct__' := RecordName} = Struct) ->
    list_to_tuple(
      lists:reverse(
        lists:foldl(
          fun(RecordField, Acc) ->
                  FieldValue = maps:get(RecordField, Struct, undefined),
                  [FieldValue|Acc]
          end, [RecordName], RecordFields)));
to_record_impl(RecordName, _RecordFields, Struct) ->
    exit({invalid_struct, RecordName, Struct}).

from_map_impl(StructName, Fields, EnforceKeys, StructFieldInits, #{} = Map) ->
    Keys = maps:keys(Map),
    MissingKeys = EnforceKeys -- Keys,
    case MissingKeys of
        [] ->
            lists:foldl(
              fun(Field, Acc) ->
                      case maps:find(Field, Map) of
                          {ok, Value} ->
                              maps:put(Field, Value, Acc);
                          error ->
                              case maps:find(Field, StructFieldInits) of
                                  {ok, Init} ->
                                      maps:put(Field, Init, Acc);
                                  error ->
                                      Acc
                              end
                      end
              end, #{'__struct__' => StructName}, Fields);
        _ ->
            exit({missing_enforce_keys, StructName, MissingKeys})
    end;
from_map_impl(_StructName, _Fields, _EnforceKeys, _StructFieldInits, Map) ->
    exit({invalid_map, Map}).

update_impl(StructName, Fields, EnforceKeys, StructFieldInits, #{'__struct__' := StructName} = Struct) ->
    from_map_impl(StructName, Fields, EnforceKeys, StructFieldInits, Struct);
update_impl(StructName, _Fields, _EnforceKeys, _StructFieldInits, Struct) ->
    exit({invalid_struct, StructName, Struct}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_structs(StructName, AstranautStructDefs) ->
    case lists:keyfind(StructName, 2, AstranautStructDefs) of
        StructDef when is_tuple(StructDef) ->
            {ok, StructDef};
        false ->
            {error, {undefined_struct, StructName}}
    end.

struct_init_abs(Line, StructInits) ->
    Fields = 
        lists:reverse(
          maps:fold(
            fun(Name, Value, Acc) ->
                    [{map_field_assoc, Line, {atom, Line, Name}, Value}|Acc]
            end, [], StructInits)),
    {map, Line, Fields}.

do_record_function(FunctionName, {atom, _Line, _RecordName} = RecordName, Record) ->
    quote(astranaut_struct:(unquote(FunctionName))(
            unquote(RecordName), record_info(fields, unquote(RecordName)), unquote(Record)));
do_record_function(_FunctionName, RecordName, _Record) ->
    exit({literal_atom_record_name_expected, RecordName}).

do_struct_function(FunctionName, {atom, Line, StructName} = AtomStructName, Struct, #{astranaut_struct_def := StructDefs}) ->
    case init_structs(StructName, StructDefs) of
        {ok, StructDef} ->
            Fields = astranaut_struct_record:fields(StructDef),
            InitValue = astranaut_struct_record:init_values(StructDef),
            EnforceKeys = astranaut_struct_record:enforce_keys(StructDef),
            InitAbs = struct_init_abs(Line, InitValue),
            FieldsAbs =  astranaut:replace_line(astranaut:abstract(Fields), Line),
            EnforceKeysAbs = astranaut:replace_line(astranaut:abstract(EnforceKeys), Line),
            quote(astranaut_struct:(unquote(FunctionName))(
                    unquote(AtomStructName), unquote(FieldsAbs), unquote(EnforceKeysAbs), unquote(InitAbs), unquote(Struct)));
        {error, Reason} ->
            {error, Reason}
    end;
do_struct_function(_Action, StructName, _Struct, #{}) ->
    exit({literal_atom_struct_name_expected, StructName}).
