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


-export_macro({[from_record/2, to_record/2], []}).
-export_macro({[from_map/3, update/3], [{attrs, [module, record]}]}).

%% API
-export([from_record/2, to_record/2, update/3, from_map/3]).
-export([from_record_impl/3, to_record_impl/3, update_impl/3, from_map_impl/3]).
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

from_map_impl(StructName, StructFieldInits, #{} = Map) ->
    maps:fold(
      fun(Field, Init, Acc) ->
              case maps:find(Field, Map) of
                  {ok, Value} ->
                      maps:put(Field, Value, Acc);
                  error ->
                      maps:put(Field, Init, Acc)
              end
      end, #{'__struct__' => StructName}, StructFieldInits);
from_map_impl(_StructName, _StructFieldInits, Map) ->
    exit({invalid_map, Map}).

update_impl(StructName, StructFieldInits, #{'__struct__' := StructName} = Struct) ->
    from_map_impl(StructName, StructFieldInits, Struct);
update_impl(StructName, _StructFieldInits, Struct) ->
    exit({invalid_struct, StructName, Struct}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_structs(Module, StructName, Records) ->
    case lists:keyfind(StructName, 1, Records) of
        Record when is_tuple(Record) ->
            RecordDef = astranaut_struct_record:record_def(Module, Record),
            Errors = astranaut_struct_record:warnings(RecordDef),
            InitMap = astranaut_struct_record:full_init_values(RecordDef),
            case Errors of
                [] ->
                    {ok, InitMap};
                Errors ->
                    {error, Errors}
            end;
        undefined ->
            {error, {undefined_struct, struct_name}}
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
            unquote(RecordName), record_info(fields, unquote(RecordName)),  unquote(Record)));
do_record_function(_FunctionName, RecordName, _Record) ->
    exit({literal_atom_record_name_expected, RecordName}).

do_struct_function(FunctionName, {atom, Line, StructName} = AtomStructName, Struct, #{module := Module, record := RecordDefs}) ->
    case init_structs(Module, StructName, RecordDefs) of
        {ok, StructInits} ->
            InitAbs = struct_init_abs(Line, StructInits),
            quote(astranaut_struct:(unquote(FunctionName))(unquote(AtomStructName), unquote(InitAbs), unquote(Struct)));
        {error, Reason} ->
            {error, Reason}
    end;
do_struct_function(_Action, StructName, _Struct, #{}) ->
    exit({literal_atom_struct_name_expected, StructName}).
