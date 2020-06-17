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
-export_macro({[update/3], [{attrs, [module, record, astranaut_struct]}]}).

%% API
-export([from_record/2, to_record/2, update/3]).
-export([from_record_impl/3, to_record_impl/3, update_impl/3]).
%%%===================================================================
%%% API
%%%===================================================================
from_record({atom, _Line, _RecordName} = RecordName, Record) ->
    quote(astranaut_struct:from_record_impl(unquote(RecordName), record_info(fields, unquote(RecordName)),  unquote(Record)));
from_record(RecordName, _Record) ->
    exit({literal_atom_record_name_expected, RecordName}).

to_record({atom, _Line, _RecordName} = RecordName, Record) ->
    quote(astranaut_struct:to_record_impl(unquote(RecordName), record_info(fields, unquote(RecordName)),  unquote(Record)));
to_record(RecordName, _Record) ->
    exit({literal_atom_record_name_expected, RecordName}).

update({atom, Line, StructName} = AtomStructName, Struct, #{module := Module, record := RecordDefs}) ->
    case init_structs(Module, StructName, RecordDefs) of
        {ok, StructInits} ->
            Fields = 
                lists:reverse(
                  maps:fold(
                    fun(Name, Value, Acc) ->
                            [{map_field_assoc, Line, {atom, Line, Name}, Value}|Acc]
                    end, [], StructInits)),
            InitAbs = {map, Line, Fields},
            quote(astranaut_struct:update_impl(unquote(AtomStructName), unquote(InitAbs), unquote(Struct)));
        {error, Reason} ->
            {error, Reason}
    end.

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

update_impl(StructName, StructFieldInits, #{'__struct__' := StructName} = Struct) ->
    lists:foldl(
      fun(Field, Init, Acc) ->
              case maps:find(Field, Struct) of
                  {ok, Value} ->
                      maps:put(Field, Value, Acc);
                  error ->
                      maps:put(Field, Init, Acc)
              end
      end, #{'__struct__' => StructName}, StructFieldInits);
update_impl(StructName, _StructFieldInits, Struct) ->
    exit({invalid_struct, StructName, Struct}).

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


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
