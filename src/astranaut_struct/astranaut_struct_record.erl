%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_struct_record).

-include("rebinding.hrl").

%% API
-export([record_def/2]).
-export([fields/1, init_values/1, full_init_values/1, types/1, warnings/1]).

-record(record_def, {module, name, fields = [], init_values = maps:new(), types = maps:new(), warnings = []}).

%%%===================================================================
%%% API
%%%===================================================================
record_def(Module, {RecordName, Fields}) ->
    RecordDef = #record_def{module = Module, name = RecordName},
    add_fields(Fields, RecordDef).

fields(#record_def{fields = Fields}) ->
    Fields.

init_values(#record_def{init_values = InitValues}) ->
    InitValues.

full_init_values(#record_def{fields = Fields, init_values = InitValues}) ->
    lists:foldl(
      fun(Field, Acc) ->
              case maps:find(Field, InitValues) of
                  {ok, InitValue} ->
                      maps:put(Field, InitValue, Acc);
                  error ->
                      maps:put(Field, {atom, 0, undefined}, Acc)
              end
      end, maps:new(), Fields).

types(#record_def{types = Types}) ->
    Types.

warnings(#record_def{warnings = Warnings}) ->
    Warnings.

add_fields([{typed_record_field, RecordField, TypeForm}|T], RecordDef) ->
    RecordDef = 
        add_detyped_field(
          fun(Name, #record_def{types = Types} = RecordDef) ->
                  Types = Types#{Name => TypeForm},
                  RecordDef#record_def{types = Types}
          end, RecordField, RecordDef),
    add_fields(T, RecordDef);
add_fields([RecordField|T], RecordDef) ->
    RecordDef = add_detyped_field(undefined, RecordField, RecordDef),
    add_fields(T, RecordDef);
add_fields([], #record_def{fields = Fields, warnings = Warnings} = RecordDef) ->
    RecordDef#record_def{fields = lists:reverse(Fields), warnings = lists:reverse(Warnings)}.

add_detyped_field(Fun, RecordField, RecordDef) ->
    case detyped_record_field(RecordField) of
        {ok, #{name := Name} = FieldDef} ->
            RecordField = add_field_def(FieldDef, RecordDef),
            apply_fun(Fun, Name, RecordField);
        {error, Reason} ->
            add_warning(Reason, RecordDef)
    end.

apply_fun(Fun, Name, RecordField) when is_function(Fun) ->
    Fun(Name, RecordField);
apply_fun(undefined, _Name, RecordField) ->
    RecordField.

detyped_record_field({record_field, _Line, {atom, _Line1, Name}}) ->
    {ok, #{name => Name}};
detyped_record_field({record_field, _Line, {atom, _Line1, Name}, Init}) ->
    {ok, #{name => Name, init => Init}};
detyped_record_field(Other) ->
    Line = erl_syntax:get_pos(Other),
    {error, {Line, {unsupported_record_field, Other}}}.

add_field_def(#{name := Name} = FieldDef, #record_def{fields = Fields} = RecordDef) ->
    Fields = [Name|Fields],
    RecordDef = RecordDef#record_def{fields = Fields},
    add_field_init(FieldDef, RecordDef).

add_field_init(#{name := Name, init := Init}, #record_def{init_values = InitValues} = RecordDef) ->
    InitValues = InitValues#{Name => Init},
    RecordDef#record_def{init_values = InitValues};
add_field_init(#{}, RecordDef) ->
    RecordDef.

add_warning({Line, Reason}, #record_def{module = Module, warnings = Warnings} = RecordDef) ->
    RecordDef#record_def{warnings = [{Line, Module, Reason}|Warnings]}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
