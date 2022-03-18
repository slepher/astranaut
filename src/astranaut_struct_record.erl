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
-export([record_def/3]).
-export([fields/1, init_values/1]).
-export([types/1, warnings/1, enforce_keys/1]).
-export([mandatory_field/2, auto_fill/1, set_auto_fill/2]).
-export([filled_init_values/1, update_init_values/1, update_enforce_keys/2]).

-record(record_def, {name :: atom(), 
                     module :: module(), 
                     line :: integer(),
                     fields = [] :: fields(),
                     init_values = maps:new() :: init_values(),
                     types = maps:new() :: types(),
                     enforce_keys = [] :: enforce_keys(),
                     auto_fill = true :: boolean(),
                     warnings = [] :: warnings()}).

-type fields() :: [atom()].
-type init_values() :: #{atom() => term()}.
-type types() :: #{atom() => term()}.
-type enforce_keys() :: [atom()].
-type warning() :: {module(), integer(), term()}.
-type warnings() :: [warning()].

%%%===================================================================
%%% API
%%%===================================================================
-spec record_def(module(), {atom(), [term()]}, integer()) -> #record_def{}.
record_def(Module, {RecordName, Fields}, Line) ->
    RecordDef = #record_def{module = Module, line = Line, name = RecordName},
    add_fields(Fields, RecordDef).

-spec fields(#record_def{}) -> fields().
fields(#record_def{fields = Fields}) ->
    Fields.

-spec init_values(#record_def{}) -> init_values().
init_values(#record_def{init_values = InitValues}) ->
    InitValues.

-spec enforce_keys(#record_def{}) -> enforce_keys().
enforce_keys(#record_def{enforce_keys = EnforceKeys}) ->
    EnforceKeys.

-spec types(#record_def{}) -> types().
types(#record_def{types = Types}) ->
    Types.

-spec warnings(#record_def{}) -> warnings().
warnings(#record_def{warnings = Warnings}) ->
    Warnings.

mandatory_field(FieldName, #record_def{auto_fill = AutoFill, enforce_keys = EnforceKeys}) ->
    AutoFill or lists:member(FieldName, EnforceKeys).

-spec auto_fill(#record_def{}) -> boolean().
auto_fill(#record_def{auto_fill = AutoFill}) ->
    AutoFill.

-spec set_auto_fill(boolean(), #record_def{}) -> #record_def{}.
set_auto_fill(AutoFill, #record_def{} = RefordDef) when is_boolean(AutoFill)  ->
    RefordDef#record_def{auto_fill = AutoFill}.

-spec filled_init_values(#record_def{}) -> init_values().
filled_init_values(#record_def{fields = Fields, init_values = InitValues, auto_fill = true}) ->
    lists:foldl(
      fun(Field, Acc) ->
              case maps:find(Field, InitValues) of
                  {ok, InitValue} ->
                      maps:put(Field, InitValue, Acc);
                  error ->
                      maps:put(Field, {atom, 0, undefined}, Acc)
              end
      end, maps:new(), Fields);
filled_init_values(#record_def{} = RecordDef) ->
    init_values(RecordDef).

-spec update_init_values(#record_def{}) -> #record_def{}.
update_init_values(#record_def{} = RecordDef) ->
    InitValues1 = filled_init_values(RecordDef),
    RecordDef#record_def{init_values = InitValues1}.

-spec update_enforce_keys([atom()],#record_def{}) -> #record_def{}.
update_enforce_keys(EnforceKeys,
                    #record_def{name = Name, fields = Fields, init_values = InitValues, warnings = Warnings} = RecordDef) ->
    UndefinedKeys = EnforceKeys -- Fields,
    case UndefinedKeys of
        [] ->
            InitedKeys = maps:keys(InitValues),
            EnforceKeys1 = EnforceKeys -- InitedKeys,
            RecordDef#record_def{enforce_keys = EnforceKeys1};
        _ ->
            EnforceKeys1 = EnforceKeys -- UndefinedKeys,
            Error = {enforce_keys_not_in_struct, Name, UndefinedKeys},
            Warnings1 = Warnings ++ [Error],
            RecordDef#record_def{enforce_keys = EnforceKeys1, warnings = Warnings1}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
