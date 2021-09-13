%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%% struct of error created by {@link astranaut:mapfold/4}.
%%% @end
%%% Created :  9 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_error).

-include("astranaut_struct_name.hrl").

-define(ENDO, astranaut_endo).

-export_type([struct/0]).
-export_type([compiler_error/0]).
-export_type([compile_file/0]).

-type struct() :: #{'__struct__' => ?MODULE,
                    file => compile_file(),
                    errors => endo(term()),
                    warnings => endo(term()),
                    formatted_errors => endo(erl_parse:error_info()),
                    formatted_warnings => endo(erl_parse:error_info()),
                    file_errors => #{file:filename() => endo(erl_parse:error_info())},
                    file_warnings => #{file:filename() => endo(erl_parse:error_info())}
                   }.

-type compile_file() :: file:filename() | undefined | eof.
-type compiler_error() :: [{file:filename(), [erl_parse:error_info()]}].

-type endo(A) :: #{?STRUCT_KEY := ?ENDO, is_empty := boolean(), list => endo_list(A)}.
-type endo_list(A) :: fun(([A]) -> [A]).
-type list_or_endo(A) :: [A] | endo(A).

%% API
-export([new/0, new/1, update_pos/3, update_file/2, eof/1, merge/2]).
-export([realize/1, printable/1, no_pending/1, is_empty/1, is_empty_error/1]).

%% get memebers from astranaut_error:struct().
-export([file/1, errors/1, warnings/1]).
-export([formatted_errors/1, formatted_warnings/1]).
-export([file_errors/1, file_warnings/1]).
-export([with_error/2, with_warning/2, with_failure/2]).
-export([with_formatted_error/2, with_formatted_warning/2, with_formatted_failure/2]).
-export([with_formatted_base_error/2, with_formatted_base_warning/2, with_formatted_base_failure/2]).
-export([with_file_errors/2, with_file_warnings/2, with_file_failures/2]).
-export([with_file_formatted_error/2, with_file_formatted_warning/2, with_file_formatted_failure/2]).
-export([with_file_base_error/2, with_file_base_warning/2, with_file_base_failure/2]).
-export([with_all_error/2, with_all_warning/2, with_all_failure/2]).
-export([with_all_formatted_error/2, with_all_formatted_warning/2, with_all_formatted_failure/2]).
-export([with_formatted_base/2, with_file_formatted/2]).

-export([new_error/1, new_errors/1, new_warning/1, new_warnings/1]).
-export([new_formatted_error/1, new_formatted_errors/1, new_formatted_warning/1, new_formatted_warnings/1]).
%% append errors to astranaut_error:struct().
-export([append_ews/3, append_error/2, append_warning/2, append_errors/2, append_warnings/2]).
-export([append_formatted_errors/2, append_formatted_warnings/2]).
-export([append_file_errors/2, append_file_warnings/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> struct().
%% @doc initialize empty struct().
new() ->
    new(undefined).

-spec new(compile_file()) -> struct().
%% @doc initialize struct() with File.
new(File) ->
    #{?STRUCT_KEY => ?MODULE, file => File,
      errors => endo_empty(), warnings => endo_empty(),
      formatted_errors => endo_empty(), formatted_warnings => endo_empty(),
      file_errors => #{}, file_warnings => #{}}.

-spec update_pos(erl_anno:location(), module(), struct()) -> struct().
%% @doc update struct with pos number and formatter, this will convert all errors and warnings to formatted_errors and formatted_warnings.
update_pos(Pos, Formatter, #{?STRUCT_KEY := ?MODULE,
                               errors := Errors0, warnings := Warnings0,
                               formatted_errors := FormattedErrors0,
                               formatted_warnings := FormattedWarnings0} = Struct) ->
    FormattedErrors1 = format_errors(Pos, Formatter, Errors0),
    FormattedWarnings1 = format_errors(Pos, Formatter, Warnings0),
    FormattedErrors2 = endo_append(FormattedErrors0, FormattedErrors1),
    FormattedWarnings2 = endo_append(FormattedWarnings0, FormattedWarnings1),
    Struct#{formatted_errors => FormattedErrors2, formatted_warnings => FormattedWarnings2,
            errors => endo_empty(), warnings => endo_empty()}.

-spec update_file(compile_file() | eof, struct()) -> struct().
%% @doc update struct file, if file changed, will convert all formatted_errors and formatted_warnings to file_errors and file_warnings with file before, then update file.
update_file(File, #{?STRUCT_KEY := ?ERROR_STATE, file := undefined} = Struct) ->
    update_file(File, File, Struct);
update_file(File, #{?STRUCT_KEY := ?ERROR_STATE, file := File0} = Struct) ->
    update_file(File0, File, Struct).

-spec eof(struct()) -> struct().
%% @doc update file to eof
%% @see update_file/2
eof(Struct) ->
    update_file(eof, Struct).

-spec no_pending(struct()) -> boolean().
%% @doc check if errors and warnings is empty, just errors and warnings, not formatted_* and file_*.
no_pending(#{?STRUCT_KEY := ?ERROR_STATE, errors := Errors, warnings := Warnings}) ->
    endo_is_empty(Errors) and endo_is_empty(Warnings).

-spec is_empty(struct()) -> boolean().
%% @doc check if all errors and warnings is empty, include formatted_* and file_*.
is_empty(#{?STRUCT_KEY := ?ERROR_STATE,
           errors := Errors, warnings := Warnings,
           formatted_errors := FormattedErrors, formatted_warnings := FormattedWarnings,
           file_errors := FErrors, file_warnings := FWarnings}) 
  when (map_size(FErrors) =:= 0) and (map_size(FWarnings) =:= 0) ->
    endo_is_empty(Errors) and endo_is_empty(Warnings)
    and endo_is_empty(FormattedErrors) and endo_is_empty(FormattedWarnings);
is_empty(_ErrorState) ->
    false.

-spec is_empty_error(struct()) -> boolean().
%% @doc check if errors, formatted_errors, file_errors is empty.
is_empty_error(#{errors := Errors,
                 formatted_errors := FormattedErrors,
                 file_errors := FErrors})
  when (map_size(FErrors) =:= 0) ->
    endo_is_empty(Errors) and endo_is_empty(FormattedErrors);
is_empty_error(_Struct) ->
    false.

-spec realize(struct()) -> {compiler_error(), compiler_error()}.
%% @doc get file_errors and file_warnings, which could be used by compiler.
realize(#{?STRUCT_KEY := ?MODULE, file_errors := FileErrors, file_warnings := FileWarnings}) ->
    {realize_errors(FileErrors), realize_errors(FileWarnings)}.

printable(#{?STRUCT_KEY := ?MODULE} = Struct) ->
    maps:fold(
        fun(Key, FileErrors, Acc) when (Key =:= file_errors) or (Key =:= file_warnings) ->
            case maps:size(FileErrors) of
                0 ->
                    Acc;
                _ ->
                    FileErrors1 = maps:map(fun(_Key, Value) -> endo_run(Value) end, FileErrors),
                    maps:put(Key, FileErrors1, Acc)
            end;
           (Key, Errors, Acc) when (Key =:= formatted_errors) or (Key =:= formatted_warnings)
                                   or (Key =:= errors) or (Key =:= warnings) ->
               case endo_is_empty(Errors) of
                    true ->
                       Acc;
                    false ->
                        maps:put(Key, endo_run(Errors), Acc)
                end;
           (_Key, _Value, Acc) ->
               Acc
        end, #{}, Struct).

-spec merge(struct(), struct()) -> struct().
%% @doc merge two astranaut_error struct into one, first update file of Struct1 to file of Struct2, then merge all errors.
merge(#{?STRUCT_KEY := ?MODULE} = Struct1, #{?STRUCT_KEY := ?MODULE} = Struct2) ->
    Struct3 = merge_file(Struct1, Struct2),
    Struct4 = merge_file_ews(Struct3, Struct2),
    Struct5 = merge_formatted_ews(Struct4, Struct2),
    Struct6 = merge_ews(Struct5, Struct2),
    Struct6.

-spec file(struct()) -> compile_file().
%% @doc get file
file(#{?STRUCT_KEY := ?MODULE, file := File}) ->
    File.

-spec errors(struct()) -> [term()].
%% @doc get errors.
errors(#{errors := Errors}) ->
    endo_run(Errors).

-spec warnings(struct()) -> [term()].
%% @doc get warnings.
warnings(#{warnings := Warnings}) ->
    endo_run(Warnings).

-spec formatted_errors(struct()) -> [erl_parse:error_info()].
%% @doc get formatted_errors.
formatted_errors(#{formatted_errors := Errors}) ->
    endo_run(Errors).

-spec formatted_warnings(struct()) -> [erl_parse:error_info()].
%% @doc get formatted_warnings.
formatted_warnings(#{formatted_warnings := Warnings}) ->
    endo_run(Warnings).

-spec file_errors(struct()) -> compiler_error().
%% @doc get file_errors.
file_errors(#{file_errors := FileErrors}) ->
   realize_errors(FileErrors).

-spec file_warnings(Struct::struct()) -> compiler_error().
%% @doc get file_warnings.
file_warnings(#{file_warnings := FileWarnings}) ->
    realize_errors(FileWarnings).

with_error(Fun, Struct) ->
   with_key(Fun, errors, fun endo_map/2, Struct).

with_warning(Fun, Struct) ->
   with_key(Fun, warnings, fun endo_map/2, Struct).

with_failure(Fun, Struct) ->
   sequence_withs(Fun, [fun with_error/2, fun with_warning/2], Struct).

with_formatted_error(Fun, Struct) ->
   with_key(Fun, formatted_errors, fun endo_map/2, Struct).

with_formatted_warning(Fun, Struct) ->
   with_key(Fun, formatted_warnings, fun endo_map/2, Struct).

with_formatted_failure(Fun, Struct) ->
    sequence_withs(Fun, [fun with_formatted_error/2, fun with_formatted_warning/2], Struct).

with_formatted_base_error(Fun, Struct) ->
    with_key(Fun, formatted_errors, fun with_formatted_base/2, Struct).

with_formatted_base_warning(Fun, Struct) ->
    with_key(Fun, formatted_warnings, fun with_formatted_base/2, Struct).

with_formatted_base_failure(Fun, Struct) ->
    sequence_withs(Fun, [fun with_formatted_base_error/2, fun with_formatted_base_warning/2], Struct).

with_formatted(Fun, {Pos, Formatter, Error}) ->
    {Pos, Formatter, Fun(Error)}.

with_formatted_base(Fun, FormattedErrors) ->
    astranaut_lib:nested_withs(Fun, [fun with_formatted/2, fun endo_map/2], FormattedErrors).

with_file_errors(Fun, Struct) ->
    with_key(Fun, file_errors, fun with_file/2, Struct).

with_file_warnings(Fun, Struct) ->
    with_key(Fun, file_warnings, fun with_file/2, Struct).

with_file_failures(Fun, Struct) ->
    sequence_withs(Fun, [fun with_file_errors/2, fun with_file_warnings/2], Struct).

with_file_formatted_error(Fun, Struct) ->
    with_key(Fun, file_errors, fun with_file_formatted/2, Struct).

with_file_formatted_warning(Fun, Struct) ->
    with_key(Fun, file_warnings, fun with_file_formatted/2, Struct).

with_file_formatted_failure(Fun, Struct) ->
    sequence_withs(Fun, [fun with_file_formatted_error/2, fun with_file_formatted_warning/2], Struct).

with_file_base_error(Fun, Struct) ->
    with_key(Fun, file_errors, fun with_file_base/2, Struct).

with_file_base_warning(Fun, Struct) ->
    with_key(Fun, file_warnings, fun with_file_base/2, Struct).

with_file_base_failure(Fun, Struct) ->
    sequence_withs(Fun, [fun with_file_base_error/2, fun with_file_base_warning/2], Struct).

with_file(Fun, FileErrors) ->
    maps:from_list(lists:map(Fun, maps:to_list(FileErrors))).

with_file_formatted(Fun, FileErrors) ->
    astranaut_lib:nested_withs(Fun, [fun endo_map/2, fun with_map_value/2], FileErrors).

with_file_base(Fun, FileErrors) ->
    astranaut_lib:nested_withs(Fun, [fun with_formatted/2, fun with_file_formatted/2], FileErrors).

with_all_error(Fun, Struct) ->
    sequence_withs(Fun, [fun with_error/2, fun with_formatted_base_error/2, fun with_file_base_error/2], Struct).

with_all_warning(Fun, Struct) ->
    sequence_withs(
      Fun, [fun with_warning/2, fun with_formatted_base_warning/2, fun with_file_base_warning/2], Struct).

with_all_failure(Fun, Struct) ->
    sequence_withs(Fun, [fun with_all_error/2, fun with_all_warning/2], Struct).

with_all_formatted_error(Fun, Struct) ->
    sequence_withs(Fun, [fun with_formatted_error/2, fun with_file_formatted_error/2], Struct).

with_all_formatted_warning(Fun, Struct) ->
    sequence_withs(Fun, [fun with_formatted_warning/2, fun with_file_formatted_warning/2], Struct).

with_all_formatted_failure(Fun, Struct) ->
    sequence_withs(Fun, [fun with_all_formatted_error/2, fun with_all_formatted_warning/2], Struct).

with_map_value(Fun, Map) ->
    maps:map(
        fun(_Key, Value) ->
            Fun(Value)
        end, Map).

with_key(Fun, Key, With, #{?STRUCT_KEY := ?MODULE} = Struct) ->
    Errors = maps:get(Key, Struct),
    Errors1 = With(Fun, Errors),
    Struct#{Key => Errors1}.

new_error(Error) ->
    new_error([Error]).

new_errors(Errors) ->
    append_errors(Errors, new()).

new_warning(Warning) ->
    new_warnings([Warning]).

new_warnings(Warnings) ->
    append_warnings(Warnings, new()).

new_formatted_error(Error) ->
    new_formatted_errors([Error]).

new_formatted_errors(Errors) ->
    append_formatted_errors(Errors, new()).

new_formatted_warning(Warning) ->
    new_formatted_warnings([Warning]).

new_formatted_warnings(Warnings) ->
    append_formatted_warnings(Warnings, new()).

-spec append_ews(list_or_endo(term()), list_or_endo(term()), struct()) -> struct().
%% @doc append errors and warnings to struct.
append_ews(Errors, Warnings, #{} = Struct) ->
    Struct1 = append_errors(Errors, Struct),
    Struct2 = append_warnings(Warnings, Struct1),
    Struct2.

-spec append_error(term(), struct()) -> struct().
%% @doc append an error to struct.
append_error(Error, Struct) ->
    append_errors([Error], Struct).

-spec append_warning(term(), struct()) -> struct().
%% @doc append a warning to struct.
append_warning(Warning, Struct) ->
    append_warnings([Warning], Struct).

-spec append_errors(list_or_endo(term()), struct()) -> struct().
%% @doc append errors to struct.
append_errors(Errors, #{errors := Errors0} = Struct) ->
    Errors1 = append(Errors0, Errors),
    Struct#{errors => Errors1}.

-spec append_warnings(list_or_endo(term()), struct()) -> struct().
%% @doc append warnings to struct.
append_warnings(Warnings, #{warnings := Warnings0} = Struct) ->
    Warnings1 = append(Warnings0, Warnings),
    Struct#{warnings => Warnings1}.

-spec append_formatted_errors(list_or_endo(erl_parse:error_info()), struct()) -> struct().
%% @doc append formatted_errors to struct.
append_formatted_errors(Errors, #{formatted_errors := Errors0} = Struct) ->
    Errors1 = append(Errors0, Errors),
    Struct#{formatted_errors => Errors1}.

-spec append_formatted_warnings(list_or_endo(erl_parse:error_info()), struct()) -> struct().
%% @doc append formatted_warnings to struct.
append_formatted_warnings(Warnings, #{formatted_warnings := Warnings0} = Struct) ->
    Warnings1 = append(Warnings0, Warnings),
    Struct#{formatted_warnings => Warnings1}.

-spec append_file_errors([compiler_error()], struct()) -> struct().
%% @doc append file_errors to struct.
append_file_errors(FileErrors, #{file_errors := FileErrors1} = Struct) ->
    FileErrors2 = from_compiler_errors(FileErrors),
    FileErrors3 = merge_file_errors(FileErrors1, FileErrors2),
    Struct#{file_errors => FileErrors3}.

-spec append_file_warnings([compiler_error()], struct()) -> struct().
%% @doc append file_warnings to struct.
append_file_warnings(FileWarnings, #{file_warnings := FileWarnings1} = Struct) ->
    FileWarnings2 = from_compiler_errors(FileWarnings),
    FileWarnings3 = merge_file_errors(FileWarnings1, FileWarnings2),
    Struct#{file_warnings => FileWarnings3}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

realize_errors(FileErrors) ->
    lists:map(fun({File, Errors}) -> {File, endo_run(Errors)} end, maps:to_list(FileErrors)).

update_file(undefined, eof, #{formatted_errors := Errors, formatted_warnings := Warnings} = Struct) ->
    case is_empty(Errors) and is_empty(Warnings) of
        true ->
            Struct;
        false ->
            erlang:error({match_eof_without_file, Struct})
    end;
update_file(File0, File1, #{?STRUCT_KEY := ?ERROR_STATE,
                            formatted_errors := Errors,
                            formatted_warnings := Warnings,
                            file_errors := ErrorsWithFile,
                            file_warnings := WarningsWithFile} = Struct) ->
    case File0 =:= File1 of
        true ->
            Struct#{file => File1};
        false ->
            ErrorsWithFile1 = add_file_errors(File0, Errors, ErrorsWithFile),
            WarningsWithFile1 = add_file_errors(File0, Warnings, WarningsWithFile),
            Struct#{formatted_errors => endo_empty(),
                    formatted_warnings => endo_empty(),
                    file_errors => ErrorsWithFile1, 
                    file_warnings => WarningsWithFile1,
                    file => File1}
    end.

merge_file(#{file := undefined} = Struct, #{file := File}) ->
    Struct#{file => File};
merge_file(#{} = Struct, #{file := undefined}) ->
    Struct;
merge_file(#{file := File1} = Struct, #{file := File2}) ->
    update_file(File1, File2, Struct).

merge_file_ews(#{file_errors := FileErrors1, file_warnings := FileWarnings1} = Struct,
               #{file_errors := FileErrors2, file_warnings := FileWarnings2}) ->
    FileErrors3 = merge_file_errors(FileErrors1, FileErrors2),
    FileWarnings3 = merge_file_errors(FileWarnings1, FileWarnings2),
    Struct#{file_errors => FileErrors3, file_warnings => FileWarnings3}.

merge_formatted_ews(#{} = Struct1, #{formatted_errors := FormattedErrors, formatted_warnings := FormattedWarnings}) ->
    Struct2 = append_formatted_errors(FormattedErrors, Struct1),
    Struct3 = append_formatted_warnings(FormattedWarnings, Struct2),
    Struct3.

merge_ews(#{} = Struct1, #{errors := Errors, warnings := Warnings}) ->
    append_ews(Errors, Warnings, Struct1).

merge_file_errors(ErrorsWithFile1, ErrorsWithFile2) ->
    maps:fold(
      fun(File, Errors, ErrorsWithFileAcc) ->
              add_file_errors(File, Errors, ErrorsWithFileAcc)
      end, ErrorsWithFile1, ErrorsWithFile2).

 add_file_errors(File, Errors1, ErrorsWithFile) ->
    case endo_is_empty(Errors1) of
        true ->
            ErrorsWithFile;
        false ->
            case maps:find(File, ErrorsWithFile) of
                {ok, Errors0} ->
                    maps:put(File, append(Errors0, Errors1), ErrorsWithFile);
                error ->
                    maps:put(File, Errors1, ErrorsWithFile)
            end
    end.

append(Errors0, Errors1) ->
    endo_append(Errors0, Errors1).

format_errors(Pos, Formatter, Errors) ->
    lists:map(fun(Error) -> {Pos, Formatter, Error} end, endo_run(Errors)).

from_compiler_errors(CompilerFileErrors) ->
    maps:from_list(
      lists:map(
        fun({File, Errors}) ->
                {File, endo(Errors)}
        end, CompilerFileErrors)).

-spec endo_empty() -> endo(_A).
endo_empty() ->
    #{?STRUCT_KEY => ?ENDO, is_empty => true}.

-spec endo([A]) -> endo(A).
endo([]) ->
    endo_empty();
endo(List) when is_list(List) ->
    endo_new(fun(List1) -> List ++ List1 end).

-spec endo_run(endo(A)) -> [A].
endo_run(#{?STRUCT_KEY := ?ENDO, is_empty := true}) ->
    [];
endo_run(#{?STRUCT_KEY := ?ENDO, list := EndoList}) ->
    EndoList([]).

-spec endo_map(fun((A) -> A), endo(A)) -> endo(A).
endo_map(_Fun, #{?STRUCT_KEY := ?ENDO, is_empty := true} = Endo) ->
    Endo;
endo_map(Fun, #{?STRUCT_KEY := ?ENDO, list := EndoList}) ->
    endo_new(fun(List) -> lists:map(Fun, EndoList(List)) end).

-spec endo_append(endo(A) | [A], endo(A) | [A]) -> endo(A).
endo_append(List1, List2) when is_list(List1) ->
    endo_append(endo(List1), List2);
endo_append(List1, List2) when is_list(List2) ->
    endo_append(List1, endo(List2));
endo_append(#{?STRUCT_KEY := ?ENDO} = Endo1, #{?STRUCT_KEY := ?ENDO} = Endo2) ->
    endo_do_append(Endo1, Endo2).

-spec endo_is_empty(endo(_A)) -> boolean().
endo_is_empty(#{?STRUCT_KEY := ?ENDO, is_empty := IsEmpty}) ->
    IsEmpty.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec endo_new(endo_list(A)) -> endo(A).
endo_new(Inner) when is_function(Inner, 1) ->
    #{?STRUCT_KEY => ?ENDO, list => Inner, is_empty => false}.

-spec endo_do_append(endo(A), endo(A)) -> endo(A).
endo_do_append(#{is_empty := true}, #{} = Endo2) ->
    Endo2;
endo_do_append(#{} = Endo1, #{is_empty := true}) ->
    Endo1;
endo_do_append(#{list := EndoList1}, #{list := EndoList2}) ->
    endo_new(fun(List) -> EndoList1(EndoList2(List)) end).

sequence_withs(Fun, Withs, Data) ->
    lists:foldl(fun(With, DataAcc) -> With(Fun, DataAcc) end, Data, Withs).
