%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_error_state).

-include_lib("astranaut/include/astranaut_struct_name.hrl").

-export_type([astranaut_error/0]).

-opaque astranaut_error() :: #{'__struct__' => ?MODULE,
                               file => file(),
                               errors => astranaut_endo:endo(error()),
                               warnings => astranaut_endo:endo(error()),
                               formatted_errors => astranaut_endo:endo(formatted_error()),
                               formatted_warnings => astranaut_endo:endo(formatted_error()),
                               file_errors => #{file() => astranaut_endo:endo(formatted_error())},
                               file_warnings => #{file() => astranaut_endo:endo(formatted_error())}
                              }.
-type file() :: string().
-type formatter() :: atom().
-type line() :: integer().
-type formatted_error() :: {line(), formatter(), error()}.
-type error() :: any().

%% API
-export([new/0, new/1, update_line/3, update_file/2, eof/1, merge/2]).
-export([realize/1, no_pending/1, is_empty/1, is_empty_error/1]).
-export([run_endo/1]).
-export([errors/1, warnings/1]).
-export([formatted_errors/1, formatted_warnings/1]).
-export([append_ews/3, append_error/2, append_warning/2, append_errors/2, append_warnings/2]).
-export([append_formatted_errors/2, append_formatted_warnings/2]).
-export([file_errors/1, file_warnings/1]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    new(undefined).

new(File) ->
    #{?STRUCT_KEY => ?MODULE, file => File,
      errors => astranaut_endo:empty(), warnings => astranaut_endo:empty(),
      formatted_errors => astranaut_endo:empty(), formatted_warnings => astranaut_endo:empty(),
      file_errors => #{}, file_warnings => #{}}.

update_line(Line, Formatter, #{?STRUCT_KEY := ?MODULE,
                               errors := Errors0, warnings := Warnings0,
                               formatted_errors := FormattedErrors0,
                               formatted_warnings := FormattedWarnings0} = AstranautError) ->
    FormattedErrors1 = format_errors(Line, Formatter, Errors0),
    FormattedWarnings1 = format_errors(Line, Formatter, Warnings0),
    FormattedErrors2 = astranaut_endo:append(FormattedErrors0, FormattedErrors1),
    FormattedWarnings2 = astranaut_endo:append(FormattedWarnings0, FormattedWarnings1),
    AstranautError#{formatted_errors => FormattedErrors2, formatted_warnings => FormattedWarnings2,
                    errors => astranaut_endo:empty(), warnings => astranaut_endo:empty()}.

update_file(File, #{?STRUCT_KEY := ?ERROR_STATE, file := undefined} = State) ->
    update_file(File, File, State);
update_file(File, #{?STRUCT_KEY := ?ERROR_STATE, file := File0} = State) ->
    update_file(File0, File, State).

eof(State) ->
    update_file(eof, State).

no_pending(#{?STRUCT_KEY := ?ERROR_STATE, errors := Errors, warnings := Warnings}) ->
    is_empty(Errors) and is_empty(Warnings).

is_empty(#{?STRUCT_KEY := ?ERROR_STATE,
           errors := Errors, warnings := Warnings,
           formatted_errors := FormattedErrors, formatted_warnings := FormattedWarnings,
           file_errors := FErrors, file_warnings := FWarnings}) 
  when (map_size(FErrors) == 0) and (map_size(FWarnings) == 0) ->
    astranaut_endo:is_empty(Errors) and astranaut_endo:is_empty(Warnings)
        and astranaut_endo:is_empty(FormattedErrors) and astranaut_endo:is_empty(FormattedWarnings);
is_empty(_ErrorState) ->
    false.

is_empty_error(#{errors := Errors,
                 formatted_errors := FormattedErrors,
                 file_errors := FErrors}) 
  when (map_size(FErrors) == 0) ->
    astranaut_endo:is_empty(Errors) and astranaut_endo:is_empty(FormattedErrors);
is_empty_error(_ErrorState) ->
    false.

realize(#{?STRUCT_KEY := ?MODULE, file_errors := FileErrors, file_warnings := FileWarnings}) ->
    {realize_errors(FileErrors), realize_errors(FileWarnings)}.

realize_errors(FileErrors) ->
    lists:map(fun({File, Errors}) -> {File, astranaut_endo:run(Errors)} end, maps:to_list(FileErrors)).

run_endo(#{?STRUCT_KEY := ?ERROR_STATE,
           errors := Errors, warnings := Warnings,
           formatted_errors := FormattedErrors, formatted_warnings := FormattedWarnings,
           file_errors := FileErrors, file_warnings := FileWarnings} = State) ->
    State#{errors => astranaut_endo:run(Errors), warnings => astranaut_endo:run(Warnings),
           formatted_errors => astranaut_endo:run(FormattedErrors), formatted_warnings => astranaut_endo:run(FormattedWarnings),
           file_errors => realize_errors(FileErrors), file_warnings => realize_errors(FileWarnings)}.

merge(#{} = State1, #{} = State2) ->
    State3 = merge_file(State1, State2),
    State4 = merge_file_ews(State3, State2),
    State5 = merge_ews(State4, State2),
    State5.


errors(#{errors := Errors}) ->
    astranaut_endo:run(Errors).

warnings(#{warnings := Warnings}) ->
    astranaut_endo:run(Warnings).

formatted_errors(#{formatted_errors := Errors}) ->
    astranaut_endo:run(Errors).

formatted_warnings(#{formatted_warnings := Warnings}) ->
    astranaut_endo:run(Warnings).

file_errors(#{file_errors := FileErrors}) ->
   realize_errors(FileErrors).

file_warnings(#{file_warnings := FileWarnings}) ->
    realize_errors(FileWarnings).

append_ews(Errors, Warnings, State0) ->
    State1 = append_errors(Errors, State0),
    State2 = append_warnings(Warnings, State1),
    State2.

append_error(Error, State) ->
    append_errors([Error], State).

append_warning(Warning, State) ->
    append_warnings([Warning], State).

append_errors(Errors1, #{errors := Errors0} = State) ->
    Errors2 = append(Errors0, Errors1),
    State#{errors => Errors2}.

append_warnings(Warnings1, #{warnings := Warnings0} = State) ->
    Warnings2 = append(Warnings0, Warnings1),
    State#{warnings => Warnings2}.

append_formatted_errors(Errors1, #{formatted_errors := Errors0} = State) ->
    Errors2 = append(Errors0, Errors1),
    State#{formatted_errors => Errors2}.

append_formatted_warnings(Warnings1, #{formatted_warnings := Warnings0} = State) ->
    Warnings2 = append(Warnings0, Warnings1),
    State#{formatted_warnings => Warnings2}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_file(undefined, eof, #{formatted_errors := Errors, formatted_warnings := Warnings} = State) ->
    case is_empty(Errors) and is_empty(Warnings) of
        true ->
            State;
        false ->
            erlang:error({match_eof_without_file, State})
    end;
update_file(File0, File1, #{?STRUCT_KEY := ?ERROR_STATE,
                            formatted_errors := Errors,
                            formatted_warnings := Warnings,
                            file_errors := ErrorsWithFile,
                            file_warnings := WarningsWithFile} = State) ->
    case File0 == File1 of
        true ->
            State#{file => File1};
        false ->
            File2 = 
                case File1 of
                    eof ->
                        undefined;
                    _ ->
                        File1
                end,
            ErrorsWithFile1 = add_file_errors(File0, Errors, ErrorsWithFile),
            WarningsWithFile1 = add_file_errors(File0, Warnings, WarningsWithFile),
            State#{formatted_errors => astranaut_endo:empty(),
                   formatted_warnings => astranaut_endo:empty(),
                   file_errors => ErrorsWithFile1, 
                   file_warnings => WarningsWithFile1,
                   file => File2}
    end.

merge_file(#{file := undefined} = State1, #{file := File}) ->
    State1#{file => File};
merge_file(#{} = State1, #{file := undefined}) ->
    State1;
merge_file(#{file := File1} = State1, #{file := File2}) ->
    update_file(File1, File2, State1).

merge_file_ews(#{file_errors := FileErrors1, file_warnings := FileWarnings1} = ErrorState,
               #{file_errors := FileErrors2, file_warnings := FileWarnings2}) ->
    FileErrors3 = merge_file_errors(FileErrors1, FileErrors2),
    FileWarnings3 = merge_file_errors(FileWarnings1, FileWarnings2),
    ErrorState#{file_errors => FileErrors3, file_warnings => FileWarnings3}.

merge_ews(#{errors := Errors1, warnings := Warnings1} = State1, 
          #{errors := Errors2, warnings := Warnings2}) ->
    Errors3 = append(Errors1, Errors2),
    Warnings3 = append(Warnings1, Warnings2),
    State1#{errors => Errors3, warnings => Warnings3}.

merge_file_errors(ErrorsWithFile1, ErrorsWithFile2) ->
    maps:fold(
      fun(File, Errors, ErrorsWithFileAcc) ->
              add_file_errors(File, Errors, ErrorsWithFileAcc)
      end, ErrorsWithFile1, ErrorsWithFile2).

add_file_errors(File, Errors1, ErrorsWithFile) ->
    case astranaut_endo:is_empty(Errors1) of
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
    astranaut_endo:append(Errors0, Errors1).

format_errors(Line, Formatter, Errors) ->
    lists:map(fun(Error) -> {Line, Formatter, Error} end, astranaut_endo:run(Errors)).
