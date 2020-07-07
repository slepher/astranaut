%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_m_error).

-export_type([astranaut_traverse_m_error/0]).
-export_type([formatter/0, line/0, file/0]).

-opaque astranaut_traverse_m_error() :: #{'__struct__' => ?MODULE,
                                          errors => [any()],
                                          warnings => [any()],
                                          line => line(),
                                          formatter => formatter(),
                                          formatted_errors => [formatted_error()],
                                          formatted_warnings => [formatted_error()],
                                          file => file(),
                                          file_errors => #{file() => formatted_error()},
                                          file_warnings => #{file() => formatted_error()}
                                         }.

-type file() :: string().
-type formatter() :: atom().
-type line() :: integer().
-type formatted_error() :: {line(), formatter(), any()}.
%% API

-export([new/0, new_file/1, new_line/2]).
-export([run/1]).
-export([is_empty/1, is_empty_error/1]).
-export([realize/1]).
-export([merge/2]).
-export([update_file/2, update_line/3, update_formatter/2]).
-export([warning/2, warnings/2, error/2, errors/2]).

%%%===================================================================
%%% API
%%%===================================================================
new_file(File) ->
    New = new(),
    New#{file => File}.

new_line(Line, Formatter) ->
    New = new(),
    New#{line => Line, formatter => Formatter}.

new() ->
    #{'__struct__' => ?MODULE,
      errors => [], warnings => [], 
      formatted_errors => [], formatted_warnings => [],
      file_errors => #{}, file_warnings => #{}}.

run(#{'__struct__' := ?MODULE, 
      errors := Errors, warnings := Warnings, 
      formatted_errors := FormattedErrors, formatted_warnings := FormattedWarnings} = State) ->
    State#{errors => lists:reverse(Errors),
           warnings => lists:reverse(Warnings),
           formatted_errors => lists:reverse(FormattedErrors),
           formatted_warnings => lists:reverse(FormattedWarnings)}.

is_empty(#{errors := [], warnings := [], 
           formatted_errors := [], formatted_errors := [], 
           file_errors := FErrors, file_warnings := FWarnings}) 
  when (map_size(FErrors) == 0) and (map_size(FWarnings) == 0) ->
    true;
is_empty(_ErrorState) ->
    false.

is_empty_error(#{errors := [],
                 formatted_errors := [],
                 file_errors := FErrors}) 
  when (map_size(FErrors) == 0) ->
    true;
is_empty_error(_ErrorState) ->
    false.

realize(#{'__struct__' := ?MODULE, file_errors := FileErrors, file_warnings := FileWarnings}) ->
    {maps:to_list(FileErrors), maps:to_list(FileWarnings)}.

update_file(File, #{file := File0} = State) ->
    update_file(File0, File, State);
update_file(File, #{} = State) ->
    update_file(File, File, State).

update_file(File0, File1, #{formatted_errors := Errors, formatted_warnings := Warnings, 
                            file_errors := ErrorsWithFile, 
                            file_warnings := WarningsWithFile} = State) ->
    case File0 == File1 of
        true ->
            State#{file => File1};
        false ->
            ErrorsWithFile1 = add_errors_file_errors(File0, Errors, ErrorsWithFile),
            WarningsWithFile1 = add_errors_file_errors(File0, Warnings, WarningsWithFile),
            State#{formatted_errors => [], formatted_warnings => [], 
                   file_errors => ErrorsWithFile1, 
                   file_warnings => WarningsWithFile1,
                   file => File1}
    end.

update_line(Line, Formatter, #{errors := Errors, warnings := Warnings} = State) ->
    State1 = State#{line => Line, formatter => Formatter, warnings => [], errors => []},
    State2 = append_warnings(Warnings, State1),
    State3 = append_errors(Errors, State2), 
    State3.

update_formatter(Formatter, #{line := _Line} = State) ->
    State#{formatter => Formatter};
update_formatter(_Formatter, #{} = State) ->
    State.


warning(Warning, State) ->
    warnings([Warning], State).
    
warnings(Warnings, State) ->
    append_warnings(lists:reverse(Warnings), State).

error(Error,  State) ->
    errors([Error], State).
    
errors(Errors, State) ->
    append_errors(lists:reverse(Errors), State).

merge(#{} = State1, #{} = State2) ->
    State3 = merge_file(State1, State2),
    State4 = merge_file_errors(State3, State2),
    State5 = merge_formatted_errors(State4, State2),
    State6 = merge_formatter(State5, State2),
    State6.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_file(#{file := File1} = State1, #{file := File2}) ->
    update_file(File1, File2, State1);
merge_file(#{} = State1, #{file := File}) ->
    State1#{file => File};
merge_file(#{} = State1, #{}) ->
    State1.

merge_file_errors(#{file_errors := FileErrors1, file_warnings := FileWarnings1} = ErrorState,
                  #{file_errors := FileErrors2, file_warnings := FileWarnings2}) ->
    FileErrors3 = merge_errors_with_file(FileErrors1, FileErrors2),
    FileWarnings3 = merge_errors_with_file(FileWarnings1, FileWarnings2),
    ErrorState#{file_errors => FileErrors3, file_warnings => FileWarnings3}.

merge_formatted_errors(#{} = State1, #{formatted_errors := Errors, formatted_warnings := Warnings}) ->
    State3 = append_formatted_warnings(Warnings, State1),
    State4 = append_formatted_errors(Errors, State3),
    State4.

merge_formatter(#{errors := Errors, warnings := Warnings} = State1, #{line := Line2, formatter := Formatter2}) ->
    State2 = State1#{line => Line2, formatter => Formatter2, errors => [], warnings => []},
    State3 = append_errors(Errors, State2),
    State4 = append_warnings(Warnings, State3),
    State4;
merge_formatter(#{} = State1, #{errors := Errors, warnings := Warnings}) ->
    State2 = append_errors(Errors, State1),
    State3 = append_warnings(Warnings, State2),
    State3.

append_warnings(Warnings, #{line := Line, formatter := Formatter} = State) ->
    Warnings1 = format_error(Warnings, Formatter, Line),
    append_formatted_warnings(Warnings1, State);
append_warnings(Warnings1, #{warnings := Warnings0} = State) ->
    Warnings2 = append(Warnings0, Warnings1),
    State#{warnings => Warnings2}.

append_formatted_warnings(Warnings1, #{formatted_warnings := Warnings0} = State) ->
    Warnings2 = append(Warnings0, Warnings1),
    State#{formatted_warnings => Warnings2}.

append_errors(Errors, #{line := Line, formatter := Formatter} = State) ->
    Errors1 = format_error(Errors, Formatter, Line),
    append_formatted_errors(Errors1, State);
append_errors(Errors1, #{errors := Errors0} = State) ->
    Errors2 = append(Errors0, Errors1),
    State#{errors => Errors2}.

append_formatted_errors(Errors1, #{formatted_errors := Errors0} = State) ->
    Errors2 = append(Errors0, Errors1),
    State#{formatted_errors => Errors2}.

format_error(Errors, Formatter, Line) ->
    lists:map(fun(Error) -> {Line, Formatter, Error} end, Errors).

merge_errors_with_file(ErrorsWithFile1, ErrorsWithFile2) ->
    maps:fold(
      fun(File, Errors, ErrorsWithFileAcc) ->
              add_errors_file_errors_1(File, Errors, ErrorsWithFileAcc)
      end, ErrorsWithFile1, ErrorsWithFile2).

add_errors_file_errors(File, Errors, ErrorsWithFile) ->
    add_errors_file_errors_1(File, lists:reverse(Errors), ErrorsWithFile).

add_errors_file_errors_1(_File, [], ErrorsWithFile) ->
    ErrorsWithFile;
add_errors_file_errors_1(File, Errors0, ErrorsWithFile) ->
    case maps:find(File, ErrorsWithFile) of
        {ok, Errors1} ->
            maps:put(File, append(Errors0, Errors1), ErrorsWithFile);
        error ->
            maps:put(File, Errors0, ErrorsWithFile)
    end.

append(Errors0, []) ->
    Errors0;
append(Errors0, Errors1) when is_list(Errors0), is_list(Errors1) ->
    Errors1 ++ Errors0.
