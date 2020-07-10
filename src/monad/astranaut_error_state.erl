%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_error_state).

-export_type([astranaut_error/0]).

-opaque astranaut_error() :: #{'__struct__' => ?MODULE,
                               file => file(),
                               errors => [formatted_error()],
                               warnings => [formatted_error()],
                               file_errors => #{file() => formatted_error()},
                               file_warnings => #{file() => formatted_error()}
                              }.
-type file() :: string().
-type formatter() :: atom().
-type line() :: integer().
-type formatted_error() :: {line(), formatter(), any()}.

%% API
-export([new/0, new/1, update_ctx/4, update_file/2, eof/1, merge/2]).
-export([realize/1, is_empty/1, is_empty_error/1]).
-export([errors/1, warnings/1]).
-export([append_errors/2, append_warnings/2]).
-export([file_errors/1, file_warnings/1]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    new(astranaut_error_ctx:new()).

new(Ctx) ->
    File = astranaut_error_ctx:file(Ctx),
    #{'__struct__' => ?MODULE, file => File, 
      errors => [], warnings => [],
      file_errors => #{}, file_warnings => #{}}.

update_ctx(Line, Formatter, Ctx, #{errors := Errors0, warnings := Warnings0} = AstranautError) ->
    Errors = astranaut_error_ctx:errors(Ctx),
    Warnings = astranaut_error_ctx:warnings(Ctx),
    Errors1 = append(Line, Formatter, Errors, Errors0),
    Warnings1 = append(Line, Formatter, Warnings, Warnings0),
    AstranautError#{errors => Errors1, warnings => Warnings1}.

update_file(File, #{file := undefined} = State) ->
    update_file(File, File, State);
update_file(File, #{file := File0} = State) ->
    update_file(File0, File, State).

eof(State) ->
    update_file(undefined, State).

is_empty(#{errors := [], warnings := [], 
           file_errors := FErrors, file_warnings := FWarnings}) 
  when (map_size(FErrors) == 0) and (map_size(FWarnings) == 0) ->
    true;
is_empty(_ErrorState) ->
    false.

is_empty_error(#{errors := [],
                 file_errors := FErrors}) 
  when (map_size(FErrors) == 0) ->
    true;
is_empty_error(_ErrorState) ->
    false.

realize(#{'__struct__' := ?MODULE, file_errors := FileErrors, file_warnings := FileWarnings}) ->
    {maps:to_list(FileErrors), maps:to_list(FileWarnings)}.

merge(#{} = State1, #{} = State2) ->
    State3 = merge_file(State1, State2),
    State4 = merge_file_ews(State3, State2),
    State5 = merge_ews(State4, State2),
    State5.    

errors(#{errors := Errors}) ->
    lists:reverse(Errors).

warnings(#{warnings := Warnings}) ->
    lists:reverse(Warnings).

file_errors(#{file_errors := FileErrors}) ->
    maps:to_list(FileErrors).

file_warnings(#{file_warnings := FileWarnings}) ->
    maps:to_list(FileWarnings).

append_errors(Errors1, #{errors := Errors0} = State) ->
    Errors2 = append(Errors0, Errors1),
    State#{errors => Errors2}.

append_warnings(Warnings1, #{errors := Warnings0} = State) ->
    Warnings2 = append(Warnings0, Warnings1),
    State#{warnings2 => Warnings2}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_file(File0, File1, #{errors := Errors, warnings := Warnings, 
                            file_errors := ErrorsWithFile, 
                            file_warnings := WarningsWithFile} = State) ->
    case File0 == File1 of
        true ->
            State#{file => File1};
        false ->
            ErrorsWithFile1 = add_file_errors(File0, Errors, ErrorsWithFile),
            WarningsWithFile1 = add_file_errors(File0, Warnings, WarningsWithFile),
            State#{errors => [], warnings => [], 
                   file_errors => ErrorsWithFile1, 
                   file_warnings => WarningsWithFile1,
                   file => File1}
    end.

merge_file(#{file := File1} = State1, #{file := File2}) ->
    update_file(File1, File2, State1);
merge_file(#{} = State1, #{file := File}) ->
    State1#{file => File};
merge_file(#{} = State1, #{}) ->
    State1.

merge_file_ews(#{file_errors := FileErrors1, file_warnings := FileWarnings1} = ErrorState,
               #{file_errors := FileErrors2, file_warnings := FileWarnings2}) ->
    FileErrors3 = merge_file_errors(FileErrors1, FileErrors2),
    FileWarnings3 = merge_file_errors(FileWarnings1, FileWarnings2),
    ErrorState#{file_errors => FileErrors3, file_warnings => FileWarnings3}.

merge_ews(#{errors := Errors1, warnings := Warnings1} = State1, 
          #{errors := Errors2, warnings := Warnings2}) ->
    Errors3 = append(Errors1, lists:reverse(Errors2)),
    Warnings3 = append(Warnings1, lists:reverse(Warnings2)),
    State1#{errors => Errors3, warnings => Warnings3}.

merge_file_errors(ErrorsWithFile1, ErrorsWithFile2) ->
    maps:fold(
      fun(File, Errors, ErrorsWithFileAcc) ->
              add_file_errors_1(File, Errors, ErrorsWithFileAcc)
      end, ErrorsWithFile1, ErrorsWithFile2).

add_file_errors(File, Errors, ErrorsWithFile) ->
    add_file_errors_1(File, lists:reverse(Errors), ErrorsWithFile).

add_file_errors_1(_File, [], ErrorsWithFile) ->
    ErrorsWithFile;
add_file_errors_1(File, Errors1, ErrorsWithFile) ->
    case maps:find(File, ErrorsWithFile) of
        {ok, Errors0} ->
            maps:put(File, Errors0 ++ Errors1, ErrorsWithFile);
        error ->
            maps:put(File, Errors1, ErrorsWithFile)
    end.

append(Errors0, Errors1) ->
    append_fun(fun(Error) -> Error end, Errors0, Errors1).

append(Line, Formatter, Errors1, Errors0) ->
    append_fun(fun(Error) -> {Line, Formatter, Error} end, Errors0, Errors1). 

append_fun(Fun, Errors0, Errors1) ->
    lists:foldl(
      fun(Error, Acc) ->
              [Fun(Error)|Acc]
      end, Errors0, Errors1).
