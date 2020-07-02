%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_traverse_error_state).

%% API
-export([new/0]).
-export([is_empty/1]).
-export([realize/1]).
-export([merge/2]).
-export([update_file/2]).
-export([warning/2, warnings/2, error/2, errors/2]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #{errors => [], warnings => [], 
      file_errors => #{}, file_warnings => #{}, 
      file => undefined, formatter => undefined, line => 0}.

is_empty(#{errors := [], warnings := [], file_errors := FErrors, file_warnings := FWarnings}) 
  when (map_size(FErrors) == 0) and (map_size(FWarnings) == 0) ->
    true;
is_empty(_ErrorState) ->
    false.

realize(#{file_errors := FileErrors, file_warnings := FileWarnings}) ->
    {maps:to_list(FileErrors), maps:to_list(FileWarnings)}.

update_file(File, #{errors := Errors, warnings := Warnings, 
                    file_errors := ErrorsWithFile, 
                    file_warnings := WarningsWithFile,
                    file := File1} = State) ->
    case File == File1 of
        true ->
            State;
        false ->
            ErrorsWithFile1 = add_errors_file_errors(File1, Errors, ErrorsWithFile),
            WarningsWithFile1 = add_errors_file_errors(File1, Warnings, WarningsWithFile),
            State#{errors => [], warnings => [], 
                   file_errors => ErrorsWithFile1, 
                   file_warnings => WarningsWithFile1,
                   file => File}
    end.

warning(Warning, State) ->
    warnings([Warning], State).
    
warnings(Warnings, State) ->
   warnings_1(lists:reverse(Warnings), State).

error(Error, State) ->
    errors([Error], State).
    
errors(Errors, State) ->
    errors_1(lists:reverse(Errors), State).

merge(#{} = ErrorState1, #{file := File, errors := Errors, warnings := Warnings} = ErrorState2) ->
    ErrorState3 = update_file(File, ErrorState1),
    ErrorState4 = merge_errors(ErrorState3, ErrorState2),
    ErrorState5 = warnings_1(Warnings, ErrorState4),
    ErrorState6 = errors_1(Errors, ErrorState5),
    ErrorState6.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_errors(#{file_errors := FileErrors1, file_warnings := FileWarnings1} = ErrorState,
             #{file_errors := FileErrors2, file_warnings := FileWarnings2}) ->
    FileErrors3 = merge_errors_with_file(FileErrors1, FileErrors2),
    FileWarnings3 = merge_errors_with_file(FileWarnings1, FileWarnings2),
    ErrorState#{file_errors => FileErrors3, file_warnings => FileWarnings3}.

warnings_1(Warnings, #{warnings := Warnings0} = State) ->
    State#{warnings => Warnings ++ Warnings0}.

errors_1(Errors, #{errors := Errors0} = State) ->
    State#{errors => Errors ++ Errors0}.

merge_errors_with_file(ErrorsWithFile1, ErrorsWithFile2) ->
    maps:fold(
      fun(File, Errors, ErrorsWithFileAcc) ->
              add_errors_file_errors_1(File, Errors, ErrorsWithFileAcc)
      end, ErrorsWithFile1, ErrorsWithFile2).

add_errors_file_errors(File, Errors, ErrorsWithFile) ->
    add_errors_file_errors_1(File, lists:reverse(Errors), ErrorsWithFile).

add_errors_file_errors_1(_File, [], ErrorsWithFile) ->
    ErrorsWithFile;
add_errors_file_errors_1(File, Errors, ErrorsWithFile) ->
    case maps:find(File, ErrorsWithFile) of
        {ok, Errors1} ->
            maps:put(File, Errors1 ++ Errors, ErrorsWithFile);
        error ->
            maps:put(File, Errors, ErrorsWithFile)
    end.
