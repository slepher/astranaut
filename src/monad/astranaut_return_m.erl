%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_return_m).

-include("astranaut_struct_name.hrl").

-export_type([astranaut_return_m/1]).

-opaque astranaut_return_m(A) :: 
          #{?STRUCT_KEY => ?RETURN_OK,
            return => A,
            error => astranaut_error_state:new()} |
          #{?STRUCT_KEY => ?RETURN_FAIL,
            error => astranaut_error_state:new()}.

%% API
-export([return_ok/1, return_ok/2, return_fail/1]).
-export([from_compiler/1]).
-export([to_monad/1]).
-export([to_compiler/1]).
-export([simplify/1]).
-export([bind/2, return/1]).
-export(['>>='/3, return/2]).
-export([with_error/2]).

%%%===================================================================
%%% API
%%%===================================================================
return_ok(Return) ->
    return_ok(Return, astranaut_error_state:new()).

return_ok(Return, #{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    #{?STRUCT_KEY => ?RETURN_OK, return => Return, error => Error}.

return_fail(#{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    #{?STRUCT_KEY => ?RETURN_FAIL, error => Error}.

from_compiler(Forms) when is_list(Forms) ->
    {ok, return_ok(Forms)};
from_compiler({warning, Forms, Warnings}) ->
    Error = astranaut_error_state:new(),
    Error1 = Error#{file_warnings => from_compiler_errors(Warnings)},
    {ok, return_ok(Forms, Error1)};
from_compiler({error, Errors, Warnings}) ->
    Error = astranaut_error_state:new(),
    Error1 = Error#{file_warnings => from_compiler_errors(Warnings), file_errors => from_compiler_errors(Errors)},
    {ok, return_fail(Error1)};
from_compiler(_Other) ->
    error.

from_compiler_errors(CompilerFileErrors) ->
    maps:from_list(
      lists:map(
        fun({File, Errors}) ->
                {File, astranaut_endo:endo(Errors)}
        end, CompilerFileErrors)).

from_return(#{?STRUCT_KEY := ?RETURN_OK} = MA) ->
    {ok, MA};
from_return(#{?STRUCT_KEY := ?RETURN_FAIL} = MA) ->
    {ok, MA};
from_return(_) ->
    error.

to_monad(A) ->
    try_concerete(A, [fun from_return/1, fun from_compiler/1]).

to_compiler(#{?STRUCT_KEY := ?RETURN_OK, return := Forms, error := Error}) ->
    case astranaut_error_state:realize(Error) of
        {[], []} ->
            Forms;
        {[], Warnings} ->
            {warning, Forms, Warnings};
        {Errors, Warnings} ->
            {error, Errors, Warnings}
    end;
to_compiler(#{?STRUCT_KEY := ?RETURN_FAIL, error := Error}) ->
    {Errors, Warnings} = astranaut_error_state:realize(Error),
    {error, Errors, Warnings}.

simplify(#{?STRUCT_KEY := ?RETURN_OK, return := Return, error := Error} = MA) ->
    case astranaut_error_state:is_empty(Error) of
        true ->
            Return;
        false ->
            MA
    end;
simplify(MA) ->
    MA.

try_concerete(A, [Convert|T]) ->
    case Convert(A) of
        {ok, B} ->
            B;
        error ->
            try_concerete(A, T)
    end;
try_concerete(A, []) ->
    erlang:error({incompatable_value, A}).

bind(MA, KMB) ->
    MA1 = to_monad(MA),
    bind_1(MA1, KMB).

return(A) ->
    return_ok(A).

'>>='(MA, KMB, ?MODULE) ->
    bind(MA, KMB).

return(A, ?MODULE) ->
    return(A).

with_error(F, #{?STRUCT_KEY := ?RETURN_OK, error := Error} = MA) ->
    Error1 = F(Error),
    MA#{error => Error1};
with_error(F, #{?STRUCT_KEY := ?RETURN_FAIL, error := Error} = MA) ->
    Error1 = F(Error), 
    MA#{error => Error1}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
bind_1(#{?STRUCT_KEY := ?RETURN_OK, return := A, error := Error0}, KMB) ->
    #{error := Error1} = MB = to_monad(KMB(A)),
    Error2 = astranaut_error_state:merge(Error0, Error1),
    MB#{error => Error2};
bind_1(#{?STRUCT_KEY := ?RETURN_FAIL} = MA, _KMB) ->
    MA.
