%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_import_module).

%% API
-export([parse_transform/2]).
-export([format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    case astranaut:attributes_with_line(import_module, Forms) of
        [{Line, ImportedModule}] ->
            case abstract_code(ImportedModule) of
                {ok, Forms1} ->
                    merge_forms(Forms, Forms1);
                {error, Reason} ->
                    {error, {Line, ?MODULE, Reason}}
            end; 
        [] ->
            Forms
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
abstract_code(ImportedModule) ->
    Beam = code:which(ImportedModule),
    case beam_lib:chunks(Beam, [debug_info]) of
        {ok, {ImportedModule, [{debug_info, {debug_info_v1, erl_abstract_code, {AbstractCode, _CompileOpts}}}]}} ->
            {ok, AbstractCode};
        {error, Reason} ->
            {error, Reason};
        {ok, Other} ->
            {error, {non_debug_info, Other}}
    end.

merge_forms(Forms, Forms1) ->
    Forms2 = 
        lists:filter(
          fun({eof, _Line}) ->
                  false;
             ({attribute, _Line, import_module, _Attr}) ->
                  false;
             (_Node) ->
                  true
          end, Forms),
    Forms3 = 
        lists:filter(
          fun({attribute, _Line, module, _Module}) -> 
                  false;
             (_Node) ->
                  true
          end, Forms1),
    Forms2 ++ Forms3.
    
