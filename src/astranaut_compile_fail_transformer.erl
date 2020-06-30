%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_compile_fail_transformer).

-include("quote.hrl").

%% API
-export([parse_transform/2]).
-export([format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, Opts) ->
    File = astranaut:file(Forms),
    case parse_file(File, Opts) of
        {ok, Forms1} ->
            compile(Forms1, Opts);
        {error, Reason} ->
            {error, Reason}
    end.
    
format_error(Reason) ->
    Reason.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% parse file functions
%%%===================================================================
parse_file(File, Opts) ->
    Dir = filename:dirname(File),
    SourceName0 = proplists:get_value(source, Opts, File),
    SourceName = case lists:member(deterministic, Opts) of
                     true -> filename:basename(SourceName0);
                     false -> SourceName0
                 end,
    case epp:parse_file(File,
                        [{includes,[".",Dir|inc_paths(Opts)]},
                         {source_name, SourceName},
                         {macros,pre_defs(Opts)},
                         {default_encoding, utf8},
                         extra]) of
	{ok,Forms,Extra} ->
	    Encoding = proplists:get_value(encoding, Extra),
	    case find_invalid_unicode(Forms, File) of
		none ->
		    {ok,Forms};
		{invalid_unicode, File, Line} ->
		    case Encoding of
			none ->
                            Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
                            {error, Es, []};
			_ ->
			    {ok, Forms}
		    end
	    end;
	{error,E} ->
	    Es = [{File,[{none,?MODULE,{epp,E}}]}],
	    {error,Es, []}
    end.

find_invalid_unicode([H|T], File0) ->
    case H of
	{attribute,_,file,{File,_}} ->
	    find_invalid_unicode(T, File);
	{error,{Line,file_io_server,invalid_unicode}} ->
	    {invalid_unicode,File0,Line};
	_Other ->
	    find_invalid_unicode(T, File0)
    end;
find_invalid_unicode([], _) -> none.
            

inc_paths(Opts) ->
    [ P || {i,P} <- Opts, is_list(P) ].

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

%%%===================================================================
%%% compile form functions
%%%===================================================================


compile(Forms, Opts) ->
    Forms1 =
        lists:filter(
          fun({attribute, _Line, compile, {parse_transform, ?MODULE}}) ->
                  false;
             (_Node) ->
                  true
          end, Forms),
    CompileFailOpts = lists:flatten(astranaut:attributes(astranaut_compile_fail, Forms)),
    case compile:forms(Forms1, Opts) of
        {error, Errors, Warnings} ->
            Forms2 = forms(Forms, Errors, Warnings),
            report_warnings(Forms2, Errors ++  Warnings, CompileFailOpts);
        {ok, _Module, _Beam, Warnings} ->
            Forms2 = forms(Forms, [], Warnings),
            report_warnings(Forms2, Warnings, CompileFailOpts)
    end.

forms(Forms, Errors, Warnings) ->
    Eof = lists:nth(length(Forms), Forms),
    Forms0 = 
        lists:filter(
          fun({attribute, _Line, module, _}) ->
                  true;
             ({attribute, _Line, file, _}) ->
                  true;
             (_Node) ->
                  false
          end, Forms),
    Forms1 =
        astranaut:exported_function(
          forms,
          quote(
            fun() ->
                    unquote(astranaut:abstract(Forms))
            end)),
    Forms2 = 
        astranaut:exported_function(
          errors,
          quote(
            fun() ->
                    unquote(astranaut_quote:quote(Errors))
            end)),
    Forms3 = 
        astranaut:exported_function(
          warnings,
          quote(
            fun() ->
                    unquote(astranaut_quote:quote(Warnings))
            end)),
    Forms0 ++ Forms1 ++ Forms2 ++ Forms3 ++ [Eof].

report_warnings(Forms, [], _Opts) ->
    Forms;
report_warnings(Forms, Warnings, Opts) ->
    KeepSilient = proplists:get_value(keep_silent, Opts, false),
    case KeepSilient of
        true ->
            Forms;
        false ->
            {warning, Forms, Warnings}
    end.
