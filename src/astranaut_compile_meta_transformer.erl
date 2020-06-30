%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_compile_meta_transformer).

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
            case meta_options(Forms1) of
                {ok, MetaOpts, ErrorState} ->
                    {_Errors, Warnings} = astranaut_traverse_error_state:realize(ErrorState),
                    Forms2 = compile(Forms1, Forms, Opts, MetaOpts),
                    append_warnings(Forms2, Warnings);
                MetaOpts ->
                    compile(Forms1, Forms, Opts, MetaOpts)
            end;
        {error, Reason} ->
            {error, Reason}
    end.
    
format_error(Reason) ->
    Reason.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% parse options functions
%%%===================================================================
meta_options(Forms) ->
    astranaut_traverse:with_attributes(
      fun(Opts, Acc) ->
              {Opts1, Warnings} = astranaut:validate_options(fun compile_meta_options_validator/2, Opts),
              Warnings1 = astranaut:update_option_warnings(astranaut_compile_meta, Warnings),
              Acc1 = maps:merge(Acc, Opts1),
              case Warnings of
                  [] ->
                      {ok, Acc1};
                  _ ->
                      {warnings, Acc1, Warnings1}
              end
      end, maps:new(), astranaut_compile_meta, Forms, #{formatter => astranaut_traverse}).

compile_meta_options_validator(silent_warning, Boolean) when is_boolean(Boolean) ->
    ok;
compile_meta_options_validator(silent_error, Boolean) when is_boolean(Boolean)  ->
    ok;
compile_meta_options_validator(silent, Boolean) when is_boolean(Boolean) ->
    ok;
compile_meta_options_validator(errors_export, ErrorsExport) when is_atom(ErrorsExport) ->
    ok;
compile_meta_options_validator(warnings_export, WarningsExport) when is_atom(WarningsExport) ->
    ok;
compile_meta_options_validator(forms_export, FormsExport) when is_atom(FormsExport) ->
    ok;
compile_meta_options_validator(_Other, _) ->
    error.

silient_error(#{slient_error := Silent}) ->
    Silent;
silient_error(#{silent := Silent}) ->
    Silent;
silient_error(#{}) ->
    false.

silient_warning(#{slient_warning := Silent}) ->
    Silent;
silient_warning(#{silent := Silent}) ->
    Silent;
silient_warning(#{}) ->
    false.

%%%===================================================================
%%% parse file functions from Erlang Otp lib/compiler/src/compile.erl
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
                            Es = [{File,[{Line, compile, reparsing_invalid_unicode}]}],
                            {error, Es, []};
			_ ->
			    {ok, Forms}
		    end
	    end;
	{error,E} ->
	    Es = [{File,[{none,compile,{epp,E}}]}],
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
compile(OriginForms, Forms, Opts, MetaOpts) ->
    OriginForms1 = remove_compile_meta_transformer(OriginForms),
    case compile:forms(OriginForms1, Opts) of
        {error, Errors, Warnings} ->
            Forms1 = append_forms([], OriginForms, Errors, Warnings, MetaOpts),
            report_warnings(Forms1, Errors, Warnings, MetaOpts);
        {ok, _Module, _Beam, Warnings} ->
            Forms1 = append_forms(Forms, OriginForms, [], Warnings, MetaOpts),
            report_warnings(Forms1, [], Warnings, MetaOpts)
    end.

remove_compile_meta_transformer(Forms) ->
    lists:filter(
      fun({attribute, _Line, compile, {parse_transform, ?MODULE}}) ->
              false;
         (_Node) ->
              true
      end, Forms).

append_forms(Forms, OrigForms, Errors, Warnings, Opts) ->
    BaseForms = base_forms(Forms, OrigForms),
    [Eof|BaseForms1] = lists:reverse(BaseForms),
    BaseForms2 = lists:reverse(BaseForms1),
    MetaForms = meta_forms(OrigForms, Errors, Warnings, Opts),
    astranaut:reorder_attributes(
      BaseForms2 ++ MetaForms ++ [Eof], #{docks => [file, module, export], spec_as_fun => true, dock_spec => true}).

base_forms([], OrigForms) ->
    [Eof|_T] = lists:reverse(OrigForms),
    [FileAttr|_T0] = astranaut:attribute_nodes(file, OrigForms),
    [ModuleAttr|_T1] = astranaut:attribute_nodes(module, OrigForms),
    [FileAttr, ModuleAttr, Eof];
base_forms(Forms, _OrigForms) ->
    Forms.

meta_forms(Forms, Errors, Warnings, Opts) ->
    FormsExport = maps:get(forms_export, Opts, forms),
    ErrorsExport = maps:get(errors_export, Opts, errors),
    WarningsExport = maps:get(warnings_export, Opts, warnings),
    FormsForms = meta_fun_forms(FormsExport, Forms),
    ErrorsForms = meta_fun_forms(ErrorsExport, Errors),
    WarningsForms = meta_fun_forms(WarningsExport, Warnings),
    FormsForms ++ ErrorsForms ++ WarningsForms.

meta_fun_forms(MetaFun, Forms) ->
    astranaut:exported_function(
      MetaFun,
      quote(
        fun() ->
                unquote(astranaut:abstract(Forms))
        end)).

report_warnings(Forms, Errors, Warnings, Opts) ->
    Errors1 = errors(Errors, Opts),
    Warnings1 = warnings(Warnings, Opts),
    report_warnings(Forms, Errors1 ++ Warnings1).

report_warnings(Forms, []) ->
    Forms;
report_warnings(Forms, Warnings) ->
    {warning, Forms, Warnings}.

errors(Errors, Opts) ->
    case silient_error(Opts) of
        true ->
            [];
        false ->
            Errors
    end.

warnings(Warnings, Opts) ->
    case silient_warning(Opts) of
        true ->
            [];
        false ->
            Warnings
    end.

append_warnings({warning, Forms, Warings}, Warnings0) ->
    {warning, Forms, Warnings0 ++ Warings};
append_warnings(Forms, []) when is_list(Forms) ->
    Forms;
append_warnings(Forms, Warnings) when is_list(Forms) ->
    {warning, Forms, Warnings}.

