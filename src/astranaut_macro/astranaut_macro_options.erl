%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_options).

%% API
-export([add/7]).

%%%===================================================================
%%% API
%%%===================================================================
add(MFA, Options, LocalModule, File, Line, Forms, {AllMacros, ModuleMacroOptions, Warnings}) ->
    {Options1, Warnings1} = validate_options(Options, Line, Warnings),
    {MacroOptions, ModuleMacroOptions1, Warnings2} =
        exported_macro_options(MFA, LocalModule, Line, Forms, ModuleMacroOptions, Warnings1),
    case kmfa_options(Options1, MFA, LocalModule) of
        {ok, Options2} ->
            Options3 = merge_macro_options(MacroOptions, Options2),
            Options4 = update_alias(Options3),
            Options5 = Options4#{local_module => LocalModule, file => File, line => Line},
            Options6 = merge_attrs(Options5, Forms),
            Options7 = formatter_opts(Options6),
            case validate_formatter(Options7, Forms) of
                ok ->
                    {[Options7|AllMacros], ModuleMacroOptions1, Warnings2};
                {error, Reason} ->
                    Options8 = Options7#{formatter => astranaut_macro},
                    {[Options8|AllMacros], ModuleMacroOptions1, [{Line, astranaut_macro, Reason}|Warnings2]}
            end;
        {error, Reason} ->
            Warnings3 = [{Line, astranaut_macro, Reason}|Warnings2],
            {AllMacros, ModuleMacroOptions1, Warnings3}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

      
%%%===================================================================
%%% Internal functions
%%%===================================================================

merge_macro_options(MacroOptions, Options) ->
    WithKeys = [formatter, attrs, order, as_attr, merge_function, auto_export, group_args, debug, debug_ast],
    maps:merge(maps:with(WithKeys, MacroOptions), Options).

exported_macro_options({Function, Arity}, LocalModule, Line, Forms, ModuleMacroOptions, Warnings) ->
    exported_macro_options(
      fun() ->
              astranaut:attributes(export_macro, Forms)
      end, LocalModule, Function, Arity, Line, ModuleMacroOptions, Warnings);
exported_macro_options({Module, Function, Arity}, _LocalModule, Line, _Forms, ModuleMacroOptions, Warnings) ->
    exported_macro_options(
      fun() ->
              astranaut:module_attributes(export_macro, Module)
      end, Module, Function, Arity, Line, ModuleMacroOptions, Warnings).

exported_macro_options(GetAttributes, Module, Function, Arity, Line, ModuleMacroOptions, Warnings) ->
    case maps:find(Module, ModuleMacroOptions) of
        {ok, MacroOptions} ->
            Options = maps:get({Function, Arity}, MacroOptions, maps:new()),
            {Options, ModuleMacroOptions, Warnings};
        error ->
            Attributes = GetAttributes(),
            {MacroOptions, Warnings1} = macro_attributes(Attributes, Line),
            Options = maps:get({Function, Arity}, MacroOptions, maps:new()),
            {Options, maps:put(Module, MacroOptions, ModuleMacroOptions), Warnings ++ Warnings1}
    end.

macro_attributes(Attributes, Line) ->
    lists:foldl(
      fun({Functions, Options}, {OptionsAcc, WarningsAcc}) ->
              {Options1, WarningsAcc1} = validate_options(Options, Line, WarningsAcc),
             OptionsAcc2 = 
                  lists:foldl(
                    fun({Function, Arity}, OptionsAcc1) ->
                            maps:put({Function, Arity}, Options1, OptionsAcc1)
                    end, OptionsAcc, Functions),
              {OptionsAcc2, WarningsAcc1}
      end, {maps:new(), []}, lists:flatten(Attributes)).

kmfa_options(Options, {Function, Arity}, LocalModule) ->
    Module = astranaut_macro_local:module(LocalModule),
    Options1 = Options#{key => Function, module => Module, function => Function, arity => Arity, local => true},
    {ok, Options1};
kmfa_options(Options, {Module, Function, Arity}, _LocalModule) ->
    Options1 = Options#{key => {Module, Function}, module => Module, function => Function, arity => Arity, local => false},
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({Function, Arity}, Exports) of
                true ->
                    {ok, Options1};
                false ->
                    {error, {unexported_macro, Module, Function, Arity}}
            end;
        {error, undef} ->
            {error, {unloaded_macro_module, Module}}
    end.

merge_attrs(#{attrs := true} = Options, Forms) ->
    merge_attrs(Options#{attrs => []}, Forms);
merge_attrs(#{attrs := Attr} = Options, Forms) when is_atom(Attr) ->
    merge_attrs(Options#{attrs => [Attr]}, Forms);
merge_attrs(#{attrs := Attrs, local_module := Module, file := File} = Opts, Forms) when is_list(Attrs) ->
    AttributesMap = 
        lists:foldl(
          fun(module, Acc) ->
                  Acc;
             (file, Acc) ->
                  Acc;
             (line, Acc) ->
                  Acc;
             (Attr, Acc) ->
                  Attributes = astranaut:attributes(Attr, Forms),
                  maps:put(Attr, Attributes, Acc)
          end, maps:new(), Attrs),
    Opts#{attributes => maps:merge(#{module => Module, file => File}, AttributesMap)};
merge_attrs(#{} = Opts, _Forms) ->
    Opts.

formatter_opts(#{formatter := Module, local_module := Module} = Options) ->
    Options#{formatter => astranaut_macro_local:module(Module), local_formatter => true};
formatter_opts(#{formatter := true, module := Module, local := true} = Options) ->
    Options#{formatter => Module, local_formatter => true};
formatter_opts(#{formatter := true, module := Module} = Options) ->
    Options#{formatter => Module};
formatter_opts(#{formatter := Formatter} = Options) ->
    Options#{formatter => Formatter};
formatter_opts(#{} = Options) ->
    Options#{formatter => astranaut_traverse}.

validate_formatter(#{local_formatter := true, local_module := Module}, Forms) ->
    Exports = astranaut:attributes(export, Forms),
    case lists:member({format_error, 1}, lists:flatten(Exports)) of
        true ->
            ok;
        false ->
            {error, {non_exported_formatter, Module}}
    end;
validate_formatter(#{formatter := Module}, _Forms) ->
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({format_error, 1}, Exports) of
                true ->
                    ok;
                false ->
                    {error, {non_exported_formatter, Module}}
            end;
        {error, undef} ->
            {error, {unloaded_formatter_module, Module}}
    end.
update_alias(#{alias := true, function := Function} = Options) ->
    Options#{macro => Function};
update_alias(#{alias := Alias} = Options) ->
    Options#{macro => Alias};
update_alias(#{key := Key} = Options) ->
    Options#{macro => Key}.

validate_options(Options, Line, Warnings) ->
    {Options1, Warnings1} = 
        astranaut:validate_options(fun validate_option_key/2, Options),
    Warnings2 = 
        lists:map(
          fun(Warning) ->
                  {Line, astranaut_macro, Warning}
          end, Warnings1),
    {Options1, Warnings ++ Warnings2}.

validate_option_key(attrs, Attrs) when is_atom(Attrs) ->
    ok;
validate_option_key(attrs, Attrs) when is_list(Attrs) ->
    case lists:filter(
           fun(Attr) ->
                   not is_atom(Attr)
           end, Attrs) of
        [] ->
            ok;
        _InvalidAttrs ->
            error
    end;
validate_option_key(order, pre) ->
    ok;
validate_option_key(order, post) ->
    ok;
validate_option_key(formatter, Formatter) when is_atom(Formatter) ->
    ok;
validate_option_key(as_attr, AsAttr) when is_atom(AsAttr) ->
    ok;
validate_option_key(auto_export, Bool) when is_boolean(Bool) ->
    ok;
validate_option_key(group_args, Bool) when is_boolean(Bool) ->
    ok;
validate_option_key(merge_function, head) ->
    ok;
validate_option_key(merge_function, tail) ->
    ok;
validate_option_key(merge_function, Bool) when is_boolean(Bool) ->
    ok;
validate_option_key(debug, Value) when is_boolean(Value) ->
    ok;
validate_option_key(debug_ast, Value) when is_boolean(Value) ->
    ok;
validate_option_key(alias, Value) when is_atom(Value) ->
    ok;
validate_option_key(_Key, _Value) ->
    error.

get_exports(Module) ->
    try
        {ok, Module:module_info(exports)}
    catch
        _:undef ->
            {error, undef}
    end.
