%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------

%% @doc The macro transformer.
%% 
%% Usage: 
%% ```-include_lib("syntax_tools/include/macro.hrl").'''
%% 
%% <dl>
%% <dt>macro.hrl add these attributes:</dt>
%% <dd>-export_macro: export functions as macro</dd>
%% <dd>Usage:</dd>
%% <dd><ul>
%% <li>-export_macro([F1/A1, F2/A2...]).</li>
%% <li>-export_macro({F/A, MacroOptions}).</li>
%% <li>-export_macro({[F1/A1, F2/A2, ...], MacroOptions}).</li>
%% </ul></dd>
%% <dd>-local_macro:declar local functions as macro without export it.</dd>
%% <dd>Usage:</dd>
%% <dd><ul>
%% <li>-local_macro([F1/A1, F2/A2...]).</li>
%% <li>-local_macro({F/A, MacroOptions}).</li>
%% <li>-local_macro({[F1/A1, F2/A2, ...], MacroOptions}).</li>
%% </ul></dd>
%% <dd>-import_macro declars which module exports macro</dd>
%% <dd>without this attribute, transform should analyze forms and detect every external module used whether has -export_macro attribute or not, it's not efficienty, so it's required to use external macro module.</dd>
%% <dd>Usage:</dd>
%% <dd><ul>
%% <li>-import_macro(Module).</li>
%% <li>-import_macro({Module, F/A, MacroOptions}).</li>
%% <li>-import_macro({Module, [F1/A1, F2/A2, ...], MacroOptions}).</li>
%% </ul></dd>
%% <dd>-use_macro add extra options for imported or local macros</dd>
%% <dd>Usage:</dd>
%% <dd><ul>
%% <li>-use_macro({Module, F/A, MacroOptions}).</li>
%% <li>-use_macro({Module, [F1/A1, F2/A2, ...], MacroOptions}).</li>
%% <li>-use_macro({F/A, MacroOptions}).</li>
%% <li>-use_macro({[F1/A1, F2/A2, ...], MacroOptions}).</li>
%% </ul></dd>
%% <dd>-exec_macro execute macro generates ast take place of -exec_macro attribute.</dd>
%% <dd>Usage:</dd>
%% <dd><ul>
%% <li>-exec_macro({Module, Function, Arguments}).</li>
%% <li>-exec_macro({Function, Arguments}).</li>
%% </ul></dd>
%% <dd>-macro_options declars global options used in this moudule.</dd>
%% <dd>exported macro options is only from -export_macro.</dd>
%% <dd>macro from external module options merge order is -export_macro -macro_options -use_macro.</dd>
%% <dd>macro from local module options merge order is -macro_options (-export_macro or -local_macro) -use_macro</dd>
%% <dd>format_error/1</dd>
%% <dd><ul>
%% <li>if there is userdefined error or warning returned from macro definition, format_error/1 should defined</li>
%% <li>if macro which returns error or warning is exported_macro, format_error/1 should be exported</li>
%% <li>if macro which returns error or warning is local_macro, format_error/1 is not need to exported</li>
%% <li>if format_error from macro module is not defined or exported, astranaut_macro will be used as default formatter</li>
%% <li>if format_error/1 is not implemented correctly, there will be no error msg details (because exception is caught by compiler).</li>
%% </ul></dd>
%% </dl>
%% <dl>
%% <dt>MacroOptions = astranaut_lib:options()</dt>
%% <dd>if an options is a <b>definition option</b>, which means it's only avaliable in  -export_macro -local_macro</dd>
%% <dd>{order, Order}, when macro is nested, which macro will executed first, definition option.</dd>
%% <dd><ul>
%% <li>Order = inner, which is default</li>
%% <li>Order = outer</li>
%% </ul></dd>
%% <dd>{inject_attrs, InjectAttrs}, extra arguments will be passed to macro function, definition option.</dd>
%% <dd><ul>
%% <li>InjectAttrs = true, #{file => File, module => Module, pos => Pos} will be extra arguments.</li>
%% <li>InjectAttrs = Attr, if -Attr(AttrValue) declared in module which executes macro, #{Attr => [AttrValue...]} with file, module, pos, will be extra arguments.</li>
%% <li>InjectAttrs = [Attr1, Attr2...], #{Attr1 => [AttrValue1...], Attr2 => [AttrValue2...]} with file, module, pos, will be extra arguments.</li>
%% </ul></dd>
%% <dd>{group_args, GroupArgs}, treat macro arguments as list, definition option.</dd>
%% <dd>{as_attr, As}, -As(Arguments) will replace -exec_macro({M, F, A}), should not be dulicated, definition option.</dd>
%% <dd>{debug, Debug}</dd>
%% <dd><ul>
%% <li>Debug = false, which is default, do nothing</li>
%% <li>Debug = true, print result to console when macro is executed</li>
%% </ul></dd>
%% <dd>{debug_ast, DebugAst}</dd>
%% <dd><ul>
%% <li>DebugAst = false, which is default, do nothing</li>
%% <li>DebugAst = true, print ast to console when macro is executed</li>
%% </ul></dd>
%% <dd>{debug_module, DebugModule}, only avaliable in -macro_options</dd>
%% <dd><ul>
%% <li>DebugModule = false, which is default, do nothing</li>
%% <li>DebugModule = true, print module after transformed to console</li>
%% </ul></dd>
%% <dd>{debug_module_ast, DebugModuleAst}, only avaliable in -macro_options</dd>
%% <dd><ul>
%% <li>DebugModuleAst = false, which is default, do nothing</li>
%% <li>DebugModuleAst = true, print module ast after transformed to console</li>
%% </ul></dd>
%% <dd>{alias, Alias}, rename macro, only avaliable in -use_macro</dd>
%% </dl>
%% @end

-module(astranaut_macro).

-include("do.hrl").
-include("stacktrace.hrl").

%% API
-export([parse_transform/2, format_error/1]).
%%%===================================================================
%%% API
%%%===================================================================
-spec parse_transform(astranaut:forms(), compile:option()) -> astranaut:parse_transform_return().
parse_transform(Forms, Options) ->
    astranaut_return:to_compiler(
      do([ return ||
             Module = astranaut_lib:analyze_forms_module(Forms),
             File = astranaut_lib:analyze_forms_file(Forms),
             {MacroModules, Macros, GlobalMacroOpts} <- load_imported_macro_attributes(Module, File, Forms),
             ExternalMacroMap <- uniform_macro_map(MacroModules, Macros),
             Forms1 <- transform_external_attribute_macros(ExternalMacroMap, Forms),
             %% load macros from attributes and transform -export_macro to -exported_macro
             %% -exported_macro is validated -export_macro attribute.
             %% add nowarn_unused_function compile options to -local_macro if it's not exported
             {Forms2, LocalMacros} <- load_local_macro_attributes(Module, File, GlobalMacroOpts, ExternalMacroMap, Forms1),
             Forms3 <- transform_uniform_macros(Module, ExternalMacroMap, LocalMacros, Forms2, Options),
             format_forms(Forms3, GlobalMacroOpts),
             return(Forms3)
         ])).

-spec format_error(term()) -> term().
format_error({import_macro_failed, Module}) ->
    io_lib:format("could not import macro from module ~p, update compile file order in Makefile or add to erl_first_files in rebar.config to make it compile first.", [Module]);
format_error({invalid_import_macro_attr, Macro}) ->
    io_lib:format("~p is not a valid module in -import_macro(~p). ", [Macro, Macro]);
format_error({unimported_macro_module, Module}) ->
    io_lib:format("-import_macro(~p). is required before use of -use_macro", [Module]);
format_error({unexported_macro, Module, Function, Arity}) ->
    io_lib:format("unexported macro ~p:~p/~p.", [Module, Function, Arity]);
format_error({undefined_macro, Function, Arity}) ->
    io_lib:format("macro ~p/~p undefined.", [Function, Arity]);
format_error({invalid_use_macro, Opts}) ->
    io_lib:format("invalid use macro ~p.", [Opts]);
format_error({macro_override, MacroKey, ExistingMacro, OverridingMacro}) ->
    io_lib:format("macro ~p is already defined by ~p and cannot be overridden by ~p without force_override.",
                  [MacroKey, format_macro_ref(ExistingMacro), format_macro_ref(OverridingMacro)]);
format_error({non_exported_formatter, Module}) ->
    io_lib:format("format_error/1 is not exported from module ~p.", [Module]);
format_error({unloaded_formatter_module, Module}) ->
    io_lib:format("formatter module ~p could not be loaded.", [Module]);
format_error(invalid_macro_attribute) ->
    io_lib:format("invalid attribute macro call: macro not found", []);
format_error({max_macro_expansion_depth_exceeded, {MacroModule, Function}, Arguments}) ->
    io_lib:format("maximum macro expansion depth exceeded when applying macro ~p:~p with arguments ~p.",
        [MacroModule, Function, Arguments]);
format_error({max_macro_expansion_depth_exceeded, Function, Arguments}) ->
    io_lib:format("maximum macro expansion depth exceeded when applying macro ~p with arguments ~p.",
        [Function, Arguments]);
format_error({macro_exception, MFA, Arguments, Exception}) ->
    io_lib:format("apply macro ~s ~p failed:~n~s",
                  [format_mfa(MFA), Arguments, eunit_lib:format_exception(Exception)]);
format_error(Error) ->
    astranaut:format_error(Error).

format_macro_ref(#{macro_module := Module, function := Function, arity := Arity}) ->
    {Module, Function, Arity};
format_macro_ref(Macro) ->
    Macro.
%%%===================================================================
%%% analyze -export_macro -use_macro attributes functions
%%%===================================================================
load_imported_macro_attributes(Module, File, Forms) ->
    do([ return ||
           Validator = global_macro_validator(),
           GlobalMacroOpts <- astranaut_lib:validate_attribute_option(Validator, ?MODULE, macro_options, Forms),
           {ImportedModules, ImportedMacros} <- imported_macros(GlobalMacroOpts, Forms),
           UsedMacros <- used_macros(File, Module, ImportedMacros, Forms),
           return({ImportedModules, UsedMacros, GlobalMacroOpts})
       ]).

load_local_macro_attributes(Module, File, GlobalMacroOpts, ExternalMacroMap, Forms) ->
    do([ return ||
           ClausesMap = function_clauses_map(Forms, maps:new()),
           {Forms1, ExportedMacros} <- exported_macros(Forms, ClausesMap),
           local_macros(Module, File, GlobalMacroOpts, ExternalMacroMap, ExportedMacros, ClausesMap, Forms1)
       ]).

%% load_attributes(Forms) ->
%%     File = astranaut_lib:analyze_forms_file(Forms),
%%     Module = astranaut_lib:analyze_forms_module(Forms),
%%     do([ return ||
%%            Validator = global_macro_validator(),
%%            GlobalMacroOpts <- astranaut_lib:validate_attribute_option(Validator, ?MODULE, macro_options, Forms),
%%            {Forms1, ExportedMacros} <- exported_macros(Forms),
%%            {ImportedModules, ImportedMacros} <- imported_macros(GlobalMacroOpts, Forms),
%%            {Forms2, LocalMacros} <- local_macros(Module, GlobalMacroOpts, ExportedMacros, Forms1),
%%            UsedMacros <- used_macros(File, Module, ImportedMacros, LocalMacros, Forms2),
%%            return({Forms2, ImportedModules, UsedMacros, GlobalMacroOpts})
%%        ]).

formatter_opts(Module, Functions, MacroOpts) ->
    FormatError = {format_error, 1},
    case lists:member(FormatError, Functions) of
        true ->
            MacroOpts#{formatter => Module};
        false ->
            MacroOpts#{formatter => astranaut_macro}
    end.

exported_macros(Forms, ClausesMap) ->
    astranaut_lib:forms_with_attribute(
      fun(Attr, Acc, #{pos := Pos}) ->
              do([ return ||
                     Validator = macro_definition_validator(),
                     {FAs, Options} <-
                         validate_macro_attribute(fun macro_without_module_attr/1, Validator, export_macro, Attr),
                     FAs1 <- remove_undefined_macros(FAs, ClausesMap),
                     case FAs1 of
                         [] ->
                             astranaut_return:return({[], Acc});
                         _ ->
                             %% export_macro options for local usage
                             Acc2 = lists:foldl(fun(FA, Acc1) -> maps:put(FA, Options, Acc1) end, Acc, FAs1),
                             %% exported_macro options for external usage
                             ExportedMacroAttribute = astranaut_lib:gen_attribute_node(exported_macro, Pos, [{FAs, Options}]),
                             ExportAttribute = astranaut_lib:gen_attribute_node(export, Pos, FAs),
                             astranaut_return:return({[ExportAttribute, ExportedMacroAttribute], Acc2})
                     end
                 ])
      end, #{}, Forms, export_macro, #{formatter => ?MODULE}).

remove_undefined_macros(FAs, ClausesMap) ->
    astranaut_return:foldl_m(
      fun({Function, Arity} = FA, Acc) ->
              case maps:is_key(FA, ClausesMap) of
                  true ->
                      astranaut_return:return([FA|Acc]);
                  false ->
                      astranaut_return:error_ok({undefined_macro, Function, Arity}, Acc)
              end
      end, [], FAs).

%% analyze -import_macro attributes.
imported_macros(GlobalMacroOpts, Forms) ->
    astranaut_return:lift_m(
      fun({Modules, MacroMap}) ->
              {lists:reverse(Modules), MacroMap}
      end,
      astranaut_lib:with_attribute(
        fun(Module, {ModulesAcc, MacroMapAcc}) when is_atom(Module) ->
                case is_loaded(Module) of
                    {file, _} ->
                        Macros = analyze_module_macros(Module),
                        Exports = Module:module_info(exports),
                        GlobalMacroOpts1 = formatter_opts(Module, Exports, GlobalMacroOpts),
                        Macros1 =
                            maps:fold(
                              fun({Function, Arity}, MacroOptions, MacrosAcc) ->
                                      MacroOptions1 = maps:merge(GlobalMacroOpts1, MacroOptions),
                                      MacroOptions2 = MacroOptions1#{module => Module,
                                                                     macro_module => Module,
                                                                     macro => {Module, Function},
                                                                     function => Function,
                                                                     arity => Arity},
                                      maps:put({Function, Arity}, MacroOptions2, MacrosAcc)
                              end, #{}, Macros),
                        MacroMapAcc1 = maps:put(Module, Macros1, MacroMapAcc),
                        ModulesAcc1 = [Module|ModulesAcc],
                        astranaut_return:return({ModulesAcc1, MacroMapAcc1});
                    false ->
                        astranaut_return:error_fail({import_macro_failed, Module})
                end;
           (Attr, _Acc) ->
                astranaut_return:error_fail({invalid_import_macro_attr, Attr})
        end, {[], #{}}, Forms, import_macro, #{formatter => ?MODULE})).

is_loaded(Module) ->
    code:ensure_loaded(Module),
    code:is_loaded(Module).

local_macros(Module, File, GlobalMacroOpts, ExternalMacroMap, ExportedMacros, ClauseMap, Forms) ->
    GlobalMacroOpts1 = local_macro_global_opts(Module, ClauseMap, GlobalMacroOpts),
    Ctx = #{module => Module,
            file => File,
            forms => Forms,
            global_macro_opts => GlobalMacroOpts1,
            external_macro_map => ExternalMacroMap,
            clause_map => ClauseMap},
    ExportedMacroMap = build_local_macro_map(Ctx, ExportedMacros),
    astranaut_lib:forms_with_attribute(
      fun(Attr, Acc, #{pos := Pos}) ->
              update_local_macro_attribute(Ctx, Pos, Attr, Acc)
      end, ExportedMacroMap, Forms, local_macro, #{formatter => ?MODULE}).

update_local_macro_attribute(Ctx, Pos, Attr, Acc) ->
    do([ return ||
           {FAs, Options} <- validate_local_macro_attribute(Ctx, Attr),
           update_local_macros(Ctx, Pos, FAs, Options, Acc)
       ]).

validate_local_macro_attribute(#{clause_map := ClauseMap}, Attr) ->
    do([ return ||
           Validator = macro_definition_validator(),
           {FAs, Options} <- validate_macro_attribute(fun macro_without_module_attr/1, Validator, local_macro, Attr),
           FAs1 <- remove_undefined_macros(FAs, ClauseMap),
           return({FAs1, Options})
       ]).

update_local_macros(_Ctx, _Pos, [], _Options, Acc) ->
    astranaut_return:return({[], Acc});
update_local_macros(Ctx, Pos, FAs, Options, Acc) ->
    do([ return ||
           NoWarnNodes = astranaut_lib:gen_attribute_node(compile, Pos, {nowarn_unused_function, FAs}),
           CurrentMacroMap = build_local_macro_map(Ctx, macro_options_by_fa(FAs, Options)),
           #{external_macro_map := ExternalMacroMap} = Ctx,
           assert_macro_map_no_overrides(CurrentMacroMap, ExternalMacroMap),
           return({[NoWarnNodes], maps:merge(Acc, CurrentMacroMap)})
       ]).

build_local_macro_map(#{file := File,
                        module := Module,
                        forms := Forms,
                        global_macro_opts := GlobalMacroOpts}, ModuleMacros) ->
    LocalModuleMacros =
        maps:map(
          fun({Function, Arity}, MacroOptions) ->
                  local_macro_options(Module, GlobalMacroOpts, Function, Arity, MacroOptions)
          end, ModuleMacros),
    update_module_macros(File, Module, Forms, LocalModuleMacros).

macro_options_by_fa(FAs, Options) ->
    lists:foldl(fun(FA, Acc) -> maps:put(FA, Options, Acc) end, #{}, FAs).

local_macro_global_opts(Module, ClauseMap, GlobalMacroOpts) ->
    case maps:is_key({format_error, 1}, ClauseMap) of
        true ->
            GlobalMacroOpts#{formatter => local_macro_module(Module)};
        false ->
            GlobalMacroOpts#{formatter => astranaut_macro}
    end.

local_macro_options(Module, GlobalMacroOpts, Function, Arity, MacroOptions) ->
    MacroOptions1 = maps:merge(GlobalMacroOpts, MacroOptions),
    MacroOptions1#{module => local_macro_module(Module),
                   macro_module => Module,
                   macro => Function,
                   function => Function,
                   arity => Arity}.

update_module_macros(File, Module, Forms, ModuleMacros) ->
    maps:fold(
      fun(_MFA, MacroOptions, Acc) ->
              MacroOptions1 = MacroOptions#{file => File, local_module => Module},
              MacroOptions2 = update_as_attr(MacroOptions1),
              MacroOptions3 = inject_attrs(MacroOptions2, Forms),
              #{macro := Macro, call_arity := CallArity} = MacroOptions4 = update_call_arity(MacroOptions3),
              maps:put({Macro, CallArity}, MacroOptions4, Acc)
      end, #{}, ModuleMacros).

used_macros(File, Module, ImportedMacros, Forms) ->
    ImportedMacroMap = effective_module_macro_maps(File, Module, Forms, ImportedMacros),
    astranaut_lib:with_attribute(
      fun(Attr, UsedMacroMapAcc) ->
              do([ return ||
                     Validator = use_macro_validator(),
                     {MFAs, Options}
                         <- validate_macro_attribute(fun macro_attr/1, Validator, use_macro, Attr),
                     case MFAs of
                         {ImportedModule, FAs} ->
                             case maps:is_key(ImportedModule, UsedMacroMapAcc) of
                                 true ->
                                     update_used_macro_maps(
                                       File, Module, Forms, ImportedModule, FAs, Options,
                                       UsedMacroMapAcc,
                                       fun({Function, Arity}) ->
                                               {unexported_macro, ImportedModule, Function, Arity}
                                       end);
                                 false ->
                                     astranaut_return:error_fail({unimported_macro_module, ImportedModule})
                             end;
                         FAs ->
                             update_used_macro_maps(
                               File, Module, Forms, Module, FAs, Options,
                               UsedMacroMapAcc,
                               fun({Function, Arity}) ->
                                       {undefined_macro, Function, Arity}
                               end)
                     end
                 ])
      end, ImportedMacroMap, Forms, use_macro, #{formatter => ?MODULE}).

effective_module_macro_maps(File, Module, Forms, ModuleMacros) ->
    maps:map(
      fun(_MacroModule, Macros) ->
              update_module_macros(File, Module, Forms, Macros)
      end, ModuleMacros).

update_used_macro_maps(File, Module, Forms, MacroModule, FAs, UsedMacroOptions, UsedMacroMapAcc, MissingFun) ->
    astranaut_return:foldl_m(
      fun(FA, Acc) ->
              ModuleMacroMap = maps:get(MacroModule, Acc, #{}),
              case find_used_macro(FA, ModuleMacroMap) of
                  {ok, MacroKey, MacroOptions} ->
                      MacroOptions1 = maps:merge(MacroOptions, UsedMacroOptions),
                      MacroOptions2 = update_alias(MacroOptions1),
                      CurrentMacroMap = update_module_macros(File, Module, Forms, #{FA => MacroOptions2}),
                      ModuleMacroMapWithoutCurrent = maps:remove(MacroKey, ModuleMacroMap),
                      ExistingUsedMacroMap = maps:put(MacroModule, ModuleMacroMapWithoutCurrent, Acc),
                      ExistingMacroMap = uniform_imported_macro_map(ExistingUsedMacroMap),
                      do([ return ||
                             assert_macro_map_no_overrides(CurrentMacroMap, ExistingMacroMap),
                             ModuleMacroMap1 = maps:merge(ModuleMacroMapWithoutCurrent, CurrentMacroMap),
                             return(maps:put(MacroModule, ModuleMacroMap1, Acc))
                         ]);
                  error ->
                      astranaut_return:error_ok(MissingFun(FA), Acc)
              end
      end, UsedMacroMapAcc, FAs).

find_used_macro({Function, Arity}, ModuleMacroMap) ->
    maps:fold(
      fun(MacroKey, #{function := Function1, arity := Arity1} = MacroOptions, error)
            when Function =:= Function1, Arity =:= Arity1 ->
              {ok, MacroKey, MacroOptions};
         (_MacroKey, _MacroOptions, Acc) ->
              Acc
      end, error, ModuleMacroMap).

assert_macro_map_no_overrides(MacroMap, ExistingMacroMap) ->
    astranaut_return:foldl_m(
      fun({MacroKey, Macro}, ExistingMacroMapAcc) ->
              case maps:find(MacroKey, ExistingMacroMapAcc) of
                  {ok, ExistingMacro} ->
                      case maps:get(force_override, Macro, false) of
                          true ->
                              astranaut_return:return(maps:put(MacroKey, Macro, ExistingMacroMapAcc));
                          false ->
                              macro_override_fail(MacroKey, ExistingMacro, Macro)
                      end;
                  error ->
                      astranaut_return:return(maps:put(MacroKey, Macro, ExistingMacroMapAcc))
              end
      end, ExistingMacroMap, maps:to_list(MacroMap)).

uniform_imported_macro_map(UsedMacroMap) ->
    maps:fold(
      fun(_MacroModule, MacroMap, Acc) ->
              maps:merge(Acc, MacroMap)
      end, #{}, UsedMacroMap).

update_alias(#{alias := true, function := Function} = Options) ->
    Options#{macro => Function};
update_alias(#{alias := Alias} = Options) ->
    Options#{macro => Alias};
update_alias(#{} = Options) ->
    Options.

update_as_attr(#{as_attr := true, function := Function} = Options) ->
    Options#{as_attr => Function};
update_as_attr(#{} = Options) ->
    Options.

inject_attrs(#{inject_attrs := true} = Options, Forms) ->
    inject_attrs(Options#{inject_attrs => []}, Forms);
inject_attrs(#{inject_attrs := Attr} = Options, Forms) when is_atom(Attr) ->
    inject_attrs(Options#{inject_attrs => [Attr]}, Forms);
inject_attrs(#{inject_attrs := Attrs, file := File, local_module := Module} = Opts, Forms) when is_list(Attrs) ->
    AttributesMap =
        lists:foldl(
          fun(module, Acc) ->
                  Acc;
             (file, Acc) ->
                  Acc;
             (pos, Acc) ->
                  Acc;
             (Attr, Acc) ->
                  Attributes = astranaut_lib:analyze_forms_attributes(Attr, Forms),
                  maps:put(Attr, Attributes, Acc)
          end, maps:new(), Attrs),
    Opts#{attributes => maps:merge(#{file => File, module => Module}, AttributesMap)};
inject_attrs(#{} = Opts, _Forms) ->
    Opts.

update_call_arity(Opts) ->
    CallArity = call_arity(Opts),
    Opts#{call_arity => CallArity}.

call_arity(#{group_args := true} = Opts) ->
    call_arity(maps:remove(group_args, Opts#{arity => 1}));
call_arity(#{arity := Arity} = Opts) ->
    case maps:get(inject_attrs, Opts, false) of
        false ->
            Arity;
        _ ->
            Arity - 1
    end.

local_macro_module(Module) ->
    list_to_atom(atom_to_list(Module) ++ "__local_macro").

analyze_module_macros(Module) ->
    ModuleMacroAttributes = astranaut_lib:analyze_module_attributes(exported_macro, Module),
    Insert =
        fun(FAs, Opts, Acc0) ->
                lists:foldl(
                  fun({Function, Arity}, Acc1) ->
                          maps:put({Function, Arity}, Opts, Acc1)
                  end, Acc0, FAs)
        end,
    lists:foldl(
      fun({FAs, Opts}, Acc) ->
              Insert(FAs, Opts, Acc);
         (FAs, Acc) when is_list(FAs) ->
              Insert(FAs, #{}, Acc);
         (FA, Acc) ->
              Insert([FA], #{}, Acc)
      end, #{}, lists:flatten(ModuleMacroAttributes)).

validate_macro_attribute(Fun, Validator, AttrName, Attr) ->
    case Fun(Attr) of
        invalid_attr ->
            astranaut_return:error_fail({invalid_attr, AttrName, Attr});
        {MFAs, Options} ->
            do([ return ||
                   validate_mfas(MFAs),
                   Options1 <- astranaut_lib:validate(Validator, Options),
                   return({MFAs, Options1})
               ])
    end.

use_macro_validator() ->
    #{
      debug => boolean,
      debug_ast => boolean,
      alias => atom,
      force_override => boolean
     }.

global_macro_validator() ->
    #{
      debug => boolean,
      debug_ast => boolean,
      debug_module => boolean,
      debug_module_ast => boolean,
      max_depth => [uinteger, {default, 100}]
     }.

macro_definition_validator() ->
    #{as_attr => atom,
      order => {one_of, [outer, inner]},
      inject_attrs => {'or', [atom, {list_of, atom}]},
      group_args => boolean,
      force_override => boolean,
      max_depth => uinteger
     }.

validate_mfas({Module, FAs}) when is_atom(Module) ->
    validate_fas(FAs);
validate_mfas(FAs) when is_list(FAs) ->
    validate_fas(FAs).

validate_fas([{Function, Arity}|T]) when is_atom(Function), is_integer(Arity), Arity >= 0 ->
    validate_fas(T);
validate_fas([FA|_T]) ->
    astranaut_return:error_fail({invalid_function_with_arity, FA});
validate_fas([]) ->
    astranaut_return:return(ok).

macro_attr({Module, FAs}) when is_atom(Module), is_list(FAs) ->
    {{Module, FAs}, []};
macro_attr({Module, FA}) when is_atom(Module), not is_integer(FA) ->
    {{Module, [FA]}, []};
macro_attr({Module, FAs, Options}) when is_atom(Module), is_list(FAs) ->
    {{Module, FAs}, Options};
macro_attr({Module, FA, Options}) when is_atom(Module) ->
    {{Module, [FA]}, Options};
macro_attr(Attr) ->
    macro_without_module_attr(Attr).

macro_without_module_attr({FA}) ->
    {[FA], []};
macro_without_module_attr({FAs, Options}) when is_list(FAs) ->
    {FAs, Options};
macro_without_module_attr({Function, Arity}) when is_integer(Arity) ->
    {[{Function, Arity}], []};
macro_without_module_attr({FA, Options}) ->
    {[FA], Options};
macro_without_module_attr(_Other) ->
    invalid_attr.

%%%===================================================================
%%% transform macros
%%%===================================================================
%% Step 1. expand external attribute macros only.
%% Step 2. find local macros and their related functions.
%% Step 3. expand local macro source snapshots with external macros only.
%% Step 4. compile and load the local macro module.
%% Step 5. expand all non-local-macro forms with the final external + local macro map.
transform_uniform_macros(Module, ExternalMacroMap, LocalMacroMap, Forms, CompileOpts) ->
    do([ return ||
           Ctx = uniform_macro_context(Module, ExternalMacroMap, LocalMacroMap, Forms, CompileOpts),
           #{local_macro_functions := LocalMacroFunctions,
             local_macro_related_functions := LocalMacroRelatedFunctions} = Ctx,
           load_local_macro_forms(LocalMacroFunctions, LocalMacroRelatedFunctions,
                                  ExternalMacroMap, Forms, CompileOpts),
           FinalMacroMap <- merge_macro_maps(ExternalMacroMap, LocalMacroMap),
           transform_uniform_macro_forms(Ctx, FinalMacroMap)
       ]).

uniform_macro_context(Module, ExternalMacroMap, LocalMacroMap, Forms, CompileOpts) ->
    ClauseMap = function_clauses_map(Forms, maps:new()),
    LocalMacroFunctions = local_macro_functions(LocalMacroMap),
    LocalMacroFunctions1 = maybe_add_local_formatter(LocalMacroFunctions, ClauseMap),
    LocalMacroRelatedFunctions = local_macro_related_functions(LocalMacroFunctions1, ClauseMap),
    #{module => Module,
      external_macro_map => ExternalMacroMap,
      forms => Forms,
      compile_opts => CompileOpts,
      local_macro_functions => LocalMacroFunctions1,
      local_macro_related_functions => LocalMacroRelatedFunctions}.

local_macro_functions(LocalMacroMap) ->
    maps:fold(
      fun(_Macro, #{function := Function, arity := Arity}, Acc) ->
              ordsets:add_element({Function, Arity}, Acc)
      end, ordsets:new(), LocalMacroMap).

maybe_add_local_formatter([], _ClauseMap) ->
    [];
maybe_add_local_formatter(LocalMacroFunctions, ClauseMap) ->
    case maps:is_key({format_error, 1}, ClauseMap) of
        true ->
            ordsets:add_element({format_error, 1}, LocalMacroFunctions);
        false ->
            LocalMacroFunctions
    end.

transform_uniform_macro_forms(
  #{module := Module,
    forms := Forms,
    local_macro_related_functions := LocalMacroRelatedFunctions}, FinalMacroMap) ->
    do([ return ||
           Forms1 <- transform_attribute_macros(FinalMacroMap, Forms),
           FinalMacroCallers = find_function_macro_callers(Forms1, FinalMacroMap, LocalMacroRelatedFunctions),
           transform_functions(Module, FinalMacroMap, Forms1, FinalMacroCallers)
       ]).

uniform_macro_map(MacroModules, ModuleMacroMap) ->
    astranaut_return:foldl_m(
      fun(MacroModule, Acc) ->
              MacroMap = maps:get(MacroModule, ModuleMacroMap, #{}),
              merge_macro_maps(Acc, MacroMap)
      end, #{}, MacroModules).

merge_macro_maps(First, Second) ->
    astranaut_return:foldl_m(
      fun({MacroKey, Macro}, Acc) ->
              case maps:find(MacroKey, Acc) of
                  {ok, ExistingMacro} ->
                      case maps:get(force_override, Macro, false) of
                          true ->
                              astranaut_return:return(maps:put(MacroKey, Macro, Acc));
                          false ->
                              macro_override_fail(MacroKey, ExistingMacro, Macro)
                      end;
                  error ->
                      astranaut_return:return(maps:put(MacroKey, Macro, Acc))
              end
      end, First, maps:to_list(Second)).

macro_override_fail(MacroKey, ExistingMacro, OverridingMacro) ->
    astranaut_return:error_fail({macro_override, MacroKey, ExistingMacro, OverridingMacro}).

attribute_macro_map(MacroMap) ->
    AttributeMap =
        maps:fold(
          fun({_Function, Arity}, #{as_attr := Attr} = Macro, Acc) ->
                  maps:put({Attr, Arity}, Macro, Acc);
             (_Key, _Macro, Acc) ->
                  Acc
          end, #{}, MacroMap),
    maps:fold(
      fun({Name, Arity}, Macro, Acc) ->
              MacroNameMap = maps:get(Name, Acc, #{}),
              MacroNameMap1 = maps:put({Name, Arity}, Macro, MacroNameMap),
              maps:put(Name, MacroNameMap1, Acc)
      end, #{}, AttributeMap).

load_local_macro_forms([], _LocalMacroRelatedFunctions, _ExternalMacroMap, _Forms, _CompileOpts) ->
    astranaut_return:return(ok);
load_local_macro_forms(LocalMacroFunctions, LocalMacroRelatedFunctions, ExternalMacroMap, Forms, CompileOpts) ->
    do([ return ||
           Forms1 <- prepare_local_macro_snapshot(LocalMacroRelatedFunctions, ExternalMacroMap, Forms),
           Forms2 = select_local_macro_forms(LocalMacroRelatedFunctions, Forms1),
           compile_local_macro_forms(LocalMacroFunctions, Forms2, CompileOpts)
       ]).

prepare_local_macro_snapshot(LocalMacroRelatedFunctions, ExternalMacroMap, Forms) ->
    LocalSnapshotMacroCallers = local_macro_snapshot_callers(LocalMacroRelatedFunctions, ExternalMacroMap, Forms),
    transform_functions_if_needed(uniform, ExternalMacroMap, Forms, LocalSnapshotMacroCallers).

local_macro_snapshot_callers(LocalMacroRelatedFunctions, ExternalMacroMap, Forms) ->
    LocalExternalMacroCallers = find_function_macro_callers(Forms, ExternalMacroMap, ordsets:new()),
    LocalMacroRelatedFunctionIds = function_ids(LocalMacroRelatedFunctions),
    ordsets:intersection(LocalExternalMacroCallers, LocalMacroRelatedFunctionIds).

select_local_macro_forms(LocalMacroRelatedFunctions, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, Pos, module, Module}, Acc) ->
                [{attribute, Pos, module, local_macro_module(Module)}|Acc];
           ({function, _Pos, Name, Arity, _Clauses} = Node, Acc) ->
                append_if(ordsets:is_element({Name, Arity}, LocalMacroRelatedFunctions), Node, Acc);
           ({attribute,_Pos, spec, {{Name,Arity}, _Body}} = Node, Acc) ->
                append_if(ordsets:is_element({Name, Arity}, LocalMacroRelatedFunctions), Node, Acc);
           ({attribute,_Pos, export, _Exports}, Acc) ->
                Acc;
           (Node, Acc) ->
                [Node|Acc]
        end, [], Forms)).

compile_local_macro_forms(LocalMacroFunctions, Forms, CompileOpts) ->
    Forms1 = astranaut_syntax:sort_forms(Forms ++ local_macro_exports(LocalMacroFunctions)),
    astranaut_lib:load_forms(Forms1, [without_warnings|CompileOpts]).

local_macro_exports(LocalMacroFunctions) ->
    lists:foldl(
      fun(Export, Acc) ->
              [astranaut_lib:gen_exports([Export], 0)|Acc]
      end, [], LocalMacroFunctions).

function_ids(Functions) ->
    lists:foldl(
      fun({Function, Arity}, Acc) ->
              ordsets:add_element({function, Function, Arity}, Acc)
      end, ordsets:new(), Functions).

transform_functions_if_needed(_Module, MacroMap, Forms, _TransformFunctions) when map_size(MacroMap) =:= 0 ->
    astranaut_return:return(Forms);
transform_functions_if_needed(_Module, _MacroMap, Forms, []) ->
    astranaut_return:return(Forms);
transform_functions_if_needed(Module, MacroMap, Forms, TransformFunctions) ->
    transform_functions(Module, MacroMap, Forms, TransformFunctions).

append_if(Boolean, Form, Forms) ->
    case Boolean of
        true ->
            [Form|Forms];
        false ->
            Forms
    end.

transform_external_attribute_macros(MacroMap, Forms) ->
    transform_attribute_macros(MacroMap, Forms, ignore_missing).

transform_attribute_macros(MacroMap, Forms) ->
    transform_attribute_macros(MacroMap, Forms, warn_missing).

transform_attribute_macros(MacroMap, Forms, MissingMode) ->
    AttributeMacroMap = attribute_macro_map(MacroMap),
    transform_attribute_macros(MacroMap, AttributeMacroMap, Forms, MissingMode).

transform_attribute_macros(MacroMap, AttributeMacroMap, Forms, MissingMode) ->
    Monad =
        astranaut:map_m(
          fun(Form) ->
                  case attribute_find_macro(Form, MacroMap, AttributeMacroMap) of
                      {ok, Macro} ->
                          expand_macro(Macro);
                      error ->
                          handle_missing_attribute_macro(Form, MissingMode);
                      not_macro ->
                          astranaut_traverse:return(Form)
                  end
          end, Forms, #{traverse => none}),
    astranaut_traverse:eval(Monad, ?MODULE, #{}, ok).

handle_missing_attribute_macro(Form, ignore_missing) ->
    astranaut_traverse:return(Form);
handle_missing_attribute_macro(Form, warn_missing) ->
    astranaut_traverse:then(
      astranaut_traverse:warning(invalid_macro_attribute),
      astranaut_traverse:return(Form)).

function_clauses_map([{function, _Pos, Name, Arity, Clauses}|T], Acc) ->
    NAcc = maps:put({Name, Arity}, Clauses, Acc),
    function_clauses_map(T, NAcc);
function_clauses_map([_H|T], Acc) ->
    function_clauses_map(T, Acc);
function_clauses_map([], Acc) ->
    Acc.

local_macro_related_functions(Functions, ClauseMap) ->
    local_macro_related_functions(Functions, ClauseMap, Functions).

local_macro_related_functions(Functions, ClauseMap, Deps) ->
    lists:foldl(
      fun(Function, Acc) ->
              case maps:find(Function, ClauseMap) of
                  {ok, Clauses} ->
                      FDeps = ordsets:union(lists:map(fun local_macro_related_functions/1, Clauses)),
                      NDeps = ordsets:union(FDeps, Acc),
                      AddedFunctions = ordsets:subtract(FDeps, Deps),
                      local_macro_related_functions(AddedFunctions, ClauseMap, NDeps);
                  error ->
                      ordsets:del_element(Function, Acc)
              end
      end, Deps, Functions).

local_macro_related_functions({clause, _Pos1, _Patterns, _Guards, Exprs}) ->
    with_local_function_call(
      fun(Function, Arity, Acc) when is_atom(Function) ->
              ordsets:add_element({Function, Arity}, Acc)
      end, ordsets:new(), Exprs).

with_local_function_call(Fun, Init, Exprs) ->
    astranaut:sreduce(
      fun({call, _Pos1, {atom, _Pos2, Function}, Arguments}, Acc) ->
              Arity = length(Arguments),
              Fun(Function, Arity, Acc);
         (_, Acc) ->
              Acc
      end, Init, Exprs, #{traverse => pre}).

find_function_macro_callers(Forms, MacroMap, ExcludedFunctions) ->
    case maps:size(MacroMap) of
        0 ->
            ordsets:new();
        _ ->
            lists:foldl(
              fun({function, _Pos, Function, Arity, Clauses}, Acc) ->
                      case ordsets:is_element({Function, Arity}, ExcludedFunctions) of
                          true ->
                              Acc;
                          false ->
                              case has_macro_call(Clauses, MacroMap) of
                                  true ->
                                      ordsets:add_element({function, Function, Arity}, Acc);
                                  false ->
                                      Acc
                              end
                      end;
                 (_Form, Acc) ->
                      Acc
              end, ordsets:new(), Forms)
    end.

has_macro_call(Nodes, MacroMap) ->
    astranaut:sreduce(
      fun(_Node, true) ->
              true;
         (Node, false) ->
              case call_find_macro(uniform, Node, MacroMap) of
                  {ok, _Macro} ->
                      true;
                  error ->
                      false
              end
      end, false, Nodes, #{traverse => pre}).

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].

%%%===================================================================
%%% transform macro MacroModule:MacroFun(Arguments) and it's help functions.
%%%===================================================================
-spec transform_functions(module(), map(), [astranaut:form()], all | {except, list()} | list()) -> term().
transform_functions(Module, MacroMap, Forms, TransformFunctions) ->
    %% just traverse function clauses, other nodes return directly
    FunctionClausesUniplate = 
        fun({function, Pos, Name, Arity, Clauses}) ->
                { [Clauses], fun([NewClauses]) -> {function, Pos, Name, Arity, NewClauses} end };
            (Node) -> 
                {[[]], fun(_) -> Node end }
        end,
    Monad =
        astranaut:map_m(
          fun({function, _Pos, Name, Arity, _Clauses} = Function) ->
                  case should_transform_function(Name, Arity, TransformFunctions) of
                      false ->
                          astranaut_traverse:return(Function);
                      true ->
                          astranaut:map_m(
                            fun(Clause) ->
                                    transform_clause(Module, MacroMap, Clause)
                            end, Function, #{traverse => subtree, uniplate => FunctionClausesUniplate})
                  end;
             (Form) ->
                  astranaut_traverse:return(Form)
          end, Forms, #{traverse => none}),
    astranaut_traverse:eval(Monad, ?MODULE, #{}, 0).

transform_clause(Module, MacroMap, {clause, Pos, Patterns, Guards, Exprs}) ->
    do([ traverse ||
           %% counter is reseted in every function clause
           astranaut_traverse:put(1),
           Guards1 <- transform_exprs(Module, MacroMap, Guards, #{depth => 0}),
           Exprs1 <- transform_exprs(Module, MacroMap, Exprs, #{depth => 0}),
           return({clause, Pos, Patterns, Guards1, Exprs1})
    ]).

transform_exprs(Module, MacroMap, Exprs, DepthOpts) ->
    astranaut:map_m(
        fun(Node) ->
            do([ traverse ||
                #{step := Step} <- astranaut_traverse:ask(),
                DepthOpts1 = DepthOpts#{rename_quoted_variables => true, step => Step},
                case match_macro_call(Module, Node, MacroMap, Step) of
                    {ok, Macro} ->
                        expand_macro_recursive(Module, MacroMap, Macro, DepthOpts1);
                    error ->
                        astranaut_traverse:return(Node)
                end
            ])
        end, Exprs, #{traverse => all}).

%%%===================================================================
%%% apply macro functions
%%%===================================================================
expand_macro_recursive(_Module, _MacroMap, #{ max_depth := MaxDepth },
    #{depth := Depth, macro := MacroName, arguments := Arguments}) when Depth >= MaxDepth ->
    astranaut_traverse:fail({max_macro_expansion_depth_exceeded, MacroName, Arguments});
expand_macro_recursive(Module, MacroMap, Macro, #{step := post } = DepthOpts) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    do([traverse ||
            Node1 <- expand_macro(Macro, DepthOpts1),
            transform_exprs(Module, MacroMap, Node1, DepthOpts1)
        ]);
expand_macro_recursive(Module, MacroMap, Macro, #{step := pre } = DepthOpts) ->
    DepthOpts1 = update_depth_opts(Macro, DepthOpts),
    do([ traverse ||
            Node1 <- expand_macro(Macro, DepthOpts1),
            %% if node1 is calling another macro with outer order, apply it too
            %% because astranaut_traverse:map_m will traverse children after this without node itself
            case match_macro_call(Module, Node1, MacroMap, pre) of
                {ok, Macro1} ->
                    expand_macro_recursive(Module, MacroMap, Macro1, DepthOpts1);
                error ->
                    astranaut_traverse:return(Node1)
            end
        ]).

match_macro_call(Module, Node, Macros, Step) ->
    case call_find_macro(Module, Node, Macros) of
        {ok, Macro} ->
            case match_macro_order(Macro, Step) of
                true ->
                    {ok, Macro};
                false ->
                    error
            end;
        error ->
            error
    end.

call_find_macro(_Module, {call, Pos1, {atom, _Pos2, Function}, Arguments}, Macros) ->
    find_macro_with_arguments(Function, Arguments, Pos1, Macros);
call_find_macro(_Module, {call, Pos1, {remote, _Pos2, {atom, _Pos3, RemoteModule}, {atom, _Pos4, Function}}, Arguments},
                Macros) ->
    find_macro_with_arguments({RemoteModule, Function}, Arguments, Pos1, Macros);
call_find_macro(_Module, _Node, _Macros) ->
    error.

match_macro_order(Macro, Step) ->
    Order = maps:get(order, Macro, inner),
    ((Order =:= inner) and (Step =:= post))
        or ((Order =:= outer) and (Step =:= pre)).

update_depth_opts(Macro, #{depth := Depth} = Opts) ->
    Opts1 = update_top_macro(Macro, Opts),
    Opts1#{depth => Depth + 1}.

update_top_macro(#{macro := Macro, arguments := Arguments }, #{depth := 0} = Opts) ->
    Opts#{macro => Macro, arguments => Arguments};
update_top_macro(_Macro, Opts) ->
    Opts.

expand_macro(Macro) ->
    expand_macro(Macro, #{}).

expand_macro(#{pos := Pos, formatter := Formatter} = Macro, Opts) ->
    do([ traverse ||
           %% TODO: validate node1 as a erl_syntax node
           Node1 <- astranaut_traverse:update_pos(Pos, Formatter, invoke_macro_function(Macro)),
           Node2 <- update_quoted_variable_name(Node1, Macro, Opts),
           Node3 = astranaut_lib:replace_pos_zero(Node2, Pos),
           format_node(Node3, Macro),
           return(Node3)
       ]).

invoke_macro_function(#{module := Module, function := Function, arguments := Arguments} = Macro) ->
    try erlang:apply(Module, Function, Arguments) of
        Return ->
            astranaut:traverse_return(Return)
    catch
        Class:Exception?CAPTURE_STACKTRACE ->
            StackTraces1 =
                lists:takewhile(
                  fun({M, F, A, _Pos}) -> 
                          {M, F, A} =/= {?MODULE, invoke_macro_function, 1};
                     (_Stack) ->
                          false
                  end, ?GET_STACKTRACE),
            Error = macro_exception(Arguments, Class, Exception, StackTraces1, Macro),
            astranaut_traverse:fail(Error)
    end.

macro_exception(Arguments, Class, Exception, StackTraces, #{macro := {Module, Function}}) ->
    MFA = #{module => Module, function => Function, arity => length(Arguments)},
    {macro_exception, MFA, Arguments, {Class, Exception, StackTraces}};
%% replace `module`__local_macro with module in stacktrace
macro_exception(Arguments, Class, Exception, StackTraces,
                #{module := LocalModule, macro_module := Module, macro := Function}) ->
    StackTraces1 =
        lists:map(
          fun({M, F, A, Pos}) when M =:= LocalModule ->
                  {Module, F, A, Pos};
             (Val) ->
                  Val
          end, StackTraces),
    MFA = #{function => Function, arity => length(Arguments), local => true},
    {macro_exception, MFA, Arguments, {Class, Exception, StackTraces1}}.
    
should_transform_function(_Function, _Arity, all) ->
    true;
should_transform_function(Function, Arity, {except, Functions}) ->
    not ordsets:is_element({Function, Arity}, Functions);
should_transform_function(Function, Arity, LocalMacroCaller) ->
    ordsets:is_element({function, Function, Arity}, LocalMacroCaller).

%% for -exec_macro, if there is no macro found, error is returned
%% for other -Attr, if there is no macro with same name, not_macro is returned
%% for other -Attr, if there is macro with same name, but arity not matched, error is returned
attribute_find_macro({attribute, Pos, exec_macro, {Function, Arguments}}, Macros, _AttributeMacros) ->
    find_macro_with_arguments(Function, Arguments, Pos, Macros);
attribute_find_macro({attribute, Pos, exec_macro, {Module, Function, Arguments}}, Macros, _AttributeMacros) ->
    find_macro_with_arguments({Module, Function}, Arguments, Pos, Macros);
attribute_find_macro({attribute, Pos, Attribute, Arguments}, _Macros, AttributeMacros) ->
    find_attribute_macro_with_arguments(Attribute, Arguments, Pos, AttributeMacros);
attribute_find_macro(_Node, _Macros, _AttributeMacros) ->
    not_macro.

find_attribute_macro_with_arguments(Function, Arguments, Pos, AttributeMacroMap) ->
    case maps:find(Function, AttributeMacroMap) of
        {ok, MacroMap} ->
            find_macro_with_arguments(Function, Arguments, Pos, MacroMap);
        error ->
            not_macro
    end.



find_macro_with_arguments(MacroName, Arguments, Pos, Macros) ->
    Arguments1 = to_list(Arguments),
    Arity = length(Arguments1),
    case find_macro(MacroName, Arity, Macros) of
        {ok, Macro} ->
            Macro1 = Macro#{pos => Pos},
            Arguments2 = group_arguments(Arguments1, Macro1),
            Arguments3 = append_attrs(Arguments2, Macro1),
            {ok, Macro1#{arguments => Arguments3}};
        error ->
            error
    end.

find_macro(MacroName, Arity, Macros) ->
    case maps:find({MacroName, Arity}, Macros) of
        {ok, Macro} ->
            {ok, Macro};
        error ->
            case maps:find({MacroName, 1}, Macros) of
                {ok, Macro} ->
                    case maps:get(group_args, Macro, false) of
                        false ->
                            error;
                        true ->
                            {ok, Macro}
                    end;
                error ->
                    error
            end
    end.

macro_name_str(#{module := Module, function := _Function, arity := _Arity}) ->
    atom_to_list(Module).

group_arguments(Arguments, #{group_args := true}) ->
    [Arguments];
group_arguments(Arguments, #{}) ->
    Arguments.

append_attrs(Arguments, #{attributes := Attrs, pos := Pos}) ->
    Arguments ++ [Attrs#{pos => Pos}];
append_attrs(Arguments, #{}) ->
    Arguments.

update_quoted_variable_name(Nodes, Macro, #{rename_quoted_variables := true}) ->
    astranaut_traverse:state(
      fun(Counter) ->
              MacroNameStr = macro_name_str(Macro),
              CounterStr = integer_to_list(Counter),
              Nodes1 =
                  astranaut:smap(
                    fun({var, Pos, VarName} = Var) ->
                            case split_varname(atom_to_list(VarName)) of
                                [Head, MacroNameStr1] when MacroNameStr =:= MacroNameStr1 ->
                                    VarName1 = list_to_atom(Head ++ "@" ++ MacroNameStr ++ "_" ++ CounterStr),
                                    {var, Pos, VarName1};
                                _ ->
                                    Var
                            end;
                       (Node) ->
                            Node
                    end, Nodes, #{traverse => post}),
              {Nodes1, Counter + 1}
      end);
update_quoted_variable_name(Nodes, _Macro, #{}) ->
    astranaut_traverse:return(Nodes).

split_varname(String) ->
    case lists:splitwith(
           fun(Char) ->
                   Char /= $@
           end, String) of
        {Head, [$@|Tail]} ->
            [Head, Tail];
        {Head, []} ->
            [Head]
    end.

%%%===================================================================
%%% format functions.
%%%===================================================================
format_forms(Forms, Opts) ->
    case maps:get(debug_module, Opts, false) of
        true ->
            lists:map(
              fun(Form) ->
                      io:format("~s~n", [astranaut_lib:ast_safe_to_string(Form)])
              end, Forms);
        false ->
            ok
    end,
    case maps:get(debug_module_ast, Opts, false) of
        true ->
            io:format("~p~n", [Forms]);
        false ->
            ok
    end.

format_node(Node, #{file := File, pos := Pos} = Opts) ->
    case maps:get(debug, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Pos, format_mfa(Opts)]),
            io:format("~s~n", [astranaut_lib:ast_safe_to_string(Node)]);
        false ->
            ok
    end,
    case maps:get(debug_ast, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Pos, format_mfa(Opts)]),
            io:format("~p~n", [Node]);
        false ->
            ok
    end.

format_mfa(#{function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(#{module := Module, function := Function, arity := Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).
