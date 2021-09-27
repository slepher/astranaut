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
             %% load macros from attributes and transform -export_macro to -exported_macro
             %% add nowarn_unused_function compile options to -local_macro if it's not exported
             %% -exported_macro is validated -export_macro attribute.
             {Forms1, MacroModules, Macros, GlobalMacroOpts} <- load_attributes(Forms),
             %% transform macros
             astranaut_return:lift_m(
               fun(Forms2) ->
                       format_forms(Forms2, GlobalMacroOpts),
                       Forms2
               end, transform_macros(Module, MacroModules, Macros, Forms1, Options))
         ])).

-spec format_error(term()) -> term().
format_error({import_macro_failed, Module}) ->
    io_lib:format("could not import macro from ~p, update compile file order in Makefile or add to erl_first_files in rebar.config to make it compile first.", [Module]);
format_error({unimported_macro_module, Module}) ->
    io_lib:format("-import_macro(~p). required.", [Module]);
format_error({unexported_macro, Module, Function, Arity}) ->
    io_lib:format("unexported macro ~p:~p/~p.", [Module, Function, Arity]);
format_error({undefined_macro, Function, Arity}) ->
    io_lib:format("undefined macro ~p/~p.", [Function, Arity]);
format_error({invalid_use_macro, Opts}) ->
    io_lib:format("invalid use macro ~p.", [Opts]);
format_error({non_exported_formatter, Module}) ->
    io_lib:format("format_error/1 is not exported from module ~p.", [Module]);
format_error({unloaded_formatter_module, Module}) ->
    io_lib:format("formatter module ~p could not be loaded.", [Module]);
format_error(invalid_macro_attribute) ->
    io_lib:format("invalid attribute macro call: macro not found", []);
format_error({macro_exception, MFA, Arguments, Exception}) ->
    io_lib:format("apply macro ~s ~p failed:~n~s",
                  [format_mfa(MFA), Arguments, eunit_lib:format_exception(Exception)]);
format_error(Error) ->
    astranaut:format_error(Error).
%%%===================================================================
%%% analyze -export_macro -use_macro attributes functions
%%%===================================================================
load_attributes(Forms) ->
    File = astranaut_lib:analyze_forms_file(Forms),
    Module = astranaut_lib:analyze_forms_module(Forms),
    do([ return ||
           Validator = global_macro_validator(),
           GlobalMacroOpts <- astranaut_lib:validate_attribute_option(Validator, ?MODULE, macro_options, Forms),
           {Forms1, ExportedMacros} <- exported_macros(Forms),
           {ImportedModules, ImportedMacros} <- imported_macros(GlobalMacroOpts, Forms),
           {Forms2, LocalMacros} <- local_macros(Module, GlobalMacroOpts, ExportedMacros, Forms1),
           UsedMacros <- used_macros(File, Module, ImportedMacros, LocalMacros, Forms2),
           return({Forms2, ImportedModules, UsedMacros, GlobalMacroOpts})
       ]).

formatter_opts(Module, Functions, MacroOpts) ->
    FormatError = {format_error, 1},
    case lists:member(FormatError, Functions) of
        true ->
            MacroOpts#{formatter => Module};
        false ->
            MacroOpts#{formatter => astranaut_macro}
    end.

exported_macros(Forms) ->
    astranaut_lib:forms_with_attribute(
      fun(Attr, Acc, #{pos := Pos}) ->
              do([ return ||
                     Validator = macro_definition_valitor(),
                     {FAs, Options} <-
                         validate_macro_attribute(fun macro_without_module_attr/1, Validator, export_macro, Attr),
                     %% export_macro options for local usage
                     ExportedMacros = lists:foldl(fun(FA, Acc1) -> maps:put(FA, Options, Acc1) end, Acc, FAs),
                     %% exported_macro options for external usage
                     ExportedMacroAttribute = astranaut_lib:gen_attribute_node(exported_macro, Pos, [{FAs, Options}]),
                     ExportAttribute = astranaut_lib:gen_attribute_node(export, Pos, FAs),
                     astranaut_return:return({[ExportAttribute, ExportedMacroAttribute], ExportedMacros})
                 ])
      end, #{}, Forms, export_macro, #{formatter => ?MODULE}).

%% analyze -import_macro attributes.
imported_macros(GlobalMacroOpts, Forms) ->
    astranaut_return:lift_m(
      fun({Modules, MacroMap}) ->
              {lists:reverse(Modules), MacroMap}
      end,
      astranaut_lib:with_attribute(
        fun(Module, {ModulesAcc, MacroMapAcc} = Acc) when is_atom(Module) ->
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
                        astranaut_return:error_ok({import_macro_failed, Module}, Acc)
                end;
           (Attr, Acc) ->
                astranaut_return:error_ok({invalid_import_macro_attr, Attr}, Acc)
        end, {[], #{}}, Forms, import_macro, #{formatter => ?MODULE})).

is_loaded(Module) ->
    code:ensure_loaded(Module),
    code:is_loaded(Module).

local_macros(Module, GlobalMacroOpts, ExportedMacros, Forms) ->
    FormsProp = erl_syntax_lib:analyze_forms(Forms),
    Functions = proplists:get_value(functions, FormsProp, []),
    GlobalMacroOpts1 = formatter_opts(local_macro_module(Module), Functions, GlobalMacroOpts),
    astranaut_return:lift_m(
      fun({Forms1, LocalMacros}) ->
              {Forms1, maps:map(
                         fun({Function, Arity}, MacroOptions) ->
                                 MacroOptions1 = maps:merge(GlobalMacroOpts1, MacroOptions),
                                 MacroOptions1#{module => local_macro_module(Module),
                                                macro_module => Module,
                                                macro => Function,
                                                function => Function,
                                                arity => Arity}
                         end, LocalMacros)}
      end,
      astranaut_lib:forms_with_attribute(
        fun(Attr, Acc, #{pos := Pos}) ->
                do([ return || 
                       Validator = macro_definition_valitor(),
                       {FAs, Options} <-
                           validate_macro_attribute(fun macro_without_module_attr/1, Validator, local_macro, Attr),
                       NoWarnNodes = astranaut_lib:gen_attribute_node(compile, Pos, {nowarn_unused_function, FAs}),
                       Acc2 = 
                           lists:foldl(
                             fun({Function, Arity}, Acc1) ->
                                     Options1 = maps:get({Function, Arity}, Acc1, #{}),
                                     Options2 = maps:merge(Options1, Options),
                                     maps:put({Function, Arity}, Options2, Acc1)
                             end, Acc, FAs),
                       return({[NoWarnNodes], Acc2})
                   ])
        end, ExportedMacros, Forms, local_macro, #{formatter => ?MODULE})).

used_macros(File, Module, ImportedMacros, LocalMacros, Forms) ->
    UsedMacros = maps:put(Module, LocalMacros, ImportedMacros),
    astranaut_return:lift_m(
      fun(UsedMacros2) ->
              maps:map(
                fun(_MacroModule, ModuleMacros) ->
                        maps:fold(
                          fun(_MFA, MacroOptions, Acc) ->
                                  MacroOptions1 = MacroOptions#{file => File, local_module => Module},
                                  MacroOptions2 = update_as_attr(MacroOptions1),
                                  MacroOptions3 = inject_attrs(MacroOptions2, Forms),
                                  #{macro := Macro, call_arity := CallArity} = MacroOptions4 = update_call_arity(MacroOptions3),
                                  maps:put({Macro, CallArity}, MacroOptions4, Acc)
                          end, #{}, ModuleMacros)
                end, UsedMacros2)
      end,
      astranaut_lib:with_attribute(
        fun(Attr, UsedMacrosAcc) ->
                do([ return ||
                       Validator = use_macro_validator(),
                       {MFAs, Options}
                           <- validate_macro_attribute(fun macro_attr/1, Validator, use_macro, Attr),
                       case MFAs of
                           {ImportedModule, FAs} ->
                               case maps:find(ImportedModule, UsedMacrosAcc) of
                                   {ok, ModuleMacros} ->
                                       do([ return ||
                                              ModuleMacros1 <- update_used_macros(ImportedModule, FAs, Options, ModuleMacros),
                                              return(maps:put(ImportedModule, ModuleMacros1, UsedMacrosAcc))
                                          ]);
                                   error ->
                                       astranaut_return:error_ok({unimported_macro_module, ImportedModule}, UsedMacrosAcc)
                               end;
                           FAs ->
                               do([ return ||
                                      LocalMacrosAcc = maps:get(Module, UsedMacrosAcc),
                                      LocalMacrosAcc1 <- update_local_used_macros(FAs, Options, LocalMacrosAcc),
                                      return(maps:put(Module, LocalMacrosAcc1, UsedMacrosAcc))
                                  ])
                       end
                   ])
        end, UsedMacros, Forms, use_macro, #{formatter => ?MODULE})).

update_used_macros(Module, FAs, UsedMacroOptions, MacroMap) ->
    astranaut_return:foldl_m(
      fun({Function, Arity}, Acc) ->
              case maps:find({Function, Arity}, MacroMap) of
                  {ok, MacroOptions} ->
                      MacroOptions1 = maps:merge(MacroOptions, UsedMacroOptions),
                      MacroOptions2 = update_alias(MacroOptions1),
                      astranaut_return:return(maps:put({Function, Arity}, MacroOptions2, Acc));
                  error ->
                      astranaut_return:error_ok({macro_not_exported, {Module, Function, Arity}}, Acc)
              end
      end, MacroMap, FAs).

update_local_used_macros(FAs, UsedMacroOptions, MacroMap) ->
    astranaut_return:foldl_m(
      fun({Function, Arity}, Acc) ->
              case maps:find({Function, Arity}, Acc) of
                  {ok, MacroOptions} ->
                      MacroOptions1 = maps:merge(MacroOptions, UsedMacroOptions),
                      MacroOptions2 = update_alias(MacroOptions1),
                      astranaut_return:return(maps:put({Function, Arity}, MacroOptions2, Acc));
                  error ->
                      astranaut_return:error_ok({macro_not_defined, {Function, Arity}}, Acc)
              end
      end, MacroMap, FAs).

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
      alias => atom
     }.

global_macro_validator() ->
    #{
      debug => boolean,
      debug_ast => boolean,
      debug_module => boolean,
      debug_module_ast => boolean
     }.

macro_definition_valitor() ->
    #{as_attr => atom,
      order => {one_of, [outer, inner]},
      inject_attrs => {'or', [atom, {list_of, atom}]},
      group_args => boolean
     }.

validate_mfas({Module, FAs}) when is_atom(Module) ->
    validate_fas(FAs);
validate_mfas(FAs) when is_list(FAs) ->
    validate_fas(FAs).

validate_fas([{Function, Arity}|T]) when is_atom(Function), is_integer(Arity) ->
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
%% Step 1. transform forms with external macros to forms1
%% Step 2. find local macro and it's related functions
%% Step 3. find rest functions and attributes which call local macro, name it local_macro_caller
%% Step 4. if local_macro_caller is empty, skip the rest step
%% Step 5. compile local macro and it's related functions from forms1, load it as local_macro_module.
%% Step 6. parse_transform forms1 with loaded local_macro_module as external macro.
transform_macros(Module, MacroModules, Macros, Forms, CompileOpts) ->
    do([ return ||
           Forms1 <- transform_external_macros(MacroModules, Macros, Forms),
           LocalMacroMap = maps:get(Module, Macros, #{}),
           LocalAttributeMacroMap = attribute_macro_map(LocalMacroMap),
           LocalMacroFunctions =
               maps:fold(
                 fun(_Macro, #{function := Function, arity := Arity}, Acc) ->
                         ordsets:add_element({Function, Arity}, Acc)
                 end, ordsets:new(), LocalMacroMap),
           ClauseMap = function_clauses_map(Forms1, maps:new()),
           LocalMacroFunctions1 =
               case maps:is_key({format_error, 1}, ClauseMap) of
                   true ->
                       ordsets:add_element({format_error, 1}, LocalMacroFunctions);
                   false ->
                       LocalMacroFunctions
               end,
           LocalMacroRelatedFunctions = local_macro_related_functions(LocalMacroFunctions1, ClauseMap),
           case local_macro_caller(Forms1, LocalMacroMap, LocalAttributeMacroMap, LocalMacroRelatedFunctions) of
               [] ->
                   astranaut_return:return(Forms1);
               LocalMacroCaller ->
                   do([ return ||
                          load_local_macro_forms(LocalMacroFunctions1, LocalMacroRelatedFunctions, Forms1, CompileOpts),
                          Forms2 <- transform_attribute_macros(LocalMacroMap, LocalAttributeMacroMap, Forms1),
                          transform_call_macros(Module, LocalMacroMap, Forms2, LocalMacroCaller)
                      ])
           end
       ]).

transform_external_macros(MacroModules, ModuleMacroMap, Forms) ->
    astranaut_return:foldl_m(
      fun(MacroModule, FormsAcc) ->
              MacroMap = maps:get(MacroModule, ModuleMacroMap, #{}),
              AttributeMacroMap = attribute_macro_map(MacroMap),
              do([ return ||
                     FormsAcc1 <- transform_attribute_macros(MacroMap, AttributeMacroMap, FormsAcc),
                     transform_call_macros(MacroModule, MacroMap, FormsAcc1, all)
                 ])
      end, Forms, MacroModules).

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

load_local_macro_forms(LocalMacroFunctions, LocalMacroRelatedFunctions, Forms, CompileOpts) ->
    Forms1 =
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
            end, [], Forms)),
    ExtraExports =
        lists:foldl(
          fun(Export, Acc) ->
                  [astranaut_lib:gen_exports([Export], 0)|Acc]
          end, [], LocalMacroFunctions),
    Forms2 = astranaut_syntax:sort_forms(Forms1 ++ ExtraExports),
    astranaut_lib:load_forms(Forms2, [without_warnings|CompileOpts]).

append_if(Boolean, Form, Forms) ->
    case Boolean of
        true ->
            [Form|Forms];
        false ->
            Forms
    end.

transform_attribute_macros(MacroMap, AttributeMacroMap, Forms) ->
    Monad =
        astranaut:map_m(
          fun(Form) ->
                  case attribute_find_macro(Form, MacroMap, AttributeMacroMap) of
                      {ok, Macro} ->
                          apply_macro(Macro);
                      error ->
                          astranaut_traverse:then(
                            astranaut_traverse:warning(invalid_macro_attribute),
                            astranaut_traverse:return(Form));
                      not_macro ->
                          astranaut_traverse:return(Form)
                  end
          end, Forms, #{traverse => none}),
    astranaut_traverse:eval(Monad, ?MODULE, #{}, ok).

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

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].
%%%===================================================================
%%% transform macro MacroModule:MacroFun(Arguments) and it's help functions.
%%%===================================================================
transform_call_macros(Module, MacroMap, Forms, TransformFunctions) ->
    Monad =
        astranaut:map_m(
          fun({function, _Pos, Name, Arity, _Clauses} = Function) ->
                  case should_transform_function(Name, Arity, TransformFunctions) of
                      false ->
                          astranaut_traverse:return(Function);
                      true ->
                          astranaut:map_m(
                            fun(Clause) ->
                                    transform_call_macros_clause(Module, MacroMap, Clause)
                            end, Function, #{traverse => subtree})
                  end;
             (Form) ->
                  astranaut_traverse:return(Form)
          end, Forms, #{traverse => none}),
    astranaut_traverse:eval(Monad, ?MODULE, #{}, 0).

transform_call_macros_clause(Module, MacroMap, Clause) ->
    do([ traverse ||
           %% counter is reseted in every function clause
           astranaut_traverse:put(1),
           astranaut:map_m(
             fun(Node) ->
                     do([ traverse ||
                            #{step := Step} <- astranaut_traverse:ask(),
                            case call_find_macro(Module, Node, MacroMap) of
                                {ok, Macro} ->
                                    case match_macro_order(Macro, Step) of
                                        true ->
                                            apply_macro(Macro#{rename_quoted_variables => true});
                                        false ->
                                            astranaut_traverse:return(Node)
                                    end;
                                error ->
                                    astranaut_traverse:return(Node)
                            end
                        ])
             end, Clause, #{traverse => all})
       ]).

match_macro_order(Macro, Step) ->
    Order = maps:get(order, Macro, inner),
    ((Order =:= inner) and (Step =:= post))
        or ((Order =:= outer) and (Step =:= pre)).

%%%===================================================================
%%% apply macro functions
%%%===================================================================
apply_macro(#{pos := Pos, formatter := Formatter} = Opts) ->
    do([ traverse ||
           %% TODO: validate node1 as a erl_syntax node
           Node1 <- astranaut_traverse:update_pos(Pos, Formatter, apply_mfa(Opts)),
           Node2 <- update_quoted_variable_name(Node1, Opts),
           Node3 = astranaut_lib:replace_pos_zero(Node2, Pos),
           format_node(Node3, Opts),
           return(Node3)
       ]).

apply_mfa(#{module := Module, function := Function, arguments := Arguments} = Opts) ->
    try erlang:apply(Module, Function, Arguments) of
        Return ->
            astranaut:traverse_return(Return)
    catch
        Class:Exception:StackTrace ->
            StackTraces1 =
                lists:takewhile(
                  fun({M, F, A, _Pos}) -> 
                          {M, F, A} =/= {?MODULE, apply_mfa, 1};
                     (_Stack) ->
                          false
                  end, StackTrace),
            Error = macro_exception(Arguments, Class, Exception, StackTraces1, Opts),
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
should_transform_function(Function, Arity, LocalMacroCaller) ->
    ordsets:is_element({function, Function, Arity}, LocalMacroCaller).

local_macro_caller(Forms, LocalMacroMap, LocalAttributeMacroMap, LocalMacroRelatedFunctions) ->
    case maps:size(LocalMacroMap) of
        0 ->
            ordsets:new();
        _ ->
            lists:foldl(
              fun({function, _Pos, Function, Arity, Clauses}, Acc) ->
                      IsLocalMacroCaller = 
                          case ordsets:is_element({function, Function, Arity}, LocalMacroRelatedFunctions) of
                              true ->
                                  false;
                              false ->
                                  with_local_function_call(
                                    fun(CallFunction, CallArity, false) ->
                                            maps:is_key({CallFunction, CallArity}, LocalMacroMap);
                                       (_CallFunction, _CallArity, true) ->
                                            true
                                    end, false, Clauses)
                          end,
                      case IsLocalMacroCaller of
                          true ->
                              ordsets:add_element({function, Function, Arity}, Acc);
                          false ->
                              Acc
                      end;
                 ({attribute, _Pos, Attribute, AttributeValue} = Node, Acc) ->
                      case attribute_find_macro(Node, LocalMacroMap, LocalAttributeMacroMap) of
                          {ok, _} ->
                              ordsets:add_element({attribute, Attribute, AttributeValue}, Acc);
                          error ->
                              Acc;
                          not_macro ->
                              Acc
                      end;
                 (_Node, Acc) ->
                      Acc
              end, ordsets:new(), Forms)
    end.

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

call_find_macro(_Module, {call, Pos1, {atom, _Pos2, Function}, Arguments}, Macros) ->
    find_macro_with_arguments(Function, Arguments, Pos1, Macros);
call_find_macro(Module, {call, Pos1, {remote, _Pos2, {atom, _Pos3, Module}, {atom, _Pos4, Function}}, Arguments},
                Macros) ->
    find_macro_with_arguments({Module, Function}, Arguments, Pos1, Macros);
call_find_macro(_Module, _Node, _Macros) ->
    error.

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

update_quoted_variable_name(Nodes, #{rename_quoted_variables := true} = Macro) ->
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
update_quoted_variable_name(Nodes, #{}) ->
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
