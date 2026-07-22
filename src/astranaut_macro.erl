%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc Parse-transform entry point and macro pass orchestration.
%%%
%%% Source-ordered scanning is implemented by astranaut_macro_scan, macro
%%% declarations and environments by astranaut_macro_registry, expansion by
%%% astranaut_macro_expander, and local-macro lifecycle management by
%%% astranaut_macro_local.
%%% @end
%%%-------------------------------------------------------------------

-module(astranaut_macro).

-include("do.hrl").

-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% Parse transform and diagnostics
%%%===================================================================

-spec parse_transform(astranaut:forms(), compile:option()) ->
          astranaut:parse_transform_return().
parse_transform(Forms, Options) ->
    astranaut_return:to_compiler(
      do([ return ||
             Module = astranaut_lib:analyze_forms_module(Forms),
             File = astranaut_lib:analyze_forms_file(Forms),
             GlobalMacroOpts0 <-
                 astranaut_macro_registry:default_options(),
             {AttributeForms, FunctionEnv} <-
                 run_attribute_pass(
                   Module, File, GlobalMacroOpts0, Forms, Options),
             FunctionForms <-
                 run_function_macro_pass(AttributeForms, FunctionEnv),
             format_forms(
               FunctionForms,
               maps:get(global_macro_opts, FunctionEnv)),
             return(FunctionForms)
         ])).

-spec format_error(term()) -> term().
format_error({import_macro_failed, Module}) ->
    io_lib:format(
      "could not import macro from module ~p, update compile file order in Makefile or add to erl_first_files in rebar.config to make it compile first.",
      [Module]);
format_error({invalid_import_macro_attr, Macro}) ->
    io_lib:format(
      "~p is not a valid module in -import_macro(~p). ",
      [Macro, Macro]);
format_error({unimported_macro_module, Module}) ->
    io_lib:format(
      "-import_macro(~p). is required before use of -use_macro",
      [Module]);
format_error({unexported_macro, Module, Function, Arity}) ->
    io_lib:format(
      "unexported macro ~p:~p/~p.",
      [Module, Function, Arity]);
format_error({undefined_macro, Function, Arity}) ->
    io_lib:format("macro ~p/~p undefined.", [Function, Arity]);
format_error({invalid_use_macro, Opts}) ->
    io_lib:format("invalid use macro ~p.", [Opts]);
format_error(
  {macro_override, MacroKey, ExistingMacro, OverridingMacro}) ->
    io_lib:format(
      "macro ~p is already defined by ~p and cannot be overridden by ~p without force_override.",
      [MacroKey,
       format_macro_ref(ExistingMacro),
       format_macro_ref(OverridingMacro)]);
format_error({non_exported_formatter, Module}) ->
    io_lib:format(
      "format_error/1 is not exported from module ~p.", [Module]);
format_error({unloaded_formatter_module, Module}) ->
    io_lib:format(
      "formatter module ~p could not be loaded.", [Module]);
format_error(invalid_macro_attribute) ->
    io_lib:format("invalid attribute macro call: macro not found", []);
format_error(
  {max_macro_expansion_depth_exceeded,
   {MacroModule, Function}, Arguments}) ->
    io_lib:format(
      "maximum macro expansion depth exceeded when applying macro ~p:~p with arguments ~p.",
      [MacroModule, Function, Arguments]);
format_error(
  {max_macro_expansion_depth_exceeded, Function, Arguments}) ->
    io_lib:format(
      "maximum macro expansion depth exceeded when applying macro ~p with arguments ~p.",
      [Function, Arguments]);
format_error({macro_exception, MFA, Arguments, Exception}) ->
    io_lib:format(
      "apply macro ~s ~p failed:~n~s",
      [astranaut_macro_expander:format_mfa(MFA),
       Arguments,
       eunit_lib:format_exception(Exception)]);
format_error({invalid_macro_return, Detail}) ->
    io_lib:format(
      "macro ~s returned invalid AST: ~p",
      [astranaut_macro_expander:format_mfa(
         invalid_macro_return_mfa(Detail)),
       Detail]);
format_error({invalid_extra_functions, Functions}) ->
    io_lib:format(
      "extra_functions contains undefined functions: ~p", [Functions]);
format_error({undefined_internal_functions, Functions}) ->
    io_lib:format(
      "internal_function contains macros not visible at the declaration point: ~p",
      [Functions]);
format_error({undefined_local_macro_retain, Functions}) ->
    io_lib:format(
      "local_macro_retain contains undefined functions: ~p", [Functions]);
format_error({ineffective_local_macro_retain, Functions}) ->
    io_lib:format(
      "local_macro_retain has no effect for functions outside every local macro closure: ~p",
      [Functions]);
format_error({duplicate_local_macro_declaration, Function}) ->
    io_lib:format(
      "duplicate local macro declaration for ~p", [Function]);
format_error(
  {conflicting_internal_function_policy, Function, Policies}) ->
    io_lib:format(
      "conflicting internal_function policy for ~p: ~p",
      [Function, Policies]);
format_error({conflicting_local_macro_closure_environment, FormId}) ->
    io_lib:format(
      "local macro closure has conflicting expansion environments for ~p",
      [FormId]);
format_error({conflicting_local_macro_whitelist, FormId, Detail}) ->
    io_lib:format(
      "local macro closure has conflicting whitelist for ~p: ~p",
      [FormId, Detail]);
format_error({illegal_locked_form_mutation, Form}) ->
    io_lib:format(
      "local macro expansion modified frozen form: ~p", [Form]);
format_error(local_macro_module_in_use) ->
    io_lib:format(
      "local macro module is in use and cannot be safely replaced", []);
format_error({illegal_macro_environment_mutation, Form}) ->
    io_lib:format(
      "local macro expansion generated illegal macro environment form: ~p",
      [Form]);
format_error({illegal_local_macro_definition_mutation, Form}) ->
    io_lib:format(
      "local macro expansion modified locked local macro snapshot form: ~p",
      [Form]);
format_error(Error) ->
    astranaut:format_error(Error).

format_macro_ref(
  #{macro_module := Module, function := Function, arity := Arity}) ->
    {Module, Function, Arity};
format_macro_ref(Macro) ->
    Macro.

invalid_macro_return_mfa(#{macro := #{mfa := MFA}}) ->
    MFA;
invalid_macro_return_mfa(#{current_macro := #{mfa := MFA}}) ->
    MFA.

%%%===================================================================
%%% Attribute pass orchestration
%%%===================================================================

run_attribute_pass(Module, File, GlobalMacroOpts0, Forms, CompileOpts) ->
    do([ return ||
           {ScannedForms, ScanState} <-
               astranaut_macro_scan:run(
                 Module, File, GlobalMacroOpts0, Forms, CompileOpts),
           #{registry := Registry,
             local_macro_state := ScanLocalState,
             local_declarations := LocalDeclarations} = ScanState,
           AttributeForms = drop_local_declarations(
                              ScannedForms, LocalDeclarations),
           PreparedForms <-
               astranaut_macro_registry:prepare_exports(AttributeForms),
           finalize_attribute_macro_pass(
             File, Registry, ScanLocalState,
             PreparedForms, CompileOpts)
       ]).

drop_local_declarations(Forms, Declarations) ->
    [Form || Form <- Forms, not maps:is_key(Form, Declarations)].

finalize_attribute_macro_pass(
  File, Registry, ScanLocalState, Forms, CompileOpts) ->
    do([ return ||
           WorkflowContext = #{source_view => Forms,
                               compile_opts => CompileOpts},
           RetainRoots = retain_roots(Forms),
           {FinalLocalEnv, FinalSkipIds, FinalLocalState} <-
               astranaut_macro_local:finalize(
                 RetainRoots, WorkflowContext, ScanLocalState),
           RetainWarnings = local_macro_retain_warnings(
                              Forms, FinalLocalState),
           {UnsortedAttributeForms, FunctionEnv0} <-
               finalize_attribute_forms(
                 Forms, Registry, FinalLocalEnv,
                 FinalSkipIds, FinalLocalState),
           AttributeForms =
               astranaut_forms:sort_forms(UnsortedAttributeForms),
           FunctionEnv = FunctionEnv0#{
                           global_macro_opts =>
                               astranaut_macro_registry:
                                 global_macro_opts(Registry)},
           astranaut_return:then(
             file_formatted_warnings(File, RetainWarnings),
             return({AttributeForms, FunctionEnv}))
       ]).

file_formatted_warnings(File, Warnings) ->
    Error0 = astranaut_error:new(File),
    Error1 = astranaut_error:append_formatted_warnings(Warnings, Error0),
    astranaut_return:ok(ok, astranaut_error:eof(Error1)).

finalize_attribute_forms(
  Forms, Registry, FinalLocalEnv, FinalSkipIds, FinalLocalState) ->
    do([ return ||
           Forms1 = remove_final_skip_forms(Forms, FinalSkipIds),
           FinalMacroEnvironment =
               astranaut_macro_registry:final_macro_environment(
                 Forms1, FinalLocalEnv, Registry),
           ResolvedFinalMacroMap =
               maps:get(macro_map, FinalMacroEnvironment),
           FunctionCallAnalysis =
               astranaut_macro_expander:function_call_analysis(
                 Forms1, ResolvedFinalMacroMap, presence),
           DetectedMacroCallers =
               astranaut_macro_expander:function_macro_callers(
                 FunctionCallAnalysis),
           RetainedFunctionIds = ordsets:from_list(
                                   [Id ||
                                      Id = {function, _, _} <-
                                          astranaut_macro_local:
                                            retained_form_ids(
                                              FinalLocalState)]),
           FinalMacroCallers = ordsets:union(
                                 DetectedMacroCallers,
                                 RetainedFunctionIds),
           FinalSkipFunctionIds = ordsets:from_list(
                                    [{function, Name, Arity}
                                     || {function, Name, Arity} <-
                                            FinalSkipIds]),
           FunctionEnv =
               #{macro_environment => FinalMacroEnvironment,
                 local_macro_state => FinalLocalState,
                 function_call_analysis => FunctionCallAnalysis,
                 callers => ordsets:subtract(
                              FinalMacroCallers,
                              FinalSkipFunctionIds)},
           return({Forms1, FunctionEnv})
       ]).

%%%===================================================================
%%% Function pass orchestration
%%%===================================================================

run_function_macro_pass(
  Forms,
  #{macro_environment := MacroEnvironment,
    local_macro_state := LocalMacroState,
    function_call_analysis := FunctionCallAnalysis,
    callers := Callers}) ->
    do([ return ||
           {ExpandedForms, _FinalState} <-
               astranaut_macro_local:expand_final_functions(
                 Forms,
                 [{Name, Arity}
                  || {function, Name, Arity} <- Callers],
                 MacroEnvironment#{function_call_analysis =>
                                       FunctionCallAnalysis},
                 LocalMacroState),
           return(ExpandedForms)
       ]).

%%%===================================================================
%%% Final form materialization and retain diagnostics
%%%===================================================================

remove_final_skip_forms(Forms, FinalSkipIds) ->
    Skip = ordsets:from_list(FinalSkipIds),
    lists:flatmap(
      fun(Form) -> remove_final_skip_form(Form, Skip) end,
      Forms).

remove_final_skip_form(
  {attribute, Pos, compile, {nowarn_unused_function, FAs}}, Skip) ->
    RemainingFAs =
        [FA || FA = {Name, Arity} <- FAs,
               not ordsets:is_element(
                     {function, Name, Arity}, Skip)],
    case RemainingFAs of
        [] -> [];
        _ ->
            [{attribute, Pos, compile,
              {nowarn_unused_function, RemainingFAs}}]
    end;
remove_final_skip_form(Form, Skip) ->
    case ordsets:is_element(
           astranaut_macro_local:form_id(Form), Skip) of
        true -> [];
        false -> [Form]
    end.

retain_roots(Forms) ->
    lists:foldl(
      fun({attribute, _Pos, local_macro_retain, Attr}, Acc) ->
              retain_fas(Attr, Acc);
         ({attribute, _Pos, export_macro, Attr}, Acc) ->
              retain_fas(Attr, Acc);
         ({attribute, _Pos, export, Attr}, Acc) ->
              retain_fas(Attr, Acc);
         (_Form, Acc) ->
              Acc
      end, [], Forms).

local_macro_retain_warnings(Forms, LocalMacroState) ->
    DefinedFAs = ordsets:from_list(
                   maps:keys(function_clauses_map(Forms, #{}))),
    lists:flatmap(
      fun({attribute, Pos, local_macro_retain, Attr}) ->
              Roots = ordsets:from_list(retain_fas(Attr, [])),
              Undefined = ordsets:subtract(Roots, DefinedFAs),
              Existing = ordsets:subtract(Roots, Undefined),
              Nonclosure =
                  astranaut_macro_local:nonclosure_retain_roots(
                    Existing, LocalMacroState),
              retain_root_warnings(Pos, Undefined, Nonclosure);
         (_Form) ->
              []
      end, Forms).

retain_root_warnings(Pos, Undefined, Nonclosure) ->
    [{Pos, astranaut_macro,
      {undefined_local_macro_retain, Undefined}}
     || Undefined =/= []] ++
    [{Pos, astranaut_macro,
      {ineffective_local_macro_retain, Nonclosure}}
     || Nonclosure =/= []].

retain_fas({FAs, _Options}, Acc) when is_list(FAs) ->
    retain_fas(FAs, Acc);
retain_fas(FAs, Acc) when is_list(FAs) ->
    [FA || FA = {Name, Arity} <- FAs,
           is_atom(Name), is_integer(Arity)] ++ Acc;
retain_fas({Name, Arity} = FA, Acc)
  when is_atom(Name), is_integer(Arity) ->
    [FA | Acc];
retain_fas(_Other, Acc) ->
    Acc.

function_clauses_map(
  [{function, _Pos, Name, Arity, Clauses} | T], Acc) ->
    function_clauses_map(
      T, maps:put({Name, Arity}, Clauses, Acc));
function_clauses_map([_H | T], Acc) ->
    function_clauses_map(T, Acc);
function_clauses_map([], Acc) ->
    Acc.

%%%===================================================================
%%% Debug output
%%%===================================================================

format_forms(Forms, Opts) ->
    case maps:get(debug_module, Opts, false) of
        true ->
            lists:map(
              fun(Form) ->
                      io:format(
                        "~s~n",
                        [astranaut_lib:ast_safe_to_string(Form)])
              end, Forms);
        false ->
            ok
    end,
    case maps:get(debug_module_ast, Opts, false) of
        true -> io:format("~p~n", [Forms]);
        false -> ok
    end.
