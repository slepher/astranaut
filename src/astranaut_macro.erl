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
-include("otp_vsn.hrl").

-export([parse_transform/2, format_error/1, format_error/2]).

%%%===================================================================
%%% Parse transform and diagnostics
%%%===================================================================

-spec parse_transform(astranaut:forms(), [compile:option()]) ->
          astranaut:parse_transform_return().
parse_transform(Forms, Options) ->
    Module = astranaut_lib:analyze_forms_module(Forms),
    File = astranaut_lib:analyze_forms_file(Forms),
    astranaut_return:to_compiler(
      do([ return ||
             GlobalMacroOpts0 <-
                 astranaut_macro_registry:default_options(),
             {AttributeForms, FunctionEnv} <-
                 run_attribute_pass(
                   Module, File, GlobalMacroOpts0, Forms, Options),
             FunctionForms <-
                 run_function_macro_pass(
                   AttributeForms, FunctionEnv),
             format_forms(
               FunctionForms,
               maps:get(global_macro_opts, FunctionEnv)),
             return(FunctionForms)
         ])).

-spec format_error(term()) -> term().
format_error(Error) ->
    format_error(Error, #{}).

-spec format_error(term(), map()) -> term().
format_error(Error, Options) ->
    try format_error_1(Error) of
        Formatted ->
            Formatted
    catch
        error:function_clause:Stacktrace ->
            case format_error_1_no_match(Stacktrace) of
                true ->
                    astranaut:format_error(Error, Options);
                false ->
                    erlang:raise(error, function_clause, Stacktrace)
            end
    end.

-spec format_error_1(term()) -> term().
format_error_1({import_macro_failed, Module}) ->
    io_lib:format(
      "could not import macro from module ~p, update compile file order in Makefile or add to erl_first_files in rebar.config to make it compile first.",
      [Module]);
format_error_1({invalid_import_macro_attr, Macro}) ->
    io_lib:format(
      "~p is not a valid module in -import_macro(~p). ",
      [Macro, Macro]);
format_error_1({unimported_macro_module, Module}) ->
    io_lib:format(
      "-import_macro(~p). is required before use of -use_macro",
      [Module]);
format_error_1({unexported_macro, Module, Function, Arity}) ->
    io_lib:format(
      "unexported macro ~p:~p/~p.",
      [Module, Function, Arity]);
format_error_1({undefined_macro, Function, Arity}) ->
    io_lib:format("macro ~p/~p undefined.", [Function, Arity]);
format_error_1({invalid_use_macro, Opts}) ->
    io_lib:format("invalid use macro ~p.", [Opts]);
format_error_1(
  {macro_override, MacroKey, ExistingMacro, OverridingMacro}) ->
    io_lib:format(
      "macro ~p is already defined by ~p and cannot be overridden by ~p without force_override.",
      [MacroKey,
       format_macro_ref(ExistingMacro),
       format_macro_ref(OverridingMacro)]);
format_error_1({non_exported_formatter, Module}) ->
    io_lib:format(
      "format_error/1 is not exported from module ~p.", [Module]);
format_error_1({unloaded_formatter_module, Module}) ->
    io_lib:format(
      "formatter module ~p could not be loaded.", [Module]);
format_error_1(invalid_macro_attribute) ->
    io_lib:format("invalid attribute macro call: macro not found", []);
format_error_1(
  {max_macro_expansion_depth_exceeded,
   {MacroModule, Function}, Arguments}) ->
    io_lib:format(
      "maximum macro expansion depth exceeded when applying macro ~p:~p with arguments ~p.",
      [MacroModule, Function, Arguments]);
format_error_1(
  {max_macro_expansion_depth_exceeded, Function, Arguments}) ->
    io_lib:format(
      "maximum macro expansion depth exceeded when applying macro ~p with arguments ~p.",
      [Function, Arguments]);
format_error_1({macro_exception, MFA, Arguments, Exception}) ->
    io_lib:format(
      "apply macro ~s ~p failed:~n~s",
      [astranaut_macro_expander:format_mfa(MFA),
       Arguments,
       format_exception(Exception)]);
format_error_1({invalid_macro_return, Detail}) ->
    io_lib:format(
      "macro ~s returned invalid AST: ~p",
      [astranaut_macro_expander:format_mfa(
         invalid_macro_return_mfa(Detail)),
       Detail]);
format_error_1({invalid_closure_roots, Functions}) ->
    io_lib:format(
      "closure_roots contains undefined functions: ~p", [Functions]);
format_error_1({macro_capability_unavailable, Provider}) ->
    io_lib:format(
      "requested macro capability ~p is unavailable", [Provider]);
format_error_1({undefined_local_macro_retain, Functions}) ->
    io_lib:format(
      "local_macro_retain contains undefined functions: ~p", [Functions]);
format_error_1({ineffective_local_macro_retain, Functions}) ->
    io_lib:format(
      "local_macro_retain has no effect for functions outside every local macro closure: ~p",
      [Functions]);
format_error_1({duplicate_local_macro_declaration, Function}) ->
    io_lib:format(
      "duplicate local macro declaration for ~p", [Function]);
format_error_1({conflicting_local_macro_closure_environment, FormId}) ->
    io_lib:format(
      "local macro closure has conflicting expansion environments for ~p",
      [FormId]);
format_error_1({conflicting_local_macro_whitelist, FormId, Detail}) ->
    io_lib:format(
      "local macro closure has conflicting whitelist for ~p: ~p",
      [FormId, Detail]);
format_error_1({illegal_locked_form_mutation, Form}) ->
    io_lib:format(
      "local macro expansion modified frozen form: ~p", [Form]);
format_error_1(
  {local_macro_diagnostic, _Formatter, _Error, Message}) ->
    Message;
format_error_1({illegal_macro_environment_mutation, Form}) ->
    io_lib:format(
      "local macro expansion generated illegal macro environment form: ~p",
      [Form]);
format_error_1({illegal_local_macro_definition_mutation, Form}) ->
    io_lib:format(
      "local macro expansion modified locked local macro snapshot form: ~p",
      [Form]);
format_error_1({invalid_attr, AttrName, Attr}) ->
    io_lib:format("invalid ~p macro attribute: ~p", [AttrName, Attr]);
format_error_1({invalid_function_with_arity, Function}) ->
    io_lib:format("invalid macro function and arity: ~p", [Function]).

-spec format_error_1_no_match(list()) -> boolean().
format_error_1_no_match([{?MODULE, format_error_1, _Arity, _Info}|_]) ->
    true;
format_error_1_no_match(_) ->
    false.

-if(?ASTRANAUT_OTP_VSN_GE(24)).
format_exception({Class, Reason, StackTrace}) ->
    erl_error:format_exception(Class, Reason, StackTrace).
-else.
format_exception({Class, Reason, StackTrace}) ->
    io_lib:format(
      "~p: ~p~nstacktrace:~n~p", [Class, Reason, StackTrace]).
-endif.

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

run_attribute_pass(
  Module, File, GlobalMacroOpts0, Forms, CompileOpts) ->
    do([ return ||
           {ScannedForms, ScanState} <-
               astranaut_macro_scan:run(
                 Module, File, GlobalMacroOpts0, Forms, CompileOpts),
           #{registry := Registry,
             capability := Capability} = ScanState,
           finalize_attribute_macro_pass(
             File, Registry, Capability, ScannedForms, CompileOpts)
       ]).

finalize_attribute_macro_pass(
  _File, Registry, disabled, Forms, _CompileOpts) ->
    do([ return ||
           PreparedForms <-
               astranaut_macro_registry:prepare_exports(Forms),
           {AttributeForms, FunctionEnv0} =
               prepare_function_environment(
                 PreparedForms, Registry, ordsets:new()),
           return({AttributeForms,
                   FunctionEnv0#{capability => disabled}})
       ]);
finalize_attribute_macro_pass(
  File, Registry,
  #{provider := Provider, state := ProviderState},
  Forms, CompileOpts) ->
    do([ return ||
           Forms0 = apply(
                      Provider, remove_declarations,
                      [Forms, ProviderState]),
           PreparedForms <-
               astranaut_macro_registry:prepare_exports(Forms0),
           {FinalForms, AdditionalCallers,
            ProviderState1, Warnings} <-
               apply(
                 Provider, finish_attribute_pass,
                 [PreparedForms, CompileOpts, ProviderState]),
           {AttributeForms, FunctionEnv0} =
               prepare_function_environment(
                 FinalForms, Registry, AdditionalCallers),
           FunctionEnv = FunctionEnv0#{
                           capability =>
                               #{provider => Provider,
                                 state => ProviderState1}},
           astranaut_return:then(
             file_formatted_warnings(File, Warnings),
             return({AttributeForms, FunctionEnv}))
       ]).

prepare_function_environment(Forms, Registry, AdditionalCallers) ->
    MacroEnvironment =
        astranaut_macro_registry:final_macro_environment(
          Forms, Registry),
    MacroMap = maps:get(macro_map, MacroEnvironment),
    FunctionCallAnalysis =
        astranaut_macro_expander:function_call_analysis(
          Forms, MacroMap, presence),
    DetectedCallers =
        astranaut_macro_expander:function_macro_callers(
          FunctionCallAnalysis),
    FunctionEnv =
        #{macro_environment => MacroEnvironment,
          function_call_analysis => FunctionCallAnalysis,
          callers => ordsets:union(
                       DetectedCallers, AdditionalCallers),
          global_macro_opts =>
              astranaut_macro_registry:global_macro_opts(Registry)},
    {astranaut_forms:sort_forms(Forms), FunctionEnv}.

file_formatted_warnings(File, Warnings) ->
    Error0 = astranaut_error:new(File),
    Error1 = astranaut_error:append_formatted_warnings(Warnings, Error0),
    astranaut_return:ok(ok, astranaut_error:eof(Error1)).

%%%===================================================================
%%% Function pass orchestration
%%%===================================================================

run_function_macro_pass(
  Forms,
  #{macro_environment := MacroEnvironment,
    function_call_analysis := FunctionCallAnalysis,
    callers := Callers,
    capability := disabled}) ->
    do([ return ||
           Tasks = function_expansion_tasks(
                     Callers, MacroEnvironment,
                     FunctionCallAnalysis),
           #{forms := ExpandedForms} <-
               astranaut_macro_expander:expand_functions(Forms, Tasks),
           return(ExpandedForms)
       ]);
run_function_macro_pass(
  Forms,
  #{capability := #{provider := Provider,
                    state := ProviderState}} = FunctionEnv) ->
    do([ return ||
           {ExpandedForms, _ProviderState1} <-
               apply(Provider, run_function_pass,
                     [Forms, FunctionEnv, ProviderState]),
           return(ExpandedForms)
       ]).

function_expansion_tasks(Callers,
                         #{macro_map := MacroMap},
                         FunctionCallAnalysis) ->
    lists:foldl(
      fun(FormId, Acc) ->
              case maps:find(FormId, FunctionCallAnalysis) of
                  {ok, #{form := Form,
                         has_macro_call := HasMacroCall}} ->
                      maps:put(
                        FormId,
                        #{form => Form,
                          macro_map => MacroMap,
                          observation_control => disabled,
                          has_macro_call => HasMacroCall},
                        Acc);
                  error ->
                      Acc
              end
      end, #{}, Callers).

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
