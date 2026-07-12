%%%-------------------------------------------------------------------
%%% @doc State and planning for local macros.
%%%
%%% This module intentionally does not own the forms scan nor macro invocation.
%%% `astranaut_macro' supplies a materialised source view and performs generic
%%% expansion; this module owns the immutable declaration snapshots and the
%%% state transitions needed to build local macro generations.
%%%-------------------------------------------------------------------
-module(astranaut_local_macro).

-include("do.hrl").

-export([new/0, register/7, ensure_available/2, ensure_available/4,
         compile_plan/2,
         cache_expanded/4, commit_compiled/3, finalize/2,
         finalize/4,
         frozen_ids/1, local_macros/1,
         source_view/2, env_fingerprint/4,
         reject_locked_mutation/2, safe_load/3, finalize_plan/1,
         verify_retained/2, module_name/1,
         form_id/1, materialize_forms/2, execute_plan/4]).

-type fa() :: {atom(), non_neg_integer()}.
-type form_id() :: {function | spec, atom(), non_neg_integer()}.
-type state() :: map().

-spec new() -> state().
new() ->
    #{local_macros => #{},
      frozen_forms => #{},
      expanded_forms => #{},
      compiled_forms => #{},
      local_macro_expanded_ids => ordsets:new(),
      retain_roots => ordsets:new(),
      generation => 0}.

-spec register([fa()], map(), [term()], map(), map(), map(), state()) ->
          {ok, state()} | {error, term()}.
register(FAs, Options, SourceView, ExternalEnv, CandidateLocalMap, MacroOps, State)
  when is_list(FAs), is_map(Options), is_map(CandidateLocalMap), is_map(MacroOps) ->
    Resolve = maps:get(resolve_local_references, MacroOps),
    ResolveReferences =
        fun(_FA, Closure, _Macros) ->
                Direct = internal_direct(Options, Closure),
                TargetEnvs =
                    [{TargetFA,
                      effective_local_env(CandidateLocalMap, Direct, TargetFA)}
                     || TargetFA <- Closure],
                Resolve(TargetEnvs, SourceView)
        end,
    do_register(FAs, Options, SourceView, ExternalEnv, ResolveReferences, State).

do_register(FAs, Options, SourceView, ExternalEnv, ResolveReferences, State) ->
    Macros = maps:get(local_macros, State),
    case duplicate_or_existing(FAs, Macros) of
        none ->
            FormMap = source_form_map(SourceView),
            case closures(FAs, Options, FormMap) of
                {ok, Closures} ->
                    case validate_internal_policies(Closures, Options, Macros) of
                        ok ->
                            Order = next_order(Macros),
                            Frozen = maps:merge(maps:get(frozen_forms, State),
                                                freeze_closures(Closures, FormMap)),
                            NewMacros = lists:foldl(
                                          fun(FA, Acc) ->
                                                  Closure = maps:get(FA, Closures),
                                                  Refs = ResolveReferences(FA, Closure, Macros),
                                                  maps:put(FA, #{order => Order,
                                                                 env_snapshot => ExternalEnv,
                                                                 closure_ids => closure_ids(Closure, FormMap),
                                                                 closure_fas => Closure,
                                                                 referenced_local_macros => Refs,
                                                                 source_view => SourceView,
                                                                 options => Options,
                                                                 status => pending}, Acc)
                                          end, Macros, FAs),
                            {ok, State#{local_macros => NewMacros, frozen_forms => Frozen}};
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end;
        FA ->
            {error, {duplicate_local_macro_declaration, FA}}
    end.

effective_local_env(CandidateLocalMap, DirectFunctions, TargetFA) ->
    Excluded = ordsets:add_element(TargetFA, DirectFunctions),
    maps:filter(
      fun(_Key, #{function := Function, arity := Arity}) ->
              not ordsets:is_element({Function, Arity}, Excluded)
      end, CandidateLocalMap).

-spec ensure_available(fa(), state()) -> {ok, [map()]} | {error, term()}.
ensure_available(FA, State) ->
    compile_plan(FA, State).

-spec ensure_available(fa(), map(), map(), state()) -> astranaut_return:struct(state()).
ensure_available(FA, Context, MacroOps, State) ->
    case compile_plan(FA, State) of
        {ok, Plan} -> execute_plan(Plan, Context, MacroOps, State);
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

%% A plan is deliberately pure. execute_plan/4 expands each requested frozen
%% form through MacroOps, validates the cache, then commits it atomically.
-spec compile_plan(fa() | all, state()) -> {ok, [map()]} | {error, term()}.
compile_plan(Needed, #{local_macros := Macros, compiled_forms := Compiled} = State) ->
    case needed_entries(Needed, Macros) of
        {error, _} = Error -> Error;
        Entries ->
            Ordered = lists:keysort(1, [{maps:get(order, Entry), FA, Entry}
                                        || {FA, Entry} <- Entries]),
            {ok, compilation_boundaries(Needed, Ordered, Compiled, maps:get(frozen_forms, State))}
    end.

%% The final generation is intentionally rebuilt from every declaration, not
%% merely pending ones.  Cached expansion results let the caller avoid work.
-spec finalize_plan(state()) -> {ok, [map()]}.
finalize_plan(State) -> compile_plan(all, State).

-spec cache_expanded(form_id(), term(), term(), state()) -> {ok, state()} | {error, term()}.
cache_expanded(FormId, Fingerprint, ExpandedForm, State) ->
    Expanded = maps:get(expanded_forms, State),
    SameForm = [Form || {{Id, _}, Form} <- maps:to_list(Expanded), Id =:= FormId],
    case lists:all(fun(Form) -> Form =:= ExpandedForm end, SameForm) of
        false -> {error, {conflicting_local_macro_closure_environment, FormId}};
        true -> {ok, State#{expanded_forms => maps:put({FormId, Fingerprint}, ExpandedForm, Expanded)}}
    end.

-spec expanded_form(form_id(), term(), state()) -> {ok, term()} | error.
expanded_form(FormId, Fingerprint, State) ->
    maps:find({FormId, Fingerprint}, maps:get(expanded_forms, State)).

-spec local_versions([fa()], state()) -> #{fa() => non_neg_integer()}.
local_versions(FAs, State) ->
    Macros = maps:get(local_macros, State),
    maps:from_list(
      [{FA, Generation}
       || FA <- FAs,
          {ok, #{status := compiled, compiled_generation := Generation}} <-
              [maps:find(FA, Macros)]]).

%% Only a successful compiler/load operation may call this.  It is therefore
%% safe to advance the generation and mark declarations callable here.
-spec commit_compiled([fa()], #{form_id() => term()}, state()) -> state().
commit_compiled(FAs, Forms, #{local_macros := Macros} = State) ->
    Generation = maps:get(generation, State) + 1,
    Macros1 = lists:foldl(
                fun(FA, Acc) ->
                        case maps:find(FA, Acc) of
                            {ok, Entry} ->
                                maps:put(FA, Entry#{status => compiled,
                                                   compiled_generation => Generation}, Acc);
                            error -> Acc
                        end
                end, Macros, FAs),
    ExpandedIds = ordsets:union(maps:keys(Forms), maps:get(local_macro_expanded_ids, State)),
    State#{local_macros => Macros1,
           compiled_forms => maps:merge(maps:get(compiled_forms, State), Forms),
           local_macro_expanded_ids => ExpandedIds,
           generation => Generation}.

%% Retain roots are supplied at the end because export/export_macro/retain can
%% occur after a declaration.  The scanner remains responsible for finding
%% those attributes in its complete output stream.
-spec finalize([fa()], state()) -> {map(), ordsets:ordset(form_id()), state()}.
finalize(RetainRoots, #{local_macros := Macros} = State) ->
    Retained = retained_ids(ordsets:from_list(RetainRoots), Macros),
    Skip = ordsets:subtract(maps:get(local_macro_expanded_ids, State), Retained),
    LocalEnv = maps:from_list([
                  {FA, Entry}
                  || {FA, Entry} <- maps:to_list(Macros), maps:get(status, Entry) =:= compiled]),
    {LocalEnv, Skip, State#{retain_roots => ordsets:from_list(RetainRoots), retained_form_ids => Retained}}.

-spec finalize([fa()], map(), map(), state()) ->
          astranaut_return:struct({map(), ordsets:ordset(form_id()), map(), state()}).
finalize(RetainRoots, FinalContext, MacroOps, State) ->
    case finalize_plan(State) of
        {error, Error} ->
            astranaut_return:error_fail(Error);
        {ok, Plan} ->
            do([ return ||
                   State1 <- execute_plan(Plan, FinalContext, MacroOps, State),
                   {FinalLocalEnv, FinalSkipIds, State2} = finalize(RetainRoots, State1),
                   RetainedForms <- expand_retained_forms(
                                      FinalLocalEnv, FinalContext, MacroOps, State2),
                   _ <- case verify_retained(RetainedForms, State2) of
                            ok -> astranaut_return:return(ok);
                            {error, VerifyError} ->
                                astranaut_return:error_fail(VerifyError)
                        end,
                   return({FinalLocalEnv, FinalSkipIds, RetainedForms, State2})
               ])
    end.

expand_retained_forms(FinalLocalEnv, Context, MacroOps, State) ->
    Retained = retained_form_ids(State),
    Frozen = maps:get(frozen_forms, State),
    astranaut_return:foldl_m(
      fun(FormId, Acc) ->
              case maps:find(FormId, Frozen) of
                  error ->
                      astranaut_return:return(Acc);
                  {ok, OriginalForm} ->
                      do([ return ||
                             Expanded <- expand_retained_form(
                                           FormId, OriginalForm, FinalLocalEnv,
                                           Context, MacroOps, State),
                             return(maps:put(FormId, Expanded, Acc))
                         ])
              end
      end, #{}, Retained).

expand_retained_form({spec, _Name, _Arity}, OriginalForm, _FinalLocalEnv,
                     _Context, _MacroOps, _State) ->
    astranaut_return:return(OriginalForm);
expand_retained_form({function, Name, Arity} = FormId, OriginalForm,
                     FinalLocalEnv, Context, MacroOps, State) ->
    TargetFA = {Name, Arity},
    Direct = direct_functions_for(TargetFA, State),
    AvailableFAs = ordsets:from_list(maps:keys(FinalLocalEnv)),
    EffectiveFAs = ordsets:subtract(
                     AvailableFAs, ordsets:add_element(TargetFA, Direct)),
    LocalMacroMap = macro_map_for_fas(
                      maps:get(local_macro_map, Context), EffectiveFAs),
    ExternalMacroMap = maps:get(external_macro_map, Context),
    Merge = maps:get(merge_macro_maps, MacroOps),
    Expand = maps:get(expand_function, MacroOps),
    Forms = maps:get(source_view, Context),
    do([ return ||
           EffectiveEnv <- Merge(ExternalMacroMap, LocalMacroMap),
           ExpandedForms <- Expand(EffectiveEnv, Forms, Forms, TargetFA),
           ExpandedMap = forms_id_map(ExpandedForms),
           return(maps:get(FormId, ExpandedMap, OriginalForm))
       ]).

direct_functions_for(TargetFA, State) ->
    case [Entry || Entry <- maps:values(maps:get(local_macros, State)),
                   ordsets:is_element(TargetFA, maps:get(closure_fas, Entry))] of
        [Entry | _] ->
            internal_direct(maps:get(options, Entry), maps:get(closure_fas, Entry));
        [] ->
            ordsets:new()
    end.

%% The scanner supplies final re-expansions for retained helpers.  Macro heads
%% are deliberately excluded: their recursive calls are ordinary Erlang calls.
-spec verify_retained(#{form_id() => term()}, state()) -> ok | {error, term()}.
verify_retained(FinalForms, State) ->
    Retained = maps:get(retained_form_ids, State, ordsets:new()),
    Heads = ordsets:from_list([{function, Name, Arity} || {{Name, Arity}, _} <- maps:to_list(maps:get(local_macros, State))]),
    Compiled = maps:get(compiled_forms, State),
    case [Id || Id <- Retained, not ordsets:is_element(Id, Heads),
                maps:is_key(Id, FinalForms), maps:is_key(Id, Compiled),
                maps:get(Id, FinalForms) =/= maps:get(Id, Compiled)] of
        [] -> ok;
        [Id | _] -> {error, {conflicting_local_macro_closure_environment, Id}}
    end.

-spec frozen_ids(state()) -> ordsets:ordset(form_id()).
frozen_ids(State) -> ordsets:from_list(maps:keys(maps:get(frozen_forms, State))).

-spec local_macros(state()) -> map().
local_macros(State) -> maps:get(local_macros, State).

-spec retained_form_ids(state()) -> ordsets:ordset(form_id()).
retained_form_ids(State) ->
    maps:get(retained_form_ids, State, ordsets:new()).

%% The declaration source view is deliberately a two-part concatenation.  A
%% generated form becomes visible only after it has entered Queue; no future
%% splice result can leak into a declaration's closure analysis.
-spec source_view([term()], [term()]) -> [term()].
source_view(Passed, Queue) -> Passed ++ Queue.

%% Keep all observable expansion inputs in the key.  `term_to_binary' gives a
%% stable value suitable for map keys without assuming a particular Env shape.
-spec env_fingerprint(map(), term(), term(), [term()]) -> binary().
env_fingerprint(ExternalEnv, LocalVersions, Options, InjectForms) ->
    term_to_binary({ExternalEnv, LocalVersions, Options, InjectForms}).

-spec reject_locked_mutation([term()], state()) -> ok | {error, term()}.
reject_locked_mutation(Forms, State) ->
    Frozen = maps:get(frozen_forms, State),
    case [Form || Form <- Forms,
                  begin Id = form_id(Form), Id =/= undefined andalso maps:is_key(Id, Frozen) end] of
        [] -> ok;
        [Form | _] -> {error, {illegal_locked_form_mutation, Form}}
    end.

%% Generic macro expansion is performed by astranaut_macro before this
%% boundary.  This module owns selection and assembly of the cumulative local
%% macro module, followed by its safe replacement. The caller holds
%% with_generation_lock/2 across expansion, this load, and the state commit.
-spec load_local_macro_forms(ordsets:ordset(fa()), ordsets:ordset(fa()),
                             #{form_id() => term()}, [term()],
                             [compile:option()]) -> astranaut_return:struct(term()).
load_local_macro_forms([], _LocalMacroRelatedFunctions, _CompiledForms,
                       _SourceForms, _CompileOpts) ->
    astranaut_return:return(ok);
load_local_macro_forms(LocalMacroFunctions, LocalMacroRelatedFunctions,
                       CompiledForms, SourceForms, CompileOpts) ->
    MaterializedForms = materialize_forms(SourceForms, CompiledForms),
    Forms = select_local_macro_forms(LocalMacroRelatedFunctions, MaterializedForms),
    compile_local_macro_forms(LocalMacroFunctions, Forms, CompileOpts).

-spec with_generation_lock(module(), fun(() -> Result)) -> Result.
with_generation_lock(Module, Fun) ->
    global:trans({?MODULE, module_name(Module)}, Fun).

%% Execute pure plan data while keeping all local-macro lifecycle decisions in
%% this module. MacroOps supplies only uniform reference/expansion behaviour.
-spec execute_plan([map()], map(), map(), state()) -> astranaut_return:struct(state()).
execute_plan([], _Context, _MacroOps, State) ->
    astranaut_return:return(State);
execute_plan([Boundary | Rest], Context, MacroOps, State) ->
    Members = maps:get(members, Boundary),
    Pending = [FA || FA <- Members,
                     case maps:find(FA, maps:get(local_macros, State)) of
                         {ok, #{status := compiled}} -> false;
                         _ -> true
                     end],
    Force = maps:get(final, Boundary, false),
    case {Members, Pending, Force} of
        {[], _, _} ->
            execute_plan(Rest, Context, MacroOps, State);
        {_, [], false} ->
            execute_plan(Rest, Context, MacroOps, State);
        _ ->
            do([ return ||
                   State1 <- execute_boundary(Boundary, Context, MacroOps, State),
                   execute_plan(Rest, Context, MacroOps, State1)
               ])
    end.

execute_boundary(#{members := Members, requests := Requests}, Context, MacroOps, State) ->
    SourceView = maps:get(source_view, Context),
    Module = astranaut_lib:analyze_forms_module(SourceView),
    with_generation_lock(
      Module,
      fun() ->
              do([ return ||
                     {ExpandedForms, ExpandedState} <-
                         expand_requests(Requests, Context, MacroOps, State),
                     CompiledForms = maps:merge(
                                       maps:get(compiled_forms, ExpandedState),
                                       ExpandedForms),
                     LocalFunctions = maybe_add_formatter(
                                        ordsets:from_list(Members), SourceView),
                     Related0 = lists:foldl(
                                  fun(Request, Acc) ->
                                          ordsets:union(maps:get(closure_fas, Request), Acc)
                                  end, ordsets:new(), Requests),
                     Related = maybe_add_formatter(Related0, SourceView),
                     load_local_macro_forms(
                       LocalFunctions, Related, CompiledForms, SourceView,
                       maps:get(compile_opts, Context)),
                     return(commit_compiled(Members, CompiledForms, ExpandedState))
                 ])
      end).

expand_requests(Requests, Context, MacroOps, State) ->
    astranaut_return:foldl_m(
      fun(Request, {FormsAcc, StateAcc}) ->
              do([ return ||
                     {RequestForms, State1} <-
                         expand_request(Request, Context, MacroOps, StateAcc),
                     return({maps:merge(FormsAcc, RequestForms), State1})
                 ])
      end, {#{}, State}, Requests).

expand_request(#{forms := FrozenForms} = Request, Context, MacroOps, State) ->
    astranaut_return:foldl_m(
      fun(FormId, {FormsAcc, StateAcc}) ->
              do([ return ||
                     {ExpandedForm, State1} <-
                         expand_request_form(FormId, Request, Context, MacroOps, StateAcc),
                     return({maps:put(FormId, ExpandedForm, FormsAcc), State1})
                 ])
      end, {#{}, State}, maps:keys(FrozenForms)).

expand_request_form(FormId, Request, Context, MacroOps, State) ->
    TargetFA = form_fa(FormId),
    Direct = internal_direct(maps:get(options, Request), maps:get(closure_fas, Request)),
    Referenced = maps:get(referenced_local_macros, Request),
    EffectiveReferenced = ordsets:subtract(
                            Referenced, ordsets:add_element(TargetFA, Direct)),
    LocalVersions = local_versions(EffectiveReferenced, State),
    ExternalEnv = maps:get(env_snapshot, Request),
    SourceView = maps:get(source_view, Request),
    Fingerprint = env_fingerprint(
                    ExternalEnv, LocalVersions, maps:get(options, Request),
                    {TargetFA, SourceView}),
    case expanded_form(FormId, Fingerprint, State) of
        {ok, Form} ->
            astranaut_return:return({Form, State});
        error ->
            expand_and_cache_form(
              FormId, TargetFA, EffectiveReferenced, Fingerprint,
              Request, Context, MacroOps, State)
    end.

expand_and_cache_form(FormId, TargetFA, EffectiveReferenced, Fingerprint,
                      #{forms := FrozenForms, env_snapshot := ExternalEnv,
                        source_view := SourceView} = _Request,
                      Context, MacroOps, State) ->
    OriginalForm = maps:get(FormId, FrozenForms),
    case FormId of
        {spec, _Name, _Arity} ->
            cache_form_result(FormId, Fingerprint, OriginalForm, State);
        {function, _Name, _Arity} ->
            ExternalMacroMap = maps:get(macro_map, ExternalEnv),
            LocalMacroMap = macro_map_for_fas(
                              maps:get(local_macro_map, Context),
                              EffectiveReferenced),
            SnapshotForms = materialize_forms(SourceView, FrozenForms),
            Merge = maps:get(merge_macro_maps, MacroOps),
            Expand = maps:get(expand_function, MacroOps),
            do([ return ||
                   EffectiveEnv <- Merge(ExternalMacroMap, LocalMacroMap),
                   ExpandedSource <- Expand(
                                       EffectiveEnv, SourceView,
                                       SnapshotForms, TargetFA),
                   ExpandedMap = forms_id_map(ExpandedSource),
                   ExpandedForm = maps:get(FormId, ExpandedMap, OriginalForm),
                   cache_form_result(FormId, Fingerprint, ExpandedForm, State)
               ])
    end.

cache_form_result(FormId, Fingerprint, Form, State) ->
    case cache_expanded(FormId, Fingerprint, Form, State) of
        {ok, State1} -> astranaut_return:return({Form, State1});
        {error, Error} -> astranaut_return:error_fail(Error)
    end.

macro_map_for_fas(MacroMap, FAs) ->
    FASet = ordsets:from_list(FAs),
    maps:filter(
      fun(_Key, #{function := Function, arity := Arity}) ->
              ordsets:is_element({Function, Arity}, FASet)
      end, MacroMap).

form_fa({function, Name, Arity}) -> {Name, Arity};
form_fa({spec, Name, Arity}) -> {Name, Arity}.

maybe_add_formatter([], _Forms) -> [];
maybe_add_formatter(Functions, Forms) ->
    case maps:is_key({function, format_error, 1}, forms_id_map(Forms)) of
        true -> ordsets:add_element({format_error, 1}, Functions);
        false -> Functions
    end.

materialize_forms(Forms, CompiledForms) ->
    lists:map(
      fun(Form) ->
              case form_id(Form) of
                  undefined -> Form;
                  Id -> maps:get(Id, CompiledForms, Form)
              end
      end, Forms).

-spec module_name(module()) -> module().
module_name(Module) ->
    list_to_atom(atom_to_list(Module) ++ "__local_macro").

select_local_macro_forms(LocalMacroRelatedFunctions, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, Pos, module, Module}, Acc) ->
                [{attribute, Pos, module, module_name(Module)} | Acc];
           ({function, _Pos, Name, Arity, _Clauses} = Node, Acc) ->
                append_if(ordsets:is_element({Name, Arity}, LocalMacroRelatedFunctions), Node, Acc);
           ({attribute, _Pos, spec, {{Name, Arity}, _Body}} = Node, Acc) ->
                append_if(ordsets:is_element({Name, Arity}, LocalMacroRelatedFunctions), Node, Acc);
           ({attribute, _Pos, export, _Exports}, Acc) -> Acc;
           ({attribute, _Pos, local_macro, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, import_macro, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, use_macro, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, macro_options, _Attr}, Acc) -> Acc;
           ({attribute, _Pos, exec_macro, _Attr}, Acc) -> Acc;
           (Node, Acc) -> [Node | Acc]
        end, [], Forms)).

compile_local_macro_forms(LocalMacroFunctions, Forms, CompileOpts) ->
    Forms1 = astranaut_syntax:sort_forms(Forms ++ local_macro_exports(LocalMacroFunctions)),
    Module = astranaut_lib:analyze_forms_module(Forms),
    safe_load_locked(Module, Forms1, [without_warnings | CompileOpts]).

local_macro_exports(LocalMacroFunctions) ->
    lists:foldl(
      fun(Export, Acc) ->
              [astranaut_lib:gen_exports([Export], 0) | Acc]
      end, [], LocalMacroFunctions).

append_if(true, Form, Forms) -> [Form | Forms];
append_if(false, _Form, Forms) -> Forms.

%% Compile then safely replace the single local-macro module generation.  This
%% has no force-purge fallback: callers retain their previous generation when
%% old code is in use.
-spec safe_load(module(), [term()], [compile:option()]) -> astranaut_return:struct({module(), binary()}).
safe_load(Module, Forms, CompileOpts) ->
    global:trans({?MODULE, Module}, fun() -> safe_load_locked(Module, Forms, CompileOpts) end).

safe_load_locked(Module, Forms, CompileOpts) ->
    astranaut_return:bind(
      astranaut_lib:compile_forms(Forms, CompileOpts),
      fun({CompiledModule, Binary}) when CompiledModule =:= Module ->
              case code:is_loaded(CompiledModule) of
                  false -> load_binary(Module, Binary);
                  {file, _} ->
                      case code:soft_purge(CompiledModule) of
                          true -> load_binary(Module, Binary);
                          false -> astranaut_return:error_fail(local_macro_module_in_use)
                      end
              end
      end).

load_binary(Module, Binary) ->
    case code:load_binary(Module, [], Binary) of
        {module, Module} -> astranaut_return:return({Module, Binary});
        {error, Reason} -> astranaut_return:error_fail(Reason)
    end.

duplicate_or_existing(FAs, Macros) ->
    case [FA || FA <- FAs, maps:is_key(FA, Macros)] ++ duplicate_fas(FAs) of
        [FA | _] -> FA;
        [] -> none
    end.

duplicate_fas(FAs) ->
    FAs -- ordsets:from_list(FAs).

next_order(Macros) ->
    lists:foldl(fun(#{order := Order}, Acc) -> max(Order, Acc) end, 0, maps:values(Macros)) + 1.

source_form_map(Forms) ->
    lists:foldl(fun(Form, Acc) ->
                        case form_id(Form) of undefined -> Acc; Id -> maps:put(Id, Form, Acc) end
                end, #{}, Forms).

form_id({function, _Pos, Name, Arity, _Clauses}) -> {function, Name, Arity};
form_id({attribute, _Pos, spec, {{Name, Arity}, _Body}}) -> {spec, Name, Arity};
form_id(_) -> undefined.

-spec forms_id_map([term()]) -> #{form_id() => term()}.
forms_id_map(Forms) ->
    source_form_map(Forms).

closures(FAs, Options, FormMap) ->
    Extra = maps:get(extra_functions, Options, []),
    case [FA || FA <- Extra, not maps:is_key({function, element(1, FA), element(2, FA)}, FormMap)] of
        [] -> closures_1(FAs, Extra, FormMap, #{});
        Missing -> {error, {invalid_extra_functions, Missing}}
    end.

closures_1([FA | T], Extra, FormMap, Acc) ->
    case closure(ordsets:from_list([FA | Extra]), FormMap, ordsets:new()) of
        {ok, Closure} -> closures_1(T, Extra, FormMap, maps:put(FA, Closure, Acc));
        Error -> Error
    end;
closures_1([], _Extra, _FormMap, Acc) -> {ok, Acc}.

closure([], _FormMap, Seen) -> {ok, Seen};
closure([FA | T], FormMap, Seen) ->
    case ordsets:is_element(FA, Seen) of
        true -> closure(T, FormMap, Seen);
        false ->
            case maps:find({function, element(1, FA), element(2, FA)}, FormMap) of
                error -> closure(T, FormMap, Seen);
                {ok, {function, _Pos, _Name, _Arity, Clauses}} ->
                    closure(ordsets:union(local_calls(Clauses), T), FormMap, ordsets:add_element(FA, Seen))
            end
    end.

local_calls(Clauses) ->
    astranaut:sreduce(
      fun({call, _Pos, {atom, _P, Name}, Args}, Acc) -> ordsets:add_element({Name, length(Args)}, Acc);
         (_, Acc) -> Acc
      end, ordsets:new(), Clauses, #{traverse => pre}).

freeze_closures(Closures, FormMap) ->
    lists:foldl(fun(Closure, Acc) ->
                        lists:foldl(fun(FA, A) -> freeze_fa(FA, FormMap, A) end, Acc, Closure)
                end, #{}, maps:values(Closures)).

freeze_fa({Name, Arity}, FormMap, Acc) ->
    FunctionId = {function, Name, Arity},
    SpecId = {spec, Name, Arity},
    Acc1 = case maps:find(FunctionId, FormMap) of {ok, FunctionForm} -> maps:put(FunctionId, FunctionForm, Acc); error -> Acc end,
    case maps:find(SpecId, FormMap) of {ok, SpecForm} -> maps:put(SpecId, SpecForm, Acc1); error -> Acc1 end.

closure_ids(Closure, FormMap) ->
    lists:foldl(fun(FA, Acc) ->
                        Ids = [{function, element(1, FA), element(2, FA)}, {spec, element(1, FA), element(2, FA)}],
                        ordsets:union([Id || Id <- Ids, maps:is_key(Id, FormMap)], Acc)
                end, ordsets:new(), Closure).

internal_direct(#{internal_function := true}, Closure) -> Closure;
internal_direct(#{internal_function := Functions}, _Closure) when is_list(Functions) -> ordsets:from_list(Functions);
internal_direct(_, _) -> ordsets:new().

validate_internal_policies(Closures, Options, Existing) ->
    NewPolicies = policy_map(maps:to_list(Closures), Options, #{}),
    OldPolicies = maps:fold(fun(_FA, Entry, Acc) ->
                                    policy_map([{undefined, maps:get(closure_fas, Entry)}], maps:get(options, Entry), Acc)
                            end, #{}, Existing),
    case [{FA, Ps} || {FA, Ps} <- maps:to_list(maps:merge_with(fun(_K, A, B) -> ordsets:union(A, B) end, OldPolicies, NewPolicies)),
                      length(Ps) > 1] of
        [] -> ok;
        [{FA, Ps} | _] -> {error, {conflicting_internal_function_policy, FA, Ps}}
    end.

policy_map([{_Root, Closure} | T], Options, Acc) ->
    Direct = internal_direct(Options, Closure),
    Acc1 = lists:foldl(fun(FA, A) ->
                               Ps = maps:get(FA, A, ordsets:new()),
                               maps:put(FA, ordsets:add_element(ordsets:is_element(FA, Direct), Ps), A)
                       end, Acc, Closure),
    policy_map(T, Options, Acc1);
policy_map([], _Options, Acc) -> Acc.

needed_entries(all, Macros) -> maps:to_list(Macros);
needed_entries(FA, Macros) ->
    case maps:find(FA, Macros) of
        error -> {error, {undefined_local_macro, FA}};
        {ok, #{order := RequestedOrder}} ->
            %% A requested declaration is compiled in the cumulative prefix;
            %% dependency analysis decides only whether that prefix needs
            %% intermediate generations, never which declarations belong to
            %% the final generation.
            [{F, Entry} || {F, #{order := Order} = Entry} <- maps:to_list(Macros),
                           Order =< RequestedOrder]
    end.

compilation_boundaries(all, Ordered, Compiled, Frozen) ->
    Prefix = [FA || {_Order, FA, _Entry} <- Ordered],
    Dependencies = lists:append(
                     [dependency_boundaries(FA, Ordered) || FA <- Prefix]),
    Boundaries = ordsets:to_list(ordsets:from_list(Dependencies ++ [Prefix])),
    Sorted = lists:sort(
               fun(A, B) -> boundary_order(A, Ordered) =< boundary_order(B, Ordered) end,
               Boundaries),
    mark_final_boundary(
      [plan_boundary(Members, Ordered, Compiled, Frozen) || Members <- Sorted]);
compilation_boundaries(Needed, Ordered, Compiled, Frozen) ->
    Prefix = [FA || {_Order, FA, _Entry} <- Ordered],
    Dependencies = dependency_boundaries(Needed, Ordered),
    Boundaries = ordsets:to_list(ordsets:from_list(Dependencies ++ [Prefix])),
    %% ordsets sorts lists lexically, not declaration order; restore the plan
    %% order by the final member's declaration index.
    Sorted = lists:sort(fun(A, B) -> boundary_order(A, Ordered) =< boundary_order(B, Ordered) end, Boundaries),
    [plan_boundary(Members, Ordered, Compiled, Frozen) || Members <- Sorted].

mark_final_boundary([]) -> [];
mark_final_boundary(Plans) ->
    lists:sublist(Plans, length(Plans) - 1) ++ [(lists:last(Plans))#{final => true}].

dependency_boundaries(FA, Ordered) ->
    Entry = entry_for(FA, Ordered),
    lists:append([dependency_boundaries(Dep, Ordered) ++ [prefix_to(Dep, Ordered)]
                  || Dep <- maps:get(referenced_local_macros, Entry, [])]).

entry_for(FA, [{_Order, FA, Entry} | _]) -> Entry;
entry_for(FA, [_ | T]) -> entry_for(FA, T).

prefix_to(FA, Ordered) ->
    DependencyOrder = element(1, lists:keyfind(FA, 2, Ordered)),
    [F || {Order, F, _Entry} <- Ordered, Order =< DependencyOrder].

boundary_order(Members, Ordered) ->
    case lists:last(Members) of
        FA -> element(1, lists:keyfind(FA, 2, Ordered))
    end.

plan_boundary(Members, Ordered, Compiled, Frozen) ->
    Entries = [{FA, entry_for(FA, Ordered)} || FA <- Members],
    #{members => Members,
      requests => [#{fa => FA,
                     closure_ids => maps:get(closure_ids, Entry),
                     closure_fas => maps:get(closure_fas, Entry),
                     referenced_local_macros => maps:get(referenced_local_macros, Entry),
                     env_snapshot => maps:get(env_snapshot, Entry),
                     source_view => maps:get(source_view, Entry),
                     options => maps:get(options, Entry),
                     already_compiled => maps:get(status, Entry) =:= compiled,
                     forms => maps:with(maps:get(closure_ids, Entry), Frozen)}
                   || {FA, Entry} <- Entries],
      compiled_forms => Compiled}.

retained_ids(Roots, Macros) ->
    lists:foldl(fun(Root, Acc) ->
                        lists:foldl(fun({_FA, Entry}, A) ->
                                            Closure = maps:get(closure_fas, Entry),
                                            case ordsets:is_element(Root, Closure) of
                                                true -> ordsets:union(maps:get(closure_ids, Entry), A);
                                                false -> A
                                            end
                                    end, Acc, maps:to_list(Macros))
                end, ordsets:new(), Roots).
