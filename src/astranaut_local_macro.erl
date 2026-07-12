%%%-------------------------------------------------------------------
%%% @doc State and planning for local macros.
%%%
%%% This module intentionally does not own the forms scan nor macro invocation.
%%% `astranaut_macro' supplies a materialised source view and performs generic
%%% expansion; this module owns the immutable declaration snapshots and the
%%% state transitions needed to build local macro generations.
%%%-------------------------------------------------------------------
-module(astranaut_local_macro).

-export([new/0, register/5, ensure_available/2, compile_plan/2,
         cache_expanded/4, commit_compiled/3, finalize/2,
         frozen_ids/1, frozen_forms/1, local_macros/1, compiled_forms/1, skip_ids/1,
         related_functions/2, source_view/2, env_fingerprint/4,
         reject_locked_mutation/2, safe_load/3, finalize_plan/1,
         verify_retained/2, load_local_macro_forms/4, module_name/1,
         form_id/1, forms_id_map/1]).

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

%% `Declaration' is already syntactically validated by the scanner.  Keeping
%% that validation at the scanner boundary lets this module stay independent
%% of macro attribute syntax while making its state transitions testable.
-spec register([fa()], map(), [term()], map(), state()) -> {ok, state()} | {error, term()}.
register(FAs, Options, SourceView, ExternalEnv, State) when is_list(FAs), is_map(Options) ->
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
                                                  Refs = referenced_macros(FA, Closure, Macros, Options),
                                                  maps:put(FA, #{order => Order,
                                                                 env_snapshot => ExternalEnv,
                                                                 closure_ids => closure_ids(Closure, FormMap),
                                                                 closure_fas => Closure,
                                                                 referenced_local_macros => Refs,
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

-spec ensure_available(fa(), state()) -> {ok, [map()]} | {error, term()}.
ensure_available(FA, State) ->
    compile_plan(FA, State).

%% A plan is deliberately pure.  The caller expands each requested frozen
%% form, feeds it through cache_expanded/4, then commits it atomically.
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

%% Only a successful compiler/load operation may call this.  It is therefore
%% safe to advance the generation and mark declarations callable here.
-spec commit_compiled([fa()], #{form_id() => term()}, state()) -> state().
commit_compiled(FAs, Forms, #{local_macros := Macros} = State) ->
    Macros1 = lists:foldl(
                fun(FA, Acc) ->
                        case maps:find(FA, Acc) of
                            {ok, Entry} -> maps:put(FA, Entry#{status => compiled}, Acc);
                            error -> Acc
                        end
                end, Macros, FAs),
    ExpandedIds = ordsets:union(maps:keys(Forms), maps:get(local_macro_expanded_ids, State)),
    State#{local_macros => Macros1,
           compiled_forms => maps:merge(maps:get(compiled_forms, State), Forms),
           local_macro_expanded_ids => ExpandedIds,
           generation => maps:get(generation, State) + 1}.

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

-spec frozen_forms(state()) -> #{form_id() => term()}.
frozen_forms(State) -> maps:get(frozen_forms, State).

-spec local_macros(state()) -> map().
local_macros(State) -> maps:get(local_macros, State).

-spec compiled_forms(state()) -> map().
compiled_forms(State) -> maps:get(compiled_forms, State).

-spec skip_ids(state()) -> ordsets:ordset(form_id()).
skip_ids(State) ->
    ordsets:subtract(maps:get(local_macro_expanded_ids, State), maps:get(retained_form_ids, State, ordsets:new())).

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
%% macro module, followed by its safe replacement.
-spec load_local_macro_forms(ordsets:ordset(fa()), ordsets:ordset(fa()),
                             [term()], [compile:option()]) -> astranaut_return:struct(term()).
load_local_macro_forms([], _LocalMacroRelatedFunctions, _PreparedForms, _CompileOpts) ->
    astranaut_return:return(ok);
load_local_macro_forms(LocalMacroFunctions, LocalMacroRelatedFunctions,
                       PreparedForms, CompileOpts) ->
    Forms = select_local_macro_forms(LocalMacroRelatedFunctions, PreparedForms),
    compile_local_macro_forms(LocalMacroFunctions, Forms, CompileOpts).

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
    safe_load(Module, Forms1, [without_warnings | CompileOpts]).

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

%% Compatibility entry point for the existing transformer while it is moved to
%% the single scan.  Keeping this graph traversal here prevents the scanner
%% from owning local-macro closure semantics.
-spec related_functions(ordsets:ordset(fa()), map()) -> ordsets:ordset(fa()).
related_functions(Functions, ClauseMap) ->
    related_functions(Functions, ClauseMap, Functions).

related_functions(Functions, ClauseMap, Deps) ->
    lists:foldl(
      fun(Function, Acc) ->
              case maps:find(Function, ClauseMap) of
                  {ok, Clauses} ->
                      FDeps = ordsets:union(lists:map(fun related_calls/1, Clauses)),
                      NDeps = ordsets:union(FDeps, Acc),
                      AddedFunctions = ordsets:subtract(FDeps, Deps),
                      related_functions(AddedFunctions, ClauseMap, NDeps);
                  error ->
                      ordsets:del_element(Function, Acc)
              end
      end, Deps, Functions).

related_calls({clause, _Pos, _Patterns, _Guards, Exprs}) ->
    with_local_calls(Exprs).

with_local_calls(Exprs) ->
    astranaut:sreduce(
      fun({call, _Pos, {atom, _P, Name}, Args}, Acc) -> ordsets:add_element({Name, length(Args)}, Acc);
         (_, Acc) -> Acc
      end, ordsets:new(), Exprs, #{traverse => pre}).

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

referenced_macros(FA, Closure, Macros, Options) ->
    Direct = internal_direct(Options, Closure),
    ordsets:from_list([Other || Other <- maps:keys(Macros), Other =/= FA,
                                  ordsets:is_element(Other, Closure), not ordsets:is_element(Other, Direct)]).

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
    [plan_boundary([FA || {_Order, FA, _Entry} <- Ordered], Ordered, Compiled, Frozen)];
compilation_boundaries(Needed, Ordered, Compiled, Frozen) ->
    Prefix = [FA || {_Order, FA, _Entry} <- Ordered],
    Dependencies = dependency_boundaries(Needed, Ordered),
    Boundaries = ordsets:to_list(ordsets:from_list(Dependencies ++ [Prefix])),
    %% ordsets sorts lists lexically, not declaration order; restore the plan
    %% order by the final member's declaration index.
    Sorted = lists:sort(fun(A, B) -> boundary_order(A, Ordered) =< boundary_order(B, Ordered) end, Boundaries),
    [plan_boundary(Members, Ordered, Compiled, Frozen) || Members <- Sorted].

dependency_boundaries(FA, Ordered) ->
    Entry = entry_for(FA, Ordered),
    lists:append([dependency_boundaries(Dep, Ordered) ++ [prefix_to(Dep, Ordered)]
                  || Dep <- maps:get(referenced_local_macros, Entry, [])]).

entry_for(FA, [{_Order, FA, Entry} | _]) -> Entry;
entry_for(FA, [_ | T]) -> entry_for(FA, T).

prefix_to(FA, Ordered) ->
    [F || {_Order, F, _Entry} <- lists:takewhile(fun({_O, F0, _E}) -> F0 =/= FA end, Ordered)] ++ [FA].

boundary_order(Members, Ordered) ->
    case lists:last(Members) of
        FA -> element(1, lists:keyfind(FA, 2, Ordered))
    end.

plan_boundary(Members, Ordered, Compiled, Frozen) ->
    Entries = [{FA, entry_for(FA, Ordered)} || FA <- Members],
    #{members => Members,
      requests => [#{fa => FA,
                     closure_ids => maps:get(closure_ids, Entry),
                     env_snapshot => maps:get(env_snapshot, Entry),
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
