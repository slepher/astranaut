%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro).

-include("quote.hrl").

%% API
-export([transform_macro/5, transform_macros/2]).
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
transform_macro(M, F, A, Opts, Forms) ->
    transform_macros([{M, F, A, Opts}], Forms).

transform_macros(MFAOpts, Forms) when is_list(MFAOpts) ->
    File = astranaut:file(Forms),
    [{Line, Module}] = astranaut:attributes_with_line(module, Forms),
    {Macros, Warnings} =
        lists:foldl(
          fun({{M, F, A}, Opts}, Acc) -> 
                  add_macro({M, F, A}, Opts, Module, File, Line, Forms, Acc)
          end, {[], []}, lists:reverse(MFAOpts)),
    exec_macros(Macros, Forms, File, Warnings).

parse_transform(Forms, Options) ->
    File = astranaut:file(Forms),
    [Module] = astranaut:attributes(module, Forms),
    {Macros, Warnings} = macros(Forms, Module, File),
    case compile_local_macros(Macros, Forms, Options) of
        {ok, LWarnings} ->
            exec_macros(Macros, Forms, File, Warnings ++ LWarnings);
        {error, Errors, NWarnings} ->
            {error, Errors, Warnings ++ NWarnings}
    end.

format_error({unexported_macro, Module, Function, Arity}) ->
    io_lib:format("unexported macro ~p:~p/~p.", [Module, Function, Arity]);
format_error({undefined_macro, Function, Arity}) ->
    io_lib:format("undefined macro ~p/~p.", [Function, Arity]);
format_error({unloaded_module, Module}) ->
    io_lib:format("module ~p could not be loaded, add to erl_first_files in rebar.config to make it compile first.", [Module]);
format_error({invalid_use_macro, Opts}) ->
    io_lib:format("invalid use macro ~p.", [Opts]);
format_error({invalid_option_value, Key, Value}) ->
    io_lib:format("invalid option value ~p ~p.", [Key, Value]);
format_error({invalid_option_value, Value}) ->
    io_lib:format("invalid option value ~p.", [Value]);
format_error({invalid_option, Options}) ->
    io_lib:format("invalid option ~p.", [Options]);
format_error({non_exported_formatter, Module}) ->
    io_lib:format("format_error/1 is not exported from module ~p.", [Module]);
format_error({unloaded_formatter_module, Module}) ->
    io_lib:format("formatter module ~p could not be loaded.", [Module]);
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
exec_macros(Macros, Forms, File, Warnings) ->
    TraverseReturn = fold_walk_macros(Macros, Forms, [], Warnings),
    NTraverseReturn = astranaut_traverse:parse_transform_return(TraverseReturn, File),
    astranaut_traverse:map_traverse_return(
      fun(FormsAcc) ->
              NFormsAcc = astranaut:reorder_exports(FormsAcc),
              format_forms(NFormsAcc),
              NFormsAcc
      end, NTraverseReturn).
fold_walk_macros([Options|T], Forms, Errors, Warnings) ->
    case walk_macro(Options, Forms) of
        {ok, NForms, NErrors, NWarnings} ->
            fold_walk_macros(T, NForms, Errors ++ NErrors, Warnings ++ NWarnings);
        {error, NErrors, NWarnings} ->
            fold_walk_macros(T, Forms, Errors ++ NErrors, Warnings ++ NWarnings);
        NForms when is_list(NForms) ->
            fold_walk_macros(T, NForms, Errors, Warnings)
    end;
            
fold_walk_macros([], Forms, [], []) ->
    Forms;
fold_walk_macros([], Forms, Errors, Warnings) ->
    {ok, Forms, Errors, Warnings}.

walk_macro(MacroOpts, Forms) ->
    Traverse = maps:get(order, MacroOpts, post),
    MOpts = formatter_opts(MacroOpts),
    Opts = #{traverse => Traverse, formatter => ?MODULE},
    Monad = 
        astranaut_traverse_monad:bind(
          exec_macro(MacroOpts, Forms, MOpts),
          fun(NForms) ->
                  astranaut_traverse:map_m(
                    fun(Node, _Attr) ->
                            Return = walk_macro_node(Node, MacroOpts),
                            astranaut_traverse:fun_return_to_monad(Return, Node, MOpts)
                    end, NForms, Opts)
          end),
    Reply = astranaut_traverse_monad:run(Monad, ok),
    astranaut_traverse:map_traverse_return(
      fun({NNode, _State}) ->
              NNode
      end, Reply).

to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].

exec_macro(MacroOpts, Forms, MOpts) ->
    astranaut_traverse_monad:lift_m(
      fun({_, NForms}) ->
              NForms
      end,
      astranaut_traverse_monad:then(
        astranaut_traverse_monad:put({1, Forms}),
        astranaut_traverse_monad:then(
          astranaut_traverse_monad:map_m(
            fun(Form) ->
                    Return = walk_exec_macro(Form, MacroOpts),
                    astranaut_traverse_monad:bind(
                      astranaut_traverse:fun_return_to_monad(Return, Form, MOpts),
                      fun(NForm) ->
                              astranaut_traverse_monad:modify(
                                fun({N, Acc}) ->
                                        if
                                            Form == NForm ->
                                                {N + 1, Acc};
                                            true ->
                                                Line = erl_syntax:get_pos(Form),
                                                {Nodes, NAcc} = update_forms(to_list(NForm), Acc, MacroOpts#{line => Line}),
                                                NNAcc = astranaut:replace_from_nth(Nodes, N, NAcc),
                                                {N + length(Nodes), NNAcc}
                                        end
                                end)
                      end)
            end, Forms),
          astranaut_traverse_monad:get()))).

walk_exec_macro({attribute, Line, exec_macro, {Function, Arguments}} = NodeA, #{macro := Function} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_exec_macro({attribute, Line, exec_macro, {Module, Function, Arguments}} = NodeA,
                #{macro := {Module, Function}} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_exec_macro({attribute, Line, Attribute, Arguments} = NodeA, #{as_attr := Attribute} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_exec_macro(Node, _MacroOpts) ->
    Node.

update_forms(Nodes, Acc, #{auto_export := true, line := Line} = Opts) ->
    Exports = exports(Nodes, Line),
    update_forms(Exports ++ Nodes, Acc, maps:remove(auto_export, Opts));
update_forms(Nodes, Acc, #{merge_function := false}) ->
    {Nodes, Acc};
update_forms(Nodes, Forms, #{merge_function := MergeFunction}) ->
    {NRests, NForms} = 
        lists:foldl(
          fun({function, _Line, _FName, _Arity, _Clauses} = Function, {Rests, Acc}) ->
                  case insert_function(Function, Acc, MergeFunction, []) of
                      {ok, NAcc} ->
                          {Rests, NAcc};
                      error ->
                          {[Function|Rests], Acc}
                  end;
             ({attribute, Line, export, FAs}, {Rests, Acc}) ->
                  case lists:filter(
                         fun({Name, Arity}) ->
                                 not already_exported(Name, Arity, Acc)
                         end, FAs) of
                      [] ->
                          {Rests, Acc};
                      NFAs ->
                          NExport = {attribute, Line, export, NFAs},
                          {[NExport|Rests], Acc}
                  end;
             (Form, {Rests, Acc}) ->
              {[Form|Rests], Acc}
          end, {[], Forms}, Nodes),
    {lists:reverse(NRests), NForms};
update_forms(Nodes, Acc, _MOpts) ->
    {Nodes, Acc}.

exports(Nodes, Line) ->
    case lists:foldl(
           fun({function, _Line, FName, Arity, _Clauses}, Acc) ->
                   [{FName, Arity}|Acc];
              (_, Acc) ->
                   Acc
           end, [], Nodes) of
        [] ->
            [];
        FAs ->
            [astranaut:exports(lists:reverse(FAs), Line)]
    end.

insert_function({function, _Line, FName, Arity, Clauses}, 
                [{function, Line, FName, Arity, FClauses}|T], Merge, Heads) ->
    {ok, lists:reverse(Heads) ++ [{function, Line, FName, Arity, merge_clauses(Clauses, FClauses, Merge)}|T]};
insert_function(_Function, [], _Merge, _Heads) ->
    error;
insert_function(Function, [H|T], Merge, Heads) ->
    insert_function(Function, T, Merge, [H|Heads]).

already_exported(Name, Arity, [{attribute, _Line, export, FAs}|T]) ->
    case lists:member({Name, Arity}, FAs) of
        true ->
            true;
        false ->
            already_exported(Name, Arity, T)
    end;
already_exported(Name, Arity, [_H|T]) ->
    already_exported(Name, Arity, T);
already_exported(_Name, _Arity, []) ->
    false.

merge_clauses(Clauses1, Clauses2, head) ->
    Clauses1 ++ Clauses2;
merge_clauses(Clauses1, Clauses2, true) ->
    Clauses1 ++ Clauses2;
merge_clauses(Clauses1, Clauses2, tail) ->
    Clauses2 ++ Clauses1.

walk_macro_node({call, Line, {atom, _Line2, Function}, Arguments} = Node, #{macro := Function} = Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, line => Line});
walk_macro_node({call, Line, {remote, Line2, {atom, Line2, Module}, {atom, Line2, Function}}, Arguments} = Node, 
                #{macro := {Module, Function}} = Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, line => Line});
walk_macro_node(Node, __MacroOpts) ->
    Node.

apply_macro(NodeA, #{module := Module, function := Function, arity := Arity, arguments := Arguments, line := Line} = Opts) ->
    Arguments1 = group_arguments(Arguments, Opts),
    Arguments2 = append_attrs(Arguments1, Opts),
    if
        length(Arguments2) == Arity ->
            MacroReturn = apply_mfa(Module, Function, Arguments2),
            case astranaut_traverse:traverse_fun_return_struct(MacroReturn) of
                #{node := NodeB} = MacroReturnStruct ->
                    NodeC = astranaut:replace_line(NodeB, Line),
                    format_node(NodeC, Opts),
                    MacroReturnStruct#{node => NodeC};
                #{} = MacroReturnStruct ->
                    MacroReturnStruct#{node => NodeA}
            end;
        true ->
            astranaut_traverse:traverse_fun_return(#{node => NodeA})
    end.


group_arguments(Arguments, #{group_args := true}) ->
    [Arguments];
group_arguments(Arguments, #{}) ->
    Arguments.

append_attrs(Arguments, #{attributes := Attrs}) ->
    Arguments ++ [Attrs];
append_attrs(Arguments, #{}) ->
    Arguments.

apply_mfa(Module, Function, Arguments) ->
    try
        erlang:apply(Module, Function, Arguments)
    catch
        _:Exception:StackTrace ->
            {error, {exception, Exception, StackTrace}}
    end.

format_forms(Forms) ->
    case astranaut:attributes(debug_macro, Forms) of
        [] ->
            ok;
        [true] ->
            io:format("~s~n", [astranaut:to_string(Forms)])
    end.

format_node(Node, #{file := File, line := Line} = Opts) ->
    case maps:get(debug, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Line, format_mfa(Opts)]),
            case maps:get(debug_ast, Opts, false) of
                true ->
                    io:format("~p~n", [Node]);
                false ->
                    ok
            end,
            io:format("~s~n", [astranaut:to_string(Node)]);
        false ->
            ok
    end.
    
format_mfa(#{function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(#{module := Module, function := Function, arity := Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).

macros(Forms, LocalModule, File) ->
    Macros = lists:flatten(astranaut:attributes_with_line(use_macro, Forms)),
    {AllMacros, Warnings} = 
        lists:foldl(
          fun({Line, {Module, {Function, Arity}}}, Acc) ->
                  add_macro({Module, Function, Arity}, [], LocalModule, File, Line, Forms, Acc);
             ({Line, {Module, {Function, Arity}, Opts}}, Acc) when is_list(Opts) ->
                  add_macro({Module, Function, Arity}, Opts, LocalModule, File, Line, Forms, Acc);
             ({Line, {{Function, Arity}}}, Acc) ->
                  add_macro({Function, Arity}, [], LocalModule, File, Line, Forms, Acc);
             ({Line, {{Function, Arity}, Opts}}, Acc) when is_list(Opts)->
                  add_macro({Function, Arity}, Opts, LocalModule, File, Line, Forms, Acc);
             ({Line, Other}, {LocalMacrosAcc, AllMacrosAcc, WarningsAcc}) ->
                  {LocalMacrosAcc, AllMacrosAcc, [{Line, ?MODULE, {invalid_use_macro, Other}}|WarningsAcc]}
          end, {[], []}, Macros),
    {lists:reverse(AllMacros), lists:reverse(Warnings)}.

kmfa_options(Options, {Function, Arity}, LocalModule) ->
    Module = local_macro_module(LocalModule),
    Options1 = Options#{key => Function, module => Module, function => Function, arity => Arity, local => true},
    {ok, Options1};
kmfa_options(Options, {Module, Function, Arity}, _LocalModule) ->
    Options1 = Options#{key => {Module, Function}, module => Module, function => Function,
                        arity => Arity, local => false},
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

merge_attrs(#{attrs := Attrs, local_module := Module, file := File, line := Line} = Opts, Forms) ->
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
    Opts#{attributes => maps:merge(#{module => Module, file => File, line => Line}, AttributesMap)};
merge_attrs(#{} = Opts, _Forms) ->
    Opts.

formatter_opts(#{formatter := Module, local_module := Module} = Options) ->
    Options#{formatter => local_macro_module(Module), local_formatter => true};
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

update_alias(#{alias := Alias} = Options) ->
    Options#{macro => Alias};
update_alias(#{key := Key} = Options) ->
    Options#{macro => Key}.

add_macro(MFA, Options, LocalModule, File, Line, Forms, {AllMacros, Warnings}) ->
    {Options1, Warnings1} = validate_options(Options, Line, Warnings),
    case kmfa_options(Options1, MFA, LocalModule) of
        {ok, Options2} ->
            Options3 = update_alias(Options2),
            Options4 = Options3#{local_module => LocalModule, file => File, line => Line},
            Options5 = merge_attrs(Options4, Forms),
            Options6 = formatter_opts(Options5),
            case validate_formatter(Options6, Forms) of
                ok ->
                    {[Options6|AllMacros], Warnings1};
                {error, Reason} ->
                    Options7 = Options6#{formatter => ?MODULE},
                    {[Options7|AllMacros], [{Line, ?MODULE, Reason}|Warnings1]}
            end;
        {error, Reason} ->
            Warnings2 = [{Line, ?MODULE, Reason}|Warnings1],
            {AllMacros, Warnings2}
    end.

validate_options(Opts, Line, Warnings) when is_map(Opts) ->
    maps:fold(
        fun(Key, Value, {OptsAcc, WarningAcc} = Acc) ->
                case validate_option_key(Key, Value) of
                    ok ->
                        Acc;
                    error ->
                        NWarningsAcc =  [{Line, ?MODULE, {invalid_option_value, Key, Value}}|WarningAcc],
                        NOptsAcc = maps:remove(Key, OptsAcc),
                        {NOptsAcc, NWarningsAcc}
                end
        end, {Opts, Warnings}, Opts);
validate_options(Opts, Line, Warnings) when is_list(Opts) ->
    {Opts1, Warnings1} = 
        lists:foldl(
          fun({Key, Value}, {OptsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, Value, OptsAcc), WarningsAcc};
             (Key, {OptsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, true, OptsAcc), WarningsAcc};
             (Value, {OptsAcc, WarningsAcc}) ->
                  {OptsAcc, [{Line, ?MODULE, {invalid_option_value, Value}}|WarningsAcc]}
          end, {maps:new(), Warnings}, Opts),
    validate_options(Opts1, Line, Warnings1);
validate_options(Opts, Line, Warnings) ->
    {maps:new(), [{Line, ?MODULE, {invalid_option, Opts}}|Warnings]}.

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

compile_local_macros(MacrosOptions, Forms, Opts) ->
    LocalMacros = 
        lists:foldl(
          fun(#{function := Function, arity := Arity, local := true}, Acc) ->
                  ordsets:add_element({Function, Arity}, Acc);
             (#{}, Acc) ->
                  Acc
          end, ordsets:new(), MacrosOptions),
    UsedFunctions = 
        lists:foldl(
          fun(#{local_formatter := true} = Optsx, Acc) ->
                  io:format("options is ~p~n", [Optsx]),
                  ordsets:add_element({format_error, 1}, Acc);
             (#{}, Acc) ->
                  Acc
          end, LocalMacros, MacrosOptions),
    UsedAttrs = 
        lists:foldl(
          fun(#{as_attr := Attr}, Acc) ->
                  ordsets:add_element(Attr, Acc);
             (#{}, Acc) ->
                  Acc
          end, ordsets:from_list([exec_macro]), MacrosOptions),
    ClauseMap = function_clauses_map(Forms, maps:new()),
    MacroDeps = functions_deps(UsedFunctions, ClauseMap),
    Warnings = 
        lists:foldl(
          fun(#{function := Function, arity := Arity, line := Line, local := true}, Acc) ->
                  case ordsets:is_element({Function, Arity}, LocalMacros) of
                      true ->
                          Acc;
                      false ->
                          [{Line, ?MODULE, {undefined_macro, Function, Arity}}|Acc]
                  end;
             (#{}, Acc) ->
                  Acc
          end, [], MacrosOptions),
    NForms = 
        lists:reverse(
          lists:foldl(
            fun({attribute, Line, module, Module}, Acc) ->
                    Node = {attribute, Line, module, local_macro_module(Module)},
                    [Node|Acc];
               ({attribute, Line, export, Exports}, Acc) ->
                    case lists:member({format_error, 1}, Exports) of
                        true ->
                            case ordsets:is_element({format_error, 1}, UsedFunctions) of
                                true ->
                                    Export = {attribute, Line, export, [{format_error, 1}]},
                                    [Export|Acc];
                                false ->
                                    Acc
                            end;
                        false ->
                            Acc
                    end;
               ({attribute, Line, use_macro, {{Function, Arity}, _Opts}}, Acc) ->
                    case ordsets:is_element({Function, Arity}, MacroDeps) of
                        true ->
                            Export = {attribute, Line, export, [{Function, Arity}]},
                            [Export|Acc];
                        false ->
                            Acc
                    end;
               ({function, _Line, Name, Arity, _Clauses} = Node, Acc) ->
                    case ordsets:is_element({Name, Arity}, MacroDeps) of
                        true ->
                            [Node|Acc];
                        false ->
                            Acc
                    end;
               ({attribute,_Line, spec, {{Name,Arity}, _Body}} = Node, Acc) ->
                    case ordsets:is_element({Name,Arity}, MacroDeps) of
                        true ->
                            [Node|Acc];
                        false ->
                            Acc
                    end;
               ({attribute, _Line, AttrName, _AttrValue} = Attr, Acc) ->
                    case ordsets:is_element(AttrName, UsedAttrs) of
                        true ->
                            Acc;
                        false ->
                            [Attr|Acc]
                    end;
               (Node, Acc) ->
                    [Node|Acc]
            end, [], Forms)),
    case compile:forms(NForms, Opts) of
        {ok, Mod, Binary, _} ->
            case code:load_binary(Mod, [], Binary) of
                {module, _Mod} ->
                    {ok, Warnings};
                Other ->
                    {error, Other, Warnings}
            end;
        {error, Errors, NWarnings} ->
            {error, Errors, Warnings ++ NWarnings}
    end.

local_macro_module(Module) ->
    list_to_atom(atom_to_list(Module) ++ "__local_macro").

function_clauses_map([{function, _Line, Name, Arity, Clauses}|T], Acc) ->
    NAcc = maps:put({Name, Arity}, Clauses, Acc),
    function_clauses_map(T, NAcc);
function_clauses_map([_H|T], Acc) ->
    function_clauses_map(T, Acc);
function_clauses_map([], Acc) ->
    Acc.

functions_deps(Functions, ClausesMap) ->
    functions_deps(Functions, ClausesMap, ordsets:from_list(Functions)).

functions_deps(Functions, ClauseMap, Deps) ->
    lists:foldl(
      fun(Function, Acc) ->
              case maps:find(Function, ClauseMap) of
                  {ok, Clauses} ->
                      FDeps = ordsets:union(lists:map(fun function_deps/1, Clauses)),
                      NDeps = ordsets:union(FDeps, Acc),
                      AddedFunctions = ordsets:subtract(FDeps, Deps),
                      functions_deps(AddedFunctions, ClauseMap, NDeps);
                  error ->
                      ordsets:del_element(Function, Acc)
              end
      end, Deps, Functions).    

function_deps({clause, _Line1, _Patterns, _Guards, Exprs}) ->
    astranaut_traverse:reduce(
      fun({call, _Line2, {atom, _Line3, Function}, Arguments}, Acc, _Attr) ->
              Arity = length(Arguments),
              ordsets:add_element({Function, Arity}, Acc);
         (_, Acc, _Attr) ->
              Acc
      end, ordsets:new(), Exprs, #{traverse => pre}).
