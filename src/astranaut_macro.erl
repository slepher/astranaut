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
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, Options) ->
    File = astranaut:file(Forms),
    [Module] = astranaut:attributes(module, Forms),
    {LocalMacros, Macros, Warnings} = macros(Forms, Module, File),
    case compile_local_macros(LocalMacros, Forms, Options) of
        {ok, LWarnings} ->
            TraverseReturn = fold_walk_macros(Macros, Forms, [], Warnings ++ LWarnings),
            NTraverseReturn = astranaut_traverse:parse_transform_return(TraverseReturn, File),
            astranaut_traverse:map_traverse_return(
              fun(FormsAcc) ->
                      NFormsAcc = astranaut:reorder_exports(FormsAcc),
                      case astranaut:attributes(debug_macro, Forms) of
                          [] ->
                              ok;
                          [true] ->
                              io:format("~s~n", [astranaut:to_string(NFormsAcc)])
                      end,
                      NFormsAcc
              end, NTraverseReturn);
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
fold_walk_macros([{Macro, Opts}|T], Forms, Errors, Warnings) ->
    case walk_macro(Macro, Opts, Forms) of
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

walk_macro(Macro, MacroOpts, Forms) ->
    Traverse = maps:get(order, MacroOpts, post),
    MOpts = maps:merge(#{formatter => astranaut_traverse}, maps:with([formatter], MacroOpts)),
    Opts = #{traverse => Traverse, formatter => ?MODULE},
    Monad = 
        astranaut_traverse_monad:bind(
          exec_macro(Macro, MacroOpts, Forms, MOpts),
          fun(NForms) ->
                  astranaut_traverse:map_m(
                    fun(Node, _Attr) ->
                            Return = walk_macro_node(Node, Macro, MacroOpts),
                            astranaut_traverse:fun_return_to_monad(Return, Node, MOpts)
                    end, NForms, Opts)
          end),
    Reply = astranaut_traverse_monad:run(Monad, ok),
    astranaut_traverse:map_traverse_return(
      fun({NNode, _State}) ->
              NNode
      end, Reply).
%%%===================================================================
%%% Internal functions
%%%===================================================================
to_list(Arguments) when is_list(Arguments) ->
    Arguments;
to_list(Arguments) ->
    [Arguments].

exec_macro(Macro, MacroOpts, Forms, MOpts) ->
    Opts = #{traverse => pre, formatter => ?MODULE},
    astranaut_traverse_monad:lift_m(
      fun({_, NForms}) ->
              NForms
      end,
      astranaut_traverse_monad:then(
        astranaut_traverse_monad:put({1, Forms}),
        astranaut_traverse_monad:then(
          astranaut_traverse:map_m(
            fun(Form, _Attr) ->
                    Return = walk_exec_macro(Form, Macro, MacroOpts),
                    astranaut_traverse_monad:bind(
                      astranaut_traverse:fun_return_to_monad(Return, Form, MOpts),
                      fun(NForm) ->
                              astranaut_traverse_monad:state(
                                fun({N, Acc}) ->
                                        if
                                            Form == NForm ->
                                                {continue, {N + 1, Acc}};
                                            true ->
                                                Line = erl_syntax:get_pos(Form),
                                                {Nodes, NAcc} = update_forms(to_list(NForm), Acc, MacroOpts#{line => Line}),
                                                NNAcc = astranaut:replace_from_nth(Nodes, N, NAcc),
                                                {continue, {N + length(Nodes), NNAcc}}
                                        end
                                end)
                      end)
            end, Forms, Opts),
          astranaut_traverse_monad:get()))).

walk_exec_macro({attribute, Line, exec_macro, {Function, Arguments}} = NodeA, Function, Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_exec_macro({attribute, Line, exec_macro, {Module, Function, Arguments}} = NodeA,
                {Module, Function}, Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_exec_macro({attribute, Line, Attribute, Arguments} = NodeA, _, #{as_attr := Attribute} = Opts) ->
    apply_macro(NodeA, Opts#{arguments => to_list(Arguments), line => Line});
walk_exec_macro(Node, _Macro, _MacroOpts) ->
    Node.

update_forms(Nodes, Acc, #{auto_export := true, line := Line} = Opts) ->
    Exports = exports(Nodes, Line),
    update_forms(Exports ++ Nodes, Acc, maps:remove(auto_export, Opts));
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

insert_function({function, _Line, FName, Arity, Clauses}, [{function, Line, FName, Arity, FClauses}|T], Merge, Heads) ->
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
merge_clauses(Clauses1, Clauses2, tail) ->
    Clauses2 ++ Clauses1.

walk_macro_node({call, Line, {atom, _Line2, Function}, Arguments} = Node, Function, Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, line => Line});
walk_macro_node({call, Line, {remote, Line2, {atom, Line2, Module}, {atom, Line2, Function}}, Arguments} = Node, 
                {Module, Function}, Opts) ->
    apply_macro(Node, Opts#{arguments => Arguments, Line => Line});
walk_macro_node(Node, _Macro, _MacroOpts) ->
    Node.

apply_macro(NodeA, #{module := Module, function := Function, arity := Arity, line := Line} = Opts) ->
    NArguments = append_attrs(Opts),
    if
        length(NArguments) == Arity ->
            MacroReturn = apply_mfa(Module, Function, NArguments),
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

append_attrs(#{arguments := Arguments, attributes := Attrs}) ->
    Arguments ++ [Attrs];
append_attrs(#{arguments := Arguments}) ->
    Arguments.

apply_mfa(Module, Function, Arguments) ->
    try
        erlang:apply(Module, Function, Arguments)
    catch
        _:Exception:StackTrace ->
            {error, {exception, Exception, StackTrace}}
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
    {LocalMacros, AllMacros, Warnings} = 
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
          end, {[], [], []}, Macros),
    {lists:reverse(LocalMacros), lists:reverse(AllMacros), lists:reverse(Warnings)}.

add_macro(MFA, Options, LocalModule, File, Line, Forms, Acc) when is_list(Options) ->
    add_macro(MFA, maps:from_list(Options), LocalModule, File, Line, Forms, Acc);

add_macro({Function, Arity}, Options, LocalModule, File, Line, Forms, {LocalMacros, AllMacros, Warnings}) ->
    Module = local_macro_module(LocalModule),
    NOptions = Options#{module => Module, function => Function, arity => Arity, file => File, line => Line, local => true},
    NNOptions = merge_attrs(NOptions, LocalModule, File, Line, Forms),
    NLocalMacros = [{Function, Arity, Line}|LocalMacros],
    NAllMacros = [{Function, NNOptions}|AllMacros],
    {NLocalMacros, NAllMacros, Warnings};

add_macro({Module, Function, Arity}, Options, LocalModule, File, Line, Forms, {LocalMacros, AllMacros, Warnings}) ->
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({Function, Arity}, Exports) of
                true ->
                    NOptions = Options#{module => Module, function => Function, arity => Arity, file => File, line => Line},
                    NNOptions = merge_attrs(NOptions, LocalModule, File, Line, Forms),
                    case maps:find(import_as, Options) of
                        {ok, As} ->
                            NAllMacros = [{As, NNOptions}|AllMacros],
                            {LocalMacros, NAllMacros, Warnings};
                        error ->
                            NAllMacros = [{{Module, Function}, NNOptions}|AllMacros],
                            {LocalMacros, NAllMacros, Warnings}
                    end;
                false ->
                    {LocalMacros, AllMacros, [{Line, ?MODULE, {unexported_macro, Module, Function, Arity}}|Warnings]}
            end;
        {error, undef} ->
            {LocalMacros, AllMacros, [{Line, ?MODULE, {unloaded_module, Module}}|Warnings]}
    end.

merge_attrs(#{attrs := Attrs} = Opts, LocalModule, File, Line, Forms) when is_list(Attrs) ->
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
    Opts#{attributes => maps:merge(#{module => LocalModule, file => File, line => Line}, AttributesMap)};
merge_attrs(#{} = Opts, _LocalModule, _File, _Line, _Forms) ->
    Opts.


get_exports(Module) ->
    try
        {ok, Module:module_info(exports)}
    catch
        _:undef ->
            {error, undef}
    end.

compile_local_macros(Macros, Forms, Opts) ->
    MacroDeps = macro_deps(Macros, Forms),
    Warnings = 
        lists:foldl(
          fun({Function, Arity, Line}, Acc) ->
                  case ordsets:is_element({Function, Arity}, MacroDeps) of
                      true ->
                          Acc;
                      false ->
                          [{Line, ?MODULE, {undefined_macro, Function, Arity}}|Acc]
                  end
          end, [], Macros),
    NForms = 
        lists:reverse(
          lists:foldl(
            fun({attribute, Line, module, Module}, Acc) ->
                    Node = {attribute, Line, module, local_macro_module(Module)},
                    [Node|Acc];
               ({attribute, _Line, export, _Exports}, Acc) ->
                    Acc;
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
               ({attribute, _Line, _AttrName, _Attr}, Acc) ->
                    Acc;
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

macro_deps(Macros, Forms) ->
    NMacros = 
        lists:map(
          fun({Function, Arity, _Line}) ->
                  {Function, Arity}
          end, Macros),
    ClauseMap = function_clauses_map(Forms, maps:new()),
    functions_deps(NMacros, ClauseMap).

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
