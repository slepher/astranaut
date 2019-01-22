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
    {LocalMacros, Macros} = macros(Forms, Module, File),
    case compile_macros(LocalMacros, Forms, Options) of
        ok ->
            TraverseReturn = fold_walk_macros(Macros, Forms),
            astranaut_traverse:map_traverse_return(
              fun(FormsAcc) ->
                      astranaut:reorder_exports(FormsAcc)
              end, TraverseReturn);
        {error, Errors, Warnings} ->
            {error, Errors, Warnings}
    end.

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

fold_walk_macros([{Macro, Opts}|T], Forms) ->
    NForms = walk_macro(Macro, Opts, Forms),
    fold_walk_macros(T, NForms);
fold_walk_macros([], Forms) ->
    Forms.

walk_macro(Macro, MacroOpts, Forms) ->
    Traverse = maps:get(order, MacroOpts, post),
    Opts = #{traverse => Traverse, formatter => ?MODULE, parse_transform => true},
    NForms = 
        lists:foldl(
          fun(Form, Acc) ->
                  walk_exec_macro(Form, Macro, MacroOpts, Acc)
          end, [], Forms),
    astranaut_traverse:map(
      fun(Node, _Attr) ->
              walk_macro_node(Node, Macro, MacroOpts)
      end, lists:reverse(NForms), Opts).
%%%===================================================================
%%% Internal functions
%%%===================================================================
walk_exec_macro({attribute, Line, exec_macro, {Function, Arguments}} = Node, Function, Opts, Acc) ->
    case apply_macro(Opts#{arguments => Arguments, line => Line}) of
        {ok, NNode} ->
            append_node(NNode, Acc);
        error ->
            append_node(Node, Acc)
    end;
walk_exec_macro({attribute, Line, exec_macro, {Module, Function, Arguments}} = Node, {Module, Function}, Opts, Acc) ->
    case apply_macro(Opts#{arguments => Arguments, line => Line}) of
        {ok, NNode} ->
            append_node(NNode, Acc);
        error ->
            append_node(Node, Acc)
    end;
walk_exec_macro(Node, _Macro, _MacroOpts, Acc) ->
    [Node|Acc].

append_node(Nodes, Acc) when is_list(Nodes) ->
    lists:reverse(Nodes) ++ Acc;
append_node(Node, Acc) ->
    [Node|Acc].

walk_macro_node({call, Line, {atom, _Line2, Function}, Arguments} = Node, Function, Opts) ->
    case apply_macro(Opts#{arguments => Arguments, line => Line}) of
        {ok, NNode} ->
            NNode;
        error ->
            Node
    end;
walk_macro_node({call, Line, {remote, Line2, {atom, Line2, Module}, {atom, Line2, Function}}, Arguments} = Node, 
                {Module, Function}, Opts) ->
    case apply_macro(Opts#{arguments => Arguments, Line => Line}) of
        {ok, NNode} ->
            NNode;
        error ->
            Node
    end;
walk_macro_node(Node, _Macro, _MacroOpts) ->
    Node.

apply_macro(Opts) ->
    #{arguments := NArguments, arity := Arity} = NOpts = append_attrs(Opts),
    if
        length(NArguments) == Arity ->
            Node = apply_mfa(NOpts),
            {ok, Node};
        true ->
            error
    end.

append_attrs(#{arguments := Arguments, attrs := Attrs} = Opts) ->
    Opts#{arguments => Arguments ++ [Attrs]};
append_attrs(#{} = Opts) ->
    Opts.

apply_mfa(#{module := Module, function := Function, arguments := Arguments, line := Line} = Opts) ->
    Node = astranaut:replace_line(erlang:apply(Module, Function, Arguments), Line),
    format_node(Node, Opts),
    Node.

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
format_mfa(#{module := Module, function := Function, arity := Arity, local := true}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).

macros(Forms, LocalModule, File) ->
    Macros = lists:flatten(astranaut:attributes_with_line(use_macro, Forms)),
    {LocalMacros, AllMacros} = 
        lists:foldl(
          fun({Line, {Module, {Function, Arity}}}, Acc) ->
                  add_macro({Module, Function, Arity}, [], LocalModule, File, Line, Acc);
             ({Line, {Module, {Function, Arity}, Opts}}, Acc) when is_list(Opts) ->
                  add_macro({Module, Function, Arity}, Opts, LocalModule, File, Line, Acc);
             ({Line, {{Function, Arity}}}, Acc) ->
                  add_macro({Function, Arity}, [], LocalModule, File, Line, Acc);
             ({Line, {{Function, Arity}, Opts}}, Acc) when is_list(Opts)->
                  add_macro({Function, Arity}, Opts, LocalModule, File, Line, Acc);
             ({Line, Other}, Acc) ->
                  io:format("invalid import macro ~p at ~p~n", [Other, Line]),
                  Acc
          end, {[], []}, Macros),
    {lists:reverse(LocalMacros), lists:reverse(AllMacros)}.

add_macro(MFA, Options, LocalModule, File, Line, Acc) when is_list(Options) ->
    add_macro(MFA, maps:from_list(Options), LocalModule, File, Line, Acc);

add_macro({Function, Arity}, Options, LocalModule, File, Line, {LocalMacros, AllMacros}) ->
    Module = local_macro_module(LocalModule),
    NOptions = Options#{module => Module, function => Function, arity => Arity, file => File, line => Line, local => true},
    NLocalMacros = [{Function, Arity}|LocalMacros],
    NAllMacros = [{Function, NOptions}|AllMacros],
    {NLocalMacros, NAllMacros};
add_macro({Module, Function, Arity}, Options, _LocalModule, File, Line, {LocalMacros, AllMacros} = Acc) ->
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({Function, Arity}, Exports) of
                true ->
                    NOptions = Options#{module => Module, function => Function, arity => Arity, file => File, line => Line},
                    case maps:find(import_as, Options) of
                        {ok, As} ->
                            NAllMacros = [{As, NOptions}|AllMacros],
                            {LocalMacros, NAllMacros};
                        error ->
                            NAllMacros = [{{Module, Function}, NOptions}|AllMacros],
                            {LocalMacros, NAllMacros}
                    end;
                false ->
                    io:format("~s:~p unexported macro ~p:~p~n", [File, Line, Module, Function]),
                    Acc
            end;
        {error, undef} ->
            Msg = "~s:~p module ~p could not be loaded, add to erl_first_files in rebar.config to make it compile first.~n",
            io:format(Msg, [File, Line, Module]),
            Acc
    end.

get_exports(Module) ->
    try
        {ok, Module:module_info(exports)}
    catch
        _:undef ->
            {error, undef}
    end.

compile_macros(Macros, Forms, Opts) ->
    MacroDeps = macro_deps(Macros, Forms),
    NForms = 
        lists:reverse(
          lists:foldl(
            fun({attribute, Line, module, Module}, Acc) ->
                    Node = {attribute, Line, module, local_macro_module(Module)},
                    Exports = {attribute, Line, export, Macros},
                    [Exports, Node|Acc];
               ({attribute, _Line, export, _Exports}, Acc) ->
                    Acc;
               ({attribute, _Line, use_macro, _Attr}, Acc) ->
                    Acc;
               ({attribute, _Line, exec_macro, _Attr}, Acc) ->
                    Acc;
               ({function, _Line, Name, Arity, _Clauses} = Node, Acc) ->
                    case ordsets:is_element({Name, Arity}, MacroDeps) of
                        true ->
                            [Node|Acc];
                        false ->
                            Acc
                    end;
               ({attribute,_Line,spec, {{Name,Arity}, _Body}} = Node, Acc) ->
                    case ordsets:is_element({Name,Arity}, MacroDeps) of
                        true ->
                            [Node|Acc];
                        false ->
                            Acc
                    end;
               (Node, Acc) ->
                    [Node|Acc]
            end, [], Forms)),
    case compile:forms(NForms, Opts) of
        {ok, Mod, Binary, _} ->
            case code:load_binary(Mod, [], Binary) of
                {module, _Mod} ->
                    ok;
                Other ->
                    {error, Other}
            end;
        {error, Errors, Warnings} ->
            {error, Errors, Warnings}
    end.

local_macro_module(Module) ->
    list_to_atom(atom_to_list(Module) ++ "__local_macro").

macro_deps(Macros, Forms) ->
    ClauseMap = function_clauses_map(Forms, maps:new()),
    functions_deps(Macros, ClauseMap).

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
                      Acc
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
