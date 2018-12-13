%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, Options) ->
    File = astranaut:file(Forms),
    [Module] = astranaut:attributes(module, Forms),
    {LocalMacros, RemoteMacros} = macros(Forms, File),
    case compile_macros(LocalMacros, Forms, Options) of
        ok ->
            Macros = maps:merge(LocalMacros, RemoteMacros),
            NForms = exec_macros(Forms, Macros, Module, File),
            NNForms = astranaut_traverse:map(fun(Node) -> walk_macros(Node, Macros, Module, File) end, NForms, pre),
            astranaut:reorder_exports(NNForms);
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk_macros(Node, Macros, Module, File) ->
    case walk_node(Node, Macros, Module, File) of
        not_match ->
            Node;
        NNode ->
            NNode
    end.

walk_node({call, Line, {atom, _Line2, Function}, Arguments}, Macros, LocalModule, File) ->
    case do_apply_macro({Function, Arguments}, Macros, LocalModule, File, Line) of
        {ok, Node} ->
            Node;
        error ->
            not_match
    end;
walk_node({call, Line, {remote, Line2, {atom, Line2, Module}, {atom, Line2, Function}}, Arguments}, Macros, LocalModule, File) ->
    case do_apply_macro({Module, Function, Arguments}, Macros, LocalModule, File, Line) of
        {ok, Node} ->
            Node;
        error ->
            not_match
    end;
walk_node(_Node, _Macros, _Module, _File) ->
    not_match.

format_node(File, Line, Module, Function, Arity, Nodes, Opts) ->
    case proplists:get_value(debug, Opts, false) of
        true ->
            io:format("from ~s:~p ~s~n", [filename:basename(File), Line, format_mfa(Module, Function, Arity)]),
            case proplists:get_value(debug_ast, Opts, false) of
                true ->
                    io:format("~p~n", [Nodes]);
                false ->
                    ok
            end,
            io:format("~s~n", [astranaut:to_string(Nodes)]);
        false ->
            ok
    end.

format_mfa(undefined, Function, Arity) ->
    io_lib:format("~p/~p", [Function, Arity]);
format_mfa(Module, Function, Arity) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).

macros(Forms, File) ->
    Macros = lists:flatten(astranaut:attributes_with_line(use_macro, Forms)),
    lists:foldl(
      fun({Line, {Module, {Function, Arity}}}, Acc) ->
              add_macro({Module, Function, Arity}, [], File, Line, Acc);
         ({Line, {Module, {Function, Arity}, Opts}}, Acc) when is_list(Opts)->
              add_macro({Module, Function, Arity}, Opts, File, Line, Acc);
         ({Line, {{Function, Arity}}}, Acc) ->
              add_macro({Function, Arity}, [], File, Line, Acc);
         ({Line, {{Function, Arity}, Opts}}, Acc) when is_list(Opts)->
              add_macro({Function, Arity}, Opts, File, Line, Acc);
         ({Line, Other}, Acc) ->
              io:format("invalid import macro ~p at ~p~n", [Other, Line]),
              Acc
      end, {maps:new(), maps:new()}, Macros).

exec_macros(Forms, Macros, LocalModule, File) ->
    lists:foldr(
      fun({attribute, Line, exec_macro, UsedMacros}, Acc) ->
              NNode = apply_macros(UsedMacros, Macros, LocalModule, File, Line),
              append_node(NNode, Acc);
         (Node, Acc) ->
              [Node|Acc]
      end, [], Forms).

apply_macros(UsedMacros, Macros, LocalModule, File, Line) when is_list(UsedMacros) ->
    lists:foldr(
      fun(UsedMacro, Acc) ->
              Node = apply_macro(UsedMacro, Macros, LocalModule, File, Line),
              append_node(Node, Acc)
      end,[], UsedMacros);
apply_macros(UsedMacro, Macros, LocalModule, File, Line) ->
    apply_macro(UsedMacro, Macros, LocalModule, File, Line).

apply_macro(Macro, Macros, LocalModule, File, Line) ->
    {Module, Function, Arity} = mfa(Macro),
    case do_apply_macro(Macro, Macros, LocalModule, File, Line) of
        {ok, Node} ->
            Node;
        error ->
            io:format("unimported macro ~s at ~p~n", [format_mfa(Module, Function, Arity), Line]),
            []
    end.

mfa({Module, Function, Arguments}) ->
    {Module, Function, length(Arguments)};
mfa({Function, Arguments}) ->
    {undefined, Function, length(Arguments)}.

do_apply_macro({Function, Arguments}, Macros, LocalModule, File, Line) ->
    Arity = length(Arguments),
    case maps:find({Function, Arity}, Macros) of
        {ok, Opts} ->
            MacroModule = local_macro_module(LocalModule),
            Node = astranaut:replace_line(erlang:apply(MacroModule, Function, Arguments), Line),
            format_node(File, Line, undefined, Function, Arity, Node, Opts),
            {ok, Node};
        error -> 
            error

    end;
do_apply_macro({Module, Function, Arguments}, Macros, _LocalModule, File, Line) ->
    Arity = length(Arguments),
    case maps:find({Module, Function, Arity}, Macros) of
        {ok, Opts} ->
            Node = astranaut:replace_line(erlang:apply(Module, Function, Arguments), Line),
            format_node(File, Line, Module, Function, Arity, Node, Opts),
            {ok, Node};
        error -> 
            error
    end.

append_node(Nodes, Acc) when is_list(Nodes) ->
    Nodes ++ Acc;
append_node(Node, Acc) ->
    [Node|Acc].

add_macro({Function, Arity}, Options, File, Line, {LocalMacros, RemoteMacros}) ->
    NLocalMacros = maps:put({Function, Arity}, [{file, File}, {line, Line}|Options], LocalMacros),
    {NLocalMacros, RemoteMacros};
add_macro({Module, Function, Arity}, Options, File, Line, {LocalMacros, RemoteMacros} = Acc) ->
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({Function, Arity}, Exports) of
                true ->
                    NRemoteMacros = maps:put({Module, Function, Arity}, Options, RemoteMacros),
                    {LocalMacros, NRemoteMacros};
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
                    Exports = {attribute, Line, export, maps:keys(Macros)},
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
    MacroFuns = maps:keys(Macros),
    ClauseMap = function_clauses_map(Forms, maps:new()),
    functions_deps(MacroFuns, ClauseMap).

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
      fun({call, _Line2, {atom, _Line3, Function}, Arguments}, Acc) ->
              Arity = length(Arguments),
              ordsets:add_element({Function, Arity}, Acc);
         (_, Acc) ->
              Acc
      end, ordsets:new(), Exprs, pre).
