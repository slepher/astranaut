%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_macro_local).

%% API
-export([compile/3, module/1]).

%%%===================================================================
%%% API
%%%===================================================================
module(Module) ->
    list_to_atom(atom_to_list(Module) ++ "__local_macro").

compile(MacrosOptions, Forms, Opts) ->
    LocalMacros = 
        lists:foldl(
          fun(#{function := Function, arity := Arity, local := true}, Acc) ->
                  ordsets:add_element({Function, Arity}, Acc);
             (#{}, Acc) ->
                  Acc
          end, ordsets:new(), MacrosOptions),
    UsedFunctions = 
        lists:foldl(
          fun(#{local_formatter := true}, Acc) ->
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
                    Node = {attribute, Line, module, module(Module)},
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
               ({attribute, Line, use_macro, {{Function, Arity}}}, Acc) ->
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

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
