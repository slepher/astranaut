%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut).

%% API exports
-export([attributes/2, attributes_with_line/2, module_attributes/2, read/1]).
-export([abstract/1, abstract/2]).
-export([file/1]).
-export([exports/1, exports/2, exported_function/2, function/2, function_fa/1, merge_clauses/1]).
-export([replace_line/2, replace_line_zero/2, to_string/1]).
-export([replace_from_nth/3]).
-export([reorder_exports/1]).
-export([validate_options/2]).
-export([ast_to_options/1]).
-export([relative_path/1]).
%%====================================================================
%% API functions
%%====================================================================

%% this method is from https://github.com/efcasado/forms/blob/master/src/forms.erl
-spec read(atom() | iolist()) -> [erl_parse:abstract_form()].
read(Module) when is_atom(Module) ->
    case beam_lib:chunks(code:which(Module), [abstract_code]) of
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {no_debug_info, _}} ->
            throw({forms_not_found, Module});
        {error, beam_lib, {file_error, _, enoent}} ->
            throw({module_not_found, Module})
    end;
read(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            Forms;
        {ok, Forms, _Extra} ->
            Forms;
        {error, enoent} ->
            throw({file_not_found, File})
    end.

abstract(Term) ->
    erl_syntax:revert(erl_syntax:abstract(Term)).

abstract(Term, Line) ->
    replace_line(abstract(Term), Line).

-spec attributes(atom(), _Forms) -> [_Attribute].

module_attributes(Attribute, Module) ->
    Attributes = Module:module_info(attributes),
    lists:reverse(
      lists:foldl(
        fun({Attr, Value}, Acc) when Attr == Attribute ->
                [Value|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Attributes)).

file(Forms) ->
    [{File, _}|_] = attributes(file, Forms),
    File.

attributes(Attribute, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, _Line, Attr, Values}, Acc) when Attr == Attribute ->
                [Values|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Forms)).

attributes_with_line(Attribute, Forms) ->
    lists:reverse(
      lists:foldl(
        fun({attribute, Line, Attr, Values}, Acc) when Attr == Attribute ->
                [{Line, Values}|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Forms)).

replace_line(Ast, Line) ->
    replace_line_cond(fun(_) -> true end, Ast, Line).

replace_line_zero(Ast, 0) ->
    Ast;
replace_line_zero(Ast, Line) ->
    replace_line_cond(
      fun(0) -> true;
         (_) -> false
      end, Ast, Line).

replace_line_cond(Cond, Ast, Line) when is_integer(Line) ->
    astranaut_traverse:map(
      fun(Node, #{node := attribute}) ->
              Node;
         (Tuple, _Attr) when is_tuple(Tuple) ->
              case tuple_to_list(Tuple) of
                  [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                      case Cond(TupleLine) of
                          true ->
                              setelement(2, Tuple, Line);
                          false ->
                              Tuple
                      end;
                  _ ->
                      Tuple
              end;
         (Node, _Attr) ->
              Node
         end, Ast, #{traverse => pre}).

exported_function(Name, {'fun', Line, {clauses, _Clauses}} = Fun) ->
    Function = function(Name, Fun),
    FunctionFa = function_fa(Function),
    [exports([FunctionFa], Line), Function].

function(Name, {'fun', Line, {clauses, Clauses}}) ->
    Arity = clause_arity(Clauses),
    {function, Line, Name, Arity, Clauses}.

function_fa({function, _Line, Name, Arity, _Clauses}) ->
    {Name, Arity}.

exports(Exports) ->
    exports(Exports, 0).

exports(Exports, Line) ->
    {attribute, Line, export, Exports}.

reorder_exports(Forms) ->
    lists:foldl(
      fun(N, Acc) ->
              Form = lists:nth(N, Forms),
              case Form of
                  {attribute, _Line, export, _FAs} = Export ->
                      replace_from_nth(Export, N, Acc);
                  _ ->
                      Acc
              end
      end, Forms, lists:seq(1, length(Forms))).

to_string(Forms) when is_list(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms));
to_string(Form) ->
    erl_prettypr:format(erl_syntax:form_list([Form])).

merge_clauses([{'fun', Line, {clauses, _}}|_T] = Nodes) -> 
    NClauses = 
        lists:flatten(
          lists:map(
            fun({'fun', _, {clauses, FClauses}}) ->
                    FClauses
            end, Nodes)),
    {'fun', Line, {clauses, NClauses}}.

replace_from_nth(Nodes, N, Forms) when is_list(Nodes), N > 0 ->
    replace_from_nth(Nodes, N, Forms, [], true);
replace_from_nth(Node, N, Forms) when N > 0 ->
    replace_from_nth([Node], N, Forms).

replace_from_nth(Nodes, 1, [_|Forms], Heads, _WithExports) ->
    lists:reverse(Heads) ++ Nodes ++ Forms;
replace_from_nth(Nodes, N, [{function, _Line0, _Name, _Arity, _Clauses} = Fun|Forms], Heads, true) ->
    {Exports, Rests} = 
        lists:partition(
          fun({attribute, _Line1, export, _FAs}) ->
                  true;
             (_) ->
                  false
          end, Nodes),
    %io:format("replaced ~p ~p~n~p~n~p~n", [Rests, N - 1, Forms, [Fun|lists:reverse(Exports) ++ Heads]]),
    replace_from_nth(Rests, N - 1, Forms, [Fun|lists:reverse(Exports) ++ Heads], false);
    
replace_from_nth(Nodes, N, [Head|Forms], Heads, WithExports) ->
    replace_from_nth(Nodes, N - 1, Forms, [Head|Heads], WithExports).

ast_to_options(Ast) ->
    ast_to_options(Ast, []).

ast_to_options(Ast, ExcepKeys) ->
    Writer = astranaut_monad_writer_t:writer_t(astranaut_monad_identity:new()),
    MonadWriter = ast_to_options(Ast, ExcepKeys, Writer),
    astranaut_monad_identity:run(astranaut_monad_writer_t:run(MonadWriter)).

ast_to_options({cons, _Line, Head, Tail}, _Keys, Writer) ->
    astranaut_monad:bind(
      ast_to_value(Head, Writer),
      fun(Head1) ->
              astranaut_monad:bind(
                ast_to_options(Tail, Writer),
                fun(Tail1) ->
                        astranaut_monad:return([Head1|Tail1], Writer)
                end, Writer)
      end, Writer);
ast_to_options({nil, _Line}, _Keys, Writer) ->
    astranaut_monad:return([], Writer);
ast_to_options({map, _Line, MapAssocs}, Keys, Writer) ->
    astranaut_monad:foldl_m(
      fun({map_field_assoc, _, {atom, _LineA, Key}, Value}, Acc) ->
              case lists:member(Key, Keys) of
                  true ->
                      astranaut_monad:return(maps:put(Key, Value, Acc), Writer);
                  false ->
                      astranaut_monad:bind(
                        ast_to_value(Value, Writer),
                        fun(Value1) ->
                                astranaut_monad:return(maps:put(Key, Value1, Acc), Writer)
                        end, Writer)
              end;
         ({map_field_assoc, _, Key, _Value}, Acc) ->
              astranaut_monad:then(
                astranaut_monad:tell([{invalid_option_key, Key}], Writer),
                astranaut_monad:return(Acc, Writer),
                Writer)
      end, maps:new(), MapAssocs, Writer);
ast_to_options(Value, _Keys, Warnings) ->
    ast_to_value(Value, Warnings).

ast_to_value({Type, _Line, Value}, Writer) when Type == atom ; Type == integer; Type == float; Type == string ->
    astranaut_monad:return(Value, Writer);
ast_to_value(Value, Writer) ->
    astranaut_monad:return(Value, Writer).

validate_options(F, Options) ->
    validate_options(F, Options, []).

validate_options(F, Options, Warnings) when is_map(Options) ->
    maps:fold(
        fun(Key, Value, {OptionsAcc, WarningAcc} = Acc) ->
                case apply_f(F, Key, Value, Options) of
                    ok ->
                        Acc;
                    error ->
                        NWarningsAcc = [{invalid_option_value, Key, Value}|WarningAcc],
                        NOptsAcc = maps:remove(Key, OptionsAcc),
                        {NOptsAcc, NWarningsAcc};
                    {error, Reason} ->
                        NWarningsAcc = [Reason|WarningAcc],
                        NOptsAcc = maps:remove(Key, OptionsAcc),
                        {NOptsAcc, NWarningsAcc}
                end
        end, {Options, Warnings}, Options);
validate_options(F, Options, Warnings) when is_list(Options) ->
    {Options1, Warnings1} = 
        lists:foldl(
          fun({Key, Value}, {OptionsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, Value, OptionsAcc), WarningsAcc};
             (Key, {OptionsAcc, WarningsAcc}) when is_atom(Key) ->
                  {maps:put(Key, true, OptionsAcc), WarningsAcc};
             (Value, {OptionsAcc, WarningsAcc}) ->
                  {OptionsAcc, [{invalid_option_value, Value}|WarningsAcc]}
          end, {maps:new(), Warnings}, Options),
    validate_options(F, Options1, Warnings1);
validate_options(F, Attr, Warings) when is_atom(Attr) ->
    validate_options(F, #{Attr => true}, Warings);
validate_options(_F, Options, Warnings) ->
    {maps:new(), [{invalid_option, Options}|Warnings]}.

relative_path(Path) ->
    case file:get_cwd() of
        {ok, BasePath} ->
            string:replace(Path, BasePath ++ "/", "");
        {error, _Reason} ->
            Path
    end.

%%====================================================================
%% Internal functions
%%====================================================================
clause_arity([{clause, _Line, Patterns, _Guards, _Body}|_T]) ->
    length(Patterns).

apply_f(F, Key, Value, _Options) when is_function(F, 2) ->
    F(Key, Value);
apply_f(F, Key, Value, Options) when is_function(F, 3) ->
    F(Key, Value, Options).
