-module(astranaut).

%% API exports
-export([attributes/2, attributes_with_line/2, module_attributes/2, read/1]).
-export([file/1]).
-export([exports/1, exports/2, exported_function/2, function/2, function_fa/1, merge_clauses/1]).
-export([replace_line/2, replace_line_zero/2, to_string/1]).
-export([reorder_exports/1]).
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

replace_line_zero(Ast, Line) ->
    replace_line_cond(
      fun(0) -> true;
         (_) -> false
      end, Ast, Line).

replace_line_cond(Cond, Ast, Line) ->
    astranaut_traverse:map(
      fun(Tuple, _Attr) when is_tuple(Tuple) ->
              case tuple_to_list(Tuple) of
                  [_Action, Line|_Rest] when is_integer(Line) ->
                      case Cond(Line) of
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
    Length = length(Forms),
    {Exports, NForms, _, Offset} = 
        lists:foldl(
          fun({attribute, _Line, export, _ExportAttrs} = Export, {ExportsAcc, FormsAcc, Offset, _}) ->
                  {[Export|ExportsAcc], FormsAcc, Offset - 1, Offset - 1};
             ({attribute, _Line, module, _} = Module, {ExportsAcc, FormsAcc, Offset, 0}) ->
                  {ExportsAcc, [Module|FormsAcc], Offset - 1, Offset - 1};
             (Node, {ExportsAcc, FormsAcc, Offset, Location}) ->
                  {ExportsAcc, [Node|FormsAcc], Offset - 1, Location}
          end, {[], [], Length, 0}, lists:reverse(Forms)),
    {Head, Tail} = lists:split(Offset, NForms),
    Head ++ Exports ++ Tail.

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

%%====================================================================
%% Internal functions
%%====================================================================
clause_arity([{clause, _Line, Patterns, _Guards, _Body}|_T]) ->
    length(Patterns).
