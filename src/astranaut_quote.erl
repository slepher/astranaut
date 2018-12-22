%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_quote).

%% API
-export([parse_transform/2, format_error/1]).
-export([quote/1, quote/2]).
-export([uncons/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    astranaut_traverse:map(fun walk/1, Forms, pre).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.


quote(Value) ->
    quote(Value, 0).

quote(Value, Line) ->
    quote(Value, Line, expr).

quote({unquote, Unquote}, _Line, _Type) ->
    Unquote;
quote({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, _Line, _Type) ->
    Unquote;
quote([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}|T], Line, Type) ->
    unquote_splicing_form(Unquotes, T, Line, Type);
quote({match, _Line1, Pattern, Value}, Line, pattern) ->
    {match, Line, quote(Pattern, Line, pattern), Value};
quote([{var, __Line1, Var} = VarForm|T], Line, Type) when is_atom(Var) ->
    metavariable_list(VarForm, T, Line, Type);
quote([{atom, _Line1, Atom} = VarForm|T], Line, Type) when is_atom(Atom) ->
    metavariable_list(VarForm, T, Line, Type);
quote({var, _Line1, Var} = VarForm, Line, Type) when is_atom(Var) ->
    metavariable(VarForm, Line, Type);
quote({atom, _Line1, Atom} = VarForm, Line, Type)  when is_atom(Atom) ->
    metavariable(VarForm, Line, Type);
quote({match, _, {atom, _, unquote}, Unquote}, _Line, _Type) ->
    Unquote;
quote([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Line, Type) ->
    unquote_splicing_form(Unquotes, T, Line, Type);
quote(Tuple, Line, Type) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Line, Type);
quote([H|T], Line, Type) ->
    {cons, Line, quote(H, Line, Type), quote(T, Line, Type)};
quote([], Line, _Type) ->
    {nil, Line};
quote(Float, Line, _Type) when is_float(Float) ->
    {float, Line, Float};
quote(Integer, Line, _Type) when is_integer(Integer) ->
    {integer, Line, Integer};
quote(Atom, Line, _Type) when is_atom(Atom) ->
    {atom, Line, Atom}.

quote_tuple(Tuple, Line, expr) when is_integer(Line) ->
    TupleList = tuple_to_list(Tuple),
    QuotedLine = 
        case TupleList of
            [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                TupleLine;
            _ ->
                Line
        end,
    {tuple, QuotedLine, lists:map(fun(Item) -> quote(Item, QuotedLine) end, TupleList)};
quote_tuple(Tuple, Line, pattern) ->
    TupleList = tuple_to_list(Tuple),
    case TupleList of
        [Action, TupleLine|Rest] when is_integer(TupleLine) ->
            {tuple, TupleLine, [quote(Action, TupleLine, expr), {var, TupleLine, '_'}|
                                lists:map(fun(Item) -> quote(Item, TupleLine, pattern) end, Rest)]};
        _ ->
            {tuple, Line, lists:map(fun(Item) -> quote(Item, Line, pattern) end, TupleList)}
    end.

call_remote(Module, Function, Arguments, Line) ->
    {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Arguments}.

uncons({cons, _Line, Head, Tail}) ->
    [Head|uncons(Tail)];
uncons({nil, _Line}) ->
    [];
uncons(Value) ->
    Value.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk({clause, Line1, Clauses, Guards, Exprs}) ->
    NClauses = 
        lists:map(
          fun({call, Line2, {atom, _Line3, quote}, [Form]}) ->
                  quote(Form, Line2, pattern);
             ({match, Line2, {call, _Line3, {atom, _Line4, quote}, [Form]}, Val}) ->
                  Quoted = quote(Form, Line2, pattern),
                  {match, Line2, Quoted, Val};
             ({match, Line2, {atom, _Line3, quote}, Form}) ->
                  quote(Form, Line2, pattern);
             ({match, Line2, {call, _Line3, {atom, _Line4, quote_code}, Codes}, Val}) ->
                  Forms = astranaut_code:quote_codes(Codes),
                  Quoted = quote(Form, Line2, pattern),
                  {match, Line2, Quoted, Val};
             ({match, Line2, {atom, _Line3, quote_code}, Form}) ->
                  Forms = astranaut_code:quote_codes(Codes),
                  quote(Form, Line2, pattern);
             (Clause) ->
                  Clause
          end, Clauses),
    {clause, Line1, NClauses, Guards, Exprs};
walk({match, Line1, {call, Line2, {atom, _Line3, quote}, [Form]}, Val}) ->
    Quoted = quote(Form, Line2, pattern),
    {match, Line1, Quoted, Val};
walk({call, _Line1, {atom, Line2, quote}, [Form]}) ->
    quote(Form, Line2);
walk({call, Line1, {atom, _Line2, quote}, [Form, Line]}) ->
    call_remote(astranaut, replace_line, [quote(Form), Line], Line1);
walk({call, _Line1, {atom, _Line2, quote_code}, Codes}) ->
    Forms = astranaut_code:quote_codes(Codes),
    quote(Forms);
walk(Node) ->
    Node.

unquote_splicing_form(Unquotes, Rest, Line, expr) ->
    {op, Line, '++', call_remote(?MODULE, uncons, [Unquotes], Line), quote(Rest, Line)};
unquote_splicing_form(Unquotes, _Rest, _Line, pattern) ->
    Unquotes.

metavariable_list({FType, _, Atom} = Form, T, Line, Type) ->
    case parse_metavariable(Atom, FType, Line) of
        {value_list, VarList} ->
            unquote_splicing_form(VarList, T, Line, Type);
        _ ->
            {cons, Line, metavariable(Form, Line, Type), quote(T, Line, Type)}
    end.

metavariable({FType, _, Atom} = Form, Line, Type) ->
    case parse_metavariable(Atom, FType, Line) of
        {atom, Var} ->
            {tuple, Line, [quote(atom, Line), line_variable(Line, Line, Type), Var]};
        {var, Var} ->
            {tuple, Line, [quote(var, Line), line_variable(Line, Line, Type), Var]};
        {value, Var} ->
            Var;
        default ->
            quote_tuple(Form, Line, Type)
    end.

line_variable(Line0, Line, expr) ->
    {integer, Line, Line0};
line_variable(_Line0, Line, pattern) ->
    {var, Line, '_'}.

parse_metavariable(Atom, var, Line) ->
    case atom_to_list(Atom) of
        [$_|T] ->
            parse_metavariable(T, Line);
        _ ->
            default
    end;
parse_metavariable(Atom, atom, Line) ->
    parse_metavariable(atom_to_list(Atom), Line).

parse_metavariable([$A,$@|T], Line) ->
    {atom, {var, Line, list_to_atom(T)}};
parse_metavariable([$V,$@|T], Line) ->
    {var, {var, Line, list_to_atom(T)}};
parse_metavariable([$I,$@|T], Line) ->
    {integer, {var, Line, list_to_atom(T)}};
parse_metavariable([$F,$@|T], Line) ->
    {float, {var, Line, list_to_atom(T)}};
parse_metavariable([$L,$@|T], Line) ->
    {value_list, {var, Line, list_to_atom(T)}};
parse_metavariable([$@|T], Line) ->
    {value, {var, Line, list_to_atom(T)}};
parse_metavariable(_, _Line) ->
    default.
