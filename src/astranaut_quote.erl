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
-export([uncons/2, cons/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    dbg:tracer(),
    dbg:tpl(compile, format_message, cx),
    dbg:p(all, [c]),
    Opts = #{traverse => pre, formatter => ?MODULE, parse_transform => true},
    File = astranaut:file(Forms),
    astranaut_traverse:map(fun(Node, Attr) -> walk(Node, Attr, File) end, Forms, Opts).

format_error({invalid_quote, Node}) ->
    io_lib:format("invalid quote ~s", [astranaut:to_string(Node)]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

quote(Value) ->
    quote(Value, #{}).

quote(Value, Options) ->
    Options1 = maps:merge(#{quote_line => 0, quote_type => expression}, Options),
    quote_0(Value, Options1).

uncons(Cons, []) ->
    uncons_1(Cons);
uncons(Cons, Rest) ->
    uncons_1(Cons) ++ Rest.

uncons_1({cons, _Line, Head, Tail}) ->
    [Head|uncons_1(Tail)];
uncons_1({nil, _Line}) ->
    [];
uncons_1(Value) ->
    Value.

cons([H|T], Rest) ->
    {cons, 0, H, cons(T, Rest)};
cons([], Rest) ->
    Rest;
cons({cons, Line, Head, Tail}, Rest) ->
    {cons, Line, Head, cons(Tail, Rest)};
cons({nil, _Line}, Rest) ->
    Rest.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk({call, _Line1, {atom, _Line2, quote}, [Form]} = Node, Attr, File) ->
    %% transform quote(Form)
    quote(Form, #{}, Node, Attr, File);
walk({call, _Line1, {atom, _Line2, quote}, [Form, Options]} = Node, Attr, File) ->
    Options1 = to_options(Options),
    quote(Form, Options1, Node, Attr, File);
walk({call, _Line1, {atom, _Line2, quote_code}, Codes} = Node, #{node := NodeType} = Attr, File) ->
    %% transform quote_code(Codes)
    case split_codes(Codes, NodeType) of
        {ok, NCodes, Options} ->
            Options1 = to_options(Options),
            Form = astranaut_code:quote_codes(NCodes),
            quote(Form, Options1, Node, Attr, File);
        {error, invalid_quote_code} ->
            astranaut_traverse:traverse_fun_return(#{error => {invalid_quote, Node}})
    end;
walk({match, _Line1, {atom, _Line2, quote}, Form} = Node, #{node := pattern} = Attr, File) ->
    %% transform quote = Form in pattern match
    quote(Form, #{}, Node, Attr, File);
walk({match, _Line1, {atom, _Line2, quote_code}, Code} = Node, #{node := pattern} = Attr, File) ->
    %% transform quote_code = Code in pattern match
    Form = astranaut_code:quote_codes([Code]),
    quote(Form, #{}, Node, Attr, File);
walk(Node, _Attr, _File) ->
    Node.

quote(Value, Options, Node, Attr, File) ->
    QuoteLine = erl_syntax:get_pos(Node),
    QuoteType = quote_type(Attr),
    Options1 = maps:merge(#{quote_line => QuoteLine, quote_type => QuoteType, file => File}, Options),
    quote(Value, Options1).

quote_0(Value, #{debug := true, quote_line := QuoteLine, file := File} = Options) ->
    astranaut_traverse:map_traverse_fun_return(
      fun(QuotedAst) ->
              QuotedCode = astranaut:to_string(QuotedAst),
              RelaPath = astranaut:relative_path(File),
              io:format("~ts:~p~n~s~n", [RelaPath, QuoteLine, QuotedCode]),
              QuotedAst
      end, quote_0(Value, Options#{debug => false}));
quote_0(Value, #{line := Line, quote_line := QuoteLine} = Options) ->
    Options1 = maps:remove(line, Options),
    astranaut_traverse:map_traverse_fun_return(
      fun(Quoted) ->
              call_remote(astranaut, replace_line_zero, [Quoted, Line], QuoteLine)
      end, quote_0(Value, Options1));
quote_0(Value, #{warnings := Warnings} = Options) ->
    Options1 = maps:remove(warnings, Options),
    Quoted = quote_0(Value, Options1),
    case astranaut_traverse:traverse_fun_return_struct(Quoted) of
        #{warnings := Warnings1} = Quoted1 ->
            Quoted1#{warnings => Warnings ++ Warnings1};
        #{} = Quoted1 ->
            Quoted1#{warnings => Warnings}
    end;
quote_0(Value, #{} = Options) ->
    quote_1(Value, Options).

quote_1({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, Opts) ->
    unquote(Unquote, Opts#{type => value});
quote_1({cons, Line, {call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}, Rest}, Opts) ->
    %quote([a, b, unquote_splicing(V), c, d]),
    RestQuote = quote_1(Rest, Opts),
    call_remote(?MODULE, cons, [Unquotes, RestQuote], Line);
quote_1([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}|T], Opts) ->
    %quote({a, b, unquote_splicing(V), c, d}),
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_1({match, _, {atom, _, unquote}, Unquote}, Opts) ->
    unquote(Unquote, Opts#{type => value});
quote_1([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_1({match, _Line1, Pattern, Value}, #{quote_line := Line, quote_type := pattern} = Opts) ->
    % _A@World = World2 => {atom, _, World} = World2
    {match, Line, quote_1(Pattern, Opts#{quote_type => pattern}), Value};
quote_1({cons, _Line1, {var, Line, VarName}, T} = Tuple, Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {value_list, Binding} ->
            RestQuote = quote_1(T, Opts),
            call_remote(?MODULE, cons, [Binding, RestQuote], Line);
        default ->
            quote_tuple(Tuple, Opts)
    end;
quote_1([{var, Line, VarName} = Var|T], Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {value_list, Binding} ->
            unquote_splicing(Binding, T, Opts#{quote_line => Line, join => list});
        _ ->
            quote_list([Var|T], Opts)
    end;
quote_1({var, Line, VarName} = Var, Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {BindingType, Binding} ->
            unquote(Binding, Opts#{type => BindingType, quote_line => Line});
        default ->
            quote_tuple(Var, Opts)
    end;
quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Opts);
quote_1(List, Opts) when is_list(List) ->
    quote_list(List, Opts);
quote_1(Float, #{quote_line := Line}) when is_float(Float) ->
    {float, Line, Float};
quote_1(Integer, #{quote_line := Line}) when is_integer(Integer) ->
    {integer, Line, Integer};
quote_1(Atom, #{quote_line := Line}) when is_atom(Atom) ->
    {atom, Line, Atom}.

quote_list([H|T], #{quote_line := Line} = Opts) ->
    {cons, Line, quote_1(H, Opts), quote_1(T, Opts)};
quote_list([], #{quote_line := Line}) ->
    {nil, Line}.

quote_tuple(Tuple, #{quote_line := Line} = Opts) ->
    TupleList = tuple_to_list(Tuple),
    case TupleList of
        [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
            {tuple, TupleLine, quote_tuple_list(TupleList, Opts)};
        _ ->
            {tuple, Line, quote_tuple_list_1(TupleList, Opts)}
    end.

quote_tuple_list([MA, Spec], #{attribute := spec} = Opts) ->
    %% special form of {attribute, Line, spec, {{F, A}, Spec}}.
    %% there is no line in {F, A}.
    NOpts = maps:remove(attribute, Opts),
    [quote_1(MA, Opts#{attribute => attr}), quote_1(Spec, NOpts)];
quote_tuple_list(TupleList, #{attribute := attr} = Opts) ->
    %% special form of {attribute, Line, export, [{F, A}...]}.
    %% special form of {attribute, Line, Attribute, T}.
    %% there is no line in {F, A} and T.
    quote_tuple_list_1(TupleList, Opts);
quote_tuple_list([Action, TupleLine|Rest] = TupleList, Opts) ->
    NOpts = update_attribute_opt(TupleList, Opts),
    [quote_1(Action, Opts), quote_line(Opts#{quote_line => TupleLine})|quote_tuple_list_1(Rest, NOpts)].

quote_tuple_list_1(List, Opts) ->
    lists:map(fun(Item) -> quote_1(Item, Opts) end, List).

quote_line(#{quote_line := Line, quote_type := pattern}) ->
    {var, Line, '_'};
quote_line(#{quote_line := Line, quote_type := expression, code_line := true} = Opts) ->
    quote_1(Line, Opts);
quote_line(#{quote_type := expression} = Opts) ->
    quote_1(0, Opts).

update_attribute_opt([attribute, _Line, spec|_T], Opts) ->
    Opts#{attribute => spec};
update_attribute_opt([attribute, _Line, type|_T], Opts) ->
    Opts;
update_attribute_opt([attribute|_T], Opts) ->
    Opts#{attribute => attr};
update_attribute_opt(_, Opts) ->
    Opts.

quote_type(#{node := pattern}) ->
    pattern;
quote_type(_) ->
    expression.

to_options(Ast) ->
    Type = erl_syntax:type(Ast),
    case lists:member(Type, [list, nil, map_expr, atom]) of
        true ->
            Line = erl_syntax:get_pos(Ast),
            {Options, Warnings} = astranaut:ast_to_options(Ast),
            validate_options(Options, Line, Warnings);
        false ->
            #{line => Ast}
    end.

validate_options(Line, _QuoteLine, Warnings) when is_integer(Line) ->
    {#{code_line => Line}, Warnings};
validate_options(Options, Line, Warnings) ->
    {Options1, Warnings1} = 
        astranaut:validate_options(
          fun validate_quote_option/2,
          Options),
    Warnings2 = 
        lists:map(
          fun(Warning) ->
                  {Line, astranaut_quote, Warning}
          end, Warnings1),
    Options1#{warnings => Warnings ++ Warnings2}.

validate_quote_option(debug, Boolean) when is_boolean(Boolean) ->
    ok;
validate_quote_option(code_line, Boolean) when is_boolean(Boolean) ->
    ok;
validate_quote_option(line, _) ->
    ok;
validate_quote_option(_Key, _Value) ->
    error.

%% check ast in quote_code valid.
split_codes(Codes, NodeType) ->
    case lists:partition(
           fun({string, _, _}) ->
                   true;
              (_) ->
                   false
           end, Codes) of
        {NCodes, [Line]} ->
            case NodeType of
                expression ->
                    {ok, NCodes, Line};
                _ ->
                    {error, invalid_quote_code}
            end;
        {NCodes, []} ->
            {ok, NCodes, {nil, 0}};
        {_NCodes, _NonCodes} ->
            {error, invalid_quote_code}
    end.

call_remote(Module, Function, Arguments, Line) ->
    {call, Line, {remote, Line, {atom, Line, Module}, {atom, Line, Function}}, Arguments}.

unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := list} = Opts) ->
    call_remote(?MODULE, uncons, [Unquotes, quote_1(Rest, Opts)], Line);
unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := cons} = Opts) ->
    call_remote(?MODULE, cons, [Unquotes, quote_1(Rest, Opts)], Line);
unquote_splicing(Unquotes, _Rest, #{quote_type := pattern, join := list}) ->
    Unquotes.

unquote(Exp, #{type := value}) ->
    Exp;
unquote(Exp, #{type := Type, quote_line := Line} = Opts) ->
    {tuple, Line, [quote_1(Type, Opts), quote_line(Opts), Exp]}.

parse_binding(Atom, Line) ->
    case parse_binding_1(atom_to_list(Atom)) of
        {VarType, VarName} ->
            {VarType, {var, Line, list_to_atom(VarName)}};
        default ->
            default
    end.

parse_binding_1([$_,$A,$@|T]) ->
    {atom, T};
parse_binding_1([$_,$V,$@|T]) ->
    {var, T};
parse_binding_1([$_,$I,$@|T]) ->
    {integer, T};
parse_binding_1([$_,$F,$@|T]) ->
    {float, T};
parse_binding_1([$_,$S,$@|T]) ->
    {string, T};
parse_binding_1([$_,$L,$@|T]) ->
    {value_list, T};
parse_binding_1([$_,$@|T]) ->
    {value, T};
parse_binding_1(_) ->
    default.
