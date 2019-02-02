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
    Opts = #{traverse => pre, formatter => ?MODULE, parse_transform => true},
    File = astranaut:file(Forms),
    astranaut_traverse:map(
               fun(Node, Attr) -> 
                       walk(Node, Attr, File) 
               end, Forms, Opts).

format_error({invalid_unquote_splicing, Binding, Var}) ->
    io_lib:format("expected unquote, not unquote_splicing ~s in ~s",
                  [astranaut:safe_to_string(Binding), astranaut:safe_to_string(Var)]);
format_error({non_empty_tail, Rest}) ->
    io_lib:format("non empty expression '~s' after unquote_splicing in pattern",
                  [astranaut:safe_to_string(Rest)]);
format_error({invalid_quote, Node}) ->
    io_lib:format("invalid quote ~s", [astranaut:safe_to_string(Node)]);
format_error(Message) ->
    astranaut_traverse:format_error(Message).

quote(Value) ->
    quote(Value, #{}).

quote(Node, Options) ->
    Options1 = maps:merge(#{quote_line => 0, quote_type => expression}, Options),
    NodeM = quote_0(Node, Options1),
    astranaut_traverse:monad_to_traverse_fun_return(NodeM).

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
    Forms = astranaut_code:quote_codes([Code]),
    quote(Forms, #{}, Node, Attr, File);
walk(Node, _Attr, _File) ->
    Node.

quote(Value, Options, Node, Attr, File) ->
    QuoteLine = erl_syntax:get_pos(Node),
    QuoteType = quote_type(Attr),
    Options1 = maps:merge(#{quote_line => QuoteLine, quote_type => QuoteType, file => File}, Options),
    quote(Value, Options1).

quote_0(Value, #{debug := true, quote_line := QuoteLine, file := File} = Options) ->
    astranaut_traverse_monad:lift_m(
      fun(QuotedAst) ->
              QuotedCode = astranaut:safe_to_string(QuotedAst),
              RelaPath = astranaut:relative_path(File),
              io:format("~ts:~p~n~s~n", [RelaPath, QuoteLine, QuotedCode]),
              QuotedAst
      end, quote_0(Value, Options#{debug => false}));
quote_0(Value, #{line := Line, quote_line := QuoteLine} = Options) ->
    Options1 = maps:remove(line, Options),
    astranaut_traverse_monad:lift_m(
      fun(Quoted) ->
              call_remote(astranaut, replace_line_zero, [Quoted, Line], QuoteLine)
      end, quote_0(Value, Options1));
quote_0(Value, #{warnings := Warnings} = Options) ->
    Options1 = maps:remove(warnings, Options),
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:warnings(Warnings),
      quote_0(Value, Options1));
quote_0(Value, #{} = Options) ->
    quote_1(Value, Options).

quote_1({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, Opts) ->
    unquote(Unquote, Opts#{type => value});
quote_1({cons, _Line1, {call, _Line2, {atom, _Line3, unquote_splicing}, [Unquotes]}, T}, Opts) ->
    %quote([a, b, unquote_splicing(V), c, d]),
    unquote_splicing(Unquotes, T, Opts#{join => cons});
quote_1([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}|T], Opts) ->
    %quote({a, b, unquote_splicing(V), c, d}),
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_1({match, _, {atom, _, unquote}, Unquote}, Opts) ->
    %% quote = Unquote in pattern
    unquote(Unquote, Opts#{type => value});
quote_1([{match, _, {atom, _, unquote_splicing}, Unquotes}|T], Opts) ->
    %% unquote_splicing = Unquotes in pattern
    unquote_splicing(Unquotes, T, Opts#{join => list});
quote_1({match, _Line1, Pattern, Value}, #{quote_line := Line, quote_type := pattern} = Opts) ->
    % _A@World = World2 => {atom, _, World} = World2
    astranaut_traverse_monad:bind(
      quote_1(Pattern, Opts),
      fun(Pattern1) ->
              astranaut_traverse_monad:return({match, Line, Pattern1, Value})
      end);
quote_1({cons, _Line1, {var, Line, VarName}, T} = Tuple, Opts) when is_atom(VarName) ->
    %% [A, _L@Unquotes, B] expression.
    case parse_binding(VarName, Line) of
        {value_list, Unquotes} ->
            unquote_splicing(Unquotes, T, Opts#{join => cons});
        default ->
            quote_tuple(Tuple, Opts)
    end;
quote_1([{var, Line, VarName}|T] = List, Opts) when is_atom(VarName) ->
    %% any L@Unquotes in list in absformat, like
    %% {A, _L@Unquotes, B} expression.
    %% fun(A, _L@Unquotes, B) -> _L@Unquotes end.
    case parse_binding(VarName, Line) of
        {value_list, Unquotes} ->
            unquote_splicing(Unquotes, T, Opts#{quote_line => Line, join => list});
        _ ->
            quote_list(List, Opts)
    end;
quote_1({var, Line, VarName} = Var, Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {value_list, Unquotes} ->
            astranaut_traverse_monad:then(
              astranaut_traverse_monad:warning({Line, ?MODULE, {invalid_unquote_splicing, Unquotes, Var}}),
              quote_tuple(Var, Opts));
        {BindingType, Unquote} ->
            unquote(Unquote, Opts#{type => BindingType, quote_line => Line});
        default ->
            quote_tuple(Var, Opts)
    end;
quote_1({LiteralType, Line, Literal}, Opts) 
  when LiteralType == atom ; 
       LiteralType == integer ; 
       LiteralType == char ; 
       LiteralType == float ; 
       LiteralType == string ->
    astranaut_traverse_monad:return(quote_literal(LiteralType, Line, Literal, Opts#{quote_line => Line}));
quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Opts);
quote_1(List, Opts) when is_list(List) ->
    quote_list(List, Opts);
quote_1(Atom, Opts) when is_atom(Atom) ->
    astranaut_traverse_monad:return(quote_atom(Atom, Opts));
quote_1(Integer, Opts) when is_integer(Integer) ->
    astranaut_traverse_monad:return(quote_integer(Integer, Opts)).

quote_list([H|T], #{quote_line := Line} = Opts) ->
    astranaut_traverse_monad:bind(
      quote_1(H, Opts),
      fun(H1) ->
              astranaut_traverse_monad:bind(
                quote_1(T, Opts),
                fun(T1) ->
                        astranaut_traverse_monad:return({cons, Line, H1, T1})
                end)
      end);
quote_list([], #{quote_line := Line}) ->
    astranaut_traverse_monad:return({nil, Line}).

quote_tuple(Tuple, Opts) ->
    TupleList = tuple_to_list(Tuple),
    TupleLine = get_tuple_line(TupleList, Opts),
    astranaut_traverse_monad:bind(
      quote_tuple_list(TupleList, Opts),
      fun(TupleList1) ->
              astranaut_traverse_monad:return({tuple, TupleLine, TupleList1})
      end).

get_tuple_line(_, #{quote_line := Line, attribute := attr}) ->
    %% if tuple is the attribute value, use the original line.
    Line;
get_tuple_line([_Action, TupleLine|_], #{}) when is_integer(TupleLine) ->
    %% use the tuple line.
    TupleLine;
get_tuple_line(_, #{quote_line := Line}) ->
    Line.

quote_tuple_list([MA, Spec], #{attribute := spec} = Opts) ->
    %% special form of {attribute, Line, spec, {{F, A}, Spec}}.
    %% there is no line in {F, A}.
    NOpts = maps:remove(attribute, Opts),
    astranaut_traverse_monad:bind(
      quote_1(MA, Opts#{attribute => attr}),
      fun(MA1) ->
              astranaut_traverse_monad:bind(
                quote_1(Spec, NOpts),
                fun(Spec1) ->
                        astranaut_traverse_monad:return([MA1, Spec1])
                end)
      end);
quote_tuple_list(TupleList, #{attribute := attr} = Opts) ->
    %% special form of {attribute, Line, export, [{F, A}...]}.
    %% special form of {attribute, Line, Attribute, T}.
    %% there is no line in {F, A} and T.
    quote_tuple_list_1(TupleList, Opts);
quote_tuple_list([Action, TupleLine|Rest] = TupleList, Opts) when is_atom(Action), is_integer(TupleLine) ->
    NOpts = update_attribute_opt(TupleList, Opts),
    astranaut_traverse_monad:bind(
      quote_tuple_list_1(Rest, NOpts),
      fun(Rest1) ->
              Action1 = quote_atom(Action, Opts),
              Line1 = quote_line(Opts#{quote_line => TupleLine}),
              astranaut_traverse_monad:return([Action1, Line1|Rest1])
      end);
quote_tuple_list(List, Opts) ->
    quote_tuple_list_1(List, Opts).

quote_tuple_list_1(List, Opts) ->
    astranaut_traverse_monad:map_m(
      fun(Item) ->
              quote_1(Item, Opts)
      end, List).

quote_literal(LiteralType, Line, LiteralValue, Opts) ->
    {tuple, Line, [quote_atom(LiteralType, Opts),
                   quote_line(Opts),
                   quote_literal_value(LiteralValue, Opts#{literal_type => LiteralType})]}.

%% quote_literal_value not return monad
quote_literal_value(Integer, #{literal_type := integer} = Opts) when is_integer(Integer) ->
    quote_integer(Integer, Opts);

quote_literal_value(Atom, #{literal_type := atom} = Opts) when is_atom(Atom) ->
    quote_atom(Atom, Opts);
quote_literal_value(Char, #{quote_line := Line, literal_type := char}) when is_integer(Char) ->
    {char, Line, Char};
quote_literal_value(Float, #{quote_line := Line}) when is_float(Float) ->
    {float, Line, Float};
quote_literal_value(String, #{quote_line := Line, literal_type := string}) when is_list(String) ->
    {string, Line, String}.

quote_integer(Integer, #{quote_line := Line}) when is_integer(Integer) ->
    {integer, Line, Integer}.

quote_atom(Atom, #{quote_line := Line}) when is_atom(Atom) ->
    {atom, Line, Atom}.

%% quote_line not return monad
quote_line(#{quote_line := Line, quote_type := pattern}) ->
    {var, Line, '_'};
quote_line(#{quote_line := Line, quote_type := expression, code_line := true} = Opts) ->
    quote_integer(Line, Opts);
quote_line(#{quote_type := expression} = Opts) ->
    quote_integer(0, Opts).

%% unquote return monad
unquote(Exp, #{type := value}) ->
    astranaut_traverse_monad:return(Exp);
unquote(Exp, #{type := Type, quote_line := Line} = Opts) ->
    astranaut_traverse_monad:return({tuple, Line, [quote_atom(Type, Opts), quote_line(Opts), Exp]}).

%% unquote_splicing return monad.
unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := list} = Opts) ->
    astranaut_traverse_monad:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse_monad:return(
                call_remote(?MODULE, uncons, [Unquotes, Rest1], Line))
      end);
unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := cons} = Opts) ->
    astranaut_traverse_monad:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse_monad:return(
                call_remote(?MODULE, cons, [Unquotes, Rest1], Line))
      end);
unquote_splicing(Unquotes, [], #{quote_type := pattern, join := list}) ->
    astranaut_traverse_monad:return(Unquotes);
unquote_splicing(Unquotes, {nil, _}, #{quote_type := pattern, join := cons}) ->
    astranaut_traverse_monad:return(Unquotes);
unquote_splicing(Unquotes, Rest, #{quote_type := pattern, quote_line := Line}) ->
    Warning = {non_empty_tail, Rest},
    astranaut_traverse_monad:then(
      astranaut_traverse_monad:warning({Line, ?MODULE, Warning}),
      astranaut_traverse_monad:return(Unquotes)).

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
            {Options, Warnings} = astranaut:ast_to_options(Ast, [line]),
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
