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
-export([uncons/2, cons/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    erlang:system_flag(backtrace_depth, 40),
    File = astranaut_lib:analyze_forms_file(Forms),
    Module = astranaut_lib:analyze_forms_module(Forms),
    astranaut_return:to_compiler(
      astranaut_return:bind(
        astranaut_lib:validate_attribute_option(quote_validator(), ?MODULE, quote_options, Forms),
        fun(#{debug := Debug, debug_module := DebugModule}) ->
                WalkOpts = #{file => File, module => Module, debug => Debug},
                astranaut_return:lift_m(
                  fun(Forms1) ->
                          debug_module(Forms1, DebugModule),
                          Forms1
                  end,
                  astranaut_traverse:eval(
                    astranaut:map_m(
                      fun(Node) ->
                              astranaut_traverse:bind(
                                astranaut_traverse:ask(),
                                fun(Attr) ->
                                        walk(Node, Attr, WalkOpts)
                                end)
                      end, Forms, #{traverse => pre}),
                    ?MODULE, #{}, ok))
        end)).

quote_validator() ->
    #{debug => [boolean, {default, false}], debug_module => [boolean, {default, false}]}.

format_error({invalid_unquote_splicing, Binding, Var}) ->
    io_lib:format("expected unquote, not unquote_splicing ~s in ~s",
                  [astranaut_lib:ast_safe_to_string(Binding), astranaut_lib:ast_safe_to_string(Var)]);
format_error({non_empty_tail, Rest}) ->
    io_lib:format("non empty expression '~s' after unquote_splicing in pattern", [astranaut_lib:ast_safe_to_string(Rest)]);
format_error({invalid_quote, Node}) ->
    io_lib:format("invalid quote ~s", [astranaut_lib:ast_safe_to_string(Node)]);
format_error(Message) ->
    astranaut:format_error(Message).

debug_module(Forms1, true) ->
    io:format("~s~n", [astranaut_lib:ast_to_string(Forms1)]);
debug_module(_Forms1, _) ->
    ok.

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
walk({call, _Line1, {atom, _Line2, quote}, [Form]} = Node, Attr, WalkOpts) ->
    %% transform quote(Form)
    quote(Form, #{}, Node, Attr, WalkOpts);
walk({call, _Line1, {atom, _Line2, quote}, [Form, Options]} = Node, Attr, WalkOpts) ->
    astranaut_traverse:bind(
      astranaut_traverse:astranaut_traverse(to_options(Options)),
      fun(Options1) ->
              quote(Form, Options1, Node, Attr, WalkOpts)
      end);
walk({call, Line1, {atom, _Line2, quote_code}, Codes} = Node, #{node := NodeType} = Attr, WalkOpts) ->
    %% transform quote_code(Codes)
    case split_codes(Codes, NodeType) of
        {ok, NCodes, Options} ->
            Options1 = to_options(Options),
            Form = merl:quote(Line1, NCodes),
            quote(Form, Options1, Node, Attr, WalkOpts);
        {error, invalid_quote_code} ->
            {error, {invalid_quote, Node}}
    end;
walk({match, _Line1, {atom, _Line2, quote}, Form} = Node, #{node := pattern} = Attr, WalkOpts) ->
    %% transform quote = Form in pattern match
    quote(Form, #{}, Node, Attr, WalkOpts);
walk({match, Line1, {atom, _Line2, quote_code}, {string, _, Code}} = Node, #{node := pattern} = Attr, WalkOpts) ->
    %% transform quote_code = Code in pattern match
    Forms = merl:quote(Line1, Code),
    quote(Forms, #{}, Node, Attr, WalkOpts);
walk(Node, Attr, _File) ->
    Type = erl_syntax:type(Node),
    astranaut_traverse:return(
      astranaut_uniplate:with_subtrees(
        fun(Subtrees) ->
                astranaut_syntax:subtrees_pge(Type, Subtrees, Attr)
        end, keep)).

quote(Value, Options, Node, Attr, #{file := File, module := Module, debug := Debug}) ->
    QuoteLine = erl_syntax:get_pos(Node),
    QuoteType = quote_type(Attr),
    Options1 = maps:merge(#{quote_line => QuoteLine, quote_type => QuoteType, 
                            file => File, module => Module, debug => Debug}, Options),
    astranaut_traverse:set_updated(quote_0(Value, Options1)).

quote_0(Value, #{debug := true} = Options) ->
    astranaut_traverse:lift_m(
      fun(QuotedAst) ->
              format_quoted_ast(QuotedAst, Options),
              QuotedAst
      end, quote_0(Value, Options#{debug => false}));
quote_0(Value, #{line := Line, quote_line := QuoteLine} = Options) ->
    Options1 = maps:remove(line, Options),
    astranaut_traverse:lift_m(
      fun(Quoted) ->
              call_remote(astranaut_lib, replace_line_zero, [Quoted, Line], QuoteLine)
      end, quote_0(Value, Options1));
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
    astranaut_traverse:bind(
      quote_1(Pattern, Opts),
      fun(Pattern1) ->
              astranaut_traverse:return({match, Line, Pattern1, Value})
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

quote_1({var, _Line, '_'} = Var, Opts) ->
    quote_tuple(Var, Opts);

quote_1({var, Line, VarName} = Var, #{module := Module} = Opts) when is_atom(VarName) ->
    case parse_binding(VarName, Line) of
        {value_list, Unquotes} ->
            astranaut_traverse:then(
                astranaut_traverse:update_pos(
                  Line, astranaut_traverse:warning({invalid_unquote_splicing, Unquotes, Var})),
              quote_tuple(Var, Opts));
        {BindingType, Unquote} ->
            unquote(Unquote, Opts#{type => BindingType, quote_line => Line});
        default ->
            VarName1 = list_to_atom(atom_to_list(VarName) ++ "@" ++ atom_to_list(Module)),
            Var1 = {var, Line, VarName1},
            quote_tuple(Var1, Opts)
    end;
quote_1({LiteralType, Line, Literal}, Opts) 
  when LiteralType == atom ; 
       LiteralType == integer ; 
       LiteralType == char ; 
       LiteralType == float ; 
       LiteralType == string ->
    astranaut_traverse:return(quote_literal(LiteralType, Line, Literal, Opts#{quote_line => Line}));
quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Opts);
quote_1(List, Opts) when is_list(List) ->
    quote_list(List, Opts);
quote_1(Atom, Opts) when is_atom(Atom) ->
    astranaut_traverse:return(quote_atom(Atom, Opts));
quote_1(Integer, Opts) when is_integer(Integer) ->
    astranaut_traverse:return(quote_integer(Integer, Opts)).

quote_list([H|T], #{quote_line := Line} = Opts) ->
    astranaut_traverse:bind(
      quote_1(H, Opts),
      fun(H1) ->
              astranaut_traverse:bind(
                quote_1(T, Opts),
                fun(T1) ->
                        astranaut_traverse:return({cons, Line, H1, T1})
                end)
      end);
quote_list([], #{quote_line := Line}) ->
    astranaut_traverse:return({nil, Line}).

quote_tuple(Tuple, Opts) ->
    TupleList = tuple_to_list(Tuple),
    TupleLine = get_tuple_line(TupleList, Opts),
    astranaut_traverse:bind(
      quote_tuple_list(TupleList, Opts),
      fun(TupleList1) ->
              astranaut_traverse:return({tuple, TupleLine, TupleList1})
      end).

get_tuple_line(_, #{quote_line := Line, attribute := attr}) ->
    %% if tuple is the attribute value, use the original line.
    Line;
get_tuple_line([Action, TupleLine|_], #{}) when is_integer(TupleLine), is_atom(Action) ->
    %% use the tuple line.
    TupleLine;
get_tuple_line([_Action, {TupleLine, TupleCol}|_], #{}) when is_integer(TupleLine), is_integer(TupleCol) ->
    %% use the tuple line.
    {TupleLine, TupleCol};
get_tuple_line(_, #{quote_line := Line}) ->
    Line.

quote_tuple_list([MA, Spec], #{attribute := spec} = Opts) ->
    %% special form of {attribute, Line, spec, {{F, A}, Spec}}.
    %% there is no line in {F, A}.
    NOpts = maps:remove(attribute, Opts),
    astranaut_traverse:bind(
      quote_1(MA, Opts#{attribute => attr}),
      fun(MA1) ->
              astranaut_traverse:bind(
                quote_1(Spec, NOpts),
                fun(Spec1) ->
                        astranaut_traverse:return([MA1, Spec1])
                end)
      end);
quote_tuple_list(TupleList, #{attribute := attr} = Opts) ->
    %% special form of {attribute, Line, export, [{F, A}...]}.
    %% special form of {attribute, Line, Attribute, T}.
    %% there is no line in {F, A} and T.
    quote_tuple_list_1(TupleList, Opts);
quote_tuple_list([Action, TupleLine|_Rest] = TupleList, Opts)
  when is_atom(Action), is_integer(TupleLine) ->
    quote_tuple_list_with_pos(TupleList, Opts);
quote_tuple_list([Action, {TupleLine, TupleCol}|_Rest] = TupleList, Opts) 
  when is_atom(Action), is_integer(TupleLine), is_integer(TupleCol) ->
    quote_tuple_list_with_pos(TupleList, Opts);

quote_tuple_list(List, Opts) ->
    quote_tuple_list_1(List, Opts).

quote_tuple_list_with_pos([Action, TupleLine|Rest] = TupleList, Opts) ->
    NOpts = update_attribute_opt(TupleList, Opts),
    astranaut_traverse:bind(
      quote_tuple_list_1(Rest, NOpts),
      fun(Rest1) ->
              Action1 = quote_atom(Action, Opts),
              Line1 = quote_line(Opts#{quote_line => TupleLine}),
              astranaut_traverse:return([Action1, Line1|Rest1])
      end).

quote_tuple_list_1(List, Opts) ->
    astranaut_traverse:map_m(
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
quote_line(#{quote_line := Line, quote_type := expression, code_line := true} = Opts) when is_integer(Line) ->
    quote_integer(Line, Opts);
quote_line(#{quote_line := Line, quote_type := expression, code_line := true} = Opts) when is_tuple(Line) ->
    quote_tuple(Line, Opts);
quote_line(#{quote_type := expression} = Opts) ->
    quote_integer(0, Opts).

%% unquote return monad
unquote(Exp, #{type := value}) ->
    astranaut_traverse:return(Exp);
unquote(Exp, #{type := Type, quote_line := Line} = Opts) ->
    astranaut_traverse:return({tuple, Line, [quote_atom(Type, Opts), quote_line(Opts), Exp]}).

%% unquote_splicing return monad.
unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := list} = Opts) ->
    astranaut_traverse:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse:return(
                call_remote(?MODULE, uncons, [Unquotes, Rest1], Line))
      end);
unquote_splicing(Unquotes, Rest, #{quote_line := Line, quote_type := expression, join := cons} = Opts) ->
    astranaut_traverse:bind(
      quote_1(Rest, Opts),
      fun(Rest1) ->
              astranaut_traverse:return(
                call_remote(?MODULE, cons, [Unquotes, Rest1], Line))
      end);
unquote_splicing(Unquotes, [], #{quote_type := pattern, join := list}) ->
    astranaut_traverse:return(Unquotes);
unquote_splicing(Unquotes, {nil, _}, #{quote_type := pattern, join := cons}) ->
    astranaut_traverse:return(Unquotes);
unquote_splicing(Unquotes, Rest, #{quote_type := pattern, quote_line := Line}) ->
    Warning = {non_empty_tail, Rest},
    astranaut_traverse:then(
      astranaut_traverse:update_pos(
        Line, astranaut_traverse:warning(Warning)),
      astranaut_traverse:return(Unquotes)).

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
            Line = astranaut_syntax:get_pos(Ast),
            Options = ast_to_options(Ast),
            validate_options(Options, Line);
        false ->
            astranaut_return:return(#{line => Ast})
    end.

validate_options(Line, _QuoteLine) when is_integer(Line) ->
    astranaut_return:return(#{code_line => Line});
validate_options(Options, Line) ->
    Return = astranaut_lib:validate(#{debug => boolean, code_line => boolean, line => any}, Options),
    astranaut_return:with_error(
        fun(ErrorStruct) ->
            astranaut_error:update_pos(Line, ?MODULE, ErrorStruct)
        end, Return).

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
                    NCodes1 = lists:map(fun({string, _, StringCode}) -> StringCode end, NCodes),
                    {ok, NCodes1, Line};
                _ ->
                    {error, invalid_quote_code}
            end;
        {NCodes, []} ->
            NCodes1 = lists:map(fun({string, _, StringCode}) -> StringCode end, NCodes),
            {ok, NCodes1, {nil, 0}};
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

ast_to_options(AstOptions) ->
    StaticTypes = [atom, char, float, integer, nil, string, list, map_expr, map_field_assoc, tuple],
    AstOptions1 =
        astranaut:smap(
          fun(Node, _Attr) ->
                  Type = erl_syntax:type(Node),
                  case lists:member(Type, StaticTypes) of
                      true ->
                          Node;
                      false ->
                          erl_uniplate:skip(astranaut_lib:abstract_form(Node))
                  end
          end, AstOptions, #{traverse => pre}),
    erl_syntax:concrete(AstOptions1).

format_quoted_ast(QuotedAst, #{quote_line := QuoteLine, file := File}) ->
    QuotedCode = astranaut_lib:ast_safe_to_string(QuotedAst),
    RelaPath = astranaut_lib:relative_path(File),
    io:format("~ts:~s~n~s~n", [RelaPath, format_quote_line(QuoteLine), QuotedCode]).

format_quote_line({Line, Col}) ->
    io_lib:format("~p,~p", [Line, Col]);
format_quote_line(Line) ->
    io_lib:format("~p", [Line]).
