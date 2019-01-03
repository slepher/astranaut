%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(astranaut_code).

%% API
-export([quote_codes/1]).

%%%===================================================================
%%% API
%%%===================================================================
quote_codes(Codes) ->
    Tokens = lists:flatten(lists:map(fun tokens/1, Codes)),
    case parse_1(Tokens) of
        [Form] ->
            Form;
        Forms when is_list(Forms) ->
            Forms;
        {error, Reason} ->
            {error, Reason}
    end.

tokens({string, Line, Code}) ->
    {ok, Ts, _} = erl_scan:string(Code, Line),
    Ts.

%% these code below are unexported function from merl.

parse_1(Ts) ->
    %% if dot tokens are present, it is assumed that the text represents
    %% complete forms, not dot-terminated expressions or similar
    case split_forms(Ts) of
        {ok, Fs} -> parse_forms(Fs);
        error ->
            parse_2(Ts)
    end.

split_forms(Ts) ->
    split_forms(Ts, [], []).

split_forms([{dot,_}=T|Ts], Fs, As) ->
    split_forms(Ts, [lists:reverse(As, [T]) | Fs], []);
split_forms([T|Ts], Fs, As) ->
    split_forms(Ts, Fs, [T|As]);
split_forms([], Fs, []) ->
    {ok, lists:reverse(Fs)};
split_forms([], [], _) ->
    error;  % no dot tokens found - not representing form(s)
split_forms([], _, [T|_]) ->
    fail("incomplete form after ~p", [T]).

parse_forms([Ts | Tss]) ->
    case erl_parse:parse_form(Ts) of
        {ok, Form} -> [Form | parse_forms(Tss)];
        {error, R} -> parse_error(R)
    end;
parse_forms([]) ->
    [].

parse_2(Ts) ->
    %% one or more comma-separated expressions?
    %% (recall that Ts has no dot tokens if we get to this stage)
    A = a0(),
    case erl_parse:parse_exprs(Ts ++ [{dot,A}]) of
        {ok, Exprs} -> Exprs;
        {error, E} ->
            parse_3(Ts ++ [{'end',A}, {dot,A}], [E])
    end.

parse_3(Ts, Es) ->
    %% try-clause or clauses?
    A = a0(),
    case erl_parse:parse_exprs([{'try',A}, {atom,A,true}, {'catch',A} | Ts]) of
        {ok, [{'try',_,_,_,_,_}=X]} ->
            %% get the right kind of qualifiers in the clause patterns
            erl_syntax:try_expr_handlers(X);
        {error, E} ->
            parse_4(Ts, [E|Es])
    end.

parse_4(Ts, Es) ->
    %% fun-clause or clauses? (`(a)' is also a pattern, but `(a,b)' isn't,
    %% so fun-clauses must be tried before normal case-clauses
    A = a0(),
    case erl_parse:parse_exprs([{'fun',A} | Ts]) of
        {ok, [{'fun',_,{clauses,Cs}}]} -> Cs;
        {error, E} ->
            parse_5(Ts, [E|Es])
    end.

parse_5(Ts, Es) ->
    %% case-clause or clauses?
    A = a0(),
    case erl_parse:parse_exprs([{'case',A}, {atom,A,true}, {'of',A} | Ts]) of
        {ok, [{'case',_,_,Cs}]} -> Cs;
        {error, E} ->
            %% select the best error to report
            parse_error(lists:last(lists:sort([E|Es])))
    end.

a0() ->
    anno(0).

anno(Location) ->
    erl_anno:new(Location).

-dialyzer({nowarn_function, parse_error/1}). % no local return

parse_error({L, M, R}) when is_atom(M), is_integer(L) ->
    fail("~w: ~ts", [L, M:format_error(R)]);
parse_error({{L,C}, M, R}) when is_atom(M), is_integer(L), is_integer(C) ->
    fail("~w:~w: ~ts", [L,C,M:format_error(R)]);
parse_error({_, M, R}) when is_atom(M) ->
    fail(M:format_error(R));
parse_error(R) ->
    fail("unknown parse error: ~tp", [R]).

-spec(fail(any()) -> no_return()).
fail(Text) ->
    fail(Text, []).

-spec(fail(any(), any()) -> no_return()).
fail(Fs, As) ->
    throw({error, lists:flatten(io_lib:format(Fs, As))}).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
