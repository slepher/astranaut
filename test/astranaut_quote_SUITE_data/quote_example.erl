%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(quote_example).

-include_lib("astranaut/include/quote.hrl").

%% API
-compile(export_all).
-compile(nowarn_export_all).

atom() ->
    quote(ok).

atom(Atom) ->
    quote(_A@Atom).
  
integer() ->
    quote(10).

tuple() ->
    quote({hello, world}).

unquote(Ast) ->
    quote({ok, unquote(Ast)}).

binding(Ast) ->
    quote({ok, _@Ast}).

atom_binding(Atom) ->
    quote({ok, _A@Atom}).

unquote_splicing_1(Ast1, Ast2) ->
    quote({ok, {hello, unquote_splicing([Ast1, Ast2]), world}}).

unquote_splicing_2(Ast1, Ast2) ->
    quote({ok, [hello, unquote_splicing([Ast1, Ast2]), world]}).

unquote_splicing_mix(Ast1, Ast2) ->
    AstList = [Ast1, Ast2],
    Fun1 = 
        quote(
          fun(unquote_splicing = AstList) ->
                  List = [hello, unquote_splicing(AstList), world],
                  Tuple = {hello, unquote_splicing(AstList), world},
                  {ok, List, Tuple}
          end),
    Fun2 = 
        quote(
          fun(unquote = Ast1, Two) ->
                  {error, unquote(Ast1), Two}
          end),
    astranaut_lib:merge_clauses([Fun1, Fun2]).

match_pattern(Ast) ->
    quote(_A@Hello(_@Foo, _L@World)) = Ast,
    quote({_A@Hello, _@Foo, _L@World}).

function_pattern(quote = {hello, _A@World = World2} = C) ->
    quote({ok, {hello2, _A@World, _@World2, _@C}});
function_pattern(Ast) ->
    quote({error, unquote(Ast)}).

case_pattern(Ast) ->
    case Ast of
        quote(_V@Function(_@Argument)) ->
            quote({ok, _V@Function(unquote(Argument) + 1)});
        quote(_@Module:_@Function(_L@Arguments)) ->
            quote({ok, {_@Module, _@Function, _L@Arguments}});
        _Other ->
            quote({error, unquote(Ast)})
    end.

code() ->
    quote_code("test_fun()").

line_1(Ast) ->
    Line = erl_syntax:get_pos(Ast),
    quote({hello, unquote(Ast)}, Line).

line_2(Ast) ->
    Pos = erl_syntax:get_pos(Ast),
    Line = erl_anno:line(Pos),
    Ast1 = quote({hello, unquote(Ast)}, Line + 3),
    quote({ok, unquote(Ast1)}, #{line => Line + 2}).

user_type(Name, Value) ->
    quote_code("-type '_A@Name'() :: '_A@Value'().").

system_type(Name, Value) ->
    quote_code("-type '_A@Name'() :: '_T@Value'().").

remote_type(Name, Module, Type) ->
    quote_code("-type '_A@Name'() :: '_A@Module':'_@Type'().").

record(Name) ->
    quote_code("-record('_A@Name', {id, hello, world}).").

spec(Name) ->
    quote_code("-spec '_A@Name'(atom()) -> atom().").

dynamic_binding() ->
    quote({hello, _D@World}) = quote({hello, world}),
    World.

guard(Var, Cond) ->
    quote(
      case _@Var of
          _@Var when _@Cond ->
              _@Var;
          _ ->
              {error, not_match}
      end).
