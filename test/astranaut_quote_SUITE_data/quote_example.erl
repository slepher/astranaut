%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(quote_example).

-include("quote.hrl").

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

unquote_map(Ast) ->
    quote({ok, #{unquote => Ast}}).

unquote_map_match(Ast) ->
    quote(#{unquote => Assoc}) = Ast,
    Assoc.

unquote_map_match_list(Ast) ->
    quote(#{a => 1, unquote_splicing => Assocs}) = Ast,
    Assocs.

unquote_record(Ast) ->
    quote({ok, #test{unquote = Ast}}).

unquote_record_match(Ast) ->
    quote(#test{unquote = Assoc}) = Ast,
    Assoc.

unquote_record_match_list(Ast) ->
    quote(#test{a = 1, unquote_splicing = Assocs}) = Ast,
    Assocs.

binding(Ast) ->
    quote({ok, _@Ast}).

atom_binding(Atom) ->
    quote({ok, _A@Atom}).

dynamic_binding(Value) ->
    quote({ok, _D@Value}).

dynamic_binding_pattern() ->
    quote({hello, _D@World}) = quote({hello, world}),
    World.

unquote_splicing_1(Ast1, Ast2) ->
    quote({ok, {hello, unquote_splicing([Ast1, Ast2]), world}}).

unquote_splicing_2(Ast1, Ast2) ->
    quote({ok, [hello, unquote_splicing([Ast1, Ast2]), world]}).

unquote_splicing_map(Ast1, Ast2) ->
    quote({ok, #{hello => 1, unquote_splicing => Ast1 ++ Ast2, world => 2}}).

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

bind_string(String) ->
    quote({ok, _S@String}).

function_pattern(quote = {hello, _A@World = World2} = C) ->
    quote({ok, {hello2, _A@World, _@World2, _@C}});
function_pattern(quote = {hello2, _@World} = C) ->
    quote(_A@World2) = World,
    quote({ok, {hello3, _A@World2, _@World, _@C}});
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

function_expression(Name) ->
    quote(fun '_A@Name'/3).

named_function_expression_1(Name) ->
    quote(fun _V@Name(0) -> 0;
              _V@Name(N) -> _V@Name(N -1) + N
         end).

named_function_expression_2() ->
    quote(fun Name(0) -> 0;
              Name(N) -> Name(N -1) + N
         end).

code() ->
    quote_code("test_fun()").

pos_1(Ast) ->
    Pos = erl_syntax:get_pos(Ast),
    quote({hello, unquote(Ast)}, Pos).

pos_2(Ast) ->
    Pos = erl_syntax:get_pos(Ast),
    Line = erl_anno:line(Pos),
    Ast1 = quote({hello, unquote(Ast)}, Line + 3),
    quote({ok, unquote(Ast1)}, #{pos => Line + 2}).

type(Name, Value) ->
    quote_code("-type '_A@Name'() :: '_A@Value'().").

exp_type(Name) ->
    Type = quote_type_code("hello:world()"),
    quote_code("-type '_A@Name'() :: _@Type.").

remote_type(Name, Module, Type) ->
    quote_code("-type '_A@Name'() :: '_A@Module':'_@Type'().").

record(Name) ->
    quote_code("-record('_A@Name', {id, hello, world}).").

spec(Name, Value1, Value2) ->
    quote_code("-spec '_A@Name'('_A@Value1'()) -> '_A@Value2'().").

guard(Var, Cond) ->
    quote(
      case _@Var of
          _@Var when _@Cond ->
              _@Var;
          _ ->
              {error, not_match}
      end).
