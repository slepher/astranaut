# traverse

### traverse functions:

```erlang
  astranaut_traverse:map(map_fun(), form(), Opts :: opts()) -> 
    traverse_return(node()) | parse_transform_return(node()).
    
  astranaut_traverse:reduce(reduce_fun(), state(), form(), Opts :: opts()) -> 
    traverse_return(state()).
    
  astranaut_traverse:map_with_state(map_state_fun(), state(), form(), Opts :: opts()) -> 
    traverse_return(node()) | parse_transform_return(node()).
    
  astranaut_traverse:mapfold(mapfold_fun(), state(), form(), Opts :: opts()) -> 
    traverse_return({form(), state()}).
```

*arguments*

```erlang
  form()    :: node() | [node()].
  node()    :: erlang ast node.
  state()   :: any().
```

*traverse_fun()*

```erlang
  map_fun()       :: (node(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return(node()).
  reduce_fun()    :: (node(), state(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return(state()).
  map_state_fun() :: (node(), state(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return(node()).
  mapfold_fun()   :: (node(), state(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return({node(), state()}).
```

*Attr*

```erlang
  attr() :: #{step => Step :: step(), node :: NodeType :: node_type(), attribute :: Attribute}.
```

*Step*

  &emsp;&emsp;which traverse step while traversing, very useful while traverse_style() in opts() is all.

```erlang
  step()  :: pre | post | leaf. 
```

*NodeType*

  &emsp;&emsp;ast node type.

```erlang
  node_type() :: form | attribute | pattern | expression | guard. 
```

*Attribute*

&emsp;&emsp;if NodeType is attribute, Attribute is name of attribute, or Attribute does not exists.

*TraverseFunReturn*

```erlang
  traverse_fun_return(A) :: A | {error, error()} | continue | {continue, A} |
                            #{'__struct__' => astranaut_traverse_fun_return, 
                              node := Node :: node(), state := State :: state(), continue := Continue :: boolean(),
                              error := error(), warning := error(), errors := [error()], warnings => [error()]}.
```

*Node*

&emsp;&emsp;node transformed to new node in traverse_walk_fun(), default is node() provided in traverse_walk_fun().

*State*

  &emsp;&emsp;state used in traverse_walk_fun(), default is state() provided in traverse_walk_fun().

*Continue*
  
&emsp;&emsp;if Continue is true or traverse_fun_return(A) is continue | {continue, A}, and Step of attr() is pre  
&emsp;&emsp;skip traverse children of currrent node and go to next node, nothing affected when Step of attr() is leaf or post.

*error()*

```erlang
  error()   :: #{'__struct__' => astranaut_traverse_error, 
                 line := Line :: integer(), module := Module :: module(), reason => Reason :: term()} | 
                {Line, Module, Reason} | {Line, Reason} | Reason.
```            
    
*Line*

&emsp;&emsp;expected error line, default is line of node in traverse_walk_fun().

*Module*

&emsp;&emsp;error formatter module which provide format_error/1, default is formatter option in opts().

*Opts*

```erlang
  opts()    :: {traverse => TraverseStyle :: traverse_style(), parse_transform => ParseTransform :: boolean(),
                node => FormType :: form_type(), formatter => Formatter}.
```


*Formatter*

&emsp;&emsp;error formatter module which provide format_error/1, default is astranaut_traverse.

*ParseTransform*

&emsp;&emsp;traverse_return(node()) will be transformed to parse_transform_return()  
&emsp;&emsp;which could directed used as return in parse_transform/2, useful in map/3, map_with_state/3.

*NodeType*

&emsp;&emsp;node_type(). if from() is incomplete erlang ast, this should be provided to help generate node_type() in attr().  
&emsp;&emsp;if top node is function or attribute, default top node_type() in attr() is form.  
&emsp;&emsp;else, default top node_type() in attr() is expression.

*TraverseStyle*

&emsp;&emsp;pre | post | all | leaf.
  
*traverse_return(Return)*  

```erlang
  traverse_return(Return) :: Return | {ok, Return, Errors :: traverse_return_error(), Warnings :: traverse_return_error()} | 
                             {error, Errors, Warnings}.
```

*parse_transform_return(Return)*  

```erlang
  parse_transform_return(Return) :: Return | {warning, Return, Warnings :: prase_transform_error()} |
                                    {error, Errors :: parse_transform_error(), Warnings}.
```

*ReturnError*

```erlang
  traverse_return_error() :: [{Line :: line(), Module :: module(), Reason :: term()}].
  parse_transform_error() :: [{File, traverse_retrun_error()}].
```

*Structs*

```erlang  
  astranaut_traverse:traverse_fun_return(#{}) -> traverse_fun_return(). 
  astranaut_traverse:traverse_error(#{}) -> error(). 
```

*Advanced*

&emsp;&emsp;powerful map_m function if you famillar with monad.

```erlang
  astranaut_traverse:map_m((A, attr()) => monad(A), map_m_opts()) -> monad(A). 
```

## Quote

### quick start
   
  with 
   
    -include_lib("astranaut/include/quote.hrl").

  you can use quote(Code) to represent ast of the code.
  
    quote(Code) | quote(Code, Options)

*Options*

```
  atom() => {atom() => true}
  proplists() => map(),
  Line => #{line => Line}
  #{line => Line, code_line => CodeLine, debug => Debug}.
```

*Line*

  
&emsp;&emsp; Line could be any expression, the ast will be transformed.

```erlang  
    quote(
      fun(_) ->
        ok
      end, 10). 
    =>
    astranaut:replace_line_zero(quote(fun(_) -> ok end), 10).
    =>
    {'fun', 10, {clauses, [{clause, 10, [{var, 10, '_'}], [], [{atom, 10, ok}]}]}}.
```

*CodeLine*

&emsp;&emsp; if CodeLine is true

```erlang
    10: quote(
    11:   fun(_) ->
    12:     ok
    13: end, code_line).
    =>  
    {'fun' 10, {clauses, [{clause, 11, [{var, 11, '_'}], [], [{atom, 12, ok}]}]}}.
```

*Debug*

&emsp;&emsp; if Debug is true, ast generated by quote will be printed to console at compile time.

### unquote

```erlang
unquote(Ast)
unquote = Ast.
unquote_splicing(Asts)
unquote_splicing = Asts.
```

*why two forms*

&emsp;&emsp; unquote(Var) is not a valid ast in function clause pattern.

```erlang
Var = {var, 0, A}
quote(fun(unquote = Var) -> unquote(Var) end).
```
    
### variable binding
  
*bind one ast*

&emsp;&emsp;_@V, same as unquote(V)
  
```erlang
    V = {var, 10, 'Var'},
    quote({hello, World, unquote(V)}) =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, V]} =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, {var, 10, 'Var'}]}
```

*bind a list of ast*

&emsp;&emsp;_L@Vs,same as unquote_splicing(Vs)

```erlang
    Vs = [{var, 2, 'Var'}, {atom, 2, atom}],
    quote({A, unquote_splicing(Vs), B}) => 
    {tuple, 1, [{var, 1, 'A'}, Vs ++ [{var, 1, 'B'}]]} =>
    {tuple, 1, [{var, 1, 'A'}, {var, 2, 'Var'}, {atom, 2, atom}, {var, 1, 'B'}]}
```

*bind a value*

```erlang
  Atom = hello,
  Integer = 10,
  Float = 1.3,
  String = "123",
  Variable = 'Var',

  _A@Atom => {atom, 0, Atom} => {atom, 0, hello}
  _I@Integer => {integer, 0, Integer} => {integer, 0, 10}
  _F@Float => {float, 0, Float} => {float, 0, 1.3}
  _S@String => {string, 0, String} => {string, 0, "123"}
  _V@Variable => {var, 0, Variable} => {var, 0, 'Var'}
```

*why binding*

&emsp;&emsp;_X@V could be used in any part of quoted ast.  
&emsp;&emsp;it's legal:
  
```erlang
    Class = 'Class0',
    Exception = 'Exception0',
    StackTrace = 'StackTrace0',
    quote(
      try
        throw(hello)
      catch
        _V@Class:_V@Exception:_V@StackTrace ->
          erlang:raise(_V@Class, _V@Exception, _V@StackTrace)
      end).
```      

&emsp;&emsp;it's illegal

```erlang
    Class = {var, 0, 'Class0'},
    Exception = {var, 0, 'Exception0'},
    StackTrace = {var, 0, 'StackTrace0'},   

    quote(
      try
        A
      catch
        unquote(Class):unquote(Exception):unquote(StackTrace) ->
          erlang:raise(_@Class, _@Exception, _@StackTrace)
      end).
```

   in other hand, V in unquote_xxx(V) could be any expression, it's more powerful than _X@V
   
### unquote and variable binding in pattern

&emsp;&emsp;quote macro could also be used in pattern match such as  
&emsp;&emsp;for limit of erlang ast format in pattern, some special forms is used
   
   left side of match

```erlang
     quote(_A@Atom) = {atom, 1, A}
     
     =>
     
     {atom, _, Atom} = {atom, 1, A}
```

&emsp;&emsp;function pattern

```erlang
     macro_clause(quote = {hello, _A@World = World2} = C) ->
       quote({hello2, _A@World, _@World2,_@C});
     
     => 
     
     macro_clause({tuple, _, [{atom, _, hello}, {atom, _, World} = World2]} = C) ->
       {tuple, 2, {atom, 2, hello2}, {atom, 2, World}, World2, C}
```

&emsp;&emsp;case clause pattern:
   
```erlang
     case Ast of
       quote(_A@Atom) ->
         Atom;
       _ ->
         other
     end.
     
     =>
     
     case ast of
         {atom, _, Atom} ->
             Atom;
         _ ->
             other
     end.
```

## Macro

*Usage*

```erlang
-include_lib("astranaut/include/macro.hrl").
```

macro.hrl add three attribute: use_macro, exec_macro debug_macro

*use_macro*

```erlang
-use_macro({Macro/A, opts()}).
-use_macro({Module, Macro/A, opts()}).
```

*exec_macro*

&emsp;&emsp;execute macro and add result to current ast.

```erlang
-exec_macro({Macro, Arguments}).
-exec_macro({Module, Macro, Arguments}).
```

*debug_macro*

```erlang
-debug_macro(true).
```

&emsp;&emsp; module will be printed to console after astranaut_macro transform.

*opts()*

```erlang
  #{debug => Debug, debug_ast => DebugAst, alias => Alias, 
    formatter => Formatter, attrs => Attrs, order => Order,
    as_attr => AsAttr, merge_function => MergeFunction, auto_export => AutoExport,
    group_args => GroupArgs}
  }
```
&emsp;&emsp; opts() could also be proplists, same usage of map().

*Debug*

&emsp;&emsp;print code generated when macro called compile time.

*DebugAst*

&emsp;&emsp;print ast generated when macro called compile time.

*Alias*

&emsp;&emsp; use Alias(Arguments) instead of Module:Macro(Arguments).

*Formatter*

&emsp;&emsp; module include format_error/1 to format macro errors,  
&emsp;&emsp; if formatter is true, formatter is the module where macro defined,  
&emsp;&emsp; default is astranaut_traverse.

*Attrs*

&emsp;&emsp; module attributes as extra args while calling macro.

```
-module(a).
-behaviour(gen_server).
-use_macro({macro/2, [{attrs, [module, line, behaviour]}]}).

hello() ->
  macro_a:macro(world).

macro(Ast, #{module => Module, line => Line, behaviour => Behaviours} = Attributes) ->
    {warning, Ast, {attributes, Module, Line, Behaviours}}.
```

*Order*

&emsp;&emsp; macro expand order for nested macro , value is pre | post. default is post.  
&emsp;&emsp; pre is expand macro from outside to inside, post is expand macro from inside to outside.

*AsAttr*

&emsp;&emsp; user defined attribute name replace of -exec_macro.

*MergeFunction*

&emsp;&emsp; -exec_macro ast function merge to function with same name and arity if exists.

*AutoExport*

&emsp;&emsp; -exec_macro ast function auto export, merge to current export if exists.

*GroupArgs* 

&emsp;&emsp; treat macro arguments as list 

```erlang
-use_macro({a, [group_args]}).

test() ->
    a(hello, world).

a(Asts) ->
  quote({unquote_splicing(Asts)}).
```

&emsp;&emsp;define macro as normal erlang functions.  
&emsp;&emsp;macro expand order is the order of -use_macro in file.  
&emsp;&emsp;macro will be expand at compile time by parse_transformer astranaut_macro.  
&emsp;&emsp;macro does not know runtime value of arguments.  
&emsp;&emsp;arguments passed in macro is erlang ast.  
&emsp;&emsp;arguments passed in -exec_macro is term.  
&emsp;&emsp;-export will be moved to appropriate location in ast forms.  
&emsp;&emsp;macro return value is same meaning of traverse_fun_return().  

```erlang
-use_macro({macro_1/1, []}).
-use_macro({macro_2/1, []}).

-export([test/0]).

test() ->
  macro_1(hello()).

macro_1(Ast) ->
  quote(
      fun() -> unquote(Ast) end
  ).

-exec_macro({macro_2, [hello]}).

macro_2(Name) ->
  astranaut:function(
    Name,
    quote(
      fun() ->
          unquote_atom(Name)
      end)).
```

=>

```erlang
-use_macro({macro_1/1, []}).
-export([test/0]).
-export([hello/0]).

test_macro_1() ->
  fun() -> hello() end.

macro_1(Ast) ->
  quote(
      fun() -> unquote(Ast) end
  ).

hello() ->
  hello.

macro_2(Name) ->
  astranaut:function(
    Name,
    quote(
      fun() ->
          unquote_atom(Name)
      end)).
```

*parse_transform*


&emsp;&emsp; for old parse_transform module which is used widely, two function is provided.

```erlang
*astranaut_macro:transform_macro(Module, Function, Arity, Opts, Forms).
*astranaut_macro:transform_macros([Macro...], Forms).
Macro = {Module, Function, Arity, Opts}.
```

&emsp;&emsp; example:

```
-module(do).

-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    astranaut_macro:transform_macro(do_macro, do, 1, [{alias, do}, formatter], Forms).
```