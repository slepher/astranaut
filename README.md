# traverse

### traverse functions:

```
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

```
  form()    :: node() | [node()].
  node()    :: erlang ast node.
  state()   :: any().
```

*traverse_walk_fun()*

```
  map_fun()       :: (node(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return(node()).
  reduce_fun()    :: (node(), state(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return(state()).
  map_state_fun() :: (node(), state(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return(node()).
  mapfold_fun()   :: (node(), state(), Attr :: attr()) -> TraverseFunReturn :: traverse_fun_return({node(), state()}).
```

*Attr*

```
  attr() :: #{step => Step :: step(), node :: NodeType :: node_type()}.
```

*Step*

  &emsp;&emsp;which traverse step while traversing, very useful while traverse_style() in opts() is all.

```
  step()  :: pre | post | leaf. 
```

*NodeType*

  &emsp;&emsp;ast node type.

```
  node_type() :: module | file | export | import | type | spec | function | pattern | expression | guard. 
```

*TraverseFunReturn*

```
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

```    
  error()   :: #{'__struct__' => astranaut_traverse_error, 
                 line := Line :: integer(), module := Module :: module(), reason => Reason :: term()} | 
                {Line, Module, Reason} | {Line, Reason} | Reason.
```            
    
*Line*

&emsp;&emsp;expected error line, default is line of node in traverse_walk_fun().

*Module*

&emsp;&emsp;error formatter module which provide format_error/1, default is formatter option in opts().

*Opts*

```
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

*TraverseStyle*

&emsp;&emsp;pre | post | all | leaf.
  
*traverse_return(Return)*  

```
  traverse_return(Return) :: Return | {ok, Return, Errors :: traverse_return_error(), Warnings :: traverse_return_error()} | 
                             {error, Errors, Warnings}.
```

*parse_transform_return(Return)*  

```
  parse_transform_return(Return) :: Return | {warning, Return, Warnings :: prase_transform_error()} |
                                    {error, Errors :: parse_transform_error(), Warnings}.
```

*ReturnError*

```
  traverse_return_error() :: [{Line :: line(), Module :: module(), Reason :: term()}].
  parse_transform_error() :: [{File, traverse_retrun_error()}].
```

*Structs*

```  
  astranaut_traverse:traverse_fun_return(#{}) -> traverse_fun_return(). 
  astranaut_traverse:traverse_error(#{}) -> error(). 
```

*Advanced*

&emsp;&emsp;powerful map_m function if you famillar with monad.

```
  astranaut_traverse:map_m((A, attr()) => monad(A), map_m_opts()) -> monad(A). 
```

## Quote

### quick start
   
  with 
   
    -include_lib("astranaut/include/quote.hrl").

  you can use quote(Code) to represent ast of the code.
  
    qute(Code) | quote(Code, Line)
  
  the second param of varaibe is line number in ast
  
    quote(
      fun(_) ->
        ok
      end, 10).
      
  generates 
  
    {'fun', 10, {clauses, [{clause, 10, [{var, 10, '_'}], [], [{atom, 10, ok}]}]}}.

  if it is not presented, current line number of code is used
  
    10: quote(
    11:   fun(_) ->
    12:     ok
    13: end).
      
  generates ast of function which is
  
    {'fun' 10, {clauses, [{clause, 11, [{var, 11, '_'}], [], [{atom, 12, ok}]}]}}.

### unquote

```
unquote(Ast)
unquote = Ast.
unquote_splicing(Asts)
unquote_splicing = Asts.
```

*why two forms*

&emsp;&emsp; unquote(Var) is not a valid ast in function clause pattern.

```
Var = {var, 0, A}
quote(fun(unquote = Var) -> unquote(Var) end).
```
    
### variable binding
  
*bind one ast*

&emsp;&emsp;_@V, same as unquote(V)
  
```
    V = {var, 10, 'Var'},
    quote({hello, World, unquote(V)}) =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, V]} =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, {var, 10, 'Var'}]}
```

*bind a list of ast*

&emsp;&emsp;_L@Vs,same as unquote_splicing(Vs)

```
    Vs = [{var, 2, 'Var'}, {atom, 2, atom}],
    quote({A, unquote_splicing(Vs), B}) => 
    {tuple, 1, [{var, 1, 'A'}, Vs ++ [{var, 1, 'B'}]]} =>
    {tuple, 1, [{var, 1, 'A'}, {var, 2, 'Var'}, {atom, 2, atom}, {var, 1, 'B'}]}
```

*bind a value*

```
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
  
```
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

```
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

```   
     quote(_A@Atom) = {atom, 1, A}
     
     =>
     
     {atom, _, Atom} = {atom, 1, A}
```

&emsp;&emsp;function pattern

```   
     macro_clause(quote = {hello, _A@World = World2} = C) ->
       quote({hello2, _A@World, _@World2,_@C});
     
     => 
     
     macro_clause({tuple, _, [{atom, _, hello}, {atom, _, World} = World2]} = C) ->
       {tuple, 2, {atom, 2, hello2}, {atom, 2, World}, World2, C}
```

&emsp;&emsp;case clause pattern:
   
```
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

```
-include_lib("astranaut/include/macro.hrl").
```

macro.hrl add two attribute: use_macro, exec_macro

*use_macro*

```
-use_macro({Macro/A, opts()}).
-use_macro({Module, Macro/A, opts()}).
```

*exec_macro*

&emsp;&emsp;execute macro and add result to current ast.

```
-exec_macro({Macro, Arguments}).
-exec_macro({Module, Macro, Arguments}).
```

*opts()*

```
  #{debug => Debug, debug_ast => DebugAst, import_as => ImportAs}
```

*Debug*

&emsp;&emsp;print code generated when macro called compile time.

*DebugAst*

&emsp;&emsp;print ast generated when macro called compile time.

*ImportAs*

&emsp;&emsp; use ImportAs(Arguments) instead of Module:Macro(Arguments).

*Usage*

&emsp;&emsp;define macro as normal erlang functions.  
&emsp;&emsp;macro will be expand at compile time by parse_transformer astranaut_macro.  
&emsp;&emsp;macro does not know runtime value of arguments.  
&emsp;&emsp;arguments passed in macro is erlang ast.  
&emsp;&emsp;arguments passed in -exec_macro is term.  
&emsp;&emsp;-export will be moved to appropriate location in ast forms.

```
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

```
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
