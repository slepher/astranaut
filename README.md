
![Travis CI](https://travis-ci.org/slepher/astranaut.svg?branch=master)

# requirements

&emsp;&emsp; erlang R19 or higher  
  
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
  traverse_fun_return(SA) :: SA | {error, error()} | {error, SA, error()} | 
                            {warning, SA, error()} | {warning, error()} |
                            continue | {continue, SA} |
                            astranaut_walk_return:astranaut_walk_return(A) |
                            astranaut_traverse_m:astranaut_traverse_m(S, A) |
                            astranaut_return_m:astranaut_return_m(A) |
                            astranaut_base_m:astranaut_base_m(A).
  SA is same return type in traverse_fun(), but A is always node(), and S is always state().
```

*Node*

&emsp;&emsp;node transformed to new node in traverse_walk_fun(), default is node() provided in traverse_walk_fun().

*State*

  &emsp;&emsp;state used in traverse\_walk\_fun(), default is state() provided in traverse_walk_fun().

*Continue*
  
&emsp;&emsp;if Continue is true or traverse\_fun\_return(A) is continue | {continue, A}, and Step of attr() is pre  
&emsp;&emsp;skip traverse children of currrent node and go to next node, nothing affected when Step of attr() is leaf or post.

*error()*

```erlang
  error()   :: Reason.
```
    
*Line*

&emsp;&emsp;expected error line, default is line of node in traverse_walk_fun().

*Module*

&emsp;&emsp;error formatter module which provide format_error/1, default is formatter option in opts().

*Opts*

```erlang
  opts()    :: {traverse => TraverseStyle :: traverse_style(), parse_transform => ParseTransform :: boolean(),
                node => FormType :: form_type(), formatter => Formatter, 
                children => Children, sequence_children => SequenceChildren}.
```


*Formatter*

&emsp;&emsp;error formatter module which provide format_error/1, default is astranaut_traverse.

*ParseTransform*

&emsp;&emsp;traverse_return(node()) will be transformed to parse_transform_return()  
&emsp;&emsp;which could directed used as return in parse_transform/2, useful in map/3, map_with_state/3.

*NodeType*

&emsp;&emsp;node\_type(). if from() is incomplete erlang ast, this should be provided to help generate node_type() in attr().  
&emsp;&emsp;if top node is function or attribute, default top node\_type() in attr() is form.  
&emsp;&emsp;else, default top node\_type() in attr() is expression.

*TraverseStyle*

&emsp;&emsp;pre | post | all | leaf.

*Children*

&emsp;&emsp; true: Only traverse children of node, not traverse node its self.

*SequenceChildren*

&emsp;&emsp; callback to defined your own traverse children method

```erlang
SequenceChildren = fun(DeepListOfChildrenM) -> MChildren end.
```

&emsp;&emsp; traverse right expression first in match expression

```erlang
SequenceChildren = 
  fun([PatternMs, ExpressionMs]) -> 
    %% reverse the traverse order, traverse ExpressionMs first
    %% deep_r_sequence_m means reverse sequence_m the first level of deep list.
    astranaut_traverse:deep_r_sequence_m([PatternMs, ExpressionMs]) 
  end.
```

&emsp;&emsp; do something special to Clause Patterns

```erlang
SequenceChildren = 
  fun([PatternMs|GuardsAndExpressionMs]) -> 
    %% PatternMs is a list of monad, sequence_m it to get a monad of list.
    PatternsM = astranaut_traverse:deep_sequence_m(PatternMs),
    %% do something special to PatternsM monad.
    PatternsM1 = do_something_special(PatternsM),
    %% deep_sequence_m the new tree.
    astranaut_traverse:deep_sequence_m([PatternsM1|GuardsAndExpressionMs]) 
  end.
```

&emsp;&emsp; do something special to Each Clause Patterns

```erlang
SequenceChildren = 
  fun([PatternMs|GuardsAndExpressionMs]) -> 
    %% PatternMs is a list of monad, sequence_m it to get a monad of list.
    PatternMs1 = lists:map(fun(PatternM) -> do_something_special(PatternM) end, PatternMs),
    %% deep_sequence_m the new tree.
    astranaut_traverse:deep_sequence_m([PatternMs1|GuardsAndExpressionMs]) 
  end.
```
  
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

&emsp;&emsp;powerful map\_m function if you famillar with monad.

```erlang
  astranaut_traverse:map_m((A, attr()) => monad(A), map_m_opts()) -> monad(A). 
```
  
# monad modules

### astranaut\_traverse\_m

&emsp;&emsp; the main monad of astranaut\_traverse.

### astranaut\_base\_m

&emsp;&emsp;a monad with errors and warnings.__
&emsp;&emsp;you could just append errors or warnings to it.

```erlang
  astranaut_base_m:then(
    astranaut_base_m:warning(warning_0),
    astranaut_base_m:return(ok)).
```

### astranaut\_return\_m

&emsp;&emsp;the monad result of astranaut\_traverse\_m:run(MA, Formatter, State).  
&emsp;&emsp;could be transformed to compiler return format with astranaut\_return\_m:to\_compiler/1.  
&emsp;&emsp;could transforme compiler return format to astranaut\_return\_m with astranaut\_return\_m:from_compiler/1.

### astranaut\_error\_state

### astranaut\_walk\_return

&emsp;&emsp; return type of Fun in astranut\_traverse:(map\_m|map|reduce|map\_with\_state|mapfold|)(Fun, Forms, Opts). 

# Quote

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

&emsp;&emsp;if Debug is true, ast generated by quote will be printed to console at compile time.__

### unquote

```erlang
unquote(Ast)
unquote = Ast.
unquote_splicing(Asts)
unquote_splicing = Asts.
```

*why two forms*

&emsp;&emsp;unquote(Var) is not a valid ast in function clause pattern.__

```erlang
Var = {var, 0, A}
quote(fun(unquote = Var) -> unquote(Var) end).
```
    
### variable binding
  
*bind one ast*

&emsp;&emsp;\_@V, same as unquote(V)
  
```erlang
    V = {var, 10, 'Var'},
    quote({hello, World, unquote(V)}) =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, V]} =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, {var, 10, 'Var'}]}
```

*bind a list of ast*

&emsp;&emsp;\_L@Vs,same as unquote_splicing(Vs)

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

&emsp;&emsp;\_X@V could be used in any part of quoted ast.  
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

# Macro

*Usage*

```erlang
-include_lib("astranaut/include/macro.hrl").
```

macro.hrl add three attribute: use\_macro, exec\_macro debug\_macro

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

*export_macro*

&emsp;&emsp;used in where macro defined, options in export\_macro will be merged to options in use_macro.

```erlang
-export_macro({[MacroA/A, MacroB/B], opts()}).
```

*debug_macro*

```erlang
-debug_macro(true).
```

&emsp;&emsp; module will be printed to console after astranaut\_macro transform.

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

&emsp;&emsp; module include format\_error/1 to format macro errors,  
&emsp;&emsp; if formatter is true, formatter is the module where macro defined,  
&emsp;&emsp; default is astranaut\_traverse.

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

&emsp;&emsp; user defined attribute name replace of -exec\_macro.

*MergeFunction*

&emsp;&emsp; -exec\_macro ast function merge to function with same name and arity if exists.

*AutoExport*

&emsp;&emsp; -exec\_macro ast function auto export, merge to current export if exists.

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
&emsp;&emsp;macro expand order is the order of -use\_macro in file.  
&emsp;&emsp;macro will be expand at compile time by parse\_transformer astranaut\_macro.  
&emsp;&emsp;macro does not know runtime value of arguments.  
&emsp;&emsp;arguments passed in macro is erlang ast.  
&emsp;&emsp;arguments passed in -exec\_macro is term.  
&emsp;&emsp;-export will be moved to appropriate location in ast forms.  
&emsp;&emsp;macro return value is same meaning of traverse\_fun\_return().  

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

*hygienic macro*

&emsp;&emsp; each macro expansion has it's unique namespace.

&emsp;&emsp; @{macro\_module\_name}@\_{counter} is added to it's original name.

```erlang
-module(macro_example).
macro_with_vars_1(Ast) ->
    quote(
      begin
          A = 10,
          B = unquote(Ast),
          A + B
      end
     ).
macro_with_vars_2(Ast) ->
    quote(
      begin
          A = 10,
          B = unquote(Ast),
          A + B
      end
     ).
```

```erlang
test_macro_with_vars(N) ->
    A1 = macro_with_vars_1(N),
    A2 = macro_with_vars_2(A1),
    A3 = macro_with_vars_2(N),
    A4 = macro_with_vars_1(A1),
    A1 + A2.
```

=>

```erlang
test_macro_with_vars(N) ->
A1 =
begin
  A@macro_example@_1 = 10,
  B@macro_example@_1 = N,
  A@macro_example@_1 + B@macro_example@_1
end,
A2 = 
begin
  A@macro_example@_3 = 10,
  B@macro_example@_3 = A1,
  A@macro_example@_3 + B@macro_example@_3
end,
A3 = 
begin
  A@macro_example@_4 = 10,
  B@macro_example@_4 = N,
  A@macro_example@_4 + B@macro_example@_4
end,
A4 =
begin
  A@macro_example@_2 = 10,
  B@macro_example@_2 = A1,
  A@macro_example@_2 + B@macro_example@_2
end,
A1 + A2 + A3 + A4.
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

# Rebinding 

```erlang
-include_lib("erlando/include/rebinding.hrl").

-rebinding_all(Opts).
-rebinding_fun(FAs).
-rebinding_fun({FAs, Opts}).

FAs = FA | [FA...].
FA = F | F/A.
Opts = Opt | [Opt...] | #{OptKey => OptValue}.
Opt = OptKey | {OptKey, OptValue}.
#{OptKey => OptValue} = #{debug => true | false}.
```

*Rebinding Attributes*

&emsp;&emsp; -rebinding\_all -rebinding\_fun defines rebinding scope.  
&emsp;&emsp; -rebinding\_all meaning rebinding scope is all function.  
&emsp;&emsp; -rebinding\_fun meaning rebinding scope is in functions mentioned.  
&emsp;&emsp; rebinding options is avaliable in scope mentioned.  
&emsp;&emsp; rebinding option debug means print code after rebinding rules applied.  
&emsp;&emsp; if neither -rebinding\_fun nor -rebinding\_all is used, rebinding scope is all function and rebinding options is [].

*Rebinding Rules* 

&emsp;&emsp; pattern variables will be renamed while already used include:  
&emsp;&emsp;&emsp;&emsp; function pattern variables  
&emsp;&emsp;&emsp;&emsp; match pattern variables  
&emsp;&emsp;&emsp;&emsp; list comprehension pattern variables  
&emsp;&emsp;&emsp;&emsp; bitstring comprehension pattern variables   
&emsp;&emsp; pattern variables with same name in same pattern scope will be renamed to same name.  
&emsp;&emsp; other variable will be renamed follow last renamed vaiable last avaliable scope used.  
&emsp;&emsp; +{pattern variable} means pinned variable like Elixir ^{pattern variable}, also works like other variable.

*Examples* 

```erlang
hello(A, A, B) ->
    {A, A, B} = {A + 1, A + 1, B + 1},
    {A, A, B}.
```

=> 

```erlang
hello(A, A, B) ->
  {A_1, A_1, B_1} = {A + 1, A + 1, B + 1},
  {A_1, A_1, B_1}.
```

```erlang
hello(A, B) ->
  A = 
    case A of
        B -> 
          B = A + B,
          A = A + B,
          B = A + B,
          B;
        A ->
          B = A + B
          B
    end,
  B = 
    case A of
        B -> 
          B = A + B,
          A = A + B,
          B = A + B,
          B;
        A ->
          B = A + B
          B
    end,
  {A, B}.
```

=>

```erlang
hello(A, B) ->
  A_2 = 
    case A of
        B -> 
          B_1 = A + B,
          A_1 = A + B_1,
          B_2 = A_1 + B_1,
          B_2;
        A ->
          B_1 = A + B
          B_1
    end,
    
  B_5 = 
    case A_2 of
        B -> 
          %% B_1 and B_2 is already used, next var name is B_3, last var name in scope is B.
          B_3 = A_2 + B,
          A_3 = A_2 + B_3,
          B_4 = A_3 + B_3,
          B_4,
        A_2 ->
          B_3 = A_2 + B
          B_3
    end,
  {A_2, B_5}.
```


```erlang
hello_f(A) ->
    A = A + 1,
    F = fun F (0) -> 0; F (A) -> A = F(A - 1), A end,
    A = F(A),
    A.
```

=> 

```erlang
hello_f(A) ->
    A_1 = A + 1,
    F = fun F(0) -> 0; F(A_2) -> A_3 = F(A_2 - 1), A_3 end,
    A_2 = F(A_1),
    F_1 = fun F_1(0) -> 0; F_1(A_3) -> A_4 = F_1(A_3 - 1), A_4 end,
    A_3 = F_1(A_2),
    A_3.
```


# Struct

*Usage*

```erlang
-include_lib("erlando/include/struct.hrl").
-record(test, {name = hello, value}).
-astranaut_struct([test]).

-export([new/0, update_name/2]).

new() ->
  #test{}.
    
update_name(Name, #test{} = Test) ->
  Test#test{name = Name}. 
```

*Desc*

&emsp;&emsp; convert erlang record to elixir like struct  
&emsp;&emsp; code above is converted to code below  

```erlang
-include_lib("erlando/include/struct.hrl").
-record(test, {name = hello, value}).
-astranaut_struct([test]).

-export([new/0, update_name/2]).

new() ->
  #{'__struct__' => test, name => hello, value => undefined}.
    
update_name(Name, #{'__struct__' := test} = Test) ->
  Test#{name => Name}.
```

*Struct Options*

&emsp;&emsp; -astranaut\_struct could have extra options:  
&emsp;&emsp; non\_auto\_fill : means fields will not set default to undefined when not defined and initialized.  
&emsp;&emsp; enforce\_keys : means compile will failed when field is not setted when construct struct, works like elixir.

```erlang
-astranaut_struct({test, [non_auto_fill, {enforce_keys, [name]}]}).

test_failed() ->
  #test{}. 
  
%% compile failed
%% the following keys must also be given when building struct test: [name]

test_non_auto_fill() ->
  #test{name = test}. 

%% ==>

test_non_auto_fill_transformed() ->
  #{'__struct__' => test, name => test}. %% value is not set to undefined
  
test_auto_fill() ->
  #test{name = test}.
  
%% ==>

test_auto_fill_transformed() ->
  #{'__struct__' => test, name => test, value => undefined}. %% value is set to undefined at default.
```

*Macros*

```erlang
astranaut_struct:from_record(StructName, Record) -> Struct. %% convert a recrod to struct with same name.
astranaut_struct:to_record(StructName, Struct) -> Record. %% convert a struct to record with same name.
astranaut_struct:from_map(StructName, Struct) -> Struct. %% build a struct from map, enforce_keys will be checked.
astranaut_struct:update(StructName, Struct) -> Struct. %% update a struct from it's old version.
```
