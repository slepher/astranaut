
[![Build Status](https://api.cirrus-ci.com/github/slepher/astranaut.svg)](https://cirrus-ci.com/github/slepher/astranaut)

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
  attr() :: #{step => Step :: step(),
              node => Role :: node_role(),
              validator => Validator :: term(),
              attribute => Attribute :: atom()}.
```

*Step*

  &emsp;&emsp;which traverse step while traversing, very useful while traverse_style() in opts() is all.

```erlang
  step()  :: pre | post | leaf. 
```

*NodeRole*

  &emsp;&emsp;traversal role of current node. It is not the Erlang AST type.

```erlang
  node_role() :: form | expression | pattern | guard | type | clause. 
```

&emsp;&emsp;Use `astranaut_syntax:type/1` when you need the concrete Erlang AST type.

*Validator*

&emsp;&emsp;`validator` is an opaque position token propagated by traversal.
Normal users should not inspect or construct it. If a walker needs to validate a
replacement node manually, pass `Attr.validator` unchanged to `astranaut_syntax`.

```erlang
case astranaut_syntax:validate_node(NewNode, Validator) of
    ok -> NewNode;
    {error, Detail} -> {error, Detail}
end.
```

&emsp;&emsp;For recursive normalization, pass the attr as context:

```erlang
case astranaut_syntax:normalize(NewNode, Validator, #{attr => Attr}) of
    {ok, NewNode1} -> NewNode1;
    {error, Detail} -> {error, Detail}
end.
```

&emsp;&emsp;Traversal may include extra diagnostic metadata in attrs. The stable
way to validate a replacement is to pass the opaque `validator` token unchanged.

*Attribute*

&emsp;&emsp;if `node` is related to an attribute body, `attribute` is the attribute name.

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
    
*Pos*

&emsp;&emsp;expected error pos, default is pos of node in traverse_walk_fun().

*Module*

&emsp;&emsp;error formatter module which provide format_error/1, default is formatter option in opts().

*Opts*

```erlang
  opts()    :: #{traverse => TraverseStyle :: traverse_style(),
                 normalize => Normalize :: boolean(),
                 role => Role :: node_role(),
                 parse_transform => ParseTransform :: boolean(),
                 formatter => Formatter :: module(),
                 attr => Attr :: map(),
                 uniplate => Uniplate :: fun()}.
```


*Formatter*

&emsp;&emsp;error formatter module which provide format_error/1, default is astranaut_traverse.

*ParseTransform*

&emsp;&emsp;traverse_return(node()) will be transformed to parse_transform_return()  
&emsp;&emsp;which could directed used as return in parse_transform/2, useful in map/3, map_with_state/3.

*Role*

&emsp;&emsp;`role => Role` explicitly sets the root traversal role. Use it when the
root node is not a complete form or when the root role cannot be inferred safely.

```erlang
astranaut:smap(Fun, Expr, #{traverse => pre, role => expression}).
```

*Normalize*

&emsp;&emsp;if `normalize => true`, traversal validates or normalizes the node
directly returned by the walker before putting it back into the current position.
This option only applies to the direct return value of the walker. A parent node
rebuilt because a child changed is not treated as a direct replacement of that
parent.

*TraverseStyle*

&emsp;&emsp;pre | post | all | leaf | subtree | none.

*Attr*

&emsp;&emsp;initial attrs merged into traversal context.

*Uniplate*

&emsp;&emsp;advanced traversal implementation hook. Most users do not need this option.

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
  traverse_return_error() :: [{Pos :: pos(), Module :: module(), Reason :: term()}].
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

# astranaut_syntax

### validation and normalization

```erlang
  astranaut_syntax:validate_node(NodeOrNodes, Validator) -> ok | {error, map()}.
  astranaut_syntax:validate_node(NodeOrNodes, Validator, Opts) -> ok | {error, map()}.

  astranaut_syntax:normalize(NodeOrNodes, Validator) ->
    {ok, NodeOrNodes1} | {error, map()}.

  astranaut_syntax:normalize(NodeOrNodes, Validator, Opts) ->
    {ok, NodeOrNodes1} | {error, map()}.
```

*Validator*

&emsp;&emsp;`Validator` is usually copied from `Attr.validator`. It is an internal
position contract and should be treated as an opaque token.

*validate_node*

&emsp;&emsp;validates the current node against the provided validator. It does not
recursively validate child nodes.

*normalize*

&emsp;&emsp;validates the current node, recursively normalizes its children, rebuilds
the AST with `subtrees/1` and `update_tree/2`, and returns the normalized node.

*Forms*

&emsp;&emsp;guard validation may require record definitions. `Attr.validator` does
not contain original forms, and traversal callbacks may not receive the original
module forms. If guard validation depends on records, pass proper record forms
explicitly:

```erlang
astranaut_syntax:validate_node(Node, Validator, #{forms => RecordForms}).
astranaut_syntax:normalize(Node, Validator, #{forms => RecordForms}).
```

### helper api

```erlang
  astranaut_syntax:child_specs(Type, Subtrees, Attr) -> [child_spec()].
  astranaut_syntax:node_roles(Type) -> [node_role()].
  astranaut_syntax:otp_vsn() -> integer() | 'pre-21'.

  astranaut_syntax:type(Node) -> Type.
  astranaut_syntax:get_pos(Node) -> Pos.
  astranaut_syntax:set_pos(Node, Pos) -> Node1.
  astranaut_syntax:subtrees(Node) -> Subtrees.
  astranaut_syntax:update_tree(Node, Subtrees) -> Node1.
  astranaut_syntax:revert(Node) -> Node1.
```

&emsp;&emsp;`child_specs/3` is an advanced API used by traversal and normalization.
It may contain internal validator data; user code should normally keep that data
opaque.

# astranaut_uniplate

&emsp;&emsp;`astranaut_uniplate` is the internal uniplate/context implementation
used by traversal. Most users should use `astranaut`, `astranaut_traverse`, and
`astranaut_syntax` instead of depending on its internal context shape.
  
# monad modules

### astranaut\_traverse\_m

&emsp;&emsp; the main monad of astranaut\_traverse.

### astranaut\_base\_m

&emsp;&emsp;a monad with errors and warnings.  
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
  Pos => #{pos => Pos}
  #{pos => Pos, code_pos => CodePos, debug => Debug}.
```

*Pos*

  
&emsp;&emsp; Pos could be any expression, the ast will be transformed.

```erlang  
    quote(
      fun(_) ->
        ok
      end, 10). 
    =>
    astranaut:replace_pos_zero(quote(fun(_) -> ok end), 10).
    =>
    {'fun', 10, {clauses, [{clause, 10, [{var, 10, '_'}], [], [{atom, 10, ok}]}]}}.
```

*CodePos*

&emsp;&emsp; if CodePos is true

```erlang
    10: quote(
    11:   fun(_) ->
    12:     ok
    13: end, code_pos).
    =>  
    {'fun', {11, 2}, {clauses, [{clause, {11,5}, [{var, {11,5}, '_'}], [], [{atom, {12, 3}, ok}]}]}}.
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

macro.hrl enables the astranaut macro parse transform.

*export_macro*

&emsp;&emsp;used in the module where macros are defined. Exported macros can be
imported by other modules.

```erlang
-export_macro([MacroA/A, MacroB/B]).
-export_macro({Macro/A, opts()}).
-export_macro({[MacroA/A, MacroB/B], opts()}).
```

*local_macro*

&emsp;&emsp;declare local functions as macros without exporting them.

```erlang
-local_macro([MacroA/A, MacroB/B]).
-local_macro({Macro/A, opts()}).
-local_macro({[MacroA/A, MacroB/B], opts()}).
```

*import_macro*

&emsp;&emsp;declare a module that exports macros. Macro selection and call options
should be configured with `-use_macro`.

```erlang
-import_macro(Module).
```

*use_macro*

&emsp;&emsp;use an imported or local macro with extra call options.

```erlang
-use_macro({Macro/A, opts()}).
-use_macro({[MacroA/A, MacroB/B], opts()}).
-use_macro({Module, Macro/A, opts()}).
-use_macro({Module, [MacroA/A, MacroB/B], opts()}).
```

*exec_macro*

&emsp;&emsp;execute macro and add result to current ast.

```erlang
-exec_macro({Macro, Arguments}).
-exec_macro({Module, Macro, Arguments}).
```

*macro_options*

&emsp;&emsp;declare module-level macro options.

```erlang
-macro_options(opts()).
```

*opts()*

```erlang
  #{debug => Debug,
    debug_ast => DebugAst,
    debug_module => DebugModule,
    debug_module_ast => DebugModuleAst,
    alias => Alias,
    order => Order,
    inject_attrs => InjectAttrs,
    as_attr => AsAttr,
    group_args => GroupArgs,
    force_override => ForceOverride,
    max_depth => MaxDepth}
```
&emsp;&emsp; opts() could also be proplists, same usage of map().

*Debug*

&emsp;&emsp;print code generated when macro called compile time.

*DebugAst*

&emsp;&emsp;print ast generated when macro called compile time.

*Alias*

&emsp;&emsp; use Alias(Arguments) instead of Module:Macro(Arguments).

*InjectAttrs*

&emsp;&emsp; module attributes as extra args while calling macro.

```
-module(a).
-behaviour(gen_server).
-local_macro({macro/2, [{inject_attrs, [module, behaviour]}]}).

hello() ->
  macro(world).

macro(Ast, #{module => Module, pos => Pos, behaviour => Behaviours} = Attributes) ->
    {warning, Ast, {attributes, Module, Pos, Behaviours}}.
```

*Order*

&emsp;&emsp; macro expand order for nested macros. `inner` is default and expands
inside before outside. `outer` expands outside before inside.

*AsAttr*

&emsp;&emsp; user defined attribute name replace of -exec\_macro.

*GroupArgs* 

&emsp;&emsp; treat macro arguments as list 

```erlang
-use_macro({a, [group_args]}).

test() ->
    a(hello, world).

a(Asts) ->
  quote({unquote_splicing(Asts)}).
```

*ForceOverride*

&emsp;&emsp; allow an intentional macro alias override. Without this option,
conflicting macro names fail with `macro_override`.

*MaxDepth*

&emsp;&emsp; maximum nested macro expansion chain depth. The default module-level
value is 100.

*Option Scope*

| Attribute | Options |
| --- | --- |
| `-macro_options` | `debug`, `debug_ast`, `debug_module`, `debug_module_ast`, `max_depth` |
| `-export_macro`, `-local_macro` | `as_attr`, `order`, `inject_attrs`, `group_args`, `force_override`, `max_depth` |
| `-use_macro` | `debug`, `debug_ast`, `alias`, `force_override` |

*Errors*

| Error | Meaning |
| --- | --- |
| `macro_override` | macro name or alias already exists and `force_override` was not set |
| `max_macro_expansion_depth_exceeded` | nested macro expansion exceeded `max_depth` |
| `invalid_macro_return` | macro returned AST that does not fit the current traversal position |
| `invalid_import_macro_attr` | invalid `-import_macro` attribute |
| `import_macro_failed` | imported macro module could not be loaded |

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
