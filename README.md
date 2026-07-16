
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

Internally, `astranaut_macro` owns the source-ordered passes and macro
environment, `astranaut_macro_expander` owns shared attribute/function macro
matching and recursive expansion, and `astranaut_local_macro` owns local
closure and generation lifecycle. All expansion paths use the same expander.

*export_macro*

&emsp;&emsp;used in the module where macros are defined. Exported macros can be
imported by other modules. This declaration alone does not make the function a
local macro in its defining module: an unqualified call there remains an
ordinary Erlang function call. Combine it with `-local_macro` on the same FA
when both local and exported macro behaviour are required.

```erlang
-export_macro([MacroA/A, MacroB/B]).
-export_macro({Macro/A, macro_definition_opts()}).
-export_macro({[MacroA/A, MacroB/B], macro_definition_opts()}).
```

*local_macro*

&emsp;&emsp;declare local functions as macros without exporting them. The
transformer freezes each declared function and its statically discovered local
function closure. A declaration containing several functions gives those
functions the same declaration-time macro environment; calls between members
of that declaration remain ordinary Erlang calls. The declaration is only a
shared registration input: after the scan, each `Function/Arity` has its own
macro entry and no persistent group identity is retained.

```erlang
-local_macro([MacroA/A, MacroB/B]).
-local_macro({Macro/A, local_macro_opts()}).
-local_macro({[MacroA/A, MacroB/B], local_macro_opts()}).
```

Local macro closure forms are expanded using the macro environment and
injectable attributes that appear before the `-local_macro` declaration.
Functions later in the source may be discovered as closure helpers, but later
imports, uses, options, and attributes do not change that declaration-time
environment.

Static closure discovery follows direct local call expressions such as
`helper(Arg)`. It does not infer indirect references such as `fun helper/1`,
dynamically selected functions, or `apply/3`; add those helpers explicitly with
`extra_functions`.

*local_macro_retain*

&emsp;&emsp;retain a local macro function or helper in the transformed module.
Retaining any member of a frozen local macro closure retains that closure's
functions and specs. `-export` and `-export_macro` are also retain roots.
Applying `local_macro_retain` to an ordinary function outside every local macro
closure has no extra effect and produces an
`ineffective_local_macro_retain` warning. Referring to an FA that is not defined
in the module instead produces `undefined_local_macro_retain`.

A retained frozen function is both part of its local macro definition and a
function in the final module. It is therefore expanded again from its original
form under the final function context and checked against the canonical result
established from its declaration context. A mismatch is an intentional
conflict rather than a choice of one context over the other.

```erlang
-local_macro_retain([Macro/A, Helper/B]).
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
-use_macro({Macro/A, use_macro_opts()}).
-use_macro({[MacroA/A, MacroB/B], use_macro_opts()}).
-use_macro({Module, Macro/A, use_macro_opts()}).
-use_macro({Module, [MacroA/A, MacroB/B], use_macro_opts()}).
```

*exec_macro*

&emsp;&emsp;execute macro and add result to current ast.

```erlang
-exec_macro({Macro, Arguments}).
-exec_macro({Module, Macro, Arguments}).
```

*macro_options*

&emsp;&emsp;declare source-ordered module-level macro defaults and final module
debug switches.

```erlang
-macro_options(macro_options_opts()).
```

`debug`, `debug_ast`, and `max_depth` are per-macro defaults. They are copied
into every external macro imported and every local macro declared after the
`-macro_options` form. They do not become arguments to the macro function, and
they do not retroactively change macros already imported or declared.
Definition-level `max_depth` overrides the global default; `use_macro` can
override `debug` and `debug_ast` for the selected macro.

`debug_module` and `debug_module_ast` are module-only switches. Their final
source-ordered values print the complete transformed module after expansion;
they are not per-macro settings.

*Option maps*

Options are scoped by attribute; there is no single option map accepted by all
four attributes.

```erlang
macro_options_opts() ::
  #{debug => boolean(),
    debug_ast => boolean(),
    debug_module => boolean(),
    debug_module_ast => boolean(),
    max_depth => non_neg_integer()}.

macro_definition_opts() ::
  #{order => Order,
    inject_attrs => InjectAttrs,
    as_attr => AsAttr,
    group_args => GroupArgs,
    force_override => ForceOverride,
    max_depth => MaxDepth}.

local_macro_opts() ::
  #{order => Order,
    inject_attrs => InjectAttrs,
    as_attr => AsAttr,
    group_args => GroupArgs,
    force_override => ForceOverride,
    max_depth => MaxDepth,
    extra_functions => [Function/Arity],
    internal_function => boolean() |
                         [Function/Arity | {Module, Function, Arity}]}.

use_macro_opts() ::
  #{debug => boolean(),
    debug_ast => boolean(),
    alias => atom(),
    force_override => boolean()}.
```

Each option map may also be written as a proplist.

*Debug*

&emsp;&emsp;print code generated when macro called compile time.

*DebugAst*

&emsp;&emsp;print ast generated when macro called compile time.

*Alias*

&emsp;&emsp; use Alias(Arguments) instead of Module:Macro(Arguments).

*InjectAttrs*

&emsp;&emsp; module attributes as extra args while calling macro. Attribute macros
see only attributes that have already passed the source-ordered attribute scan
at their call site. Local macro closure forms use the attributes visible before
their `-local_macro` declaration. Ordinary function macros are expanded after
the attribute scan and use the final module forms.

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

*ExtraFunctions*

&emsp;&emsp; explicitly add local functions to a local macro's statically
discovered closure. Use this when a helper cannot be discovered from ordinary
local calls in the macro function. Each entry must be `Function/Arity` and must
exist in the module.

```erlang
-local_macro({macro/1, [{extra_functions, [helper/1]}]}).
```

*InternalFunction*

&emsp;&emsp; make selected macros visible immediately before a `local_macro`
declaration behave as ordinary Erlang function calls inside that macro's
frozen closure. `false` selects none, `true` selects all macros visible at that
point, and a list selects exact local call keys as `Function/Arity` or remote
call keys as `{Module, Function, Arity}`. The tuple is the attribute-term
representation of `Module:Function/Arity`.

Every listed key must resolve to a macro at the declaration point; an ordinary
module function is not sufficient. A local macro key remains a local function
call. If `Function/Arity` resolves through an existing `use_macro` `alias`, the
frozen call is rewritten to the imported macro's original
`Module:Function(...)` call and both the alias and original remote macro keys
are removed from the expansion environment. Thus the imported macro function
is called normally instead of being expanded as a macro. The same filtering
and rewrite are applied when a retained function is re-expanded with the final
function context.

```erlang
-import_macro(macro_uniform_a).
-use_macro({macro_uniform_a, to_a/1, [{alias, direct_to_a}]}).
-local_macro({outer/1, [{internal_function, [direct_to_a/1]}]}).

outer(Ast) ->
    direct_to_a(Ast). % frozen as macro_uniform_a:to_a(Ast)
```

Declarations whose closures overlap must use a compatible internal macro
policy for each shared helper form.

*Option Scope*

| Option | `macro_options` | `export_macro` | `local_macro` | `use_macro` | Role |
| --- | --- | --- | --- | --- | --- |
| `debug`, `debug_ast` | global default | — | — | per-use override | print each macro result as source or AST |
| `debug_module`, `debug_module_ast` | module only | — | — | — | print the final transformed module |
| `max_depth` | global default | definition override | definition override | — | maximum nested expansion depth |
| `order` | — | definition | definition | — | nested expansion order |
| `as_attr` | — | definition | definition | — | expose a macro as an attribute |
| `inject_attrs` | — | definition | definition | — | append selected module attributes to invocation arguments |
| `group_args` | — | definition | definition | — | pass source arguments as one list |
| `force_override` | — | definition | definition | per-use | permit this incoming macro mapping to replace a conflicting key |
| `alias` | — | — | — | use only | rename the selected macro at its use site |
| `extra_functions` | — | — | local definition only | — | add statically undiscovered helpers to a local closure |
| `internal_function` | — | — | local definition only | — | treat declaration-visible macro calls as ordinary functions |

`extra_functions` and `internal_function` describe local macro closure
construction and its frozen macro environment. They are relevant when the definition is declared with
`-local_macro`; an `-export_macro` declaration alone only publishes the macro
for importing modules. Neither option is a module-level `-macro_options`
setting. Supplying either closure option to `-macro_options` or
`-export_macro` reports it as an unexpected option and ignores it.

*Expansion Phases and Source Order*

Macro expansion has an ordered attribute pass followed by a function-body
pass:

1. External and callable local attribute macros are scanned together from left
   to right. Their generated forms are inserted at the current queue position
   and scanned in order before the remaining source forms.
2. `import_macro`, `use_macro`, `macro_options`, and generated forms of those
   kinds affect only forms scanned after them. Already processed attributes are
   not revisited.
3. A generated `local_macro` declaration enters the same scan and can make its
   macro available to later attribute calls. An attribute call only invokes
   the canonical definition captured at that declaration; it never expands or
   redefines the local macro at the call site. Multiple attribute calls reuse
   the same definition and callable generation.
4. After the attribute pass and local macro finalization, ordinary function
   bodies are recursively expanded with the completed macro environment. An
   ordinary function can therefore use all callable final local macros, while
   frozen local macro closure forms preserve their declaration-time local macro
   dependencies. Retained frozen functions also run through this final context
   and must reproduce their declaration-context canonical result.

This source ordering is separate from the `order` option: source order controls
when macro declarations and environment updates become visible, while `inner`
or `outer` controls nested expansion at an individual macro call.

*Errors*

| Error | Meaning |
| --- | --- |
| `macro_override` | macro name or alias already exists and `force_override` was not set |
| `max_macro_expansion_depth_exceeded` | nested macro expansion exceeded `max_depth` |
| `invalid_macro_return` | macro returned AST that does not fit the current traversal position |
| `invalid_import_macro_attr` | invalid `-import_macro` attribute |
| `import_macro_failed` | imported macro module could not be loaded |
| `invalid_extra_functions` | `extra_functions` names functions that are not defined in the module |
| `undefined_internal_functions` | `internal_function` names macro keys not visible at the local declaration point |
| `duplicate_local_macro_declaration` | a local macro FA was declared more than once, including duplicates in one declaration |
| `conflicting_internal_function_policy` | overlapping local macro closures assign incompatible internal macro environments to a shared helper |
| `conflicting_local_macro_closure_environment` | a retained or reused local macro closure expands differently under another required environment |
| `conflicting_local_macro_whitelist` | repeated expansion of a frozen closure observes a different set of local macro dependencies |
| `illegal_locked_form_mutation` | attribute expansion attempted to replace a frozen local macro closure form |
| `illegal_macro_environment_mutation` | local macro expansion generated a macro-environment form where environment mutation is not allowed |
| `illegal_local_macro_definition_mutation` | local macro expansion attempted to change a locked local macro definition |
| `local_macro_module_in_use` | the generated local macro module cannot be safely replaced while its old code is still in use |

*Warnings*

| Warning | Meaning |
| --- | --- |
| `undefined_local_macro_retain` | an explicitly retained FA is not defined in the module |
| `ineffective_local_macro_retain` | an explicitly retained FA belongs to no local macro closure and is therefore handled only as an ordinary function |

&emsp;&emsp;define macro as normal erlang functions.  
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
