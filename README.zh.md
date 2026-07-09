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

&emsp;&emsp;当前 traversal step。`traverse` 选项为 `all` 时尤其有用。

```erlang
  step()  :: pre | post | leaf.
```

*NodeRole*

&emsp;&emsp;当前节点的 traversal role。它不是 Erlang AST type。

```erlang
  node_role() :: form | expression | pattern | guard | type | clause.
```

&emsp;&emsp;如果需要具体 Erlang AST type，使用 `astranaut_syntax:type/1`。

*Validator*

&emsp;&emsp;`validator` 是 traversal 传播的 opaque 位置令牌。普通用户不需要理解或构造它。如果 walker 需要自行校验替换节点，把 `Attr.validator` 原样传给 `astranaut_syntax`。

```erlang
case astranaut_syntax:validate_node(NewNode, Validator) of
    ok -> NewNode;
    {error, Detail} -> {error, Detail}
end.
```

&emsp;&emsp;如果需要递归规范化，把 attr 作为上下文传入：

```erlang
case astranaut_syntax:normalize(NewNode, Validator, #{attr => Attr}) of
    {ok, NewNode1} -> NewNode1;
    {error, Detail} -> {error, Detail}
end.
```

&emsp;&emsp;Traversal 可能在 attr 中包含额外诊断信息。校验替换节点的稳定方式是原样传递 opaque `validator`。

*Attribute*

&emsp;&emsp;当 `node` 与 attribute body 相关时，`attribute` 是 attribute 名称。

*TraverseFunReturn*

```erlang
  traverse_fun_return(SA) :: SA | {error, error()} | {error, SA, error()} |
                            {warning, SA, error()} | {warning, error()} |
                            continue | {continue, SA} |
                            astranaut_walk_return:astranaut_walk_return(A) |
                            astranaut_traverse_m:astranaut_traverse_m(S, A) |
                            astranaut_return_m:astranaut_return_m(A) |
                            astranaut_base_m:astranaut_base_m(A).
```

*Continue*

&emsp;&emsp;如果返回 `continue | {continue, A}`，且当前 `step` 是 `pre`，会跳过当前节点 children，继续下一个节点。`leaf` 或 `post` 阶段不受影响。

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

&emsp;&emsp;提供 `format_error/1` 的错误格式化模块，默认是 `astranaut_traverse`。

*ParseTransform*

&emsp;&emsp;把 `traverse_return(node())` 转换为可直接作为 `parse_transform/2` 返回值的 compiler return 格式。

*Role*

&emsp;&emsp;`role => Role` 是 traversal 的公开参数，用来显式指定根节点 traversal role。当根节点不是完整 form，或根 role 不能安全推断时使用。

```erlang
astranaut:smap(Fun, Expr, #{traverse => pre, role => expression}).
```

*Normalize*

&emsp;&emsp;如果 `normalize => true`，walker 直接返回新节点后，traversal 会在放回当前位置之前校验或规范化这个返回值。该选项只作用于 walker 的直接返回值；因为 child 变化而重建的 parent 不视为 walker 对 parent 的直接替换。

*TraverseStyle*

&emsp;&emsp;pre | post | all | leaf | subtree | none.

*Attr*

&emsp;&emsp;合并到 traversal 上下文中的初始 attrs。

*Uniplate*

&emsp;&emsp;高级 traversal 实现 hook。多数用户不需要该选项。

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

*Advanced*

&emsp;&emsp;熟悉 monad 时可以直接使用更底层的 `map_m`。

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

&emsp;&emsp;`Validator` 通常直接来自 `Attr.validator`。它是内部位置契约，应作为 opaque token 处理。

*validate_node*

&emsp;&emsp;校验当前节点是否满足传入 validator，不递归校验 child 节点。

*normalize*

&emsp;&emsp;校验当前节点，递归规范化 children，通过 `subtrees/1` 和 `update_tree/2` 重建 AST，并返回规范化后的节点。

*Forms*

&emsp;&emsp;guard 校验可能需要 record 定义。`Attr.validator` 不包含原始 forms，traversal callback 也未必能拿到原始 module forms。如果 guard 校验依赖 record，需要显式传入适当的 record forms：

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

&emsp;&emsp;`child_specs/3` 是 traversal 和 normalization 使用的高级 API。它可能包含内部 validator 数据；用户代码通常应保持这些数据 opaque。

# astranaut_uniplate

&emsp;&emsp;`astranaut_uniplate` 是 traversal 内部使用的 uniplate/context 实现模块。大多数用户应通过 `astranaut`、`astranaut_traverse` 和 `astranaut_syntax` 使用 traversal 能力，不应依赖其内部 context 结构。

# monad modules

### astranaut\_traverse\_m

&emsp;&emsp;`astranaut_traverse` 的主 monad。

### astranaut\_base\_m

&emsp;&emsp;带 errors 和 warnings 的 monad，可用于追加错误或警告。

### astranaut\_return\_m

&emsp;&emsp;`astranaut_traverse_m:run(MA, Formatter, State)` 的 monad 结果。可以用 `astranaut_return_m:to_compiler/1` 转为 compiler return 格式，也可以用 `astranaut_return_m:from_compiler/1` 从 compiler return 格式转换回来。

### astranaut\_error\_state

### astranaut\_walk\_return

&emsp;&emsp;`astranaut_traverse:(map_m|map|reduce|map_with_state|mapfold)(Fun, Forms, Opts)` 中 Fun 的返回类型。

# Quote

### quick start

```erlang
-include_lib("astranaut/include/quote.hrl").
```

&emsp;&emsp;可以使用 `quote(Code)` 表示代码对应的 AST。

```erlang
quote(Code) | quote(Code, Options)
```

*Options*

```erlang
  atom() => {atom() => true}
  proplists() => map()
  Pos => #{pos => Pos}
  #{pos => Pos, code_pos => CodePos, debug => Debug}
```

*Pos*

&emsp;&emsp;`Pos` 可以是任意表达式，生成的 AST 会被替换为该位置。

*CodePos*

&emsp;&emsp;如果 `CodePos` 为 true，quote 生成的 AST 会使用 quote 代码自身的位置。

*Debug*

&emsp;&emsp;如果 `Debug` 为 true，quote 生成的 AST 会在编译期打印到控制台。

### unquote

```erlang
unquote(Ast)
unquote = Ast
unquote_splicing(Asts)
unquote_splicing = Asts
```

&emsp;&emsp;`unquote(Var)` 不能出现在函数 clause pattern 中，因此 pattern 中可使用 `unquote = Var` 形式。

### variable binding

&emsp;&emsp;`_@V` 等价于 `unquote(V)`。`_L@Vs` 等价于 `unquote_splicing(Vs)`。`_A@Atom`、`_I@Integer`、`_F@Float`、`_S@String`、`_V@Variable` 可以把普通值绑定成 AST。

### unquote and variable binding in pattern

&emsp;&emsp;quote macro 也可以用于 pattern match。由于 Erlang AST pattern 的限制，pattern 中使用一些特殊形式表示 unquote。

# Macro

*Usage*

```erlang
-include_lib("astranaut/include/macro.hrl").
```

*export_macro*

&emsp;&emsp;在定义宏的模块中使用。导出的宏可以被其它模块 import。

```erlang
-export_macro([MacroA/A, MacroB/B]).
-export_macro({Macro/A, opts()}).
-export_macro({[MacroA/A, MacroB/B], opts()}).
```

*local_macro*

&emsp;&emsp;把本地函数声明为宏，但不导出这些函数。

```erlang
-local_macro([MacroA/A, MacroB/B]).
-local_macro({Macro/A, opts()}).
-local_macro({[MacroA/A, MacroB/B], opts()}).
```

*import_macro*

&emsp;&emsp;声明导出宏的模块。具体宏选择和调用选项通过 `-use_macro` 配置。

```erlang
-import_macro(Module).
```

*use_macro*

&emsp;&emsp;使用 imported 或 local macro，并附加调用选项。

```erlang
-use_macro({Macro/A, opts()}).
-use_macro({[MacroA/A, MacroB/B], opts()}).
-use_macro({Module, Macro/A, opts()}).
-use_macro({Module, [MacroA/A, MacroB/B], opts()}).
```

*exec_macro*

&emsp;&emsp;执行宏并把结果加入当前 AST。

```erlang
-exec_macro({Macro, Arguments}).
-exec_macro({Module, Macro, Arguments}).
```

*macro_options*

&emsp;&emsp;声明模块级宏选项。

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

&emsp;&emsp;`opts()` 也可以写成 proplists。

*Debug / DebugAst*

&emsp;&emsp;在编译期打印宏调用生成的代码或 AST。

*Alias*

&emsp;&emsp;使用 `Alias(Arguments)` 调用宏，而不是 `Module:Macro(Arguments)`。

*InjectAttrs*

&emsp;&emsp;把模块 attributes 作为额外参数传给宏函数。

*Order*

&emsp;&emsp;嵌套宏展开顺序。默认 `inner` 表示先展开内部宏；`outer` 表示先展开外部宏。

*AsAttr*

&emsp;&emsp;使用自定义 attribute 名替代 `-exec_macro`。

*GroupArgs*

&emsp;&emsp;把宏调用参数作为列表传入。

*ForceOverride*

&emsp;&emsp;允许有意覆盖宏 alias。未设置时，宏名冲突会失败并返回 `macro_override`。

*MaxDepth*

&emsp;&emsp;最大嵌套宏展开链深度。模块级默认值为 100。

*Option Scope*

| Attribute | Options |
| --- | --- |
| `-macro_options` | `debug`, `debug_ast`, `debug_module`, `debug_module_ast`, `max_depth` |
| `-export_macro`, `-local_macro` | `as_attr`, `order`, `inject_attrs`, `group_args`, `force_override`, `max_depth` |
| `-use_macro` | `debug`, `debug_ast`, `alias`, `force_override` |

*Errors*

| Error | Meaning |
| --- | --- |
| `macro_override` | 宏名或 alias 已存在，且未设置 `force_override` |
| `max_macro_expansion_depth_exceeded` | 嵌套宏展开超过 `max_depth` |
| `invalid_macro_return` | 宏返回的 AST 不适合当前位置 |
| `invalid_import_macro_attr` | `-import_macro` attribute 无效 |
| `import_macro_failed` | 导入的宏模块无法加载 |

&emsp;&emsp;宏定义为普通 Erlang 函数。宏在编译期由 parse transformer `astranaut_macro` 展开；宏不知道运行期值，传入宏的参数是 Erlang AST。

# Rebinding

```erlang
-include_lib("erlando/include/rebinding.hrl").

-rebinding_all(Opts).
-rebinding_fun(FAs).
-rebinding_fun({FAs, Opts}).
```

&emsp;&emsp;`-rebinding_all` 和 `-rebinding_fun` 定义 rebinding 作用域。`debug` 选项会打印 rebinding 后的代码。

*Rebinding Rules*

&emsp;&emsp;当 pattern variable 已经使用过时会被重命名，包括函数参数、match pattern、list comprehension pattern、bitstring comprehension pattern。同一 pattern scope 中同名变量会重命名为同一个新名字。

# Struct

*Usage*

```erlang
-include_lib("erlando/include/struct.hrl").
-record(test, {name = hello, value}).
-astranaut_struct([test]).
```

*Desc*

&emsp;&emsp;把 Erlang record 转换为类似 Elixir struct 的 map。

```erlang
new() ->
  #test{}.
```

&emsp;&emsp;会转换为：

```erlang
new() ->
  #{'__struct__' => test, name => hello, value => undefined}.
```

*Struct Options*

&emsp;&emsp;`-astranaut_struct` 可带额外选项：

&emsp;&emsp;`non_auto_fill`：未定义或未初始化的字段不会自动填为 `undefined`。

&emsp;&emsp;`enforce_keys`：构造 struct 时必须给定指定字段，否则编译失败。

*Macros*

```erlang
astranaut_struct:from_record(StructName, Record) -> Struct.
astranaut_struct:to_record(StructName, Struct) -> Record.
astranaut_struct:from_map(StructName, Struct) -> Struct.
astranaut_struct:update(StructName, Struct) -> Struct.
```
