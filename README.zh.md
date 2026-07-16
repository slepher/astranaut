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

`macro.hrl` 启用 `astranaut_macro` parse transform。

实现上，`astranaut_macro` 负责源码有序的 pass 与宏环境，
`astranaut_macro_expander` 负责 attribute/function 共用的宏匹配、调用和递归展开，
`astranaut_local_macro` 负责本地闭包及 generation 生命周期。所有展开路径共用同一个
expander。

*export_macro*

&emsp;&emsp;在定义宏的模块中使用。导出的宏可以被其它模块 import。仅声明
`-export_macro` 不会使该函数在定义模块中成为本地宏：本模块内的非限定调用仍是
普通 Erlang 函数调用。如果需要同时支持本地和导出宏行为，应对同一 FA 再声明
`-local_macro`。

```erlang
-export_macro([MacroA/A, MacroB/B]).
-export_macro({Macro/A, macro_definition_opts()}).
-export_macro({[MacroA/A, MacroB/B], macro_definition_opts()}).
```

*local_macro*

&emsp;&emsp;把本地函数声明为宏，但不导出这些函数。transformer 会冻结每个声明函数
及通过静态本地调用发现的函数闭包。同一个 declaration 中的多个函数共享声明位点
的宏环境；这些成员之间的调用保持为普通 Erlang 调用。该 declaration 只是一次共享
注册输入：扫描完成后每个 `Function/Arity` 都是独立宏条目，不再保留持久的分组身份。

```erlang
-local_macro([MacroA/A, MacroB/B]).
-local_macro({Macro/A, local_macro_opts()}).
-local_macro({[MacroA/A, MacroB/B], local_macro_opts()}).
```

本地宏闭包 forms 使用 `-local_macro` declaration 之前已经生效的宏环境和可注入
attributes 展开。源码后方的函数仍可被发现为闭包 helper，但后续 import、use、
options 和 attributes 不会反向改变该声明位点环境。

静态闭包发现只跟随 `helper(Arg)` 这类直接本地调用，不会推断 `fun helper/1`、动态
选择的函数或 `apply/3` 等间接引用；这些 helper 必须通过 `extra_functions` 显式加入。

*local_macro_retain*

&emsp;&emsp;在转换后的模块中保留本地宏函数或 helper。命中冻结本地宏闭包中的任一
成员，会保留该闭包的全部函数和 specs。`-export` 与 `-export_macro` 也会成为 retain
roots。若 `local_macro_retain` 命中的是不属于任何本地宏闭包的普通函数，则不会产生
额外效果，并产生 `ineffective_local_macro_retain` warning；如果 FA 在模块中根本没有
定义，则改为产生 `undefined_local_macro_retain`。

保留的冻结函数既属于 local macro 定义，也属于最终模块中的 function。因此它会从
原始 form 在最终 function context 下重新展开，并与声明位点建立的 canonical 结果
比较；若结果不一致，应报告冲突，而不是任意选择其中一个 context。

```erlang
-local_macro_retain([Macro/A, Helper/B]).
```

*import_macro*

&emsp;&emsp;声明导出宏的模块。具体宏选择和调用选项通过 `-use_macro` 配置。

```erlang
-import_macro(Module).
```

*use_macro*

&emsp;&emsp;使用 imported 或 local macro，并附加调用选项。

```erlang
-use_macro({Macro/A, use_macro_opts()}).
-use_macro({[MacroA/A, MacroB/B], use_macro_opts()}).
-use_macro({Module, Macro/A, use_macro_opts()}).
-use_macro({Module, [MacroA/A, MacroB/B], use_macro_opts()}).
```

*exec_macro*

&emsp;&emsp;执行宏并把结果加入当前 AST。

```erlang
-exec_macro({Macro, Arguments}).
-exec_macro({Module, Macro, Arguments}).
```

*macro_options*

&emsp;&emsp;声明按源码顺序生效的模块级宏默认值，以及最终模块 debug 开关。

```erlang
-macro_options(macro_options_opts()).
```

`debug`、`debug_ast` 和 `max_depth` 是逐宏默认值：它们会复制到该
`-macro_options` form 之后 import 的每个 external macro，以及之后声明的每个 local
macro。它们不会成为宏函数的实参，也不会反向修改此前已经 import 或声明的宏。
definition 位点的 `max_depth` 会覆盖 global 默认值；`use_macro` 可以为选中的宏覆盖
`debug` 和 `debug_ast`。

`debug_module` 和 `debug_module_ast` 只属于模块级最终输出。源码顺序下的最终值在展开
结束后打印完整 transformed module，不是逐宏配置。

*Option maps*

options 按 attribute 分区；不存在一份同时适用于四种 attribute 的通用 option map。

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

每类 option map 也可以写成 proplist。

*Debug / DebugAst*

&emsp;&emsp;在编译期打印宏调用生成的代码或 AST。

*Alias*

&emsp;&emsp;使用 `Alias(Arguments)` 调用宏，而不是 `Module:Macro(Arguments)`。

*InjectAttrs*

&emsp;&emsp;把模块 attributes 作为额外参数传给宏函数。attribute 宏只能看到其调用点
之前已经通过源码顺序扫描的 attributes；本地宏闭包 forms 使用 `-local_macro`
declaration 之前可见的 attributes；普通 function 宏在 attribute scan 完成后展开，
使用最终模块 forms。

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

*ExtraFunctions*

&emsp;&emsp;显式把本地函数加入 local macro 静态发现的闭包。当 helper 无法从宏函数的
普通本地调用中发现时使用。每一项必须是模块中已定义的 `Function/Arity`。

```erlang
-local_macro({macro/1, [{extra_functions, [helper/1]}]}).
```

*InternalFunction*

&emsp;&emsp;让 `local_macro` declaration 之前立即可见的指定宏，在该宏的冻结闭包中
作为普通 Erlang 函数调用。`false` 不选择任何宏，`true` 选择声明点可见的全部宏；
列表可用 `Function/Arity` 选择本地调用 key，或用 `{Module, Function, Arity}` 选择
远程调用 key。三元组是 attribute term 中对 `Module:Function/Arity` 的表示。

列表中的每个 key 都必须在 declaration 位点解析为宏；仅存在同名普通模块函数并不
满足条件。本地宏 key 保持为本地函数调用。如果 `Function/Arity` 通过现有
`use_macro` 的 `alias` 解析到 imported macro，冻结时会把该调用改写回原始
`Module:Function(...)`，并从展开环境同时移除 alias key 与原始远程宏 key。因此它会
正常调用导入模块的函数，而不再作为宏展开。retain 的函数在最终 function context 中
重展开时也应用相同的过滤和改写。

```erlang
-import_macro(macro_uniform_a).
-use_macro({macro_uniform_a, to_a/1, [{alias, direct_to_a}]}).
-local_macro({outer/1, [{internal_function, [direct_to_a/1]}]}).

outer(Ast) ->
    direct_to_a(Ast). % 冻结为 macro_uniform_a:to_a(Ast)
```

如果多个 declaration 的闭包重叠，它们必须为每个共享 helper form 使用兼容的
internal macro 策略。

*Option Scope*

| Option | `macro_options` | `export_macro` | `local_macro` | `use_macro` | 作用 |
| --- | --- | --- | --- | --- | --- |
| `debug`, `debug_ast` | global 默认值 | — | — | use 位点覆盖 | 以源码或 AST 打印每次宏展开结果 |
| `debug_module`, `debug_module_ast` | 仅模块级 | — | — | — | 打印最终 transformed module |
| `max_depth` | global 默认值 | definition 覆盖 | definition 覆盖 | — | 最大嵌套展开深度 |
| `order` | — | definition | definition | — | 嵌套展开顺序 |
| `as_attr` | — | definition | definition | — | 把宏暴露为 attribute |
| `inject_attrs` | — | definition | definition | — | 把选中的模块 attributes 追加到调用实参 |
| `group_args` | — | definition | definition | — | 把源码实参作为一个列表传入 |
| `force_override` | — | definition | definition | use 位点 | 允许当前宏映射替换冲突 key |
| `alias` | — | — | — | 仅 use | 在 use 位点重命名选中的宏 |
| `extra_functions` | — | — | 仅 local definition | — | 把静态扫描未发现的 helper 加入本地闭包 |
| `internal_function` | — | — | 仅 local definition | — | 把声明点可见的指定宏调用作为普通函数 |

`extra_functions` 和 `internal_function` 描述本地宏闭包的构造及其冻结宏环境，仅在定义通过
`-local_macro` 声明时有意义；单独的 `-export_macro` 只负责发布宏供其它模块导入。
两者也都不是模块级 `-macro_options`。若把它们传给 `-macro_options` 或
`-export_macro`，它们会作为 unexpected options 报告并忽略。

*展开阶段与源码顺序*

宏展开由按顺序执行的 attribute pass 和随后的 function-body pass 组成：

1. external attribute 宏和已经可调用的 local attribute 宏在同一次扫描中从左到右
   处理。宏生成的 forms 插回当前队列位置，并在剩余源码 forms 之前按顺序扫描。
2. `import_macro`、`use_macro`、`macro_options` 以及宏生成的这些 forms 只影响其后
   扫描到的 forms；已经处理过的 attributes 不会回扫。
3. 宏生成的 `local_macro` declaration 会进入同一扫描，并可供后续 attribute 调用。
   attribute 调用只调用 declaration 位点确定的 canonical 定义，不会在调用点展开或
   重新定义 local macro；多个 attribute 调用复用同一定义和 callable generation。
4. attribute pass 和 local macro 收尾完成后，普通函数体使用完整宏环境递归展开。
   因此普通函数可以使用最终全部可调用的 local macros，而冻结的 local macro 闭包
   forms 保留声明位点发现的本地宏依赖。被 retain 的冻结函数也通过该最终 context，
   并且必须重现 declaration context 下的 canonical 结果。

这种源码顺序与 `order` 选项是两个概念：源码顺序决定声明和宏环境更新何时可见，
`inner` 或 `outer` 则决定单个宏调用中的嵌套展开顺序。

*Errors*

| Error | Meaning |
| --- | --- |
| `macro_override` | 宏名或 alias 已存在，且未设置 `force_override` |
| `max_macro_expansion_depth_exceeded` | 嵌套宏展开超过 `max_depth` |
| `invalid_macro_return` | 宏返回的 AST 不适合当前位置 |
| `invalid_import_macro_attr` | `-import_macro` attribute 无效 |
| `import_macro_failed` | 导入的宏模块无法加载 |
| `invalid_extra_functions` | `extra_functions` 中存在模块未定义的函数 |
| `undefined_internal_functions` | `internal_function` 中存在 declaration 位点不可见的宏 key |
| `duplicate_local_macro_declaration` | local macro FA 被重复声明，包括同一 declaration 内的重复项 |
| `conflicting_internal_function_policy` | 重叠的 local macro 闭包为共享 helper 指定了不兼容的 internal macro 环境 |
| `conflicting_local_macro_closure_environment` | 被保留或复用的 local macro 闭包在另一个必要环境中产生不同展开结果 |
| `conflicting_local_macro_whitelist` | 冻结闭包再次展开时观察到的 local macro 依赖集合不同 |
| `illegal_locked_form_mutation` | attribute 展开尝试替换已冻结的 local macro 闭包 form |
| `illegal_macro_environment_mutation` | local macro 展开在不允许修改环境的位置生成了宏环境 form |
| `illegal_local_macro_definition_mutation` | local macro 展开尝试修改已锁定的 local macro 定义 |
| `local_macro_module_in_use` | 旧代码仍在使用中，无法安全替换生成的 local macro 模块 |

*Warnings*

| Warning | Meaning |
| --- | --- |
| `undefined_local_macro_retain` | 显式 retain 的 FA 在模块中不存在 |
| `ineffective_local_macro_retain` | 显式 retain 的 FA 不属于任何 local macro 闭包，因此只能按普通函数处理 |

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
