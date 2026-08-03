# 从 Syn 借鉴适合 Astranaut 的能力

## 结论

Syn 最值得 Astranaut 借鉴的不是 Rust AST 节点本身，而是它对 AST schema、解析、遍历、构造、诊断和版本兼容的组织方式。

推荐优先实现：

1. 用机器可读的 AST schema 生成 traversal 元数据、validator 和 OTP 可用性规则。
2. 增加类型明确的 Parse/Quote fragment API。
3. 建立节点优先的 span 与诊断接口。
4. 从 AST schema 生成分层的 Visit/Fold behavior。
5. 可选增加未知 AST 节点的兼容策略。

不建议直接移植 Syn 的 `VisitMut`、借用/lifetime 模型、`Punctuated` 或 Rust hygiene。这些能力依赖 Rust 的类型系统、token 模型或所有权语义，与 Erlang abstract forms 不匹配。

## 1. AST Schema 与代码生成

这是收益最高的改进。

Astranaut 的 AST 处理包含两类职责：

- AST 的投影与重建：`subtrees/1`、`update_tree/2`、`revert/1`。
- child 的语义与合法性：`child_specs/3`、`node_roles/1`、slot
  validator 和 OTP 版本规则。

投影与重建应继续以 OTP `erl_syntax` 为权威实现。Astranaut 不重复生成
所有节点的 `subtrees/1` 和 `update_tree/2` clauses，只为已确认的 OTP
不对称行为保留局部 compatibility adapter。schema 则作为 child 语义、
validator 和版本合法性的单一来源。

可以借鉴 Syn 的机器可读 AST 描述，为 Astranaut 建立这样的 schema：

```erlang
#{
    type => call,
    since => 21,
    roles => [expression, guard],
    children => [
        #{slot => callee, role => expression, cardinality => one},
        #{slot => args, role => expression, cardinality => many}
    ]
}.
```

对于存在 OTP 差异的节点，可以显式记录版本条件：

```erlang
#{
    type => maybe_expr,
    since => 25,
    roles => [expression],
    children => [
        #{slot => body, role => expression, cardinality => many},
        #{slot => else_clauses, role => clause, cardinality => many,
          optional => true}
    ]
}.
```

由 schema 自动生成：

```text
AST schema
├── child layout / child_specs 元数据
├── node_roles
├── slot validators
├── visitor/fold dispatch
├── OTP node/format/slot availability rules
├── abstract-form documentation
└── coverage tests

erl_syntax
├── subtrees/update_tree
└── ordinary-node projection/reconstruction

Astranaut compatibility adapters
├── confirmed erl_syntax asymmetries only
└── focused symmetry/round-trip regression tests
```

### 必须保持的不变量

- `subtrees(Node)` 的结果必须能由 `update_tree(Node, Subtrees)` 重建为
  语义等价的 abstract form，允许 OTP 规定的 representation 或 annotation
  normalization；该不变量用于验证 OTP authority 和局部 adapter，
  不意味着 Astranaut 要自行生成拆装代码。
- 每个 child slot 必须声明 cardinality 和 role。
- 每个可替换 slot 必须具有 validator。
- schema 中的 OTP 版本必须能生成测试条件。
- 未识别的节点不得在严格模式下静默作为普通叶节点。
- 生成代码应保持当前公开 API，避免强迫用户迁移。

### 预期收益

- 新 OTP AST 节点由 `erl_syntax` 负责拆装，Astranaut 只需补充 schema 语义；
  只有发现真实不对称时才增加 adapter。
- traversal 和 validator 不会分别遗漏节点。
- 可以审计 schema layout 与 OTP `subtrees/1` 的一致性，并对 adapter
  执行定向 round-trip 测试。
- 可以自动生成 OTP 21～当前版本的测试矩阵。
- 后续 Visit/Fold API 不需要手写大量节点分派代码。

## 2. 类型明确的 Parse Fragment API

Syn 的 `Parse` 和 `parse_quote!` 允许调用者明确指定目标 AST 类型。Astranaut 可以提供对应的 fragment parser：

```erlang
astranaut_parse:expr(Code).
astranaut_parse:pattern(Code).
astranaut_parse:guard(Code).
astranaut_parse:type(Code).
astranaut_parse:clause(Code).
astranaut_parse:form(Code).
astranaut_parse:forms(Code).
```

建议统一返回 Astranaut result：

```erlang
-type parse_return(A) ::
    {ok, A} |
    {error, astranaut_error:struct()}.
```

示例：

```erlang
case astranaut_parse:expr("A + B") of
    {ok, Expr} -> Expr;
    {error, Error} -> handle_error(Error)
end.
```

内部处理流程：

```text
源码片段
  → erl_scan/erl_parse/merl
  → 显式 root role validation
  → 位置规范化
  → Astranaut result
```

### API 要求

- 解析错误必须保留输入位置。
- parser 不应因为普通输入错误使 parse transform 崩溃。
- `expr`、`pattern`、`type` 等入口必须执行对应 root validator。
- 应允许传入起始位置和文件名。
- 应提供文本、binary 和已有 token 输入形式时的清晰转换规则。

## 3. 类型明确的 Quote API

当前 `quote(Code)` 很灵活，但生成结果的合法角色常依赖所在上下文。可以增加显式 fragment quote：

```erlang
quote_expr(Code)
quote_pattern(Code)
quote_guard(Code)
quote_type(Code)
quote_clause(Code)
quote_form(Code)
quote_forms(Code)
```

函数层 API 可以是：

```erlang
astranaut_quote:quoted_expr(Node, Options).
astranaut_quote:quoted_pattern(Node, Options).
astranaut_quote:quoted_type(Node, Options).
astranaut_quote:quoted_form(Node, Options).
```

显式 Quote 应在构造点完成校验：

```text
quote syntax
  → unquote/binding/splicing
  → 构造 abstract form
  → 按指定 role 校验
  → 应用位置策略
```

建议同时提供不抛异常的安全版本：

```erlang
astranaut_quote:try_quoted(Node, #{role => expression}).
%% {ok, Expr} | {error, Diagnostics}
```

现有 quote pattern matching、unquote 和 splicing 能力应保留；它们是 Astranaut 相比 Syn `quote!` 更强的部分，不需要按 Syn 的较弱模型重写。

## 4. Spanned 与节点优先的诊断 API

Syn 的诊断接口允许直接从 AST 节点取得 span、创建错误并合并多个错误。Astranaut 已经有位置传播、formatter 和错误聚合，但可以增加更直接的节点优先接口：

```erlang
astranaut_diagnostic:error(Node, Reason).
astranaut_diagnostic:error(Node, Formatter, Reason).
astranaut_diagnostic:warning(Node, Reason).
astranaut_diagnostic:combine(Diagnostics).
```

建议的统一诊断结构：

```erlang
#{
    severity => error,
    code => invalid_macro_return,
    location => Location,
    end_location => EndLocation,
    file => File,
    node_type => call,
    role => expression,
    slot => argument,
    origin => MacroOrigin,
    reason => Reason
}.
```

`end_location` 应允许为 `undefined`。Erlang abstract forms 不一定包含完整 token range，不应为了模仿 Rust span 而伪造范围。

Traversal 中可以提供：

```erlang
astranaut_traverse:error(Node, Reason).
astranaut_traverse:warning(Node, Reason).
```

这两个接口自动读取当前 traversal context 中的：

- file
- formatter
- parent role
- child slot
- macro origin/current macro
- 当前 traversal phase

这样 callback 不必手工执行 `get_pos`、选择 formatter 并补充上下文。

## 5. 分层 Visit/Fold Behavior

Syn 将遍历分为只读 `Visit`、可变 `VisitMut` 和 owning `Fold`。Erlang AST 不可变，因此 Astranaut 不需要移植 `VisitMut`；现有能力可以对应为：

| Syn | Astranaut |
| --- | --- |
| `Visit` | `reduce/search` |
| `VisitMut` | 不直接适用 |
| `Fold` | `map/mapfold` |

值得借鉴的是“按节点种类提供 hook，同时允许默认递归”。可以增加可选 behavior：

```erlang
-callback visit_form(Node, Context, State) -> visit_return().
-callback visit_expression(Node, Context, State) -> visit_return().
-callback visit_pattern(Node, Context, State) -> visit_return().
-callback visit_type(Node, Context, State) -> visit_return().
-callback visit_call(Node, Context, State) -> visit_return().
-callback visit_case(Node, Context, State) -> visit_return().
```

示例：

```erlang
visit_call({call, Pos, {atom, _, old_name}, Args}, _Context, State) ->
    {{call, Pos, {atom, Pos, new_name}, Args}, State};
visit_call(Node, _Context, State) ->
    {Node, State}.
```

建议提供两层 hook：

```text
role hook
├── visit_form
├── visit_expression
├── visit_pattern
├── visit_guard
└── visit_type

node-kind hook
├── visit_call
├── visit_case
├── visit_function
└── ...
```

推荐调用顺序：

```text
通用 enter hook
  → role hook
  → node-kind hook
  → children
  → node-kind leave hook
  → role leave hook
  → 通用 leave hook
```

这些 callback 和默认实现必须由 AST schema 生成，不应手写维护数百个函数。

简单变换仍应继续使用现有 `map/reduce` 匿名函数 API；behavior 面向大型、可复用 transformer，不应替代现有入口。

## 6. 未知节点与前向兼容

Syn 通过 non-exhaustive AST 和 `Verbatim` 处理尚未完整建模的语法。Astranaut 面对新 OTP abstract format 时，可以增加显式策略：

```erlang
#{unknown_node => error | preserve | leaf}.
```

- `error`：报告未知节点，默认值。
- `preserve`：保留整个节点，不进入其内部。
- `leaf`：把节点当作叶节点处理，仅适合明确允许的分析任务。

默认必须是 `error`。如果默认静默跳过，未知节点内部的宏、变量或需要 rebinding 的 pattern 可能不被处理，容易产生错误语义。

可以进一步让 schema 区分：

```erlang
#{
    unknown_node => preserve,
    allowed_unknown_roles => [type],
    report_warning => true
}.
```

## 7. ParseStream 风格组合解析

Syn 的 `ParseStream` 提供 token peek、expect、delimiter 和 separated-list 等组合解析能力。Astranaut 只有在引入自定义 attribute DSL 时才需要类似接口，例如：

```erlang
-my_transform(rename old_name to new_name, recursive).
```

可能的 API：

```erlang
astranaut_parser:peek(Token, Stream).
astranaut_parser:expect(Token, Stream).
astranaut_parser:parse_atom(Stream).
astranaut_parser:parse_list(Parser, Separator, Stream).
astranaut_parser:optional(Parser, Stream).
```

目前大多数 Astranaut attribute 已被 Erlang compiler 解析为 term，现有 option map 和 validator 已足够。因此该功能属于低优先级，除非确实要建立新的文本 DSL。

## 8. 不建议移植的 Syn 能力

### `VisitMut`

Erlang 数据不可变，`map`/`fold` 更符合语言模型。模拟原地修改只会让 API 语义混乱。

### 借用和 lifetime visitor

这是 Rust 所有权系统的需要。BEAM term 的共享与垃圾回收模型不需要对应抽象。

### `Punctuated<T, P>`

Erlang abstract forms 通常不保留逗号、分号等 punctuation。除非 Astranaut 改为操作 lossless CST，否则无法获得同等价值。

### Rust hygiene/span 模型

Rust token hygiene 与 Erlang 变量、module 和 parse transform 语义不同。只能借鉴“位置优先诊断”，不能移植 hygiene 规则。

### `ToTokens`

Astranaut 已基于 `erl_syntax`、`erl_prettypr` 和 abstract forms 工作，不需要建立 Rust 式 TokenStream trait。

### feature-gated AST 子集

Syn 通过 crate features 控制 derive/full/visit 等 AST 能力。Astranaut 的主要兼容维度是 OTP 版本，适合使用 schema 的 `since`、`until` 和 capability 信息，而不是复制 feature flags。

## 9. 实施顺序

### Phase 1：Schema 原型

1. 选择 5～10 个常见节点建立 schema。
2. 生成 child layout、`child_specs` 元数据和 validators。
3. 对照 OTP `erl_syntax:subtrees/1` 审计 group 数量与 cardinality。
4. 对现有 compatibility adapters 运行定向 symmetry/round-trip tests。
5. 确认生成的 dispatch 代码性能不低于当前实现。

### Phase 2：覆盖全部 AST

1. 把当前 OTP 21～当前版本节点迁入 schema。
2. 自动生成版本条件。
3. 自动生成 coverage matrix。
4. 将 `erl_syntax` 作为 projection/reconstruction oracle。
5. 删除重复的手写规则，但保留有回归证据的局部 adapters。

### Phase 3：Typed Parse/Quote

1. 实现 `astranaut_parse` fragment API。
2. 为 Quote 增加显式 role 入口。
3. 统一 parse、quote 和 traversal validation 的错误结构。
4. 保持原有 `quote(Code)` 完全兼容。

### Phase 4：Diagnostic 与 Visitor

1. 增加节点优先诊断构造器。
2. 让 traversal context 自动补充诊断信息。
3. 从 schema 生成 role/node-kind visitor hooks。
4. 先用于一个真实 transformer，再决定公开 API 的最终形状。

## 10. 优先级总结

| 能力 | 收益 | 成本 | 优先级 |
| --- | --- | --- | --- |
| AST schema/codegen | 很高 | 中高 | P0 |
| Typed Parse fragments | 高 | 中 | P1 |
| Typed Quote fragments | 高 | 中 | P1 |
| 节点优先 diagnostics | 高 | 中 | P1 |
| Generated Visit/Fold | 中高 | 中高 | P2 |
| Unknown-node policy | 中 | 低 | P2 |
| ParseStream combinators | 低到中 | 中 | P3 |
| `Punctuated`/`VisitMut`/lifetime | 低或不适用 | 高 | 不做 |

最终建议是先做 AST schema/codegen，但不重新实现 OTP 已提供的 AST
projection/reconstruction。schema 聚焦 traversal 语义、validator 和版本
合法性；`erl_syntax` 负责普通节点拆装；Astranaut 只保留必要的局部
compatibility adapters。这为后续 Typed Parse、Typed Quote 和 generated visitor
提供共同基础，同时避免承担完整的 OTP 版本拆装成本。
