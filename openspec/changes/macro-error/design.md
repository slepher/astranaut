## Context

registry 在分析 macro provider 时，以是否导出 `format_error/1` 选择 descriptor 的 `formatter`：有则为 provider module，无则为 `astranaut_macro`。这个选择只应表示用户 macro 是否拥有自身领域诊断。

当前 `astranaut_macro_expander:expand_macro_with/3` 把 descriptor formatter 施加到整个 `invoke_macro_function/1` computation。成功调用返回的 `astranaut_return` 错误因此正确归用户 formatter；但 `invoke_macro_function/1` 捕获用户函数异常后构造的 `{macro_exception, ...}` 也继承同一 formatter。测试夹具只好为这个框架 reason 增加特定条款并调用 `astranaut_macro:format_error/2`。

现有后续校验路径已经体现目标边界：`process_macro_return/3` 产生的 `invalid_macro_return` 在用户 formatter 的局部 invocation computation 之外运行，使用外层 `astranaut_macro`。本 change 将异常包装路径收敛到相同规则。

## Goals / Non-Goals

**Goals:**

- formatter 在 reason 产生时按语义所有者确定。
- 用户 macro 主动返回的领域错误和 warning 使用 registry formatter。
- `macro_exception` 及其他框架 reason 固定使用 `astranaut_macro`。
- 用户 formatter 无需引用、代理或理解 `astranaut_macro` 的 reason。
- 保持位置、文件、reason、异常 payload、恢复和累计诊断行为。

**Non-Goals:**

- 不建立按 reason 动态探测的 formatter registry。
- 不建立“用户 formatter 未匹配后再尝试 `astranaut_macro`”的 fallback 链。
- 不改变 `dispatch_error/3`、`format_default_error/2` 或编译器 formatter 协议。
- 不改变 macro 返回 AST、递归展开、local macro generation 或 registry 的 macro 选择语义。

## Decisions

### 在异常包装的产生点覆盖 formatter

`invoke_macro_function/1` 捕获异常并构造 `macro_exception` 时，失败 computation MUST 显式使用 `astranaut_macro`。外层调用位置仍由现有 `update_pos` 路径附加；成功调用返回的 computation 不覆盖 formatter，继续继承 descriptor 中的用户 formatter。

这样，同一个调用的两类结果保持清晰分离：

```text
erlang:apply 成功
  └─ 执行用户返回的 astranaut_return/traverse computation
       └─ error/warning → registry formatter

erlang:apply 抛出异常
  └─ 框架包装 macro_exception
       └─ error → astranaut_macro
```

选择在产生点覆盖，而不是在 realization 或 formatter 调用阶段按 tuple 形状重路由，因为错误结构已经保存 formatter 身份；晚期推断会把所有 formatter 与 reason 表示耦合，也无法可靠区分相同 term 的不同领域含义。

### registry formatter 只代表用户领域协议

保留 `astranaut_macro_registry:formatter_opts/3` 的现有选择规则。provider 导出 `format_error/1` 时，registry formatter 仍为该 module；否则为 `astranaut_macro`。这个 formatter 只作用于用户 macro 成功返回的诊断 computation，不获得框架 reason 的所有权。

local macro 继续把源码模块的公开 formatter closure 编译到生成模块；用户领域诊断使用生成模块身份。生成模块的私有 helper 和导出协议不因本 change 改变。

### 不使用 formatter fallback 链

每个 formatter 继续通过 `astranaut_lib:dispatch_error/3` 处理自身条款和统一默认 fallback。用户 formatter 顶层不匹配时 MUST 直接进入 `format_default_error/2`，不得再调用 `astranaut_macro`；框架 formatter 也遵循相同规则。

因此，测试和示例中只为 `macro_exception` 增加的显式代理条款将被删除。用户领域 reason 的具体条款保留。

### 删除无领域语义的 struct facade

`astranaut_struct` 没有自己的 formatter 条款，现有 `format_error/1,2` 只无条件代理 `astranaut_macro`。删除这两个导出和函数，使 registry 为其 macro descriptor 自然选择 `astranaut_macro`。struct parse-transformer 的领域诊断仍固定归 `astranaut_struct_transformer`，不受影响。

这是公开函数层面的兼容性移除，但避免继续把 facade 误认为错误所有者。调用方应直接调用错误 tuple 中记录的 formatter；若必须手动格式化 macro 框架 reason，则调用 `astranaut_macro:format_error/1,2`。

## Risks / Trade-offs

- [嵌套 formatter 覆盖范围过大，误把用户返回错误归框架] → 只包裹异常 catch 分支产生的 fail computation；用同一 macro 同时产生异常和主动领域错误的回归用例验证两条路径。
- [local macro 生成模块身份变化导致既有诊断断言失败] → 仅 `macro_exception` 改为 `astranaut_macro`；用户主动返回的 error/warning 必须继续断言生成 local module。
- [`astranaut_struct:format_error/1,2` 的外部调用失效] → 在发布说明中标记 breaking change，并提供按真正所有者调用的迁移方式。
- [错误消息看似仍可由 generic fallback 输出而掩盖路由错误] → 测试同时断言原始 `{Pos, Formatter, Reason}` 中的 formatter identity 和最终非空消息。

## Migration Plan

1. 在异常包装分支为 `macro_exception` 固定 `astranaut_macro`，保持调用位置包装和恢复路径不变。
2. 更新 external/local macro 回归断言：框架异常归 `astranaut_macro`，主动返回领域错误仍归 registry formatter。
3. 删除用户 formatter 中仅用于转发 `macro_exception` 的条款和对 `astranaut_macro` 的引用。
4. 删除 `astranaut_struct:format_error/1,2` facade，并更新其 API/formatter contract 测试。
5. 运行 macro error、macro local、struct 及全量 Common Test，并执行 OpenSpec strict validation。

回滚时可恢复异常分支的继承 formatter 行为及 struct facade；reason 数据结构和持久化格式没有迁移要求。

## Open Questions

无。
