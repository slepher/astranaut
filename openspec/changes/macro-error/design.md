## Context

registry 在分析 macro provider 时，以是否导出 `format_error/1` 选择 descriptor 的 `formatter`：有则为 provider module，无则为 `astranaut_macro`。这个选择只应表示用户 macro 是否拥有自身领域诊断。

当前 `astranaut_macro_expander:expand_macro_with/3` 把 descriptor formatter 施加到整个 `invoke_macro_function/1` computation。成功调用返回的 `astranaut_return` 错误因此正确归用户 formatter；但 `invoke_macro_function/1` 捕获用户函数异常后构造的 `{macro_exception, ...}` 也继承同一 formatter。测试夹具只好为这个框架 reason 增加特定条款并调用框架 formatter。

异常不能据此视为用户 macro 的领域错误接口。对 ErLando 在 2026-07-01 之前历史的复核只发现 `gen_fun_macro` 中一处显式 `exit(undefined_type)`；2026-08 新增 macro provider 中集中使用 `erlang:error/1` 的实现晚于该边界，不能作为 Astranaut 既有领域协议的参照。该历史事实只支持保留最小异常隔离兼容，不支持鼓励新 macro 以异常报告可预期校验失败。

现有后续校验路径已经体现目标边界：`process_macro_return/3` 产生的 `invalid_macro_return` 在用户 formatter 的局部 invocation computation 之外运行，使用外层 `astranaut_macro`。本 change 将异常包装路径收敛到相同规则。

已提交的 transform-error capability 已确定 formatter adapter：内部 diagnostic 保留 `{Position, Formatter, Reason}`，compiler boundary 和 shared fallback 由 `astranaut_lib:format_error/1,2` 负责，领域 callback 只保留直接的 `format_error/1` clauses。macro-error 遵循这个协议，ownership 仍在 reason 产生点决定。

## Goals / Non-Goals

**Goals:**

- formatter 在 reason 产生时按语义所有者确定。
- 用户 macro 通过返回值表达的领域错误和 warning 使用 registry formatter。
- `macro_exception` 及其他框架 reason 固定使用 `astranaut_macro`。
- 明确 `error/throw/exit` 是意外执行故障而不是用户领域错误协议；可预期失败使用返回的 error/warning computation。
- 用户 formatter 无需引用、代理或理解 `astranaut_macro` 的 reason。
- successful user computations 继承 descriptor formatter；异常 catch branch 产生的框架 error 显式使用 `astranaut_macro`。
- 领域 formatter 保持纯 `format_error/1` clauses；compiler adaptation 和 fallback 遵循 `astranaut_lib:format_error/1,2`。
- 保持位置、文件、reason、异常 payload、恢复和累计诊断行为。

**Non-Goals:**

- 不建立按 reason 动态探测的 formatter registry。
- 不建立“用户 formatter 未匹配后再尝试 `astranaut_macro`”的 fallback 链。
- 不改变已提交的 transform-error compiler adapter、shared fallback 或 formatter callback 协议。
- 不改变 macro 返回 AST、递归展开、local macro generation 或 registry 的 macro 选择语义。
- 不重新设计 compiler tuple、missing-formatter warning、registry protocol 或 shared fallback。
- 不把异常包装描述为推荐的用户 macro 领域错误能力，也不根据异常 reason 选择用户 formatter。

## Decisions

### 异常包装只是调用边界的故障隔离

`invoke_macro_function/1` 捕获异常并构造 `macro_exception` 时，失败 computation MUST 在产生点通过 `astranaut_traverse:update_pos(Pos, astranaut_macro, ...)` 固定位置和框架 formatter。`expand_macro_with/3` 外层使用 descriptor formatter 的 `update_pos` 不会重写已经格式化的异常诊断；成功调用返回的 computation 不提前格式化，继续继承 descriptor 中的用户 formatter。

这样，同一个调用的两类结果保持清晰分离：

```text
erlang:apply 成功
  └─ 执行用户返回的 astranaut_return/traverse computation
       └─ error/warning → registry formatter

erlang:apply 抛出异常
  └─ 调用边界隔离意外故障并包装 macro_exception
       └─ error → astranaut_macro
```

选择在产生点覆盖，而不是在 realization 或 formatter 调用阶段按 tuple 形状重路由，因为错误结构已经保存 formatter 身份；晚期推断会把所有 formatter 与 reason 表示耦合，也无法可靠区分相同 term 的不同领域含义。

这个 catch 分支是防止单个 macro 故障破坏整个扫描的兼容性保护，不是领域诊断构造 API。用户 macro 对输入、配置或类型的可预期校验失败 MUST 返回 `astranaut_return` 可接受的 error/warning computation；深层 helper 不得仅把 `erlang:error/1` 机械替换为 `{error, Reason}` 后继续由上层当普通值使用，而应把失败显式传递到 macro 返回边界。

### registry formatter 只代表用户领域协议

保留 `astranaut_macro_registry:formatter_opts/3` 的现有选择规则。provider 导出 `format_error/1` 时，registry formatter 仍为该 module；否则为 `astranaut_macro`。这个 formatter 只作用于用户 macro 成功返回的诊断 computation，不获得框架 reason 的所有权。

local macro 继续把源码模块的公开 formatter closure 编译到生成模块；用户领域诊断使用生成模块身份。生成模块的私有 helper 和导出协议不因本 change 改变。

### 不使用 formatter fallback 链

每个领域 formatter 只包含自己拥有的 `format_error/1` clauses，不接收 options，也不提供 throw mode。compiler adaptation、formatter 调用和统一默认 fallback 继续由 `astranaut_lib:format_error/1,2` 提供；unknown reason 以及 formatter 调用范围内的 `error:function_clause` 行为以 transform-error capability 为准。不得在 macro formatter 之间添加 proxy 或按 reason shape 推断 ownership。

因此，测试和示例中只为 `macro_exception` 增加的显式代理条款将被删除。用户领域 reason 的具体 clauses 保留，直接 callback 调用保持普通 Erlang clause 语义。

### 保留 struct formatter export 作为 universal fallback

`astranaut_struct` 保留 `-export([format_error/1]).`，并将完整 formatter 实现收敛为唯一 clause：

```erlang
format_error(Msg) ->
    astranaut_lib:format_default_error(Msg).
```

该公开 API 保留 deep character list 原样返回及其他 term 经 `io_lib:write/1` 的默认语义，不代理 `astranaut_macro`，不增加 reason-specific clause、`/2`、reason dispatch 或 fallback chain。registry 因观察到 `astranaut_struct:format_error/1` 而选择 present 路径，不产生 `{missing_macro_formatter, astranaut_struct}` warning。

struct parse-transformer 的领域诊断仍固定归 `astranaut_struct_transformer`；macro 框架 reason 仍在产生点归 `astranaut_macro`。两者不因 struct universal fallback 改变 ownership。

### 保持 struct transformer ownership

`astranaut_struct_transformer` 继续直接实现自己拥有的 struct-transform `format_error/1` clauses。struct transformer reason 不因 macro descriptor formatter 或 shared adapter fallback 而改换 ownership；compiler boundary 的 adaptation 仍统一经过 `astranaut_lib:format_error/1,2`。

## Risks / Trade-offs

- [嵌套 formatter 覆盖范围过大，误把用户返回错误归框架] → 只包裹异常 catch 分支产生的 fail computation；用同一 macro 同时产生异常和主动领域错误的回归用例验证两条路径。
- [调用方把被捕获的异常误解为推荐的领域错误接口] → 文档和测试命名统一使用“unexpected exception”或“fault containment”；领域错误示例只展示返回 computation。
- [local macro 生成模块身份变化导致既有诊断断言失败] → 仅 `macro_exception` 改为 `astranaut_macro`；用户主动返回的 error/warning 必须继续断言生成 local module。
- [`astranaut_struct` universal fallback 改变默认格式] → 直接复用 `astranaut_lib:format_default_error/1` 的既有 deep-character-list 与 `io_lib:write/1` 分支，并用 callable export 和 no-warning 回归测试锁定行为。
- [错误消息看似仍可由 generic fallback 输出而掩盖路由错误] → 测试同时断言原始 `{Pos, Formatter, Reason}` 中的 formatter identity 和最终非空消息。

## Migration Plan

1. 在异常包装分支通过显式 `update_pos(Pos, astranaut_macro, ...)` 为 `macro_exception` 固定框架 formatter，保持 payload 和恢复路径不变。
2. 更新 external/local macro 回归断言：框架异常归 `astranaut_macro`，主动返回领域错误仍归 registry formatter。
3. 删除用户 formatter 中仅用于转发 `macro_exception` 的条款和对 `astranaut_macro` 的引用。
4. 将异常测试和说明标记为故障隔离；用户领域错误示例只使用返回的 error/warning computation。
5. 保留 `astranaut_struct` 的 `/1` export，替换为单一 universal fallback，公开 `astranaut_lib:format_default_error/1`，并更新 API/formatter contract 与 no-warning 测试。
6. 后续更新 `README.md` 和 `README.zh.md` 的 macro sections，记录正式领域错误返回协议、异常隔离边界、保留的 struct formatter export、public default helper 与 no-warning consequence。
7. 运行 macro error、macro local、struct 及全量 Common Test，并执行 OpenSpec strict validation。

回滚时可恢复异常分支的继承 formatter 行为或 struct 的单一 fallback implementation；公开 helper、diagnostic reason 数据结构和持久化格式没有迁移要求。

## Open Questions

无。
