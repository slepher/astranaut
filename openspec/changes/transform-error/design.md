## Context

Astranaut 的内部诊断以 `{Position, FormatterModule, Reason}` 保存。`astranaut_return:to_compiler/1` 通过 `astranaut_error:realize/1` 将其组织为 Erlang parse-transform 返回值；OTP 随后直接调用 compiler tuple 中 module 的 `format_error/1`。

当前 strict formatter 迁移把 fallback 外壳放进每个领域 callback：公开 `/1` 转发到 `/2`，`/2` 调用 shared dispatcher，private `format_error_1/1` 才保存实际领域 clauses。第一次 `transform-error` 设计虽删除了 `/2` 和 `_1`，却仍要求每个 `/1` 用匿名 fun 调 shared dispatcher，因而没有解决根本问题：领域 formatter 仍同时承担领域映射和共享 fallback。

统一边界实际是 `to_compiler/1`。它应把内部诊断适配为由 `astranaut_lib` 格式化的 compiler diagnostic，同时在 payload 中保留真正的领域 formatter。

## Goals / Non-Goals

**Goals:**

- 领域 `format_error/1` 只用直接 clauses 完成 reason 到文本的纯映射。
- `to_compiler/1` 成为内部诊断协议到 OTP compiler formatter 协议的唯一适配边界。
- `astranaut_lib` 统一拥有 compiler callback、formatter dispatch 和默认 fallback。
- 删除领域 formatter `/2`、通用 `format_error_1/1`、options 和 throw mode。
- 用户 macro 缺少 formatter 时给出可见 warning。

**Non-Goals:**

- 不在 `astranaut_error` 的积累、位置绑定或 `realize/1` 阶段预先生成文本。
- 不让领域 `format_error/1` 调用 shared dispatcher 或实现 catch-all。
- 不根据 reason tuple 猜测错误所有者；所有者继续由内部 diagnostic formatter 字段确定。
- 不捕获 `function_clause` 之外的异常。

## Decisions

### to_compiler 统一包装 formatter identity

`astranaut_error:realize/1` 继续返回内部形状：

```erlang
{Position, DomainFormatter, Reason}
```

`astranaut_return:to_compiler/1` 在构造 parse-transform 返回值时，针对 errors 和 warnings 的每个 diagnostic 转换为：

```erlang
{Position, astranaut_lib, {DomainFormatter, Reason}}
```

文件分组、顺序、位置及 error/warning 分类保持不变。这个转换只发生在 `to_compiler/1`；直接调用 `astranaut_error:realize/1` 的内部 API 和测试仍可观察原始 formatter identity。

选择 wrapper payload 而不是预先格式化文本，是为了延续 OTP 的延迟格式化协议，并保留原始 formatter 和 reason 供 diagnostics 消费者检查。

### astranaut_lib 同时提供 compiler callback 与 shared dispatch

`astranaut_lib` 公开：

```erlang
-spec format_error({module(), term()}) -> term().
format_error({Module, Reason}) ->
    format_error(Reason, fun Module:format_error/1).

-spec format_error(term(), fun((term()) -> term())) -> term().
format_error(Reason, FormatterFun) ->
    try FormatterFun(Reason) of
        Formatted -> Formatted
    catch
        error:function_clause -> default_format_error(Reason)
    end.
```

`format_error/1` 是 OTP compiler 在看到 adapter tuple 后调用的固定 callback；`format_error/2` 是可复用 dispatcher。持有 module 的其他调用方同样使用 `fun Module:format_error/1`，不需要 module-specific overload。

`default_format_error/1` 为 library-private：deep character list 原样返回，其他 term 使用 `io_lib:write/1`。公开 API 不接收 options，不提供 throw mode。

### 领域 formatter 保持纯粹

parse-transformer 和用户 macro formatter 直接定义自己拥有的 reason clauses：

```erlang
format_error({owned_reason, Value}) ->
    io_lib:format("invalid value: ~p", [Value]);
format_error(another_owned_reason) ->
    "another message".
```

领域 callback 不调用 `astranaut_lib:format_error/2`，不包含 generic catch-all，也不需要 `/2`、anonymous dispatch fun 或 private `format_error_1/1`。如果具体领域消息需要复杂计算，可以调用有领域名称的普通 helper；禁止仅为协议跳转而保留通用 `_1` helper。

已明确属于另一个 formatter 的共享 reason 应在诊断产生处绑定正确 formatter。迁移期间若某模块仍确实拥有一个组合 reason，可用精确 clause 调用领域 helper；不得使用 catch-all delegation 代替所有权。

### 任意 function_clause 都触发 shared fallback

只有经过 `astranaut_lib:format_error/1,2` 的 formatter 调用具有 fallback。其动态范围内任意 `error:function_clause` 都返回原始 Reason 的默认格式，不检查 stack frame，也不重新抛出。这包括领域 `/1` 无匹配 clause，以及已匹配 clause 下游 helper 的 `function_clause`。其他异常类型保留原 class、reason 和 stacktrace。

领域 callback 被直接调用时保持普通 Erlang 函数语义；未知 reason 会抛出 `function_clause`。需要 fallback 的调用方必须经过 shared adapter。正常 compiler 路径由 `to_compiler/1` 保证这一点。

### 测试分别验证内部与 compiler 协议

内部诊断测试继续针对 `astranaut_error:realize/1` 断言原 formatter ownership。compiler boundary 测试针对 `astranaut_return:to_compiler/1` 断言 adapter tuple，并通过 `astranaut_lib:format_error/1` 验证最终消息。

直接测试领域 formatter 时只传入其已拥有的 reason，并断言精确消息；未知 reason fallback 必须通过 `astranaut_lib:format_error(Reason, fun Module:format_error/1)` 测试。测试不再调用 formatter `/2` 或使用 throw option。

### 缺失用户 macro formatter 产生框架 warning

external registry 在导入 provider exports、local workflow 在首次解析源码 formatter protocol 时检查 `format_error/1`。不存在时继续选择 `astranaut_macro`，并以 `astranaut_macro` formatter 产生 `{missing_macro_formatter, Module}` warning。

同一 provider 在一次 source module 编译中最多警告一次。external provider 使用实际 module；local macro 使用声明 source module，而不是 generation module。仅导出 `/2` 不构成有效 formatter。

## Risks / Trade-offs

- [compiler tuple 的 formatter identity 变为 adapter] → 原 formatter identity 明确保存在 `{DomainFormatter, Reason}` payload；内部 `realize/1` 形状不变，并分别测试两个协议层。
- [绕过 to_compiler 的直接 formatter 调用没有 fallback] → 这是纯 callback 的预期语义；需要安全格式化的调用方显式使用 `astranaut_lib:format_error/2`。
- [领域 helper 的真实 `function_clause` 被 adapter fallback 掩盖] → 这是 shared dispatch 的明确语义；精确领域消息测试负责暴露常见实现错误，其他异常仍传播。
- [删除 `/2` 破坏外部调用方] → 标记 breaking change；迁移到纯 `/1` 或 shared `format_error(Reason, fun Module:format_error/1)`。
- [缺失 formatter warning 对无自定义 reason 的 provider 产生噪音] → 每个 provider 每次编译只报告一次，且不阻止注册或展开。

## Migration Plan

1. 为 `astranaut_lib:format_error/1,2` 和 `to_compiler/1` adapter tuple 建立回归测试。
2. 实现 shared compiler callback/dispatcher，并在 `to_compiler/1` 统一包装 errors 和 warnings。
3. 将所有领域 formatter 恢复为直接 `/1` clauses，删除 callback 内的 shared dispatch、`/2` 和通用 `_1`。
4. 收敛 local generated formatter closure 到纯 `/1` 及其真实 helper。
5. 更新显式 module 格式化调用点和测试 helper；区分内部 ownership 与 compiler adapter 断言。
6. 加入 external/local missing formatter warning。
7. 运行专项和完整 Common Test、xref、dialyzer及 OpenSpec strict validation。

迁移一次完成，不提供 strict `/2` 兼容期。当前按旧版 `transform-error` 设计产生的匿名-dispatch callback 实现必须返工，不作为兼容形态保留。

## Open Questions

无。
