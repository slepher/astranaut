## Context

Erlang compiler 对 `{Position, FormatterModule, Reason}` 直接调用 `FormatterModule:format_error(Reason)`。Astranaut 当前为了让每个 callback 同时支持具体领域消息和默认消息，在多个模块重复以下结构：公开 `/1` 转发到 `/2`、`/2` 调用 `dispatch_error/3`、私有 `format_error_1/1` 保存真正条款。测试再通过 `#{default => throw}` 判断具体条款是否命中。

这个结构把同一套控制流复制到了每个 parse-transformer。formatter 的固定外壳应由共享库拥有，transformer 只提供领域映射。当前 `dispatch_error/3` 还通过 stack frame 判断 `function_clause` 是顶层未匹配还是 formatter 内部失败；新协议明确不作该区分。

## Goals / Non-Goals

**Goals:**

- 用一个二参数共享函数统一 formatter dispatch 和普通默认格式化。
- parse-transformer 只公开 OTP 要求的 `format_error/1`。
- 删除 `/2`、private `format_error_1/1`、options 和 `default => throw` 协议。
- module 与 fun 调用方共享完全相同的 dispatch 语义。
- 用户 macro 缺少 formatter 时给出可见 warning。

**Non-Goals:**

- 不改变 OTP diagnostic tuple 或 compiler 对 `Module:format_error/1` 的调用方式。
- 不根据 stacktrace、reason tuple 或 formatter module 推断领域所有权。
- 不保留 formatter 内部 `function_clause` 的异常传播。
- 不捕获 `function_clause` 之外的异常。

## Decisions

### 共享 API 只接受消息和一元 formatter fun

`astranaut_lib` 提供：

```erlang
-spec format_error(term(), fun((term()) -> term())) -> term().
format_error(Msg, FormatterFun) ->
    try FormatterFun(Msg) of
        Formatted -> Formatted
    catch
        error:function_clause -> default_format_error(Msg)
    end.
```

`default_format_error/1` 为 library-private：若 `Msg` 已是 deep character list，则原样返回；否则返回 `io_lib:write(Msg)`。公开 API 不接收 options，也不提供 throw mode。

持有 module 的调用方不需要另一套重载：

```erlang
astranaut_lib:format_error(Msg, fun Module:format_error/1)
```

动态 remote fun 在目标 OTP 基线上可用，并保留 module/name/arity 身份。共享函数不需要知道第二个参数最初来自 module 还是本地/匿名 fun。

### Parse-transformer callback 使用匿名领域 formatter

每个 callback 保持 OTP 的固定单参数入口，在内部把具体条款作为匿名 fun 交给共享 API：

```erlang
format_error(Msg) ->
    astranaut_lib:format_error(
      Msg,
      fun({owned_reason, Value}) ->
              io_lib:format("invalid value: ~p", [Value]);
         (another_owned_reason) ->
              "another message"
      end).
```

因此 callback 没有 catch-all、`format_error/2` 或 `format_error_1/1`。OTP 直接调用 `/1` 时仍会进入 shared dispatch；模块外的显式调用也得到相同结果。

选择匿名 fun 而不是命名 private helper，是因为 `/1` 的唯一可变部分就是局部领域映射；额外命名函数只会重建一层所有 transformer 都相同的跳转。较大的 formatter 仍按 reason 顺序组织匿名 fun clauses。

### 任意 function_clause 都触发 fallback

shared dispatch 捕获 formatter fun 执行动态范围内的任意 `error:function_clause`，不检查首个 stack frame，也不重新抛出。这包括：

- 匿名 fun 没有匹配当前消息；
- 已匹配条款调用的 helper 内发生 `function_clause`；
- remote `Module:format_error/1` 自身或其下游发生 `function_clause`。

这是有意的简化：`function_clause` 在 formatter 协议中统一表示“无法格式化该消息”。formatter 的其他异常类型仍传播，避免把所有实现错误静默转换成文本。

### 删除 strict `/2` 测试协议

测试调用 formatter 时统一使用：

```erlang
astranaut_lib:format_error(Error, fun Formatter:format_error/1)
```

具体领域条款通过精确消息断言证明命中；未知消息通过 `io_lib:write/1` 等价断言证明 fallback。由于任意 `function_clause` 都合法进入 fallback，测试不再使用 throw mode 区分命中路径。

### 缺失用户 macro formatter 产生框架 warning

external macro registry 在导入 provider exports、local macro workflow 在首次解析源码 formatter protocol 时检查 `format_error/1`。不存在时继续选择 `astranaut_macro`，并以 `astranaut_macro` formatter 产生 `{missing_macro_formatter, Module}` warning。

同一 provider 在一次 source module 编译中 MUST 只警告一次，避免每个导出 macro 或每次调用重复报告。external provider 使用实际 module；local macro 使用声明它的 source module，而不是带 generation identity 的临时 module。仅导出 `/2` 不构成有效 formatter。

## Risks / Trade-offs

- [formatter helper 的真实 `function_clause` 被默认文本掩盖] → 这是协议定义；为领域条款增加精确消息测试，其他异常类型继续传播。
- [匿名 fun 使大型 formatter 函数变长] → 保持每个 reason 一个 clause，并允许 clause 调用具有领域名称的普通消息构造 helper；不再建立通用 `format_error_1` 跳转层。
- [删除 `/2` 破坏外部调用方] → 标记 breaking change；迁移到 `/1` 或 `astranaut_lib:format_error(Msg, fun Module:format_error/1)`。
- [缺失 formatter warning 对无自定义 reason 的 macro provider 产生噪音] → 每个 provider 每次编译仅报告一次，并继续使用框架 formatter，不阻止编译。

## Migration Plan

1. 用 `format_error/2` 替换 `dispatch_error/3` 和公开 default helper，先建立匹配、fallback、任意 `function_clause` 及其他异常测试。
2. 将 Astranaut 自带 formatter 迁移为单一 `/1` callback 和匿名领域 fun，删除 `/2`、`format_error_1/1` 及跨 formatter strict delegation。
3. 将 macro 测试 provider 和 local generated formatter protocol 收敛到仅 `/1`。
4. 更新所有显式 module formatter 调用点与测试 helper，删除 throw-option 断言。
5. 在 external/local macro formatter 检测处加入一次性缺失 warning。
6. 运行专项及完整 Common Test、API surface 检查和 OpenSpec strict validation。

该迁移一次完成；混用 strict `/2` 与新 `/1` protocol 会造成检测歧义，不设置双协议过渡期。

## Open Questions

无。
