## Why

当前 parse-transformer formatter 为了获得默认格式化行为，普遍重复公开 `/2` wrapper、私有 `format_error_1/1` 和 options-based dispatch。这个协议把固定的 fallback 机制泄漏到每个 transformer，并引入仅用于测试匹配覆盖的 `default => throw` 分支。

## What Changes

- 新增统一的 `astranaut_lib:format_error/1,2` adapter：`/1` 是 compiler callback，`/2` 负责 formatter 调用和默认 fallback。
- 任意 `error:function_clause` 都直接触发共享 fallback，不再检查 stack frame 或区分顶层 no-match 与 formatter 内部 `function_clause`。
- `astranaut_return:to_compiler/1` 把内部 `{Pos, FormatterModule, Reason}` 统一转换为 `{Pos, astranaut_lib, {FormatterModule, Reason}}`，让 compiler 经共享 adapter 格式化。
- parse-transformer 只公开纯粹的 `format_error/1` callback，以直接 clauses 映射自己拥有的 reason；callback 不调用共享 helper，也不实现 fallback。
- 持有 formatter module 的调用方通过 `astranaut_lib:format_error(Msg, fun Module:format_error/1)` 统一 dispatch。
- **BREAKING**：删除 `dispatch_error/3`、`format_default_error/2` 的公开协议、各 formatter 的 `/2` 入口以及 `default => throw` 行为。
- 用户 macro provider 未导出 `format_error/1` 时产生框架 warning，同时继续使用 `astranaut_macro` 作为 descriptor formatter。

## Capabilities

### New Capabilities

- `transform-error-formatting`: 定义 parse-transformer `format_error/1`、共享 formatter fun dispatch、默认 fallback 及缺失用户 macro formatter 的诊断协议。

### Modified Capabilities

<!-- 当前 openspec/specs 中没有需要修改的既有 capability。 -->

## Impact

- 影响 `astranaut_lib`、`astranaut_return:to_compiler/1` 的 compiler adapter 边界，以及所有 Astranaut parse-transformer、macro formatter和相关测试夹具。
- 影响测试与内部工具调用 formatter module 的方式。
- formatter 中意外发生的 `function_clause` 将被视为未格式化消息并进入默认 fallback；其他异常仍向外传播。
- `astranaut_error` 内部 diagnostic tuple、reason 所有权、位置、文件和 parse-transform 行为保持不变；`to_compiler/1` 输出的 formatter module 统一为 `astranaut_lib`，原 formatter 与 reason 保存在 wrapper payload 中。
