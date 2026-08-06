## Why

当前 parse-transformer formatter 为了获得默认格式化行为，普遍重复公开 `/2` wrapper、私有 `format_error_1/1` 和 options-based dispatch。这个协议把固定的 fallback 机制泄漏到每个 transformer，并引入仅用于测试匹配覆盖的 `default => throw` 分支。

## What Changes

- 新增统一的 `astranaut_lib:format_error(Msg, FormatterFun)`，负责 formatter 调用和默认 fallback。
- 任意 `error:function_clause` 都直接触发共享 fallback，不再检查 stack frame 或区分顶层 no-match 与 formatter 内部 `function_clause`。
- parse-transformer 只公开固定的 `format_error/1` callback；具体领域条款放在传给共享 helper 的匿名 formatter fun 中，不再定义 `format_error/2` 或私有 `format_error_1/1`。
- 持有 formatter module 的调用方通过 `astranaut_lib:format_error(Msg, fun Module:format_error/1)` 统一 dispatch。
- **BREAKING**：删除 `dispatch_error/3`、`format_default_error/2` 的公开协议、各 formatter 的 `/2` 入口以及 `default => throw` 行为。
- 用户 macro provider 未导出 `format_error/1` 时产生框架 warning，同时继续使用 `astranaut_macro` 作为 descriptor formatter。

## Capabilities

### New Capabilities

- `transform-error-formatting`: 定义 parse-transformer `format_error/1`、共享 formatter fun dispatch、默认 fallback 及缺失用户 macro formatter 的诊断协议。

### Modified Capabilities

<!-- 当前 openspec/specs 中没有需要修改的既有 capability。 -->

## Impact

- 影响 `astranaut_lib` 的公开 API，以及所有 Astranaut parse-transformer、macro formatter 和相关测试夹具。
- 影响测试与内部工具调用 formatter module 的方式。
- formatter 中意外发生的 `function_clause` 将被视为未格式化消息并进入默认 fallback；其他异常仍向外传播。
- 不改变 compiler diagnostic tuple、reason 所有权、位置、文件或 parse-transform 行为。
