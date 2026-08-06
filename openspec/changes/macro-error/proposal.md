## Why

当前 macro descriptor 的 registry formatter 同时覆盖用户 macro 主动返回的领域诊断和框架在调用过程中产生的 `macro_exception`，导致框架错误被错误地交给用户模块。用户 formatter 因而需要了解并代理 `astranaut_macro:format_error/2`，错误所有权与 formatter 身份不一致。

## What Changes

- 按 reason 的语义产生者确定 formatter，而不是让一次 macro 调用共享同一个 formatter。
- macro 注册、解析、展开、异常包装和返回值校验产生的框架 reason 固定使用 `astranaut_macro`。
- 用户 macro 主动返回的错误和 warning 继续使用 registry 为该 macro 选择的用户 formatter。
- 用户 formatter 只实现自己的领域 reason，不再为 `macro_exception` 等框架 reason 引用或代理 `astranaut_macro`。
- 未导出 `format_error/1` 的 macro provider 继续自然使用 `astranaut_macro`；**BREAKING**：移除 `astranaut_struct` 没有领域条款的历史 formatter facade。
- 保持 `astranaut_lib:dispatch_error/3` 的统一默认 fallback 和内部 `function_clause` 传播语义，不建立 formatter fallback 链。

## Capabilities

### New Capabilities

- `macro-error-ownership`: 规定 macro 框架诊断、用户 macro 领域诊断及 registry formatter 的所有权和路由边界。

### Modified Capabilities

<!-- 当前 openspec/specs 中没有需要修改的既有 capability。 -->

## Impact

- 影响 macro 调用诊断的 formatter 标记与 `astranaut_macro_expander` 的异常路径。
- 影响 external macro 与生成 local macro formatter 的测试夹具和断言。
- `astranaut_struct:format_error/1,2` 不再作为公开 facade；依赖该历史入口的调用方需要改用真正拥有对应 reason 的 formatter。
- 不改变错误 reason、位置、异常 payload、兄弟错误恢复、默认格式化协议或 macro 返回 AST 语义。
