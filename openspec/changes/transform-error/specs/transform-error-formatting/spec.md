## ADDED Requirements

### Requirement: 共享 formatter dispatch

系统 MUST 提供 `astranaut_lib:format_error(Msg, FormatterFun)`，调用一元 formatter fun，并在 formatter 执行期间抛出任意 `error:function_clause` 时返回统一默认格式。该 API MUST NOT 接收 options 或提供 throw mode。

#### Scenario: Formatter 匹配消息

- **WHEN** formatter fun 对 `Msg` 返回格式化结果
- **THEN** shared dispatch 原样返回该结果

#### Scenario: Formatter 顶层条款不匹配

- **WHEN** formatter fun 没有匹配 `Msg` 的 clause 并抛出 `error:function_clause`
- **THEN** shared dispatch 对 character-list 消息原样返回
- **AND** 对其他消息返回 `io_lib:write(Msg)` 等价格式

#### Scenario: Formatter 内部发生 function_clause

- **WHEN** 已匹配 formatter clause 的下游 helper 抛出 `error:function_clause`
- **THEN** shared dispatch 直接返回 `Msg` 的统一默认格式
- **AND** 不检查 stack frame或重新抛出该 `function_clause`

#### Scenario: Formatter 发生其他异常

- **WHEN** formatter fun 抛出非 `error:function_clause` 的异常
- **THEN** shared dispatch 原样传播该异常及其 stacktrace

### Requirement: Module formatter 使用 remote fun

持有 formatter module 的调用方 MUST 通过 `fun Module:format_error/1` 将其适配为 shared dispatch 所需的一元 fun，不得维护 module 专用 dispatch 分支或 options 协议。

#### Scenario: 动态 module formatter 匹配

- **WHEN** 调用方持有动态 `Module`，且 `Module:format_error(Msg)` 有匹配条款
- **THEN** `astranaut_lib:format_error(Msg, fun Module:format_error/1)` 返回该 module 的格式化结果

#### Scenario: 动态 module formatter 不匹配

- **WHEN** 动态 module 的 `format_error/1` 对 `Msg` 抛出 `function_clause`
- **THEN** 同一 shared dispatch 返回统一默认格式

### Requirement: Parse-transformer 只公开固定 format_error/1

Astranaut parse-transformer MUST 只以 `format_error/1` 作为 formatter callback。callback MUST 调用 shared dispatch，并以内联匿名 fun 定义自身领域 reason；MUST NOT 导出 `format_error/2`、定义通用 `format_error_1/1` 跳转层或实现本地 catch-all。

#### Scenario: Compiler 格式化 transformer 领域错误

- **WHEN** Erlang compiler 对 `{Position, Transformer, OwnedReason}` 直接调用 `Transformer:format_error(OwnedReason)`
- **THEN** callback 通过 shared dispatch 调用匿名领域 formatter
- **AND** 返回该 transformer 的具体领域消息

#### Scenario: Compiler 传入未知 reason

- **WHEN** compiler 直接调用 transformer 的 `format_error/1`，但匿名领域 formatter 不匹配该 reason
- **THEN** callback 内的 shared dispatch 返回统一默认格式
- **AND** transformer 不包含自己的 fallback clause

#### Scenario: Formatter API surface

- **WHEN** 检查 Astranaut parse-transformer exports 和 formatter closure
- **THEN** `format_error/1` 是唯一 formatter callback export
- **AND** 不存在公开 `format_error/2` 或私有通用 `format_error_1/1`

### Requirement: 用户 macro formatter 使用单一 callback 协议

用户 macro formatter MUST 以导出的 `format_error/1` 声明其领域格式化能力；local macro 生成模块 MUST 只复制和导出该 `/1` callback 及其真实依赖，不得要求 `/2` 或 strict options。

#### Scenario: 用户 macro formatter 存在

- **WHEN** external 或 local macro provider 导出 `format_error/1`
- **THEN** registry 将对应 provider 或生成 local module 记录为用户领域诊断 formatter
- **AND** formatter 调用通过单一 `/1` 协议完成

#### Scenario: Local formatter 使用普通 helper

- **WHEN** local `format_error/1` 的领域 clause 调用具有业务名称的普通 helper
- **THEN** generated local module 的 formatter closure 包含该真实 helper
- **AND** 不为协议生成 `format_error_1/1` 跳转层

### Requirement: 缺失用户 macro formatter 产生 warning

用户 macro provider 未导出 `format_error/1` 时，系统 MUST 以 `astranaut_macro` formatter 产生 `{missing_macro_formatter, Module}` warning，并 MUST 继续使用 `astranaut_macro` 作为该 provider descriptor 的 formatter。

#### Scenario: External macro provider 缺失 formatter

- **WHEN** source module 导入一个未导出 `format_error/1` 的 external macro provider
- **THEN** 系统在该次 source module 编译中为该 provider 产生一次 missing formatter warning
- **AND** macro 注册和展开继续进行

#### Scenario: Local macro provider 缺失 formatter

- **WHEN** source module 声明 local macro 但没有定义 `format_error/1`
- **THEN** 系统以 source module identity 产生一次 missing formatter warning
- **AND** 生成 local descriptor 使用 `astranaut_macro`

#### Scenario: 仅存在 format_error/2

- **WHEN** macro provider 仅导出 `format_error/2`
- **THEN** 系统把它视为缺失 formatter 并产生 warning
- **AND** 不把 `/2` 视为兼容协议

#### Scenario: 同一 provider 被多次使用

- **WHEN** 同一 source module 多次 import、use 或调用同一个缺失 formatter 的 provider
- **THEN** missing formatter warning 在该次编译中只出现一次

### Requirement: 移除 options-based formatter 协议

系统 MUST 移除 `dispatch_error/3`、公开 `format_default_error/2`、formatter `/2` 和 `default => throw` 的行为。测试 MUST 通过精确领域消息和默认消息分别验证 match 与 fallback。

#### Scenario: 验证具体领域条款

- **WHEN** 测试需要证明 formatter 的具体领域 clause 已执行
- **THEN** 测试断言该 clause 的精确消息
- **AND** 不使用 throw option 推断是否匹配

#### Scenario: 验证默认 fallback

- **WHEN** 测试向 formatter 提供未知消息
- **THEN** 测试通过 shared dispatch 断言统一默认格式
- **AND** 不调用已删除的 `/2` callback
