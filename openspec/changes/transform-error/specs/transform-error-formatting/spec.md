## ADDED Requirements

### Requirement: to_compiler 统一适配 diagnostics

`astranaut_return:to_compiler/1` MUST 将每个内部 `{Position, DomainFormatter, Reason}` error 或 warning 转换为 `{Position, astranaut_lib, {DomainFormatter, Reason}}`。文件分组、顺序、位置、分类、原 formatter 和 reason MUST 保持不变。

#### Scenario: 成功返回包含 warning

- **WHEN** return monad 包含 forms 和内部 formatted warning
- **THEN** `to_compiler/1` 返回 `{warning, Forms, Warnings}`
- **AND** warning tuple 使用 `astranaut_lib` 作为 compiler formatter
- **AND** payload 保存原 DomainFormatter 和 Reason

#### Scenario: 失败返回包含 errors 和 warnings

- **WHEN** return monad 失败并包含多个文件的 errors 和 warnings
- **THEN** `to_compiler/1` 转换每个 diagnostic 的 formatter wrapper
- **AND** 不改变文件分组、诊断顺序、位置或 error/warning 分类

#### Scenario: 内部 realize 保持原始 ownership

- **WHEN** 调用方直接执行 `astranaut_error:realize/1`
- **THEN** 返回的 diagnostic 仍为 `{Position, DomainFormatter, Reason}`
- **AND** 不提前包装或生成最终消息

### Requirement: astranaut_lib 提供 compiler callback 和 shared dispatch

系统 MUST 提供 `astranaut_lib:format_error({Module, Reason})` 作为 compiler callback，并通过 `astranaut_lib:format_error(Reason, fun Module:format_error/1)` 调用领域 formatter。shared dispatch MUST 在 formatter 动态范围内抛出任意 `error:function_clause` 时返回统一默认格式，且 MUST NOT 接收 options 或提供 throw mode。`astranaut_lib:format_default_error/1` MUST 是公开的默认格式化 primitive：deep character list 原样返回，其他 term 返回 `io_lib:write/1`；`format_error/2` MUST 在 `error:function_clause` fallback 时调用该 public helper。

#### Scenario: Compiler adapter 格式化已拥有 reason

- **WHEN** compiler 调用 `astranaut_lib:format_error({Module, Reason})` 且 `Module:format_error/1` 匹配 Reason
- **THEN** adapter 返回领域 formatter 的结果

#### Scenario: Formatter 无匹配 clause

- **WHEN**领域 formatter 对 Reason 抛出 `error:function_clause`
- **THEN** shared dispatch 对 deep character list 原样返回
- **AND** 对其他 Reason 返回 `io_lib:write(Reason)` 等价格式

#### Scenario: Formatter helper 发生 function_clause

- **WHEN** 已匹配领域 clause 的下游 helper 抛出 `error:function_clause`
- **THEN** shared dispatch 返回原始 Reason 的统一默认格式
- **AND** 不检查 stack frame 或重新抛出该异常

#### Scenario: Formatter 发生其他异常

- **WHEN** formatter 抛出非 `error:function_clause` 的异常
- **THEN** shared dispatch 原样传播其 class、reason 和 stacktrace

#### Scenario: 显式动态 module 调用

- **WHEN** 非 compiler 调用方持有 formatter Module 和 Reason
- **THEN** 调用方使用 `astranaut_lib:format_error(Reason, fun Module:format_error/1)` 获得同一 dispatch 语义

### Requirement: 领域 formatter 保持纯 format_error/1

Astranaut parse-transformer 和用户 macro formatter MUST 只以直接 `format_error/1` clauses 映射自己拥有的 reason。领域 callback MUST NOT 调用 shared dispatcher、实现 generic catch-all、导出 `format_error/2` 或定义通用 `format_error_1/1` 跳转层。

#### Scenario: 直接格式化已拥有 reason

- **WHEN** 直接调用领域 formatter 的 `format_error/1` 并传入其拥有的 Reason
- **THEN** 对应 clause 返回精确领域消息
- **AND** 不经过 shared fallback

#### Scenario: 直接调用未知 reason

- **WHEN** 绕过 adapter，直接调用领域 formatter 的 `format_error/1` 并传入未知 Reason
- **THEN** callback 保持普通 Erlang clause 语义并抛出 `function_clause`
- **AND** callback 不自行兜底

#### Scenario: Compiler 调用未知 reason

- **WHEN**同一未知 Reason 经 `to_compiler/1` adapter 进入 compiler 格式化路径
- **THEN** `astranaut_lib` 捕获领域 callback 的 `function_clause`
- **AND** 返回统一默认格式

#### Scenario: Formatter API surface

- **WHEN** 检查 production formatter exports 和源码结构
- **THEN** `format_error/1` 是唯一领域 formatter callback export
- **AND** 不存在 `/2`、callback 内 shared dispatch、generic catch-all 或通用 `format_error_1/1`

### Requirement: Local macro formatter 使用纯 callback closure

local macro 生成模块 MUST 只复制和导出源模块的纯 `format_error/1` 及其真实 helper 依赖，不得要求 `/2`、anonymous dispatch wrapper 或 strict options。

#### Scenario: Local formatter 使用领域 helper

- **WHEN** local `format_error/1` 的具体 clause 调用具有领域名称的普通 helper
- **THEN** generated module closure 包含该 helper
- **AND** helper 保持 private

#### Scenario: Local formatter 没有额外 helper

- **WHEN** local formatter 的全部领域消息直接写在 `/1` clauses 中
- **THEN** generated module 只为 formatter 导出 `/1`
- **AND** 不生成通用 `_1` 或 `/2` wrapper

### Requirement: 缺失用户 macro formatter 产生 warning

用户 macro provider 未导出 `format_error/1` 时，系统 MUST 以 `astranaut_macro` formatter 产生 `{missing_macro_formatter, Module}` warning，并 MUST 继续使用 `astranaut_macro` 作为 provider descriptor formatter。

#### Scenario: External provider 缺失 formatter

- **WHEN** source module 导入未导出 `format_error/1` 的 external macro provider
- **THEN** 系统在该次编译中为该 provider 产生一次 warning
- **AND** macro 注册和展开继续进行

#### Scenario: Local provider 缺失 formatter

- **WHEN** source module 声明 local macro 但未定义 `format_error/1`
- **THEN** warning 使用 source module identity
- **AND** generated descriptor 使用 `astranaut_macro`

#### Scenario: 仅存在 format_error/2

- **WHEN** provider 仅导出 `format_error/2`
- **THEN** 系统把它视为缺失 formatter
- **AND** `/2` 不构成兼容协议

#### Scenario: 同一 provider 多次使用

- **WHEN** 同一次 source module 编译多次 import、use 或调用同一缺失 formatter 的 provider
- **THEN** missing formatter warning 只出现一次

### Requirement: 移除 options-based formatter 协议

系统 MUST 移除 `dispatch_error/3`、公开 `format_default_error/2`、领域 formatter `/2` 和 `default => throw`。测试 MUST 分别验证纯领域 callback、内部 ownership、compiler adapter 和 shared fallback。

#### Scenario: 验证领域映射

- **WHEN** 测试需要证明具体领域 clause 已执行
- **THEN** 直接调用领域 `/1` 并断言精确消息

#### Scenario: 验证 compiler fallback

- **WHEN** 测试需要验证未知 reason 的默认消息
- **THEN** 测试通过 `astranaut_lib:format_error/1,2` adapter 路径断言默认格式
- **AND** 不调用已删除的领域 `/2` 或 throw option
