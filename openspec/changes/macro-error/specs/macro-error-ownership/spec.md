## ADDED Requirements

### Requirement: Macro 框架诊断固定归框架 formatter

系统 MUST 在 macro 框架 reason 的产生点将 formatter 固定为 `astranaut_macro`，不得因当前 macro descriptor 注册了用户 formatter 而改变所有权。框架 reason 包括注册、导入、解析、展开、异常包装、递归限制和返回值校验诊断。

#### Scenario: External macro 抛出异常

- **WHEN** 导出自定义 `format_error/1` 的 external macro 函数抛出异常
- **THEN** 系统将异常包装为保留 MFA、参数和 stack payload 的 `macro_exception`
- **AND** 该诊断的 formatter 是 `astranaut_macro`

#### Scenario: Local macro 抛出异常

- **WHEN** 拥有生成 local formatter 的 local macro 函数抛出异常
- **THEN** 系统将异常包装为保留 local MFA 和 stack payload 的 `macro_exception`
- **AND** 该诊断的 formatter 是 `astranaut_macro`，不是生成 local module

#### Scenario: Macro 返回值校验失败

- **WHEN** 用户 macro 成功返回但返回值不是有效 macro AST 或 computation
- **THEN** 系统产生 `invalid_macro_return`
- **AND** 该诊断的 formatter 是 `astranaut_macro`

### Requirement: 用户 macro 领域诊断使用 registry formatter

系统 MUST 要求用户 macro 通过成功返回的 error 和 warning computation 表达可预期领域失败，并使用该 macro descriptor 的 registry formatter。该规则 MUST 与框架在调用外围产生的诊断相互独立。用户 macro 抛出的 `error/throw/exit` MUST NOT 被解释成用户领域诊断。

#### Scenario: External macro 主动返回领域错误

- **WHEN** 导出 `format_error/1` 的 external macro 成功返回用户领域 error
- **THEN** 该诊断的 formatter 是 external macro module
- **AND** 其具体 reason 由该 module 的 formatter 条款处理

#### Scenario: Local macro 主动返回领域 warning

- **WHEN** 定义 formatter 的 local macro 成功返回用户领域 warning
- **THEN** 该诊断的 formatter 是对应的生成 local module
- **AND** 生成模块公开 formatter 的导出与私有 helper 隔离协议保持不变

#### Scenario: Provider 没有用户 formatter

- **WHEN** macro provider 未导出 `format_error/1`
- **THEN** registry 将其 descriptor formatter 设为 `astranaut_macro`
- **AND** provider 无需引用或代理框架 formatter

#### Scenario: 用户 macro 抛出与领域 reason 相同的异常 term

- **WHEN** 用户 macro 抛出一个 term，且该 term 恰好也可被用户 `format_error/1` 匹配
- **THEN** 系统仍将其视为意外执行故障并包装为 `macro_exception`
- **AND** formatter 是 `astranaut_macro`，不得根据异常 reason 调用 registry formatter

### Requirement: Macro 异常捕获是故障隔离而非领域错误协议

系统 MUST 在 macro 调用边界隔离 `error/throw/exit`，保存异常 class、reason、stacktrace、MFA 和 arguments，并以 `astranaut_macro` 记录 `macro_exception`。该行为 MUST 被描述为意外故障的兼容性保护，而不是用户 macro 报告可预期领域错误的接口。

#### Scenario: 可预期领域校验失败

- **WHEN** 用户 macro 检测到无效输入、配置或领域类型
- **THEN** macro 通过返回 error/warning computation 报告该失败
- **AND** 诊断使用 registry formatter，而不是构造或抛出异常

#### Scenario: 非预期执行异常

- **WHEN** 用户 macro 执行期间意外抛出 `error/throw/exit`
- **THEN** 调用边界将其包装为框架 `macro_exception`
- **AND** 保留原始异常 payload，以便诊断实现缺陷
- **AND** 该隔离行为不把异常升级为用户领域错误 API

### Requirement: Formatter ownership 不通过 fallback 推断

系统 MUST 在记录诊断时确定 formatter，不得在最终格式化时根据 reason shape 推断 ownership。领域 formatter MUST 只包含自己拥有的直接 `format_error/1` clauses；unknown-reason fallback 以及 formatter 调用范围内的 `error:function_clause` 行为 MUST 遵循已提交 transform-error capability 的 `astranaut_lib:format_error/1,2` shared adapter。

#### Scenario: 领域 formatter 只处理自己的 reason

- **WHEN** 直接调用领域 formatter 的 `format_error/1` 并传入其拥有的 Reason
- **THEN** 对应 clause 返回精确领域消息
- **AND** callback 不包含 formatter-to-formatter proxy 或 generic catch-all

#### Scenario: Unknown reason 使用共享 adapter fallback

- **WHEN** 一个已记录 formatter 无法匹配 Reason，且调用方需要 compiler-safe 的格式化结果
- **THEN** 调用方通过 `astranaut_lib:format_error/1,2` shared adapter 获得 transform-error capability 定义的默认格式
- **AND** 系统不尝试 `astranaut_macro` 或其他 formatter

#### Scenario: Formatter function_clause 行为遵循共享 adapter

- **WHEN** formatter 调用范围内发生 `error:function_clause`
- **THEN** 结果完全遵循 transform-error shared adapter 的 fallback contract
- **AND** macro-error 不增加额外 stack inspection、proxy 或 ownership inference

#### Scenario: 用户 formatter 不代理框架 reason

- **WHEN** 用户 macro 同时定义自身领域 formatter 且执行期间发生 `macro_exception`
- **THEN** 框架直接把该诊断记录为 `astranaut_macro`
- **AND** 用户 formatter 不需要针对 `macro_exception` 增加框架代理条款

### Requirement: Formatter 导出表达实际领域所有权

macro provider MUST 只在拥有自身领域 reason 时导出 formatter。仅无条件代理其他 formatter、但不拥有具体领域条款的 facade MUST NOT 被 registry 视为自定义 formatter。

#### Scenario: astranaut_struct 没有自身领域 reason

- **WHEN** registry 分析 `astranaut_struct` 的 macro exports
- **THEN** 系统 MUST 移除 `astranaut_struct` 现有的 `format_error/1` facade
- **AND** registry 为其 macro descriptor 选择 `astranaut_macro`

#### Scenario: Struct transformer 产生领域错误

- **WHEN** `astranaut_struct_transformer` 在 struct AST 转换期间产生 struct-specific reason
- **THEN** 诊断 formatter 是 `astranaut_struct_transformer`
- **AND** 该 formatter 保持纯 `format_error/1` callback，compiler-safe fallback 由 `astranaut_lib:format_error/1,2` 提供

### Requirement: Formatter 归属变更不改变诊断内容与恢复

除 formatter identity 和删除的兼容 facade 外，系统 MUST 保持 macro 错误的 reason、位置、文件、异常 payload、兄弟诊断顺序及失败调用恢复行为。

#### Scenario: 一个展开树包含多个失败 sibling

- **WHEN** sibling macro calls 分别抛出异常、返回用户领域错误及返回无效 AST
- **THEN** 系统继续累计全部 sibling 诊断
- **AND** 异常和无效返回分别使用 `astranaut_macro`
- **AND** 用户主动返回的领域错误使用 registry formatter
- **AND** 每条诊断保持原有位置、reason 和恢复后的调用树行为

#### Scenario: Diagnostic payload and ordering remain stable

- **WHEN** macro expansion produces external or local exceptions, user error/warning computations, and return-validation failures in one traversal
- **THEN** each exception retains its original MFA, arguments, and stack payload
- **AND** every diagnostic retains its original position, file, reason, error/warning classification, sibling order, and count
- **AND** AST returns and sibling recovery behavior remain unchanged apart from the formatter identity required by this capability
