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

系统 MUST 让用户 macro 成功返回的 error 和 warning computation 使用该 macro descriptor 的 registry formatter。该规则 MUST 与框架在调用外围产生的诊断相互独立。

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
- **AND** provider 无需引用或代理 `astranaut_macro:format_error/1,2`

### Requirement: Formatter 不通过 fallback 推断错误所有权

系统 MUST 在记录诊断时确定 formatter。`dispatch_error/3` 的顶层条款不匹配 MUST 直接使用 `format_default_error/2`，不得把 reason 继续转发给另一个 formatter；formatter 内部发生的真实 `function_clause` MUST 保留 stacktrace 重新抛出。

#### Scenario: 用户 formatter 不匹配领域 reason

- **WHEN** 用户 formatter 的具体条款不匹配一个 reason
- **THEN** 普通 options 使用统一默认字符列表格式
- **AND** `#{default => throw}` 抛出原始 reason
- **AND** 系统不尝试 `astranaut_macro` formatter

#### Scenario: 用户 formatter 内部失败

- **WHEN** 已匹配的用户 formatter 条款内部触发 `function_clause`
- **THEN** 系统保留原 stacktrace 重新抛出该异常
- **AND** 系统不得把它当成顶层 no-match 或改由其他 formatter 处理

#### Scenario: 用户 formatter 不代理框架 reason

- **WHEN** 用户 macro 同时定义自身领域 formatter 且执行期间发生 `macro_exception`
- **THEN** 框架直接把该诊断记录为 `astranaut_macro`
- **AND** 用户 formatter 不需要针对 `macro_exception` 调用 `astranaut_macro:format_error/1,2`

### Requirement: Formatter 导出表达实际领域所有权

macro provider MUST 只在拥有自身领域 reason 时导出 formatter。仅无条件代理其他 formatter、但不拥有具体领域条款的 facade MUST NOT 被 registry 视为自定义 formatter。

#### Scenario: astranaut_struct 没有自身领域 reason

- **WHEN** registry 分析 `astranaut_struct` 的 macro exports
- **THEN** `astranaut_struct` 不导出 `format_error/1,2`
- **AND** registry 为其 macro descriptor 选择 `astranaut_macro`

#### Scenario: Struct transformer 产生领域错误

- **WHEN** `astranaut_struct_transformer` 在 struct AST 转换期间产生 struct-specific reason
- **THEN** 诊断 formatter 是 `astranaut_struct_transformer`
- **AND** 该 formatter 通过 `dispatch_error/3` 使用统一默认 fallback

### Requirement: Formatter 归属变更不改变诊断内容与恢复

除 formatter identity 和删除的兼容 facade 外，系统 MUST 保持 macro 错误的 reason、位置、文件、异常 payload、兄弟诊断顺序及失败调用恢复行为。

#### Scenario: 一个展开树包含多个失败 sibling

- **WHEN** sibling macro calls 分别抛出异常、返回用户领域错误及返回无效 AST
- **THEN** 系统继续累计全部 sibling 诊断
- **AND** 异常和无效返回分别使用 `astranaut_macro`
- **AND** 用户主动返回的领域错误使用 registry formatter
- **AND** 每条诊断保持原有位置、reason 和恢复后的调用树行为
