# Local Macro 规范增量

## ADDED Requirements

### Requirement: 按 FA 注册 local macro

系统 MUST 对一次 declaration 的全部 FA 先完成唯一性检查，再写入共享 order/context 的逐 FA 条目并维护各自闭包根和 callable 状态；任一成员重复时不得部分注册。系统不得为共享字段另建独立 group 状态。

#### Scenario: 重复 declaration 失败

- **给定** 两个 `-local_macro` declaration 注册相同 FA
- **当** 注册第二个 declaration 时
- **那么** 编译以 `duplicate_local_macro_declaration` 失败

#### Scenario: 后声明 macro 属于先声明闭包

- **给定** A 在 B 前声明，且 B 是 A 的静态闭包成员
- **当** 计算 A 的闭包时
- **那么** B 按 helper 处理，可同时属于 A 与自身的闭包

### Requirement: 声明位点环境

每个 declaration 的 function forms MUST 使用 declaration 前 passed forms 所确定的预展开 `MacroRuntimeContext`：其中包含已 pass 的有效宏环境、options，以及这些 passed forms 可提供的 `inject_attrs` 值。包含 remaining queue 的源码视图只用于发现闭包，不属于宏运行时上下文。GenerationCompiler MUST NOT 读取该 context。

#### Scenario: 声明前 import 对闭包可见

- **给定** `-import_macro(macro_a)` 在 `-local_macro([foo/1])` 之前已 pass
- **并且** foo 的闭包实际调用 macro_a 中的宏
- **当** 预展开 foo 时
- **那么** 其展开环境包含 macro_a

#### Scenario: 声明后 import 不回溯进入闭包环境

- **给定** `-local_macro([foo/1])` 已注册
- **并且** 后续属性生成 `-import_macro(macro_b)`
- **当** 预展开 foo 时
- **那么** foo 的 declaration 环境不包含 macro_b

#### Scenario: 声明后 use_macro 不改变 local forms 的调用语义

- **给定** `-local_macro([foo/1])` 已保存声明位点环境
- **并且** 后续 `-use_macro(...)` 修改同一宏的 alias、调用参数或 `inject_attrs` 配置
- **当** foo 在 declaration 点预展开，或之后因依赖就绪补充展开
- **那么** foo 的 frozen forms 仍使用 declaration 时保存的宏名称、调用参数和 `inject_attrs` 配置
- **并且** 后续 use_macro 只影响其声明位置之后的普通 attribute/function 展开

#### Scenario: local forms 的 inject_attrs 只看声明前 passed forms

- **给定** 目标 attribute `early` 已在 `-local_macro([foo/1])` 前 pass
- **并且** 目标 attribute `late` 位于该 declaration 之后
- **并且** foo 的 frozen function 调用声明了 `inject_attrs` 的宏
- **当** foo 在 declaration 点或后续准备 canonical forms 时展开
- **那么** 注入值只包含 `early`
- **并且** declaration 自身、`late` 与 remaining queue 中其他尚未 pass 的 forms 均不可见

#### Scenario: 闭包源码视图不等于注入视图

- **给定** declaration 后方的 helper function 已存在于当时的 remaining queue
- **当** 注册 local macro 并展开其 frozen forms
- **那么** helper 可以通过完整 closure source view 被纳入静态闭包
- **但是** helper 前后的尚未 pass attributes 不得通过该 source view 进入 `inject_attrs`

#### Scenario: 仅实际引用的 local macro 进入环境

- **给定** 多个 local macro 已注册或已编译
- **并且** foo 的闭包仅实际使用其中 a/1
- **当** 构造 foo 的 declaration 或 final 展开环境时
- **那么** LocalEnv 部分仅包含 a/1
- **并且** 后声明或未被 form 扫描识别的 local macros 不参与匹配

#### Scenario: 自身递归调用不是宏依赖

- **给定** `-local_macro([foo/1])` 且 foo/1 函数体调用 foo/1
- **当** 计算 foo 的闭包和引用 local macro 集合时
- **那么** 该调用按普通 Erlang 函数调用处理
- **并且** foo/1 不进入自己的 local macro 依赖集合

#### Scenario: 同 declaration 的成员共享环境且互不作为宏

- **给定** `-local_macro([foo/1, bar/1])`
- **当** 预展开 foo/1 与 bar/1 的 frozen forms
- **那么** 二者使用同一个 declaration MacroRuntimeContext fingerprint
- **并且** declaration 前候选环境自然不包含 foo/1 与 bar/1，因此二者不进入扫描得到的 local-macro 白名单
- **并且** bar/1 对 foo/1 的调用保持普通 Erlang 本地调用，反向同理

#### Scenario: 实际 local 引用使用统一宏匹配语义

- **给定** A 已注册，且 B 的静态闭包包含对 A 同名 FA 的调用
- **当** 解析 B 实际引用的 local macro
- **那么** 使用与普通 function 展开相同的宏环境和调用匹配规则
- **并且** 只有实际匹配为 local macro 的 A 才进入 B 的依赖集合

### Requirement: 冻结原始闭包 forms

系统 MUST 在 declaration 注册时冻结闭包的原始 function/spec forms，并保证所有环境展开都从该原始输入开始。

#### Scenario: 闭包在注册时冻结

- **给定** 扫描遇到 `-local_macro([foo/1])`
- **当** foo 的闭包计算完成
- **那么** 闭包 function/spec 的原始 forms 保存为冻结输入

#### Scenario: 后续 splice 不可改写冻结输入

- **给定** function/spec 已被冻结
- **当** 属性宏 splice 输出生成相同 form ID
- **那么** 报 `illegal_locked_form_mutation`
- **并且** 不插入该 attribute 的 splice 结果
- **并且** 统一扫描继续处理后续 forms

### Requirement: 跨环境展开比对与缓存

ExpansionValidator MUST 按环境 fingerprint 复用展开结果；同一 FormId 在不同环境下的结果 MUST 与最后一次已接受结果一致，并只产生一个 canonical expanded form。

#### Scenario: 相同环境复用最后一次结果

- **给定** 同一原始 form 再次以相同 EnvFingerprint 需要展开
- **当** ExpansionValidator 处理该 form
- **那么** 复用缓存结果，不重复展开

#### Scenario: 不同环境结果不一致报错

- **给定** helper 同时属于两个闭包
- **并且** 两个环境下的展开结果不同
- **当** 新结果与最后一次已接受结果比较时
- **那么** 报 `conflicting_local_macro_closure_environment`

#### Scenario: 不同环境结果一致时成功

- **给定** 同一 helper 属于两个 declaration 环境不同的闭包
- **并且** 两个环境下从原始 form 展开的结果一致
- **当** ExpansionValidator 比较结果时
- **那么** 接受并更新 last expansion record
- **并且** canonical expanded form 保持唯一

#### Scenario: 编译器不按环境重新展开

- **给定** 某累计 boundary 所需 FormIds 已存在 canonical expanded forms
- **当** GenerationCompiler 编译该 boundary
- **那么** 只读取 canonical expanded forms
- **并且** 不接收 declaration MacroRuntimeContext
- **并且** 不遍历 request 重新执行 function expansion

### Requirement: 按声明顺序最小累计编译

DependencyScheduler MUST 依据 declaration 顺序和真实 local macro 依赖生成最小累计 boundary；GenerationCompiler MUST 只消费 canonical expanded forms。

#### Scenario: B 需要 A

- **给定** A 在 B 前声明，且 B 的闭包实际使用 A 作为宏
- **当** 首次需要调用 B 时
- **那么** 先编译 `{A}`，再编译 `{A,B}`

#### Scenario: B 不需要 A

- **给定** A 在 B 前声明，且 B 的闭包不使用 A 作为宏
- **当** 首次需要调用 B 时
- **那么** 直接编译 `{A,B}`

#### Scenario: 独立声明本身不产生中间编译

- **给定** `-local_macro([foo/1])` 后出现 `-local_macro([bar/1])`
- **并且** bar/1 的 function form 不实际依赖 foo/1 作为宏
- **当** bar/1 完成注册和 declaration-time 预展开
- **那么** 不编译 `{foo}` 或 `{foo,bar}`
- **并且** 首次真正需要可调用或 scan 收尾时直接编译 `{foo,bar}`

#### Scenario: 相同累计成员不重新编译

- **给定** 累计 local macro members 已成功编译并提交
- **并且** 此后没有引入新的 local_macro declaration
- **当** 不同展开环境或触发阶段再次请求该累计 boundary
- **那么** 复用已提交 generation
- **并且** MacroRuntimeContext、注入 forms 和 compile options 不产生新 boundary identity

#### Scenario: scan 收尾编译全部 local macro

- **给定** 某些 local macro 已在 earlier attribute 调用时编译，另一些从未被调用
- **当** local-macro 工作流收尾时
- **那么** 按注册顺序构造包含全部 local macro 的最终累计模块
- **并且** 编译输入直接取已确认的 canonical expanded forms

### Requirement: extra_functions 与 internal_function

系统 MUST 将 `extra_functions` 纳入静态闭包；`internal_function` MUST 解析 declaration
MacroEnv 中当前可见的宏 key，把选中调用固化为普通函数调用，并且不得把该策略交给
通用展开器。

#### Scenario: extra_functions 补充闭包

- **给定** local macro options 包含 `{extra_functions, [helper/1]}`
- **当** 计算闭包时
- **那么** helper/1 进入闭包并按同一冻结、展开和比对规则处理

#### Scenario: 间接函数引用需要 extra_functions

- **给定** local macro 只通过 `fun helper/1`、动态函数值或 `apply/3` 间接引用 helper
- **并且** options 未声明 helper/1 为 `extra_functions`
- **当** 计算静态闭包
- **那么** 该间接引用不自动形成闭包边
- **并且** 只有显式加入 `extra_functions` 后 helper/1 才进入冻结闭包

#### Scenario: extra_functions 引用不存在函数失败

- **给定** `{extra_functions, [missing/1]}`
- **当** 注册 local macro 时
- **那么** 编译以 `invalid_extra_functions` 失败

#### Scenario: internal_function 必须解析声明点宏

- **给定** `{internal_function, [helper/1]}`，且模块存在普通 helper/1，但 declaration 前 MacroEnv 没有 helper/1 宏
- **当** 注册 local macro
- **那么** 编译以 `undefined_internal_functions` 失败

#### Scenario: 远程宏作为普通函数

- **给定** declaration 前 MacroEnv 包含远程宏 `M:F/A`
- **并且** `internal_function` 使用 `{M,F,A}`
- **当** 展开 frozen closure
- **那么** 远程宏 key 不进入通用展开器的 MacroEnv
- **并且** AST 中的 `M:F(...)` 保持普通远程函数调用

#### Scenario: alias 恢复原始远程函数

- **给定** `use_macro` 已用现有 `alias` 把 `M:F/A` 暴露为 `Alias/A`
- **并且** local declaration 的 `internal_function` 选择 `Alias/A`
- **当** 展开 frozen closure 或 retain function 的最终重展开
- **那么** AST 中 `Alias(Args)` 改写为 `M:F(Args)`
- **并且** alias key 与原始远程 key 均从有效 MacroEnv 移除

#### Scenario: 共享闭包函数的 internal_function 策略冲突

- **给定** 同一 helper 属于两个 local macro 闭包
- **并且** 两个 declaration 为该 helper 提供的 internal macro 环境不兼容
- **当** 校验策略时
- **那么** 编译以 `conflicting_internal_function_policy` 失败

#### Scenario: internal_function 在构造环境时应用

- **给定** declaration 将声明点可见的 helper/1 宏标记为 internal_function
- **当** 展开该 declaration 的闭包 function
- **那么** helper/1 宏 key 不出现在传给通用 function 展开器的 MacroEnv 中
- **并且** 展开器无需解释 internal_function option

#### Scenario: local macro 自身不进入自身宏环境

- **给定** foo/1 是 local macro 且其 function form 递归调用 foo/1
- **当** 工作流请求通用 function 展开器展开 foo/1
- **那么** 传入的 MacroEnv 不包含 foo/1
- **并且** 递归调用保持普通 Erlang 本地调用

### Requirement: retain 与最终跳过集合

系统 MUST 从 retain roots 计算完整闭包和 `FinalSkipIds`，并让所有 retained functions 与普通 Step 2 functions 使用同一个 `FinalMacroRuntimeContext` 和 ExpansionValidator。属于 local macro 闭包的 function MUST 以注册时 form 扫描得到的 `referenced_local_macros` 过滤 FinalLocalEnv；不属于任何 local 闭包的普通 function MUST 使用完整 FinalLocalEnv。

#### Scenario: final local 环境复用声明扫描白名单

- **给定** B 的 declaration form 扫描只识别到先声明 local macro A
- **并且** local macro C 在 B 之后声明
- **当** retained B 或其闭包 helper 在 final context 展开
- **那么** LocalEnv 包含 A，但不包含 B、C 或其他白名单外 local macros
- **并且** 不再执行按 order、自身或同声明成员计算的 final 排除
- **并且** 即使后声明 C 的闭包引用 B，B 宏头仍只使用 B 自身 declaration 的白名单

#### Scenario: 普通 final function 使用完整 local 环境

- **给定** ordinary function 不属于任何 local macro 闭包
- **当** ordinary function 在 final context 展开
- **那么** 它可以匹配 FinalLocalEnv 中全部可调用 local macros

#### Scenario: retain 根保留完整闭包

- **给定** local macro 根或 helper 被 `local_macro_retain`、`export` 或 `export_macro` 命中
- **当** 计算 retain 集合时
- **那么** 该根的完整闭包及其 spec forms 均被保留

#### Scenario: 非冻结显式 retain root 产生 warning

- **给定** `-local_macro_retain([ordinary/0])` 且 ordinary/0 不属于任何 local macro 闭包
- **当** 收尾计算 retain 集合时
- **那么** 报告 `ineffective_local_macro_retain` warning
- **并且** ordinary/0 仍按普通 form 处理，不产生额外生命周期效果
- **并且** 普通 `export`/`export_macro` 隐式 roots 不使用该 warning

#### Scenario: 不存在的显式 retain root 使用独立 warning

- **给定** `-local_macro_retain([missing/0])` 且模块中没有 missing/0 定义
- **当** 收尾计算 retain 集合时
- **那么** 报告 `undefined_local_macro_retain` warning
- **并且** 不把它合并为 `ineffective_local_macro_retain`
- **并且** warning 使用该 `local_macro_retain` attribute 的源码位置

#### Scenario: 最终跳过未 retain 的已展开 forms

- **给定** local macro form 已展开且不属于 retain 闭包
- **当** 收尾计算 FinalSkipIds 时
- **那么** 该 form 出现在 FinalSkipIds 中

#### Scenario: retain form 参与最终展开

- **给定** frozen form 属于 retain 闭包
- **并且** 最终环境比对通过
- **当** 最终函数体展开执行时
- **那么** 该 form 不在 FinalSkipIds 中并参与递归展开

#### Scenario: retain helper 的最终环境比对不一致失败

- **给定** retain 闭包中的 helper 在 declaration 环境与最终环境下展开结果不同
- **当** 收尾执行最终环境比对时
- **那么** 报 `conflicting_local_macro_closure_environment`

#### Scenario: local macro 宏头参与最终环境比对

- **给定** local macro 宏头属于 retain 闭包
- **当** 使用 FinalMacroRuntimeContext 执行最终 function 展开时
- **那么** 宏头使用与 helper 和普通 function 相同的 ExpansionValidator
- **并且** final fingerprint 不同时，从原始 form 展开并与最后一次 local result 比较
- **并且** 宏头仍不在 FinalSkipIds 中

#### Scenario: 普通 Step 2 function 使用最终环境一致性逻辑

- **给定** function 曾作为 local closure form 在 declaration context 下展开
- **并且** 该 function 在 Step 2 被选为普通 macro caller
- **当** 使用 FinalMacroRuntimeContext 展开
- **那么** 使用与 retain function 相同的 last-env 与 last-result 比对逻辑

### Requirement: 安全覆盖加载

GenerationCompiler MUST 以安全覆盖方式加载累计 local macro module；old code 仍被引用时 MUST 保留上一代并报告错误。

#### Scenario: old code 被引用时失败

- **给定** 覆盖加载前 `code:soft_purge(<Module>__local_macro)` 返回 `false`
- **当** 尝试加载新的累计模块时
- **那么** 报 `local_macro_module_in_use`
- **并且** 不调用 `code:purge/1`
