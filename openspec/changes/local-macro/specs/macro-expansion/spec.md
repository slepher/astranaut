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

每个 declaration 的 function forms MUST 使用 declaration 前 passed forms 所确定的延迟展开 `MacroRuntimeContext`：其中包含已 pass 的有效宏环境、options，以及这些 passed forms 可提供的 `inject_attrs` 值。包含 remaining queue 的源码视图只用于发现闭包，不属于宏运行时上下文。GenerationCompiler MUST NOT 读取该 context。

#### Scenario: 声明前 import 对闭包可见

- **给定** `-import_macro(macro_a)` 在 `-local_macro([foo/1])` 之前已 pass
- **并且** foo 的闭包实际调用 macro_a 中的宏
- **当** 延迟展开 foo 时
- **那么** 其展开环境包含 macro_a

#### Scenario: 声明后 import 不回溯进入闭包环境

- **给定** `-local_macro([foo/1])` 已注册
- **并且** 后续属性生成 `-import_macro(macro_b)`
- **当** 延迟展开 foo 时
- **那么** foo 的 declaration 环境不包含 macro_b

#### Scenario: 声明后 use_macro 不改变 local forms 的调用语义

- **给定** `-local_macro([foo/1])` 已保存声明位点环境
- **并且** 后续 `-use_macro(...)` 修改同一宏的 alias、调用参数或 `inject_attrs` 配置
- **当** foo 在 declaration 点延迟展开，或之后因依赖就绪补充展开
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

#### Scenario: 快照保留全部前序候选

- **给定** A 在 B 前声明，B 的外部宏可能生成 A 的调用
- **当** 冻结 B 的宏环境
- **那么** 保留声明前完整有效宏映射，后声明宏不参与匹配

#### Scenario: 自身递归调用不是宏依赖

- **给定** `-local_macro([foo/1])` 且 foo/1 函数体调用 foo/1
- **当** 计算 foo 的闭包和引用 local macro 集合时
- **那么** 该调用按普通 Erlang 函数调用处理
- **并且** foo/1 不进入自己的 local macro 依赖集合

#### Scenario: 同 declaration 的成员共享环境且互不作为宏

- **给定** `-local_macro([foo/1, bar/1])`
- **当** 延迟展开 foo/1 与 bar/1 的 frozen forms
- **那么** 二者使用同一个 declaration MacroRuntimeContext
- **并且** declaration 前候选环境自然不包含 foo/1 与 bar/1，因此二者不作为本次新增的本地宏参与匹配
- **并且** bar/1 对 foo/1 的调用保持普通 Erlang 本地调用，反向同理

#### Scenario: 实际 local 引用使用统一宏匹配语义

- **给定** A 已注册，且 B 的静态闭包包含对 A 同名 FA 的调用
- **当** 解析 B 实际引用的 local macro
- **那么** 使用与普通 function 展开相同的宏环境和调用匹配规则
- **并且** 只有实际匹配为 local macro 的 A 才进入 B 的依赖集合

### Requirement: 冻结原始闭包 forms

系统 MUST 在 declaration 注册时冻结闭包的原始 function/spec forms，并永久绑定首次声明前环境。

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

### Requirement: 首次绑定与单次 canonical 展开

系统 MUST 为每个 FormId 保存首次进入闭包时的原始 form、声明前宏环境及 record 上下文。后续声明 MUST 复用该绑定。每个 form MUST 至多成功展开一次，所有编译和 retain MUST 复用 canonical form，不进行多环境重放或白名单校验。

#### Scenario: 共享 helper 的后声明环境不同

- **给定** helper 首次在 A 的闭包中绑定，B 的声明环境随后改变
- **当** B 的闭包包含该 helper
- **那么** helper 使用 A 时点的绑定和 canonical form，不报告环境冲突

#### Scenario: 无宏闭包直接使用原始 form

- **给定** frozen function 在绑定环境下无宏调用，或 form 是 spec
- **当** 准备 canonical forms
- **那么** 直接复用原始 form，不调用 expander

#### Scenario: record 上下文保持首次绑定

- **给定** frozen function 使用 record 且之后加入其他闭包
- **当** 延迟展开该函数
- **那么** 使用首次 source view 的 record forms，不遍历无关函数

### Requirement: 纯登记与延迟累计编译

注册 MUST 零展开、零编译。只有属性目标确保可调用和 scan 收尾触发编译。系统 MUST 按策略 A 对含宏 frozen function 预先编译其绑定 order 之前的累计本地宏前缀，使递归展开中的全部候选宏已可调用。

#### Scenario: 独立声明只编译一次

- **给定** 连续声明 A、B、C，闭包无宏调用且没有中途属性调用
- **当** 扫描收尾
- **那么** 编译 `{A,B,C}` 一次，注册阶段无展开或编译

#### Scenario: B 直接或动态依赖 A

- **给定** A 在 B 前声明，B 含宏调用，其快照包含 A
- **当** 准备 B，或其宏 replacement 生成对 A 的调用
- **那么** 先编译 `{A}`，展开 B 一次，再编译 `{A,B}`，不挂起或重试

#### Scenario: 仅调用外部宏也采用前序就绪

- **给定** B 只含外部宏调用，但快照包含未就绪的本地 A
- **当** 准备 B
- **那么** 仍先编译 `{A}`，以覆盖 replacement 动态引用

#### Scenario: 同声明成员保持完整 prefix

- **给定** A、B 属于同一声明
- **当** 属性调用要求 A 可调用
- **那么** 累计 prefix 同时包含 A、B，其相互调用保持普通函数调用

#### Scenario: 已有 generation 不回退或重复编译

- **给定** 累计 `{A,B}` 已加载
- **当** 再次请求 A、B 或 scan 收尾且无新增声明
- **那么** 复用当前 generation，不重新加载较小 prefix

#### Scenario: 请求前缀的其他成员也有展开依赖

- **给定** A、B、C 按顺序声明，B 含宏调用，C 不含宏调用
- **当** 首次请求 C
- **那么** 先确保 A 就绪，再准备 B，最终编译全部 prefix

### Requirement: closure_roots 补充静态闭包

系统 MUST 将 `closure_roots` 纳入静态闭包。

#### Scenario: closure_roots 补充闭包

- **给定** local macro options 包含 `{closure_roots, [helper/1]}`
- **当** 计算闭包时
- **那么** helper/1 进入闭包并按同一首次绑定和单次展开规则处理

#### Scenario: 间接函数引用需要 closure_roots

- **给定** local macro 只通过 `fun helper/1`、动态函数值或 `apply/3` 间接引用 helper
- **并且** options 未声明 helper/1 为 `closure_roots`
- **当** 计算静态闭包
- **那么** 该间接引用不自动形成闭包边
- **并且** 只有显式加入 `closure_roots` 后 helper/1 才进入冻结闭包

#### Scenario: closure_roots 引用不存在函数失败

- **给定** `{closure_roots, [missing/1]}`
- **当** 注册 local macro 时
- **那么** 编译以 `invalid_closure_roots` 失败

### Requirement: 宏调用与普通函数调用严格区分

与当前 MacroEnv 匹配的直接调用 MUST 始终作为宏调用。系统 MUST NOT 提供
declaration 级调用排除或 AST 改写 option。宏实现若要普通调用同一函数，MUST 使用
未注册为宏的 helper，或使用 Erlang 间接调用形式。

#### Scenario: helper 普通调用宏函数

- **给定** `M:F/A` 在当前模块的 MacroEnv 中可作为宏调用
- **并且** 独立 helper `H/A` 未注册为宏，其函数体普通调用 `M:F/A`
- **当** local macro 调用 `H/A`
- **那么** 对 `H/A` 的调用不匹配宏
- **并且** helper 在执行时把 AST term 传给 `M:F/A`

#### Scenario: internal_function 不受支持

- **给定** local macro options 包含 `internal_function`
- **当** 校验 declaration
- **那么** 该 key 作为 unexpected option 报告并忽略

#### Scenario: local macro 自身不进入自身宏环境

- **给定** foo/1 是 local macro 且其 function form 递归调用 foo/1
- **当** 工作流请求通用 function 展开器展开 foo/1
- **那么** 传入的 MacroEnv 不包含 foo/1
- **并且** 递归调用保持普通 Erlang 本地调用

### Requirement: retain 复用 canonical forms

系统 MUST 计算 `RemovedIds = compiled_ids - retained_form_ids` 并删除 local-only forms。剩余 frozen forms MUST 物化 canonical forms，且 MUST 从最终 function caller 集合排除。普通非闭包函数 MUST 使用完整 FinalMacroEnvironment。删除集合与展开跳过集合 MUST 分开。

#### Scenario: retained helper 与宏头不重新展开

- **给定** 闭包函数由 retain、export 或 export_macro 保留，之后环境发生变化
- **当** 最终函数 pass 执行
- **那么** 保留首次 canonical form，不在最终环境重放，不报告环境或白名单冲突

#### Scenario: 普通 final function 可调用后声明宏

- **给定** function 不属于 frozen closure
- **当** 最终展开
- **那么** 使用完整最终环境，包括后声明本地宏

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

### Requirement: 安全覆盖加载

GenerationCompiler MUST 以安全覆盖方式加载累计 local macro module；old code 仍被引用时 MUST 保留上一代并报告错误。

#### Scenario: old code 被引用时失败

- **给定** 覆盖加载前 `code:soft_purge(<Module>__local_macro)` 返回 `false`
- **当** 尝试加载新的累计模块时
- **那么** 报 `local_macro_module_in_use`
- **并且** 不调用 `code:purge/1`
