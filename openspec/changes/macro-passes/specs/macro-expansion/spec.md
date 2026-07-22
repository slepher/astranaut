# Macro Expansion Spec Delta

## ADDED Requirements

### Requirement: local 与普通 function 使用同构展开

`astranaut_macro` MUST 编排 `astranaut_macro_registry` 构造的同一 `MacroRuntimeContext`，并以统一展开验证实现处理 local declaration 预展开、retain 与最终普通 function pass。

#### Scenario: 展开器不解释 local 专属策略

- **给定** local-macro 工作流已经构造最终 MacroEnv
- **当** 调用统一 function 展开操作
- **那么** 展开器只按 MacroEnv 匹配和递归展开宏调用
- **并且** 不读取 internal_function、generation、retain 或 declaration order

#### Scenario: local 引用复用统一调用匹配

- **给定** local-macro 工作流提供候选 local 宏环境和闭包 functions
- **当** 展开器在原始 function 或递归 replacement AST 中实际匹配 local macro
- **那么** 使用与普通 function 展开相同的调用匹配语义，并在调用前观察该 FA

#### Scenario: 同一 declaration 成员保持普通调用

- **给定** `-local_macro([foo/1, bar/1])` 在同一 declaration 中注册
- **当** 工作流预展开 foo/1 或 bar/1
- **那么** 二者使用相同 declaration MacroRuntimeContext
- **并且** declaration-time MacroEnv 按构造时点自然不包含 foo/1 与 bar/1
- **并且** 它们之间的调用保持普通 Erlang 本地调用
- **并且** 首次完整递归展开收集 canonical whitelist，final LocalEnv 按该名单过滤，无需最终排除二者

### Requirement: 白名单检查必须显式启用

通用 function 展开接口 MUST 接收显式 `LocalMacroWhitelistControl`。控制值 MUST 区分 `disabled`、首次 `collect` 和后续 `verify(Expected)`；系统 MUST NOT 根据 MacroEnv 或 function 名称隐式启用白名单。

#### Scenario: 普通 function 禁用白名单

- **给定** 普通 Step 2 function 调用一个或多个 local macros
- **当** 通用 function 展开器处理该 form
- **那么** 调用方传入 `disabled`
- **并且** 系统不创建 observed whitelist、不比较 canonical whitelist，也不产生 whitelist conflict
- **并且** 返回的 local whitelist 为 `disabled`，callable 调度请求为空
- **并且** function 仍按完整 FinalMacroEnv 的普通规则展开

#### Scenario: local frozen function 启用收集

- **给定** frozen FormId 首次作为 local-macro closure function 展开
- **当** `astranaut_macro_local` 调用通用 function 展开器
- **那么** 调用方传入 `collect` 和该 FormId
- **并且** 成功结果返回本次实际匹配的 local macro FA ordset

#### Scenario: local frozen function 后续处理启用校验

- **给定** frozen FormId 已有 canonical whitelist
- **当** 它在另一个 declaration context 或 final context 下再次处理
- **那么** 调用方传入 `verify(Expected)`
- **并且** retained local-macro head 与 frozen helper 使用相同校验规则

#### Scenario: final retained 环境按 canonical whitelist 过滤

- **给定** retained frozen FormId 已有 canonical whitelist
- **并且** FinalEnv 包含其他同声明或后声明 local macros
- **当** 系统构造该 FormId 的 final expansion env
- **那么** 只有 canonical whitelist 中的 local entries 进入有效 MacroEnv
- **并且** 名单外调用保持普通 Erlang 调用，不产生 unexpected whitelist 错误

### Requirement: 宏返回 AST 在 process_macro_return 中批量收集

启用白名单时，原始 function 的 local macro FA MUST 在 `match_macro_call` 成功后观察。每个 macro 返回 AST 的 local macro FAs 与任意 macro presence MUST 由 `process_macro_return` 在既有规范化 traversal 中一次性收集；该函数 MUST 返回 `{ProcessedNode, ReturnAnalysis}`，且收集过程中 MUST NOT 校验 whitelist 或执行 replacement 中的 macro。调用方 MUST 在函数返回后合并并校验 local FA 批次；accepted replacement 只有在 `has_macro_call` 为 true 时才进入递归 macro traversal。系统 MUST NOT 为白名单或 presence 增加第二次 return-tree traversal、完整 function 重扫或 expanded/original AST diff。

#### Scenario: 宏返回值生成新的 local macro 调用

- **给定** local frozen function 首先调用 external macro A
- **并且** A 的 replacement AST 包含 local macro B 调用
- **当** `process_macro_return` 规范化 A 的完整返回 AST
- **那么** B 被加入该次 `ReturnObserved`
- **并且** `process_macro_return` 返回后，调用方将该批结果合并到当前 FormId 的 observed whitelist
- **并且** 批量校验通过后 B 才由原有递归路径展开
- **并且** original function 的其他节点和已处理 siblings 不被重扫

#### Scenario: 无宏 replacement 跳过递归预检查

- **给定** macro A 的 replacement AST 不包含当前 MacroEnv 可匹配的宏调用
- **当** `process_macro_return` 完成规范化与 presence 收集
- **那么** `has_macro_call` 为 false
- **并且** accepted replacement 直接返回，不再执行一次 `transform_exprs` 预检查 traversal

#### Scenario: 多层 replacement 继承同一控制参数

- **给定** A 的 replacement 调用 B，B 的 replacement 又调用 C
- **当** local frozen function 以 `collect` 或 `verify` 展开
- **那么** A 与 B 的每次返回 AST 分别产生自己的 `ReturnObserved`
- **并且** 调用方依次合并这些批次到同一 function-level accumulator
- **并且** `process_macro_return` 不校验或执行 B、C，也不启动额外 scan pass

#### Scenario: replacement 首次发现尚未 callable 的 local macro

- **给定** external macro replacement 首次生成候选 local macro B 调用
- **并且** B 尚未进入 callable generation
- **当** 展开器匹配到 B
- **那么** 展开器返回 B 的 callable 调度请求且不调用 B
- **并且** local-macro workflow 通过 `need_callable` 编译所需 boundary
- **并且** 系统从 frozen form 重试并在成功后才提交 whitelist 与 expanded form

### Requirement: function 闭包分析复用宏 presence

系统 MUST 在构造 function 调用闭包的同一次 per-function traversal 中，同时收集普通本地调用边、local macro FAs 与任意 macro presence。同一 declaration 的多个 closure roots MUST 复用已分析 function 的结果。final function caller 筛选 MUST 使用不构造调用边集合的 presence-only analysis，并将该结果传入 expansion task；可信 presence 存在时 MUST NOT 再调用独立的 `has_macro_call` 预检查。宏 presence MUST 按对应的有效 MacroEnv 判断；form 或环境不满足安全复用条件时 MUST 回退到现场检查。

#### Scenario: closure walk 同时分析 function calls

- **给定** 两个 local macro roots 的调用闭包共享 helper function H
- **当** declaration 使用对应 MacroEnv 构造两个 closures
- **那么** H 的 AST 只分析一次
- **并且** 该结果同时提供本地调用边、local macro FAs 与 `has_macro_call`

#### Scenario: final caller analysis 直接供 expansion task 使用

- **给定** final function caller 筛选已用 FinalMacroEnv 分析全部 functions
- **当** 系统为未变化的原始 function 建立 expansion task
- **那么** task 复用该 function 的 `has_macro_call`
- **并且** final analysis 不构造 `local_calls` 或 `local_macro_calls`
- **并且** expander 不再运行独立的 `has_macro_call` traversal

### Requirement: 白名单不匹配必须独立报告

首次成功的 `collect` 结果 MUST 成为该 FormId 的 canonical whitelist。后续 `verify` MUST 在每个 Return AST 完成收集后，对整批 `ReturnObserved` 一次性检查 expected 之外的 local FAs；若存在 unexpected，该批 MUST 只产生一个汇总错误且不得进入 replacement 展开。系统 MUST 在完整 function expansion 结束后检查缺失 FA。

#### Scenario: 同一返回 AST 的多个 unexpected FA 一次报告

- **给定** canonical whitelist 是 `[a/1]`
- **并且** 某次 macro 返回 AST 同时包含 `b/1` 与 `c/1`
- **当** `process_macro_return` 完成该返回 AST 的收集
- **那么** 收集过程不校验也不中断
- **并且** 调用方随后只报告一个 `conflicting_local_macro_whitelist`
- **并且** `unexpected` 同时包含 `b/1` 与 `c/1`
- **并且** 该返回 AST 不进入递归宏展开
- **并且** 本场景适用于 declaration/非-final verify；final retained 已先过滤环境

#### Scenario: 缺失 FA 只在完成后失败

- **给定** canonical whitelist 是 `[a/1, b/1]`
- **并且** 当前 traversal 暂时只观察到 `a/1`
- **当** traversal 尚未完成
- **那么** 系统不得提前报告缺失 `b/1`
- **当** 完整 function expansion 结束仍未观察到 `b/1`
- **那么** 系统报告 `conflicting_local_macro_whitelist`

#### Scenario: 白名单不同但 AST 相同仍冲突

- **给定** 两个环境的最终 expanded form 相同
- **但是** observed local macro whitelist 不同
- **那么** 系统报告 `conflicting_local_macro_whitelist`

### Requirement: 展开结果一致性检查必须保留

白名单一致只证明 local macro 调用集合一致。系统 MUST 继续比较不同 external/options/inject contexts 下的最终 expanded form。

#### Scenario: 白名单相同但 AST 不同

- **给定** 两个环境具有相同 canonical whitelist
- **并且** 不同 `inject_attrs` 值产生不同 expanded form
- **那么** 系统报告 `conflicting_local_macro_closure_environment`

#### Scenario: 普通 function 不参与两类白名单比较

- **给定** 普通 function 以 `disabled` 展开
- **当** 其匹配或递归生成 local macro 调用
- **那么** 系统不读取或更新 local-macro whitelist ExpansionRecord

### Requirement: 属性宏统一参与 scan-and-splice

外部属性宏与已可调用的本地属性宏 MUST 在同一次 scan-and-splice 扫描中按源码顺序处理。

#### Scenario: 本地属性宏在统一扫描中展开

- **给定** `-local_macro([foo/1])` 已由 local-macro 工作流注册并可调用
- **并且** 后续存在 `-foo(...)`
- **当** 模块执行统一属性扫描时
- **那么** 该属性在当前位置以当前宏环境展开

#### Scenario: 外部属性宏生成后续外部属性调用

- **给定** 外部属性宏 A 展开后生成属性宏调用 B
- **当** A 在统一扫描中展开
- **那么** B 插回当前位置并在同一扫描中展开

#### Scenario: 普通生成 forms 延后到最终函数体展开

- **给定** 属性宏展开后生成包含宏调用的 function form
- **当** 属性扫描继续
- **那么** function form 保留在 forms 流中
- **并且** 不在属性扫描阶段递归展开其函数体

#### Scenario: 尚不可调用的本地属性宏产生通用 NeedCallable

- **给定** `-local_macro([foo/1])` 已注册但尚不可调用
- **并且** 扫描遇到 `-foo(...)`
- **当** 处理该属性时
- **那么** 扫描向通用 dependency scheduler 提交 `NeedCallable(foo/1)`
- **并且** compiler 只消费已经确认的 canonical expanded forms
- **并且** 随后在当前位置展开 `-foo(...)`

#### Scenario: 无法执行的宏 attribute 只诊断一次

- **给定** form 在语法上是 `exec_macro` 或已注册 attribute macro 调用
- **并且** 当前环境不能提供可执行宏
- **当** 统一扫描处理该 form
- **那么** 在当前位置报告 `invalid_macro_attribute` 并保留原 form
- **并且** attribute pass 收尾不重复报告该诊断

### Requirement: 宏环境前向生效

`import_macro`、`use_macro`、`macro_options` 以及属性宏生成的这些 form MUST 仅影响后续扫描到的 forms。

#### Scenario: 属性宏生成 import

- **给定** 属性宏展开后生成 `-import_macro(macro_b)`
- **并且** 后续 form 依赖 `macro_b`
- **当** 扫描继续时
- **那么** 后续 form 在包含 `macro_b` 的环境中处理

#### Scenario: 已处理属性不回扫

- **给定** 一个属性在 `macro_b` 导入前已完成展开
- **并且** 后续属性生成 `-import_macro(macro_b)`
- **当** 扫描继续时
- **那么** 先前属性的结果不重新处理

#### Scenario: 生成的 import 对同一 splice 后续属性可见

- **给定** 属性宏返回 `[import_macro(macro_b), DependentAttribute]`
- **并且** DependentAttribute 依赖 macro_b
- **当** 扫描处理该 splice 结果时
- **那么** import_macro 先更新环境
- **并且** DependentAttribute 随后在包含 macro_b 的环境中展开

#### Scenario: 生成的 local_macro declaration 进入同一扫描

- **给定** 属性宏展开后生成 `-local_macro([foo/1])`
- **当** 该 declaration 被重新扫描时
- **那么** 扫描委托 local-macro 工作流注册 foo/1
- **并且** 后续 `-foo(...)` 可按 local-macro 规则请求可调用性

#### Scenario: use_macro 的同名 option 由后者覆盖

- **给定** 先处理 `-use_macro({FA, [{xxx1, true}, {xxx2, true}]}).`
- **并且** 随后处理 `-use_macro({FA, [{xxx1, false}]}).`
- **当** 后续 form 使用 FA 时
- **那么** 有效 options 为 `[{xxx1, false}, {xxx2, true}]`

#### Scenario: 宏 key 冲突需要显式覆盖

- **给定** 当前环境已有宏 key K
- **并且** 后续 import、use alias 或其他宏映射为 K 提供不同定义
- **当** 新定义未声明 `force_override`
- **那么** 编译以 `macro_override` 失败
- **并且** 只有声明 `force_override` 时新定义才覆盖旧定义

#### Scenario: 环境 form 的输出语义

- **给定** 扫描依次遇到 `import_macro`、`use_macro` 与 `macro_options`
- **当** 它们成功更新环境
- **那么** import/use forms 被消费
- **并且** macro_options form 被保留并成为后续 passed form

#### Scenario: export_macro 不进入本模块的 local macro 环境

- **给定** 模块声明 `-export_macro([foo/0])`
- **并且** 未声明 `-local_macro([foo/0])`
- **当** 本模块出现非限定调用 `foo()`
- **那么** 该调用保持普通 Erlang 函数调用，不作为宏展开

#### Scenario: local_macro 与 export_macro 可组合

- **给定** 同一 `foo/0` 同时声明为 `-local_macro([foo/0])` 和 `-export_macro([foo/0])`
- **当** 本模块出现非限定调用 `foo()`
- **那么** 该调用使用 local_macro 的 declaration-time 本地宏环境展开
- **并且** 原模块中的 `foo/0` 保持导出，供其他模块 import 为宏

### Requirement: 导出与本地声明使用分层 option validator

系统 MUST 让 `export_macro` 与 `local_macro` 共享通用宏定义 options，但 MUST 只允许
`local_macro` 使用 `extra_functions` 闭包构造 option 和 `internal_function` 宏环境 option；
`macro_options` 和 `export_macro` 均 MUST 拒绝这两个 key。

#### Scenario: macro_options 不接受本地闭包 options

- **给定** `macro_options` 包含 `extra_functions` 或 `internal_function`
- **当** 校验模块级 options
- **那么** 这些 key 作为 unexpected options 报告并忽略
- **并且** 不把它们保存到后续 declaration 的全局 MacroRuntimeContext options

### Requirement: macro_options 区分逐宏默认值与最终模块选项

系统 MUST 将 `debug`、`debug_ast`、`max_depth` 作为按源码顺序传播的逐宏默认值，
并将 `debug_module`、`debug_module_ast` 只作为最终模块输出选项。global defaults MUST
NOT 成为宏函数调用实参。

#### Scenario: global default 只传播到后续宏

- **给定** `macro_options` 位于两个 macro import 或 local declaration 之间
- **当** 构造各自 macro descriptor
- **那么** 之前的宏保持原有配置
- **并且** 之后的宏取得新的 `debug`、`debug_ast`、`max_depth` defaults

#### Scenario: definition 与 use 覆盖 global default

- **给定** global defaults 已设置
- **并且** macro definition 另设 `max_depth`，或 `use_macro` 另设 `debug`/`debug_ast`
- **当** 构造有效调用配置
- **那么** definition `max_depth` 覆盖 global `max_depth`
- **并且** use 位点 `debug`/`debug_ast` 覆盖对应 global defaults

#### Scenario: module debug 不传播为逐宏行为

- **给定** `macro_options` 设置 `debug_module` 或 `debug_module_ast`
- **当** 执行单次宏调用与最终 module formatting
- **那么** 单次调用不读取这两个 key
- **并且** 最终 formatting 使用 scan 完成后的 global value 打印完整 module

#### Scenario: export_macro 不接受本地闭包 options

- **给定** 单独的 `export_macro` 声明包含 `extra_functions` 或 `internal_function`
- **当** 校验该声明 options
- **那么** 这些 key 作为 unexpected options 报告并忽略
- **并且** 不执行 extra helper 校验或本地 internal policy 构造

#### Scenario: local_macro 接受本地闭包 options

- **给定** `local_macro` 声明包含 `extra_functions` 或 `internal_function`
- **当** 校验并注册该声明
- **那么** `extra_functions` 进入 local closure 构造
- **并且** `internal_function` 在 declaration MacroEnv 上解析和校验

#### Scenario: internal_function 解析 alias 来源

- **给定** 现有 `use_macro` `alias` 将 imported `M:F/A` 映射为 `Alias/A`
- **并且** 后续 `local_macro` 的 `internal_function` 选择 `Alias/A`
- **当** 构造 declaration 有效环境与原始 frozen form
- **那么** alias key 和原始远程 key 都不进入通用展开器 MacroEnv
- **并且** frozen form 的 `Alias(Args)` 改写为普通 `M:F(Args)`
- **并且** 该绑定进入 input fingerprint

### Requirement: attribute injection 使用扫描位置视图

attribute 宏的运行期注入环境 MUST 只由调用位置之前已经通过统一扫描的 forms 构造；该规则 MUST 对 external 与 local attribute macro 使用同一实现，不得为 local macro 建立另一套调用点规则。

#### Scenario: 只注入已通过扫描的 attributes

- **给定** attribute 宏声明 `inject_attrs`
- **并且** 调用位置之前和之后都存在目标 attribute
- **当** 在统一扫描中调用该宏
- **那么** 只注入调用位置之前已通过扫描的 attribute
- **并且** 尚在队列中的 attribute 不可见

#### Scenario: splice form 处理后才进入 passed 视图

- **给定** 属性宏 splice 出 G1、G2
- **当** 扫描正在处理 G1
- **那么** G2 尚不属于 passed forms
- **并且** G1 成功保留后才对后续调用可见

#### Scenario: external 与 local attribute 使用相同运行期规则

- **给定** external attribute macro 与已编译 local attribute macro 位于各自调用点
- **当** 统一扫描解析调用名称、组织调用参数并执行 `inject_attrs`
- **那么** 二者都使用各自调用点前已生效的 MacroEnv 与 passed forms
- **并且** local macro 不具有独立的运行期上下文规则

### Requirement: local macro 预展开使用声明位点运行时上下文

local macro frozen function forms 的预展开 MUST 使用 `-local_macro` declaration 前 passed forms 所确定的 `MacroRuntimeContext`，包括已生效的宏名称、调用参数、options 与可注入 attribute。完整 remaining source view MUST 只用于闭包发现，不能作为 context 或 `inject_attrs` 输入。GenerationCompiler MUST 只消费 canonical expanded forms，不得载入每个 declaration context 后重新展开。

#### Scenario: NeedCallable 不改变 declaration-time 预展开上下文

- **给定** `-local_macro([foo/1])` 前存在 attribute `early`
- **并且** declaration 后存在 `use_macro` 配置变化和 attribute `late`
- **并且** 更晚的 `-foo(...)` 产生 `NeedCallable(foo/1)`
- **当** 准备 canonical forms、完成必要编译并随后展开 `-foo(...)`
- **那么** frozen forms 使用 declaration 时的宏名称、调用参数和 `inject_attrs` 配置
- **并且** frozen forms 的注入值只来自 declaration 前 passed forms，因此可见 `early` 而不可见 `late`
- **并且** `-foo(...)` 本身随后按所有 attribute 宏共用的运行期规则执行

#### Scenario: remaining queue 只参与闭包发现

- **给定** local declaration 后方的 helper function 与 attribute 尚在 remaining queue
- **当** 注册、预展开并最终编译该 local macro
- **那么** helper function 可以进入 closure source view
- **并且** 尚未 pass 的 attribute 不能进入 local macro forms 的 `inject_attrs`

### Requirement: 扫描顺序保留局部 splice 顺序

统一扫描 MUST 保留 splice 结果的局部顺序，并只在 function 合并确有需要时执行最小整理。

#### Scenario: 生成 forms 位于当前位置

- **给定** 属性宏在原始 form F 的位置展开为 `[G1, G2]`
- **并且** 原队列在 F 后仍有 R
- **当** 扫描继续时
- **那么** G1、G2 先于 R 按该顺序处理

#### Scenario: 扫描不全局重排生成 function/spec

- **给定** 属性宏生成无冲突的 function 或 spec
- **当** 统一属性扫描结束时
- **那么** 该 form 保持其 splice 后的局部相对位置
- **并且** 不因全局 Generated/Base 拆分被移动

#### Scenario: 仅在 __original__ 冲突时合并

- **给定** 属性宏生成同名同 arity function
- **当** 生成 function 调用 `__original__/Arity`
- **那么** 原 function 被重命名且相关调用同步替换
- **并且** 无关 forms 的相对顺序保持不变

### Requirement: 宏执行隔离 traverse state

用户宏返回的 traverse computation MUST 在私有 state 中运行，且不得覆盖调用方的扫描或遍历 state。

#### Scenario: 属性宏的 state 不覆盖扫描 state

- **给定** 属性宏返回会执行 `put` 的 traverse computation
- **当** 统一扫描调用该宏
- **那么** computation 在私有 state 中执行
- **并且** 当前 ExternalEnv、passed forms 与扫描队列状态保持不变

#### Scenario: function 宏的 state 不泄漏到兄弟调用

- **给定** function 宏返回会修改 traverse state 的 computation
- **当** 最终 function pass 展开多个宏调用
- **那么** 每次宏执行的私有 state 不泄漏到调用方或兄弟调用
- **并且** formatter、位置和错误仍沿外层遍历传播

### Requirement: 最终 function 使用统一 FinalMacroRuntimeContext

retain 与最终普通 function pass MUST 使用 attribute scan 完成后的同一个 `FinalMacroRuntimeContext` 和同一个 ExpansionValidator。local closure target 的 LocalEnv MUST 由首次完整递归展开收集的 canonical whitelist 过滤，并 MUST 重放 declaration 的 internal macro key 移除及 alias-to-remote 改写；非 local-closure ordinary target MUST 保留完整 FinalLocalEnv，并以 `disabled` 展开。

#### Scenario: 最终展开跳过工作流指定的 forms

- **给定** local-macro 收尾返回 `FinalLocalEnv` 和 `FinalSkipIds`
- **当** 最终函数体展开执行时
- **那么** 使用包含 FinalLocalEnv、最终 options 与最终 injection forms 的 `FinalMacroRuntimeContext`
- **并且** 不展开 `FinalSkipIds` 中的 forms

#### Scenario: 最终映射不包含未编译 local macro

- **给定** local-macro 收尾返回的 FinalLocalEnv 只包含最终可调用 FA
- **当** 构造 function pass 的宏映射
- **那么** local macro 映射按 FinalLocalEnv 过滤
- **并且** 未编译或不可用的 FA 不参与最终匹配

#### Scenario: 最终环境与最后一次 local 展开结果比对

- **给定** function form 曾在 declaration MacroRuntimeContext 下作为 local closure 展开
- **并且** 该 form 被 retain 或被 Step 2 caller detection 选中
- **当** FinalMacroRuntimeContext fingerprint 与最后一次 local expansion 不同
- **那么** 从原始 form 应用 declaration internal bindings 后，在 FinalMacroRuntimeContext 下重新展开
- **并且** internal bindings 属于该次 input fingerprint
- **并且** 与最后一次已接受的 local expansion result 比较
- **并且** 结果不同时报告 `conflicting_local_macro_closure_environment`

#### Scenario: retained local macro 宏头不跳过最终比对

- **给定** retained function 是 local macro 宏头
- **当** 它进入最终 function 展开
- **那么** 使用与 helper 和普通 function 相同的 ExpansionValidator
- **并且** 不存在宏头专用的跳过比对规则

## MODIFIED Requirements

### Requirement: local-macro ExpansionRecord

local-macro ExpansionRecord MUST 同时保存 canonical whitelist 与 canonical expanded form。cache input key MUST 基于展开前可知的运行环境；白名单作为展开结果保存，不得作为首次 lookup 的唯一 key。

#### Scenario: 同一输入复用完整 expansion 结果

- **给定** 某个 FormId 和 input fingerprint 已缓存 whitelist 与 expanded form
- **当** 相同 input fingerprint 再次请求该 local-macro function expansion
- **那么** 系统复用缓存的 whitelist 和 expanded form
- **并且** 不为 cache lookup 预先执行白名单扫描
