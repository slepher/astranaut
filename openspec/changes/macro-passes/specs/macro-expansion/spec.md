# Macro Expansion Spec Delta

## ADDED Requirements

### Requirement: local 与普通 function 使用同构展开

`astranaut_macro` MUST 以同一 function 展开实现处理最终普通 function pass 与 local-macro 工作流提交的目标 functions。

#### Scenario: 展开器不解释 local 专属策略

- **给定** local-macro 工作流已经构造最终 MacroEnv
- **当** 调用统一 function 展开操作
- **那么** 展开器只按 MacroEnv 匹配和递归展开宏调用
- **并且** 不读取 internal_function、generation、retain 或 declaration order

#### Scenario: local 引用复用统一调用匹配

- **给定** local-macro 工作流提供候选 local 宏环境和闭包 functions
- **当** 解析闭包实际引用的 local macros
- **那么** 使用与普通 function 展开相同的调用匹配语义

#### Scenario: 目标自身移除由 local 工作流完成

- **给定** local-macro 工作流请求展开 TargetFA
- **当** 统一展开器收到 MacroEnv
- **那么** TargetFA 已由 local-macro 工作流从环境移除
- **并且** 展开器不包含 local macro 自身递归特判

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

#### Scenario: 尚不可调用的本地属性宏触发工作流

- **给定** `-local_macro([foo/1])` 已注册但尚不可调用
- **并且** 扫描遇到 `-foo(...)`
- **当** 处理该属性时
- **那么** 扫描委托 local-macro 工作流完成必要编译
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

### Requirement: local macro 编译使用声明位点注入快照

local macro 的唯一特殊上下文规则是：其 frozen function forms 的编译 MUST 使用 `-local_macro` declaration 前 passed forms 所确定的完整宏展开上下文，包括已生效的宏名称、调用参数、options 与可注入 attribute。完整 remaining source view MUST 只用于闭包发现，不能作为编译上下文或 `inject_attrs` 输入。更晚的 attribute 调用即使触发按需编译，也不得把调用点环境传入 local macro forms 展开。

#### Scenario: 按需编译不改变 declaration-time 编译上下文

- **给定** `-local_macro([foo/1])` 前存在 attribute `early`
- **并且** declaration 后存在 `use_macro` 配置变化和 attribute `late`
- **并且** 更晚的 `-foo(...)` 触发 foo 按需编译
- **当** 编译 foo 的 frozen forms 并随后展开 `-foo(...)`
- **那么** frozen forms 使用 declaration 时的宏名称、调用参数和 `inject_attrs` 配置
- **并且** frozen forms 的注入值只来自 declaration 前 passed forms，因此可见 `early` 而不可见 `late`
- **并且** `-foo(...)` 本身随后按所有 attribute 宏共用的运行期规则执行

#### Scenario: remaining queue 只参与闭包发现

- **给定** local declaration 后方的 helper function 与 attribute 尚在 remaining queue
- **当** 注册并最终编译该 local macro
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

### Requirement: 最终展开使用 local-macro 收尾结果

最终 function pass MUST 使用 local-macro 收尾返回的最终环境和跳过集合。

#### Scenario: 最终展开跳过工作流指定的 forms

- **给定** local-macro 收尾返回 `FinalLocalEnv` 和 `FinalSkipIds`
- **当** 最终函数体展开执行时
- **那么** 使用 `ExternalEnv + FinalLocalEnv`
- **并且** 不展开 `FinalSkipIds` 中的 forms

#### Scenario: 最终映射不包含未编译 local macro

- **给定** local-macro 收尾返回的 FinalLocalEnv 只包含最终可调用 FA
- **当** 构造 function pass 的宏映射
- **那么** local macro 映射按 FinalLocalEnv 过滤
- **并且** 未编译或不可用的 FA 不参与最终匹配
