# 宏展开规范增量

## 需求：local 与普通 function 使用同构展开

`astranaut_macro` 必须以同一 function 展开实现处理最终普通 function pass 与
local-macro 工作流提交的目标 functions。

### 场景：展开器不解释 local 专属策略

- **给定** local-macro 工作流已经构造最终 MacroEnv
- **当** 调用统一 function 展开操作
- **那么** 展开器只按 MacroEnv 匹配和递归展开宏调用
- **并且** 不读取 internal_function、generation、retain 或 declaration order

### 场景：local 引用复用统一调用匹配

- **给定** local-macro 工作流提供候选 local 宏环境和闭包 functions
- **当** 解析闭包实际引用的 local macros
- **那么** 使用与普通 function 展开相同的调用匹配语义

### 场景：目标自身移除由 local 工作流完成

- **给定** local-macro 工作流请求展开 TargetFA
- **当** 统一展开器收到 MacroEnv
- **那么** TargetFA 已由 local-macro 工作流从环境移除
- **并且** 展开器不包含 local macro 自身递归特判

## 需求：属性宏统一参与 scan-and-splice

外部属性宏与已可调用的本地属性宏必须在同一次 scan-and-splice 扫描中按源码顺序处理。

### 场景：本地属性宏在统一扫描中展开

- **给定** `-local_macro([foo/1])` 已由 local-macro 工作流注册并可调用
- **并且** 后续存在 `-foo(...)`
- **当** 模块执行统一属性扫描时
- **那么** 该属性在当前位置以当前宏环境展开

### 场景：外部属性宏生成后续外部属性调用

- **给定** 外部属性宏 A 展开后生成属性宏调用 B
- **当** A 在统一扫描中展开
- **那么** B 插回当前位置并在同一扫描中展开

### 场景：普通生成 forms 延后到最终函数体展开

- **给定** 属性宏展开后生成包含宏调用的 function form
- **当** 属性扫描继续
- **那么** function form 保留在 forms 流中
- **并且** 不在属性扫描阶段递归展开其函数体

### 场景：尚不可调用的本地属性宏触发工作流

- **给定** `-local_macro([foo/1])` 已注册但尚不可调用
- **并且** 扫描遇到 `-foo(...)`
- **当** 处理该属性时
- **那么** 扫描委托 local-macro 工作流完成必要编译
- **并且** 随后在当前位置展开 `-foo(...)`

## 需求：宏环境前向生效

`import_macro`、`use_macro`、`macro_options` 以及本地属性宏生成的这些 form 仅影响后续扫描到的 forms。

### 场景：本地属性宏生成 import

- **给定** 已可调用的本地属性宏展开后生成 `-import_macro(macro_b)`
- **并且** 后续 form 依赖 `macro_b`
- **当** 扫描继续时
- **那么** 后续 form 在包含 `macro_b` 的环境中处理

### 场景：已处理属性不回扫

- **给定** 一个属性在 `macro_b` 导入前已完成展开
- **并且** 后续属性生成 `-import_macro(macro_b)`
- **当** 扫描继续时
- **那么** 先前属性的结果不重新处理

### 场景：生成的 import 对同一 splice 后续属性可见

- **给定** 属性宏返回 `[import_macro(macro_b), DependentAttribute]`
- **并且** DependentAttribute 依赖 macro_b
- **当** 扫描处理该 splice 结果时
- **那么** import_macro 先更新环境
- **并且** DependentAttribute 随后在包含 macro_b 的环境中展开

### 场景：生成的 local_macro declaration 进入同一扫描

- **给定** 属性宏展开后生成 `-local_macro([foo/1])`
- **当** 该 declaration 被重新扫描时
- **那么** 扫描委托 local-macro 工作流注册 foo/1
- **并且** 后续 `-foo(...)` 可按 local-macro 规则请求可调用性

### 场景：use_macro 的同名 option 由后者覆盖

- **给定** 先处理 `-use_macro({FA, [{xxx1, true}, {xxx2, true}]}).`
- **并且** 随后处理 `-use_macro({FA, [{xxx1, false}]}).`
- **当** 后续 form 使用 FA 时
- **那么** 有效 options 为 `[{xxx1, false}, {xxx2, true}]`

### 场景：export_macro 不进入本模块的 local macro 环境

- **给定** 模块声明 `-export_macro([foo/0])`
- **并且** 未声明 `-local_macro([foo/0])`
- **当** 本模块出现非限定调用 `foo()`
- **那么** 该调用保持普通 Erlang 函数调用，不作为宏展开

### 场景：local_macro 与 export_macro 可组合

- **给定** 同一 `foo/0` 同时声明为 `-local_macro([foo/0])` 和 `-export_macro([foo/0])`
- **当** 本模块出现非限定调用 `foo()`
- **那么** 该调用使用 local_macro 的 declaration-time 本地宏环境展开
- **并且** 原模块中的 `foo/0` 保持导出，供其他模块 import 为宏

## 需求：扫描顺序保留局部 splice 顺序

### 场景：生成 forms 位于当前位置

- **给定** 属性宏在原始 form F 的位置展开为 `[G1, G2]`
- **并且** 原队列在 F 后仍有 R
- **当** 扫描继续时
- **那么** G1、G2 先于 R 按该顺序处理

### 场景：扫描不全局重排生成 function/spec

- **给定** 属性宏生成无冲突的 function 或 spec
- **当** 统一属性扫描结束时
- **那么** 该 form 保持其 splice 后的局部相对位置
- **并且** 不因全局 Generated/Base 拆分被移动

## 需求：最终展开使用 local-macro 收尾结果

### 场景：最终展开跳过工作流指定的 forms

- **给定** local-macro 收尾返回 `FinalLocalEnv` 和 `FinalSkipIds`
- **当** 最终函数体展开执行时
- **那么** 使用 `ExternalEnv + FinalLocalEnv`
- **并且** 不展开 `FinalSkipIds` 中的 forms
