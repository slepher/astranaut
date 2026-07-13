# Macro Passes

## 摘要

将属性宏展开重构为单一的 scan-and-splice 流：外部属性宏与已经可调用的本地属性宏按源码顺序处理，宏环境变更只前向生效。

`-local_macro(...)` 的注册、闭包快照、按需累计编译、retain 与最终跳过集合属于独立的 [local-macro](../local-macro/proposal.md) 变更。本变更只定义统一扫描如何调用该流程。

`astranaut_macro` 继续提供唯一的 function 宏引用匹配和展开实现。local-macro 工作流向它传入已经应用声明位点快照、`internal_function` 及目标 FA 自身移除规则的最终 `MacroEnv`；同一展开器不区分普通 function 与 local macro function。实际 local 引用的识别也复用该统一宏匹配语义。

## 范围

- 外部与本地属性宏统一参与同一次 scan-and-splice。
- `import_macro`、`use_macro`、`macro_options` 的环境更新在扫描中前向生效。
- 生成 forms 在当前位置插回队列，普通 function body 延后到最终 function pass 展开。
- attribute injection 以当前位置之前已经通过扫描的 forms 为可见视图。
- scan-and-splice 保留局部顺序，只对需要 `__original__/Arity` 合并的生成 function/spec 做最小整理。
- 扫描遇到 `local_macro` declaration 时委托 local-macro 工作流注册。
- local macro 唯一特殊上下文规则是：编译其 function forms 时只使用 declaration 前 passed forms；完整闭包源码视图仅用于结构分析。编译完成后的 attribute 运行规则对 external/local 宏统一。
- 扫描需要调用尚不可用的本地属性宏时委托该工作流完成必要编译。
- 扫描结束后使用该工作流提供的最终本地宏环境与最终跳过集合执行函数体展开。
- 为 local-macro 工作流提供同构的引用解析与 function 展开操作。
- 保留宏冲突诊断、traverse/return 错误信息及宏执行 state 隔离。

## 不在范围内

- local macro 的闭包定义、冻结、缓存、累计编译和安全加载细节。
- retain 根及 retain 闭包计算。
- 修改 `outer` / `inner` 的遍历语义，或引入模块级 fixed-point。
