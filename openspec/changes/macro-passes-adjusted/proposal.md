# Macro Passes Adjusted

## 摘要

将属性宏展开重构为单一的 scan-and-splice 流：外部属性宏与已经可调用的本地属性宏按源码顺序处理，宏环境变更只前向生效。

`-local_macro(...)` 的注册、闭包快照、按需累计编译、retain 与最终跳过集合属于独立的 [local-macro](../local-macro/proposal.md) 变更。本变更只定义统一扫描如何调用该流程。

## 范围

- 外部与本地属性宏统一参与同一次 scan-and-splice。
- `import_macro`、`use_macro`、`macro_options` 的环境更新在扫描中前向生效。
- 扫描遇到 `local_macro` declaration 时委托 local-macro 工作流注册。
- 扫描需要调用尚不可用的本地属性宏时委托该工作流完成必要编译。
- 扫描结束后使用该工作流提供的最终本地宏环境与最终跳过集合执行函数体展开。

## 不在范围内

- local macro 的闭包定义、冻结、缓存、累计编译和安全加载细节。
- retain 根及 retain 闭包计算。
- 修改 `outer` / `inner` 的遍历语义，或引入模块级 fixed-point。
