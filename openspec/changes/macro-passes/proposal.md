# Macro Passes

## 摘要

将属性宏展开重构为单一的 scan-and-splice 流：外部属性宏与已经可调用的本地属性宏按源码顺序处理，宏环境变更只前向生效。

`-local_macro(...)` 的逐 FA 声明注册、闭包快照、预展开/一致性记录、依赖驱动累计编译、retain 与最终跳过集合属于独立的 [local-macro](../local-macro/proposal.md) 变更。本变更定义统一扫描如何调用该流程，以及 retain/普通 function 如何共享最终 `MacroRuntimeContext`。

`astranaut_macro` 负责 pass 编排，`astranaut_macro_scan` 负责 source-ordered
scan-and-splice，`astranaut_macro_registry` 负责宏声明与阶段化环境，
`astranaut_macro_expander` 提供唯一的 function 宏发现—执行实现。local-macro 工作流直接向 expander
传入声明位点候选环境，以及显式的 `disabled`、`collect` 或
`verify(Expected)` 白名单控制；普通 function 始终传 `disabled`。白名单记录 frozen
function 自身展开的真实 match，并由 `process_macro_return` 在既有返回树 traversal 中
一次性收集每个 macro 返回 AST 的 local macro presence。

首次成功展开建立 FormId 的 canonical whitelist；后续 declaration/final 处理在同一次 traversal 中校验。白名单冲突与最终 AST 结果冲突是两条独立不变量。final retained local closure 按 canonical whitelist 过滤 FinalLocalEnv，名单外调用保持普通 Erlang 调用；普通 Step 2 function 仍使用完整 FinalMacroEnv。

## 范围

- 外部与本地属性宏统一参与同一次 scan-and-splice。
- `import_macro`、`use_macro`、`macro_options` 的环境更新在扫描中前向生效。
- 生成 forms 在当前位置插回队列，普通 function body 延后到最终 function pass 展开。
- attribute 的 `inject_attrs` 从当前位置之前已经进入增量 `AttributeEnv` 的
  attributes 取值；尚未扫描的 forms 不可见。
- scan-and-splice 保留局部顺序，只对需要 `__original__/Arity` 合并的生成 function/spec 做最小整理。
- 扫描遇到 `local_macro` declaration 时委托 local-macro 工作流注册。
- local macro 唯一特殊上下文规则是：预展开其 function forms 时使用 declaration
  前已解析的宏环境；完整闭包源码视图仅用于结构分析。编译完成后的 attribute
  运行规则对 external/local 宏统一。
- 扫描需要调用尚不可用的本地属性宏时委托该工作流完成必要编译。
- 扫描结束后使用该工作流提供的最终本地宏环境与最终跳过集合执行函数体展开。
- 为 local-macro 工作流提供统一的 function 展开、白名单观察和结果验证操作。
- 为通用 function 展开入口增加显式 whitelist control；原始 function 在统一发现—执行点观察，返回 AST 在 `process_macro_return` 的既有 traversal 中批量收集。
- `process_macro_return` 返回 `{ProcessedNode, ReturnAnalysis}`，同次收集 local FAs 与总体 macro presence，不校验、不展开；调用方合并并批量校验 local FAs，只有含宏的 accepted replacement 才进入原有递归展开路径。
- 在 ExpansionRecord 中保存 canonical whitelist 及 per-input whitelist/result，并区分 `conflicting_local_macro_whitelist` 与 `conflicting_local_macro_closure_environment`。
- replacement 首次匹配尚不可调用的候选 local macro 时，通过通用 `NeedCallable` 编译所需累计 boundary，并从 frozen form 重试后原子提交结果。
- final retained local closure 使用 canonical whitelist 过滤有效 LocalEnv 和 fingerprint；普通 function 明确禁用 whitelist。
- 保留宏冲突诊断、traverse/return 错误信息及宏执行 state 隔离。

## 不在范围内

- local macro 的闭包定义、冻结、缓存、累计编译和安全加载细节。
- retain 根及 retain 闭包计算。
- 修改 `outer` / `inner` 的遍历语义，或引入模块级 fixed-point。
- 为白名单增加独立 scanner、完整 function 重扫、expanded/original AST diff、declaration group 或 callback 状态通道。
- 用白名单替代不同 external/options/inject 运行环境下的最终 AST 一致性比较。
