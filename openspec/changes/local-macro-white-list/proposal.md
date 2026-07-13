# Local Macro White List

## 摘要

为 local-macro frozen function 的宏展开增加显式、可关闭的 local-macro 白名单控制。白名单只记录该 function 在自身展开及宏返回 AST 的递归处理过程中实际匹配的 local macro FA；普通 function forms 继续使用现有通用展开路径，不收集也不比较白名单。

白名单处理复用宏返回值已有的 `process_macro_return` traversal。宏返回的 replacement AST 在完成规范化、位置和变量更新时同步参与后续宏匹配，不增加第二次完整 AST traverse。

## 动机

仅从最终环境排除自身或同 declaration 成员，仍可能让后声明或与当前 frozen form 无关的 local macros 改变旧 function 的调用解释。现有结果一致性比较只能在完整展开完成后报告 AST 差异，也不能直接指出具体改变了哪些 local macro 引用。

白名单把“同一 frozen FormId 实际匹配的 local macro 集合保持一致”定义为独立不变量：首次 local-macro function 展开收集 canonical whitelist，后续 declaration/final 处理在同一次 traversal 中校验；发现 canonical whitelist 之外的 FA 时可立即失败，完整 traversal 结束后再检查缺失 FA。

## 范围

- 为通用 function 展开入口增加显式 whitelist control 参数。
- 仅在 `astranaut_local_macro` 发起的 frozen function 展开及其宏返回 AST 处理中启用。
- 将 local macro 匹配观察接入 `process_macro_return` 已有 traversal，不新增扫描 pass。
- 在 ExpansionRecord 中保存 canonical whitelist，并区分白名单冲突与展开结果冲突。
- 保持普通 Step 2 function、普通 retained function 和一般 attribute macro 调用的 whitelist control 为 `disabled`。

## 非目标

- 不改变普通 function forms 可见的最终宏环境。
- 不用白名单替代不同运行环境下的最终 AST 一致性比较。
- 不为白名单单独建立 declaration group、额外 forms 扫描器或 AST diff。
- 不改变 GenerationCompiler、累计 boundary、safe load 或 retain 生命周期规则。

