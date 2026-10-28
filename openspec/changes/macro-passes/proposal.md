# Macro Passes

## 摘要

外部与本地属性宏共用源码序 scan-and-splice，环境更新只前向生效。通用展开器共用匹配、调用、返回树规范化及 outer/inner 递归，不持有本地宏白名单、可调用调度或重试协议。

2026-10-28 采用 [local-macro](../local-macro/design.md) 的首次绑定与延迟展开方案：声明只登记，provider 在本地属性调用或扫描收尾准备 canonical forms 和累计 generation；retained forms 在收尾物化并从最终 caller 集合排除。普通函数继续使用完整 FinalMacroEnvironment。

## 范围

- 保持增量 AttributeEnv、注入时点、局部 splice 顺序及 __original__ 最小合并。
- 删除 observation/whitelist、needed_macro_ids 和 function-pass provider 钩子。
- 返回 AST 在一次 traversal 中完成规范化及宏 presence 检查。
- provider 通过 ExpansionSkipIds 明确表达保留但不再展开的 forms。
- 保持宏冲突诊断、错误位置、用户宏 state 隔离和单次 attribute 收尾排序。

## 不在范围内

registry 双轨映射清理、模块级 fixed-point、编译器内 JIT 回调及 outer/inner 语义变更。
