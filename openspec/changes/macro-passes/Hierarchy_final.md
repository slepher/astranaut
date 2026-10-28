# Macro Passes 最终处理层级

2026-10-28：本模型采用首次绑定、延迟单次展开和策略 A，替代此前的 ExpansionValidator／白名单／重放协议。完整契约见 [macro-passes 设计](design.md) 和 [local-macro 设计](../local-macro/design.md)。

```text
Attribute pass
  -> 按源码顺序 scan-and-splice
     -> 环境指令前向更新 registry
     -> local declaration 只登记闭包及首次声明前快照
     -> 属性目标按调用点环境解析
        -> local provider 必要时准备 canonical forms 与累计 generation
        -> 通用 expander 执行目标，返回 forms 原位 splice
  -> provider 收尾
     -> 准备全部 canonical forms
        -> 无宏函数/spec 直接复用
        -> 含宏函数先确保首次绑定前缀就绪，再展开一次
     -> 确保最终累计 generation
     -> 删除 local-only forms，物化 retained canonical forms
     -> 返回 ExpansionSkipIds 与 warnings
  -> 构造 FinalMacroEnvironment，筛选 callers 并排除 ExpansionSkipIds
  -> 排序 forms
Function pass
  -> 通用 expander 展开其余普通 callers
```

| 场景 | 环境与行为 |
| --- | --- |
| 属性调用 | 调用点环境与当前 AttributeEnv |
| 首次绑定闭包函数 | 首次进入闭包时的声明前快照；延迟展开一次 |
| 共享 helper、retain、export | 直接复用首次 canonical form |
| 普通最终函数 | 完整 FinalMacroEnvironment |
| generation compiler | 仅消费 canonical forms 与编译用 source view |

函数展开器只有整数重命名 state；返回树规范化使用隔离的 presence state。它不管理本地宏可调用性、白名单或重试。provider 不再参与最终函数 pass；扫描器不解释编译计划。

策略 A 允许含外部宏的闭包提前编译未实际调用的前序本地宏，以保证动态 replacement 无需挂起。独立无宏闭包合并编译一次；重复或较小 prefix 请求复用当前 generation。
