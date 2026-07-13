# Local Macro

## 摘要

为 `-local_macro(...)` 建立独立工作流：按 FA 注册、基于声明位点环境冻结和展开闭包、按需生成最小累计编译计划、缓存展开结果，并最终提供本地宏环境及函数体展开跳过集合。声明位点环境同时冻结宏名称/调用参数等 MacroEnv 配置和 declaration 前 passed forms 注入快照；完整 remaining source view 只用于闭包发现，不用于 `inject_attrs`。

该工作流实现为独立的 `astranaut_local_macro` 模块，而不是继续扩展 `astranaut_macro`。前者拥有 local macro 的状态与生命周期；后者继续拥有统一属性扫描、通用宏展开和 forms 队列。

两者通过同构的 function 展开接口协作：`astranaut_local_macro` 为每个目标
function 构造最终宏环境，`astranaut_macro` 只按该环境执行与普通 function
相同的引用匹配和递归展开。local macro 实际引用的识别继续复用
`astranaut_macro` 的统一宏匹配语义；`internal_function`、声明快照以及目标 FA
不进入自身宏环境等 local 专属规则由 `astranaut_local_macro` 负责。

## 范围

- local macro 注册、重复声明检查与声明顺序。
- 静态函数闭包、原始 form 冻结与多环境冲突检测。
- 按需及 scan 收尾的累计编译、同模块安全覆盖加载。
- `local_macro_retain`、`export`、`export_macro` 的统一 retain 规则。
- 最终本地宏环境和跳过集合。

统一属性扫描如何调度该工作流见 [macro-passes](../macro-passes/proposal.md)。
