# Tasks

## 规格

- [x] 记录闭包为静态函数依赖集合，而非词法 closure。
- [x] 记录按 FA 注册、重复声明与声明顺序。
- [x] 记录完整源码视图、已 pass 环境快照及原始 form 冻结。
- [x] 记录 declaration 前 import 可见、后续环境不回溯，以及仅实际引用 local macro 进入环境。
- [x] 记录多环境展开缓存与冲突规则。
- [x] 记录按 declaration 顺序的最小累计编译。
- [x] 记录 scan 收尾重新构造全部 local macro 的最终累计模块。
- [x] 记录同模块安全覆盖加载。
- [x] 记录 retain 根、闭包 retain 与最终跳过集合。
- [x] 记录 `extra_functions`、`internal_function` 和 local macro 自身递归的闭包规则。
- [x] 记录同构 function 展开接口、统一 local 引用解析及逐目标 EffectiveEnv。

## 实现

- [x] 新增 `astranaut_macro_local.erl`，实现按 FA 的注册表和状态转换。
- [x] 实现闭包计算、`extra_functions`、`internal_function` 策略和重复声明检查。
- [x] 实现冻结 form 改写保护及按环境展开缓存。
- [x] 实现多环境结果比较和 `conflicting_local_macro_closure_environment`。
- [x] 实现最小累计编译计划与最终全量累计编译。
- [x] 实现 declaration 环境快照，且不受后续外部环境更新影响。
- [x] 实现 `<Module>__local_macro` 的安全覆盖加载和模块级互斥。
- [x] 实现 retain 根闭包展开、最终环境比对和 FinalSkipIds。
- [x] 将 plan 执行留在 local-macro 工作流，并通过统一 MacroOps 解析引用和展开 function。
- [x] 实现 `EffectiveEnv = snapshot + referenced - internal - target`，不在通用展开器中加入 local 特判。

## 测试

- [x] 同一 FA 重复 declaration 报错。
- [x] 后声明 local macro 属于先声明闭包时按 helper 多环境规则处理。
- [x] 共享 form 的不同环境结果一致时成功，不一致时报错。
- [x] 声明前 import 可见、声明后 import 不可见，且闭包仅取得实际引用 local macro。
- [x] local macro 自身递归调用保持普通函数调用。
- [x] B 依赖 A 时按 `{A}`、`{A,B}` 编译；无依赖时仅 `{A,B}`。
- [x] scan 收尾重新编译全部 local macro，并复用既有展开缓存。
- [x] 相同 form/environment 命中缓存，不重复展开。
- [x] extra_functions 成功补充 helper；不存在的 helper 报 `invalid_extra_functions`。
- [x] 共享闭包函数的 internal_function 策略冲突报错。
- [x] `local_macro_retain`、`export`、`export_macro` 保留完整闭包及 spec forms。
- [x] retain helper 的最终环境比对不一致报错；retain 宏头跳过该比对。
- [x] 已展开未 retain forms 进入 FinalSkipIds；retain 闭包参与最终展开。
- [x] old code 仍被引用时安全加载以 `local_macro_module_in_use` 失败。
- [x] local 引用识别与普通 function 展开使用相同调用匹配语义。
- [x] internal_function 在 local 模块裁剪环境；通用展开器不解释该 option。
- [x] internal_function 在 declaration 位点校验宏 key，支持 `{M,F,A}` 远程表示。
- [x] 复用现有 `use_macro` `alias` 来源，将 internal alias 调用还原为普通 `M:F(...)`，并同时屏蔽 alias/remote 宏 key。
- [x] declaration 与 FinalFunctionContext 重展开共用 internal bindings，且 bindings 进入 fingerprint。
- [x] B 可展开 A，而展开共享 A form 时 A 不进入自身宏环境。

## 声明位点注入快照任务（新增，保留既有任务状态）

### 规格

- [x] 明确 closure source view 只用于闭包发现，local frozen forms 的 `inject_attrs` 使用 declaration 前 passed forms 快照。

### 实现

- [x] 在 local macro 注册条目和 expansion request 中保存唯一的 `runtime_context_snapshot`，其 `inject_forms` 与 `closure_source_view` 分离。
- [x] 使用 declaration-time `runtime_context_snapshot.inject_forms` 调用共享 function 展开器，并将该值纳入 `EnvFingerprint`。
- [x] 保证 `LocalMacroWorkflowContext.source_view` 只服务累计模块物化、分析和加载，不覆盖 request 的 declaration-time MacroRuntimeContext。

### 测试

- [x] 验证 declaration 后 attributes 不进入 local frozen forms 的 `inject_attrs`。
- [x] 验证声明后 use/options 变化不改变 frozen forms 的宏名称、调用参数与注入配置。
- [x] 验证同一 frozen form 在不同 declaration 注入快照下按 fingerprint 分离缓存，并在展开结果不一致时维持既有冲突诊断。

## 声明快照、预展开与 canonical 编译任务

> 本节取代旧实现中的逐 TargetFA 环境、编译计划内展开以及 retain 宏头跳过最终比对规则；保留上述条目作为历史完成记录。

### 实现

- [x] 让同 declaration members 的逐 FA 条目共享 order、`MacroRuntimeContext`、options 与 environment fingerprint，不建立独立 group 状态。
- [x] declaration-time form 扫描以声明前候选环境产生 `referenced_local_macros` 白名单；declaration/final 展开均只开放白名单 local FAs，成员间调用不记录为 local macro dependency。
- [x] 注册完成后调用统一预展开操作，仅在真实依赖需要 callable local macro 时进入 dependency scheduler。
- [x] 用 `ExpansionRecord` 取代编译计划内的 `{FormId, EnvFingerprint}` request 展开：记录 last env/result、canonical result 与 per-env cache。
- [x] 让 `execute_plan` 显式协调 expansion preparation、dependency scheduling 和 canonical generation compilation，并保持 `compile_boundary` 不执行展开。
- [x] 让 compiler 只消费 `canonical_expanded_forms`，成功后才更新 `compiled_forms`、boundary cache、status 和 generation；boundary key 仅为累计 members。
- [x] retain 只计算生命周期集合；实际 retain function 与普通 Step 2 function 一起使用 final context 展开验证。
- [x] 删除 local macro 宏头跳过最终环境比对的特殊规则。

### 测试

- [x] 同 declaration 多 FA 共享 order/context，互相不作为宏，但仍可形成普通闭包调用。
- [x] 预展开无依赖、预展开触发依赖编译、预展开失败的 record 原子性。
- [x] 环境 E1 → E2 → E1 时命中 per-env cache，且 canonical result 保持唯一。
- [x] compiler 对 canonical forms 的输入不触发任何 request-specific 展开。
- [x] 独立的连续 declaration 不在预展开时编译，最终只产生一个累计 members generation。
- [x] retain 宏头、retain helper 和 Step 2 ordinary function 使用同一 final comparison 行为。

### 上下文接口收敛

- [x] Entry 和 ExpansionRequest 只保存一份完整的 `runtime_context_snapshot`；删除 `declaration_groups`、`group_id` 和 `group_members` 双重状态。
- [x] 删除 ExpansionRequest 中未消费的 `fa`、`already_compiled`、`options` 及重复 snapshot 字段，仅保留 6 个必需字段。
- [x] 用命名 map type 约束 workflow context、MacroRuntimeContext、MacroOps、ExpansionRequest 和 CompilationBoundary。

### 简洁性复核收口

- [x] 删除 workflow context 中重复的 `local_macro_map` 和 MacroOps 中只做 map merge 的回调，注册时直接保存完整 RuntimeContext。
- [x] 用扫描得到的 `referenced_local_macros` 作为 declaration/final 共用白名单，删除按 order/self/direct-call 计算的 final 排除路径。
- [x] 删除重复的 expanded-form cache、final boundary 标记、空 retained forms 返回值和仅供测试的 retained 校验旁路。
- [x] 将扫描主流程改为直接呈现互斥 form 分支，并删除无语义 monadic bind。
- [x] 覆盖同声明成员最终普通调用，以及普通 attribute 分隔的独立 declaration 合并边界。
- [x] 覆盖后声明 local macro 不进入先前闭包的 final 环境，以及普通 final function 仍可见完整 FinalLocalEnv。

## 最终审核落实

- [x] 文档明确闭包扫描只跟随直接本地 call，间接引用使用 `extra_functions`。
- [x] 对不存在及存在但未命中冻结闭包的显式 `local_macro_retain` 分别发出带位置 warning。
- [x] 保留 retained frozen function 的 FinalMacroRuntimeContext 重新展开与 canonical 结果比对。
- [x] 明确多 FA declaration 只共享注册时快照，扫描后不保留 group identity。
