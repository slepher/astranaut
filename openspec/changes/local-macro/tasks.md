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

- [x] 新增 `astranaut_local_macro.erl`，实现按 FA 的注册表和状态转换。
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
- [x] B 可展开 A，而展开共享 A form 时 A 不进入自身宏环境。

## 声明位点注入快照任务（新增，保留既有任务状态）

### 规格

- [x] 明确 closure source view 只用于闭包发现，local frozen forms 的 `inject_attrs` 使用 declaration 前 passed forms 快照。

### 实现

- [x] 在 local macro 注册条目和 compile request 中保存 `inject_forms_snapshot`，并与 `closure_source_view` 分离。
- [x] 使用 declaration-time `inject_forms_snapshot` 调用共享 function 展开器，并将该快照纳入 `EnvFingerprint`。
- [x] 保证 `CompileContext.source_view` 只服务累计模块物化、分析和加载，不覆盖 request 的 declaration-time MacroEnv/InjectForms。

### 测试

- [x] 验证 declaration 后 attributes 不进入 local frozen forms 的 `inject_attrs`。
- [x] 验证声明后 use/options 变化不改变 frozen forms 的宏名称、调用参数与注入配置。
- [x] 验证同一 frozen form 在不同 declaration 注入快照下按 fingerprint 分离缓存，并在展开结果不一致时维持既有冲突诊断。

## DeclarationGroup、预展开与 canonical 编译任务（新增）

> 本节取代旧实现中的逐 TargetFA 环境、编译计划内展开以及 retain 宏头跳过最终比对规则；保留上述条目作为历史完成记录。

### 实现

- [ ] 新增 declaration group 状态，使 members 共享 order、`MacroRuntimeContext`、options 与 environment fingerprint。
- [ ] 构造 group MacroEnv 时整体排除 declaration members，成员间调用不记录为 local macro dependency。
- [ ] 注册完成后调用统一预展开操作，仅在真实依赖需要 callable local macro 时进入 dependency scheduler。
- [ ] 用 `ExpansionRecord` 取代编译计划内的 `{FormId, EnvFingerprint}` request 展开：记录 last env/result、canonical result 与 per-env cache。
- [ ] 将 `execute_plan` 拆成 expansion preparation、dependency scheduling 和 canonical generation compilation。
- [ ] 让 compiler 只消费 `canonical_expanded_forms`，成功后才更新 `compiled_forms`、boundary cache、status 和 generation。
- [ ] retain 只计算生命周期集合；实际 retain function 与普通 Step 2 function 一起使用 final context 展开验证。
- [ ] 删除 local macro 宏头跳过最终环境比对的特殊规则。

### 测试

- [ ] 同 declaration 多 FA 共享 context，互相不作为宏，但仍可形成普通闭包调用。
- [ ] 预展开无依赖、预展开触发依赖编译、预展开失败的 record 原子性。
- [ ] 环境 E1 → E2 → E1 时命中 per-env cache，且 canonical result 保持唯一。
- [ ] compiler 对 canonical forms 的输入不触发任何 request-specific 展开。
- [ ] retain 宏头、retain helper 和 Step 2 ordinary function 使用同一 final comparison 行为。
