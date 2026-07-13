# Tasks

local macro 专属任务移至 [local-macro/tasks.md](../local-macro/tasks.md)。

## 规格

- [x] 记录外部与本地属性宏统一参与 scan-and-splice。
- [x] 记录环境变更的前向生效与不回扫规则。
- [x] 记录生成属性、环境 form 与普通 forms 的当前位置处理及延后函数体展开规则。
- [x] 记录 splice 的局部顺序与禁止全局 Generated/Base 重排规则。
- [x] 记录 attribute injection 只读取当前位置之前的 passed forms。
- [x] 记录 import/use 的消费语义、macro_options 的保留语义及宏 key 冲突规则。
- [x] 记录 traverse/return 桥接和用户宏 traverse state 隔离。
- [x] 记录 `local_macro` declaration 和未就绪本地属性调用委托 local-macro 工作流。
- [x] 记录 scan 收尾使用 local-macro 提供的最终环境和跳过集合。
- [x] 记录 local 与普通 function 使用同构展开，以及统一 local 引用匹配边界。

## 实现

- [x] 重构 attribute pass，使其携带 ExternalEnv 与 LocalMacroState。
- [x] 将 `local_macro` 注册和按需可调用性检查接入 local-macro 模块。
- [x] 保留 scan-and-splice 对生成属性的当前位置重扫语义。
- [x] 让生成的 import/use/macro_options 在处理下一 form 前更新 ExternalEnv。
- [x] 维护 passed forms 与 remaining queue 两种不同的扫描视图。
- [x] 仅对需要 `__original__/Arity` 合并的生成 function/spec 做最小整理。
- [x] 通过冲突检查合并宏映射，仅在 `force_override` 时覆盖不同定义。
- [x] 以 scoped state 执行用户宏返回的 traverse computation。
- [x] 在 attribute pass 收尾剔除 FinalSkipIds，再运行 function pass。
- [x] 将 FinalLocalEnv 接入 function pass，并过滤未编译 local macro。
- [x] 提供不含 local 专属策略的统一 function 展开和 local 引用解析操作。
- [x] 扫描器只调用 local-macro 注册、确保可调用和收尾接口，不自行执行编译计划。

## 测试

- [x] 增加测试：外部与已就绪本地属性宏按同一源码顺序扫描。
- [x] 增加测试：本地属性宏生成环境变更后，后续 form 可见且先前 form 不回扫。
- [x] 增加测试：尚未就绪的本地属性宏调用会触发 local-macro 工作流。
- [x] 增加测试：属性宏生成 import 与依赖该 import 的后续属性时按 splice 顺序展开。
- [x] 增加测试：attribute injection 只包含已经通过扫描的 attributes。
- [x] 增加测试：生成的 macro_options 对后续展开生效。
- [x] 增加测试：宏 key 冲突失败及 `force_override` 成功。
- [x] 增加测试：生成的普通 function 延后到最终函数体展开，且无冲突 function/spec 不被全局重排。
- [x] 增加测试：attribute/function 宏的 traverse state 与框架 state 隔离。
- [x] 重新运行现有 uniform macro 与 macro validation 测试套件。
- [x] 增加测试：local macro function 与普通 function 复用同一展开语义。
- [x] 增加测试：目标 FA 自身移除和 internal_function 不在通用展开器中实现。

## Hierarchy_final 任务（新增，保留既有任务状态）

### 规格

- [x] 明确唯一的 `runtime_context_snapshot` 同时携带 macro map/options/inject forms，`closure_source_view` 仅是闭包结构输入而非宏上下文。
- [x] 明确 local macro 唯一特殊规则是 function-form 编译上下文仅限 declaration 前 passed forms；attribute 运行期规则对 external/local 宏通用。
- [x] 将 `Hierarchy_final.md` 识别的 P0–P3 差距转化为实现和测试任务。

### 实现

- [x] **P0：声明位点注入快照。** 注册 local declaration 时单独保存 declaration 前 `passed_forms`；展开 frozen local forms 时用它执行 `inject_attrs` 和构造环境 fingerprint，不再把包含 remaining queue 的 closure source view 作为 `InjectForms`。
- [x] **P0：隔离编译期与运行期。** attribute 触发 `need_callable` 时，只允许 declaration 前 passed forms 进入 local function-form 编译上下文；编译完成后的 attribute 调用继续走 external/local 共用的运行期 MacroEnv/PassedForms 规则，不新增 local 专用运行路径。
- [x] **P1：统一跨来源有效宏环境。** 让 external/local 宏 entry 按源码位置走同一冲突与 `force_override` 更新规则，避免固定 `maps:merge(External, Local)` 决定 winner 或延迟冲突。
- [x] **P2：明确并实现 `__original__` 的 spec merge。** 按 `Hierarchy_final.md` 的 spec 归属规则处理原 spec、生成 spec 与重命名原函数。
- [x] **P3：封装 local declaration 单次语义校验。** 注册和 local macro map 构造共享同一份成功校验结果，同时保留失败不回滚先前注册且诊断不重复的行为。

### 测试

- [x] local declaration 前后存在目标 attribute 时，frozen local forms 的 `inject_attrs` 只包含 declaration 前已 pass 的值。
- [x] declaration 后 `use_macro` 修改 alias、调用参数或 `inject_attrs` 配置时，frozen local forms 仍使用 declaration-time 配置。
- [x] 更晚 attribute 触发按需编译时，验证 local function forms 仍只使用 declaration 前 passed forms，并验证后续 attribute 与 external attribute 使用同一运行期规则。
- [x] remaining queue 中 helper 可进入 local closure，但尚未 pass 的 attributes 不进入 local forms 注入。
- [x] external → local 与 local → external 的冲突、双方 `force_override`、生成环境 form 交错均按源码顺序裁决。
- [x] `__original__` 合并覆盖原函数带 spec、wrapper 自带 spec、原/生成 spec 同时存在三类场景。
- [x] local declaration 无效、重复及部分失败场景只产生一次诊断且不破坏先前成功注册。

## 最终统一展开/编译层级任务（新增，不覆盖上述历史任务）

> 本节由最终 MacroRuntimeContext 讨论新增。上述已完成项目记录旧层级的实现状态；若与本节冲突，以本节和 `Hierarchy_final.md` 为最终需求。

### 规格

- [x] 记录 attribute、local declaration、retain 和 Step 2 function 使用同一个 `MacroRuntimeContext` 构造逻辑，仅快照时点不同。
- [x] 记录同一 `-local_macro([...])` declaration 的成员共享环境并整体从该环境排除。
- [x] 记录宏环境用于展开缓存和多次展开结果一致性，而 GenerationCompiler 只消费 canonical forms。
- [x] 记录 retain 与普通 Step 2 function 均使用 `FinalMacroRuntimeContext`，并与最后一次 local expansion result 比较。

### 实现

- [x] **P0：统一 MacroRuntimeContext builder。** 让 attribute 调用点、local declaration 快照和 final function context 使用同一个数据模型与宏映射/options/injection 规则。
- [x] **P0：DeclarationGroup。** 同一 declaration 的成员引用同一个 context fingerprint，并整体从 group MacroEnv 排除；删除按单个 TargetFA 生成 group 环境的行为。
- [x] **P1：ExpansionValidator。** 将 function expansion 从 `compile_boundary` 分离为显式准备阶段，维护 last environment/result、canonical result 与可选 per-environment cache。
- [x] **P1：Canonical GenerationCompiler。** 编译 boundary 只读取 canonical expanded forms，不接收 declaration environment 或 expansion requests。
- [x] **P2：声明点预展开。** local declaration 注册、冻结和依赖建图后立即预展开就绪 forms；需要未就绪 local dependency 时产生通用 `NeedCallable`。
- [x] **P2：通用 DependencyScheduler。** declaration 预展开、attribute、retain、Step 2 和 finalize 共用 `NeedCallable`，并按累计 members boundary 去重编译；未引入新 local macro 时不重新编译。
- [x] **P3：统一最终 function 路径。** retain 与普通目标 function 使用 `FinalMacroRuntimeContext` 和 ExpansionValidator；删除 retain 宏头跳过比对的例外。
- [x] 将 `PreparedFunctionIds` 降级为调度优化，确保即使重复调度也通过相同 final fingerprint 命中缓存而不会二次展开 AST。

### 测试

- [x] `-local_macro([foo/1, bar/1])` 的 foo/bar 共享 fingerprint，且双方调用保持普通 Erlang 调用。
- [x] declaration 注册后在无未就绪 local 依赖时完成预展开，但不因此编译当前 group。
- [x] declaration 预展开需要先声明 local macro 时，通过 `NeedCallable` 编译最小依赖 boundary 后恢复展开。
- [x] 同一 FormId 在相同环境复用 last result；不同环境结果相同则更新 record，结果不同则诊断冲突。
- [x] GenerationCompiler 在已有 canonical forms 时不调用 function expander。
- [x] 同一累计 members boundary 由预展开、attribute 或 finalize 触发时只编译一次；独立连续 declaration 在预展开后 generation 仍为 0。
- [x] retained helper、retained local macro 宏头和普通 Step 2 function 均与最后一次 local result 做 final-context 比对。
- [x] final fingerprint 相同时直接复用，fingerprint 不同时从 original form 重新展开，禁止在 expanded AST 上继续展开。

### 上下文接口收敛

- [x] 通过 `local_macro_workflow_context/3` 统一构造 scheduler/compiler context，并删除未消费的 `external_macro_map`。
- [x] 强制 declaration snapshot 使用唯一的 `MacroRuntimeContext` 形状，删除裸 MacroMap、`env_snapshot` 和独立 inject snapshot 兼容路径。
- [x] 为 MacroRuntimeContext、workflow context、MacroOps、ExpansionRequest 和 CompilationBoundary 增加命名 map 类型。
- [x] 将 `expand_final_functions/5` 参数调整为业务输入在前、`RuntimeContext, MacroOps, State` 固定收尾。
