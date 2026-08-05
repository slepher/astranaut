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
- [x] 增加测试：目标 FA 自身移除；通用展开器只按 MacroEnv 匹配调用。
- [x] 删除 declaration 级函数调用排除策略；普通函数调用使用 helper 或 Erlang 间接调用。
- [x] retain 最终重展开复用同一宏调用匹配规则和 input fingerprint。

## Hierarchy_final 任务（新增，保留既有任务状态）

### 规格

- [x] 明确唯一的 `macro_environment_snapshot` 携带含已解析 `attributes` 的 macro map 与 options；`closure_source_view` 仅是闭包结构输入。
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
- [x] 记录同一 `-local_macro([...])` declaration 的成员共享声明前环境；首次完整递归展开的真实匹配结果成为 declaration/final 共用 canonical whitelist。
- [x] 记录宏环境用于展开缓存和多次展开结果一致性，而 GenerationCompiler 只消费 canonical forms。
- [x] 记录 retain 与普通 Step 2 function 均使用 `FinalMacroRuntimeContext`，并与最后一次 local expansion result 比较。

### 实现

- [x] **P0：统一 MacroRuntimeContext builder。** 让 attribute 调用点、local declaration 快照和 final function context 使用同一个数据模型与宏映射/options/injection 规则。
- [x] **P0：声明条目。** 同一 declaration 的逐 FA 条目共享 order/context fingerprint；删除独立 group 状态和按单个 TargetFA 生成声明环境的行为。
- [x] **P1：ExpansionValidator。** 将 function expansion 从 `compile_boundary` 分离为显式准备阶段，维护 last environment/result、canonical result 与可选 per-environment cache。
- [x] **P1：Canonical GenerationCompiler。** 编译 boundary 只读取 canonical expanded forms，不接收 declaration environment 或 expansion requests。
- [x] **P2：声明点预展开。** local declaration 注册、冻结和依赖建图后立即预展开就绪 forms；需要未就绪 local dependency 时产生通用 `NeedCallable`。
- [x] **P2：通用 DependencyScheduler。** declaration 预展开、attribute、retain、Step 2 和 finalize 共用 `NeedCallable`，并按累计 members boundary 去重编译；未引入新 local macro 时不重新编译。
- [x] **P3：统一最终 function 路径。** retain 与普通目标 function 使用 `FinalMacroRuntimeContext` 和 ExpansionValidator；删除 retain 宏头跳过比对的例外。
- [x] 将 `PreparedFunctionIds` 降级为调度优化，确保即使重复调度也通过相同 final fingerprint 命中缓存而不会二次展开 AST。

### 测试

- [x] `-local_macro([foo/1, bar/1])` 的 foo/bar 共享 fingerprint，且双方调用保持普通 Erlang 调用。
- [x] declaration 注册后在无未就绪 local 依赖时完成预展开，但不因此编译当前 declaration。
- [x] declaration 预展开需要先声明 local macro 时，通过 `NeedCallable` 编译最小依赖 boundary 后恢复展开。
- [x] 同一 FormId 在相同环境复用 last result；不同环境结果相同则更新 record，结果不同则诊断冲突。
- [x] GenerationCompiler 在已有 canonical forms 时不调用 function expander。
- [x] 同一累计 members boundary 由预展开、attribute 或 finalize 触发时只编译一次；独立连续 declaration 在预展开后 generation 仍为 0。
- [x] retained helper、retained local macro 宏头和普通 Step 2 function 均与最后一次 local result 做 final-context 比对。
- [x] final fingerprint 相同时直接复用，fingerprint 不同时从 original form 重新展开，禁止在 expanded AST 上继续展开。

### 简洁性复核收口

- [x] scan form 分派直接使用互斥 form pattern，删除无语义的分类器/布尔跳转和 monadic bind。
- [x] local-macro 注册接口只接收完整 RuntimeContext；workflow context 不再携带重复宏映射。
- [x] 删除 DeclarationGroup、重复 expanded cache、final boundary flag 和 retained 专用校验旁路。
- [x] 验证同声明成员在 final context 仍互为普通调用，且普通 attribute 不阻止独立 declarations 合并为一个累计 boundary。
- [x] 验证 local closure 的 final 环境不包含 canonical whitelist 外的后声明宏，而普通 final function 仍使用完整 local 环境并禁用 whitelist control。

### 上下文接口收敛

- [x] 通过 `local_macro_workflow_context/3` 统一构造 scheduler/compiler context，并删除未消费的 `external_macro_map`。
- [x] 强制 declaration snapshot 使用唯一的已解析 `MacroEnvironment` 形状，删除裸 MacroMap、`env_snapshot` 和独立 inject snapshot 兼容路径。
- [x] 为 MacroEnvironment、workflow context、ExpansionRequest 和 CompilationBoundary 增加命名 map 类型，并删除 MacroOps callback map。
- [x] 将 `expand_final_functions/4` 参数调整为业务输入在前、`MacroEnvironment, State` 固定收尾。

## Local macro whitelist 合并任务

> 本节合并原 `local-macro-white-list` change。它以展开期间的真实 local macro match 取代旧的独立 form 引用扫描，并保持既有 scan-and-splice 与 GenerationCompiler 边界。

### 规格

- [x] 明确 whitelist control 的 `disabled`、`collect`、`verify(Expected)` 三种模式。
- [x] 明确仅 local frozen function 及其 replacement AST 启用，普通 function forms 禁用。
- [x] 明确白名单冲突和 expanded-form 冲突是两条独立不变量。
- [x] 明确 whitelist 观察接入统一 macro 发现—执行点，`process_macro_return` 不负责展开，也不增加扫描 pass。

### 数据契约

- [x] 为 `LocalMacroWhitelistControl` 和带 whitelist 结果的 function expansion 定义命名类型。
- [x] 在 ExpansionRecord 中增加 `canonical_whitelist` 和 per-input whitelist/result。
- [x] 保证 input fingerprint 只包含展开前可知输入，并排除白名单外 local generations。
- [x] final retained 使用 canonical whitelist 过滤 FinalEnv，名单外 local descriptors 不进入有效环境或 fingerprint。
- [x] 删除普通 function 接口中的隐式 whitelist 推断和双重返回形状容错。

### 展开接入

- [x] 为通用 function 展开入口增加 whitelist control 参数，所有调用点显式传值。
- [x] local declaration 首次展开传 `collect`，已有 canonical 的后续处理传 `verify`。
- [x] 普通 Step 2 function、普通 retained function 和 attribute invocation 传 `disabled`。
- [x] 将 local macro match 观察接入原始 function 与 replacement 共用的发现—执行路径。
- [x] replacement AST 递归处理继承相同 control、accumulator、depth 和错误上下文。
- [x] replacement 首次匹配未 callable local macro 时请求 `NeedCallable`，编译后从 frozen form 重试。
- [x] 保留既有 post-return 递归展开路径；不为 whitelist 增加额外 `transform_exprs` traversal。
- [x] 保持用户宏 State 通过 `scoped_state` 隔离，whitelist accumulator 由框架拥有。

### 冲突与事务

- [x] `collect` 成功后原子提交 canonical whitelist 与 canonical result。
- [x] 非-final `verify` 观察到 unexpected FA 时立即报告 `conflicting_local_macro_whitelist` 并跳过该 macro 调用。
- [x] 完整 function expansion 后检查 missing FA。
- [x] whitelist 相同后继续执行 `conflicting_local_macro_closure_environment` AST 比较。
- [x] whitelist/AST/宏执行任一失败时不提交部分 cache、canonical form 或 generation。

### 简化

- [x] 删除 final order/self/declaration-member 排除路径。
- [x] 删除共享 helper owner whitelist 的运行时猜测或无条件并集路径。
- [x] 不新增独立 whitelist scanner、whole-form rescan、AST diff 或 callback map。
- [x] 检查参数顺序保持 `subject, runtime context, control, state` 的函数族一致性。

### 测试

- [x] 普通 function 调用 local macro 时保持 `disabled`，不创建或校验 whitelist。
- [x] local frozen function 原始 AST 中的 local 调用进入 canonical whitelist。
- [x] external/local macro replacement 生成的新 local 调用在统一发现—执行点进入 whitelist。
- [x] 多层 replacement 递归继承同一 whitelist control，且不增加 whitelist 专用遍历。
- [x] unexpected FA 在完整 function 结束前立即失败；missing FA 只在完整 function expansion 后失败。
- [x] whitelist 不同但 AST 相同仍报告 whitelist conflict；whitelist 相同但 AST 不同仍报告 closure-environment conflict。
- [x] retained local-macro head 和 frozen helper 使用 `verify`。
- [x] final retained 的名单外同声明/后声明 local 调用保持普通调用。
- [x] external replacement 首次生成 pending local 调用时按需编译并成功重试。
- [x] 普通 retained/function forms 不启用 whitelist。
- [x] whitelist 冲突不提交部分 declaration、ExpansionRecord 或 generation。
- [x] 完整 Common Test suite 通过。

## Return AST whitelist 批量收集变更

> 本节取代上方“replacement 仅在发现—执行点逐个观察和立即校验”的历史策略；原有已完成项目保留为变更历史。

### 文档与契约

- [x] 规定 `process_macro_return` 在既有规范化 traversal 中同时收集当前 Return AST 的 local macro FAs 与总体 macro presence，不校验、不展开。
- [x] 规定返回形状为 `{ProcessedNode, ReturnAnalysis}`，其中 analysis map 是 scoped traversal state。
- [x] 规定调用方合并并批量校验 ReturnObserved；missing 仍只在完整 function expansion 后检查。

### 实现

- [x] 让 `process_macro_return` 通过 `scoped_state_run` 返回 `{Node, ReturnAnalysis}`。
- [x] 在 `expand_macro_with` 中合并 ReturnObserved，并对同一返回 AST 的 unexpected FAs 只生成一个错误。
- [x] 冲突批次不得进入 replacement 递归展开；accepted replacement 继续使用原有 pre/post 展开路径。
- [x] 修正逐个发现逻辑，使既已收集的 FA 不重复写入，并且 earlier unexpected 不会阻止 later expected macro。

### 验证

- [x] 同一返回 AST 包含两个 unexpected local macros 时只报告一个包含完整集合的错误。
- [x] `process_macro_return` 收集阶段不调用任何 replacement macro。
- [x] accepted replacement、动态 NeedCallable、missing 和 final whitelist 过滤场景继续通过。
- [x] 完整 Common Test suite 与 OpenSpec strict validation 通过。

## Function/Return 宏 presence 复用优化

- [x] closure walk 的单次 per-function traversal 同时收集本地调用边、local macro FAs 与任意 macro presence。
- [x] 同一 declaration 的多个 roots 复用已分析的 closure functions，不预扫完整 SourceView。
- [x] final caller 筛选把同一次全量 analysis 传给 expansion task，消除可信场景下的重复 `has_macro_call` traversal。
- [x] `process_macro_return` 在规范化 traversal 中同时记录 `has_macro_call`，无宏 replacement 不再进入递归 `transform_exprs`。
- [x] form 或环境不能安全复用 analysis 时保留现场预检查回退。
- [x] 增加静态 function analysis 与 external replacement 递归展开测试。

## Local workflow 两批热路径优化

- [x] 第一批删除未消费的 Forms 参数传递、空 internal-binding traversal，并复用单次 formatter presence 检查。
- [x] final caller selection 使用 presence-only analysis，保留可信提示与现场回退规则。
- [x] 第二批让一个 ExpansionRequest 共用一次 record context，每个 task 只遍历目标 frozen function 与 records，保持串行宏执行和 `NeedCallable` 原子重试。
- [x] compile plan 建立 FA entry/order/prefix 索引并 memoize 依赖边界，保持相同 declaration order members 的累计语义。
- [x] final preparation 直接按 FormId 的 closure whitelist 构造有效环境。

## 最终审核落实

- [x] 将 attribute/function 共用的宏目标解析、调用、返回 AST 规范化和递归展开拆到 `astranaut_macro_expander`。
- [x] 删除 `astranaut_macro:expand_function/5` 兼容入口和内部 `MacroOps`，所有 local/final 目标统一调用 `astranaut_macro_expander:expand_functions/2`。
- [x] 将 expander 加入 `erl_first_files`，确保 parse transform 使用前已经编译加载。
- [x] 拆分 `export_macro` 与 `local_macro` validator，只让后者接受闭包构造 options。
- [x] 明确只有 `local_macro` 接受 `closure_roots`，且所有 validator 均拒绝
  `internal_function`，并覆盖诊断测试。
- [x] 记录 `macro_options` 的逐宏 defaults、源码顺序、覆盖优先级及 module-only debug 选项。
- [x] 为 declaration、attribute call 与 final function context 使用阶段化构造函数命名。
- [x] 用命名 map type 明确 attribute scanner state 的必需字段。
- [x] 同步 retain 双重身份、间接闭包引用限制和无效 retain warning 的中英文文档与测试。
- [x] 为 ordinary attribute 建立增量 name/arity 索引，只解析已选中宏的 `inject_attrs`。
- [x] 将完整 macro map 解析限制在 declaration 与 final function 两个批量边界。
- [x] 将单 function task 结果收紧为 `form`，删除 local 缓存前的完整 forms 回扫。
- [x] 删除 final function environment 的重复字段和单行环境转发函数。
- [x] 用真实 expander 覆盖 retained function 的最终环境结果冲突。

## Parse-transform 职责拆分

- [x] 提取 `astranaut_macro_registry`，统一拥有宏声明、checked override、`AttributeEnv` 与阶段化 `MacroEnvironment`。
- [x] 提取 `astranaut_macro_scan`，统一拥有 source-ordered queue、attribute buffer、scan traverse state 与 `map_forms_splice/3`。
- [x] 收缩 `astranaut_macro`，只保留 parse-transform 门面、两阶段编排、最终 forms 物化、retain 诊断与 `format_error/1`。
- [x] 保持 `astranaut_macro_expander` 与 `astranaut_macro_local` 的既有职责和接口，不增加 pipeline 转发层。
- [x] 更新 `erl_first_files`、专项测试入口及中英文架构文档。
- [x] `rebar3 compile` 无 warning，`rebar3 xref` 通过；scanner/macro/local 专项 132 项测试及完整 354 项 Common Test 全部通过。

## Optional local provider

- [x] scanner state 只保存 `disabled | #{provider, state}` capability，不再保存平行的 local state/declaration maps。
- [x] 首个 local declaration 动态注册 provider，并让该 form 立即进入 provider callback；后续生成 declaration 复用同一扫描顺序。
- [x] disabled attribute/function pass 不加载或调用 local provider。
- [x] scanner 使用不可变 declaration context 调用 provider，并通过 registry 统一提交其 definitions delta。
- [x] 通用 orchestrator 统一负责 final MacroEnvironment、caller analysis、warning 格式化与 forms 排序。
- [x] shared expander 删除 local descriptor 分支，改用通用 observation task/result 字段。
- [x] registry 删除 local validator、descriptor builder 和 final-local filtering。
