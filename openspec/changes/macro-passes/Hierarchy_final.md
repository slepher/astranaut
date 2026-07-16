# Macro Passes 最终处理层级

> 本文以 [`MacroPassesHierarchy.md`](MacroPassesHierarchy.md) 的独立设计为起点，并吸收后续关于 declaration 预展开、依赖驱动编译、统一 `MacroRuntimeContext`、多环境结果比对、local macro canonical whitelist 和最终 function 展开的讨论结果。本文是 macro-passes 与 local-macro 协作关系的最终权威模型。

## 1. 最终结论

整个流程不再按“attribute 阶段展开并编译 local macro、finalize 再按 request 环境重放、Step 2 另行展开 function”划分责任，而是由三个正交组件组成：

```text
ExpansionValidator
  -> 使用某个 MacroRuntimeContext 从原始 form 展开
  -> 以显式 disabled / collect / verify 控制观察真实 local macro match
  -> 缓存环境、canonical whitelist 与结果
  -> 分别验证同一 FormId 的 whitelist 和 expanded AST 一致

DependencyScheduler
  -> 在某次展开或调用真正需要 local macro 可调用时产生 NeedCallable
  -> 计算规范化的最小累计编译边界

GenerationCompiler
  -> 只消费已经确认的 CanonicalExpandedForms
  -> 编译、加载并提交 local macro module generation
  -> 不读取 declaration MacroRuntimeContext，不按 request 重新展开 forms
```

源码阶段只决定使用哪个 `MacroRuntimeContext`：

| 场景 | MacroRuntimeContext |
|---|---|
| attribute macro 调用 | 当前 attribute 调用点上下文 |
| local macro function-form 预展开 | `-local_macro` declaration 前的上下文快照 |
| retain function 展开 | attribute scan 完成后的最终上下文 |
| Step 2 普通 function 展开 | attribute scan 完成后的最终上下文 |

四者使用相同的环境构造、宏匹配、调用参数、`inject_attrs` 和结果验证逻辑。local frozen function 通过显式 whitelist control 启用真实匹配观察；普通 function/attribute 传 `disabled`。local macro 的特殊性只剩 declaration-time 快照、canonical whitelist、同声明成员的普通调用语义、闭包/依赖及 callable generation 生命周期。

实现模块按此边界拆分：`astranaut_macro` 是 parse-transform 门面和 pass 编排器，拥有
source-ordered scan、环境更新、splice、final context 及诊断入口；
`astranaut_macro_expander` 拥有 attribute/function 共用的目标解析、宏调用、返回 AST
规范化、递归展开和展开期 traversal state；`astranaut_macro_local` 拥有声明、闭包、
一致性记录、调度及 generation 生命周期。expander 不反向调用扫描器或 local-macro
工作流。为兼容既有调用，`astranaut_macro:expand_function/5` 只做一次委托；内部
`MacroOps` 直接引用 expander，避免门面往返。

## 2. 与当前实现对照

当前实现已经完成最终层级重构：

| 最终契约 | 当前实现证据 | 对比结论 |
|---|---|---|
| 统一 attribute runtime | `astranaut_macro_expander:resolve_attribute_target/2`、`need_callable/4`、`expand_attribute_target/2` | 已实现；local 只增加通用可调用性前置条件。 |
| local 调用白名单 | `astranaut_macro_expander:expand_function/5`、`process_macro_return/3`、`canonical_whitelist` | 已实现；原始 AST 观察真实 match，返回 AST 一次性收集并批量 verify，final 按 canonical whitelist 过滤。 |
| declaration 时尽可能预展开 | `prepare_declaration/4` | 已实现；无真实 local 依赖时只预展开、不编译。 |
| 环境只控制展开与一致性 | `prepare_requests/3`、`expansion_records` | 已实现；环境 fingerprint 不参与 generation 身份。 |
| 编译仅消费 canonical forms | `compile_boundary/3`、`canonical_expanded_forms` | 已实现；compiler 不按 declaration context 重放展开。 |
| 编译由 `NeedCallable` 驱动 | `need_callable/4` | 已实现；仅真实 local 依赖可产生中间 boundary。 |
| retain 与 Step 2 共用最终上下文和验证器 | `expand_final_functions/5` | 已实现；两类目标走同一 final-context 路径。 |
| retain 宏头也比较最后一次 local 展开结果 | `expand_final_functions/5`、`cache_expanded/4` | 已实现；宏头没有专用校验旁路。 |
| 相同最终输入直接复用 | `expansion_records.results_by_input` | 已实现；E1 → E2 → E1 可直接复用 E1 的 whitelist/result。 |
| `__original__` spec 局部 merge | `map_forms_splice_merge_specs/1` | 已实现并保留。 |

## 3. 最终 Hierarchy

```text
Module Macro Pipeline
├─ 1. Attribute Scan-and-Splice
│  ├─ 1.1 Initialize
│  │  ├─ EffectiveMacroMap
│  │  ├─ ExternalRegistry / LocalRegistry
│  │  ├─ LocalMacroState
│  │  ├─ PassedForms
│  │  └─ Queue
│  ├─ 1.2 Forward Scan
│  │  ├─ import_macro / use_macro / macro_options
│  │  │  └─ Update EffectiveMacroMap in source order
│  │  ├─ local_macro declaration
│  │  │  ├─ Build DeclarationMacroRuntimeContext from pre-declaration state
│  │  │  ├─ Register one entry per FA with the same order/context snapshot
│  │  │  ├─ Use the pre-declaration MacroEnv, which contains no new member
│  │  │  ├─ Freeze original function/spec forms and closure source view
│  │  │  ├─ Update static local dependency graph
│  │  │  └─ Pre-expand ready forms through ExpansionValidator
│  │  │     ├─ collect actual local matches on first success
│  │  │     ├─ verify against canonical whitelist on later contexts
│  │  │     └─ NeedCallable(dependency) when a matched local macro is unavailable
│  │  ├─ generic attribute macro runtime (external / local)
│  │  │  ├─ Build CallSiteMacroRuntimeContext
│  │  │  ├─ Resolve target
│  │  │  ├─ NeedCallable(target) only when selected local target is unavailable
│  │  │  ├─ Build invocation from the same call-site context
│  │  │  └─ Invoke / validate / splice
│  │  └─ ordinary form
│  └─ 1.3 Scan Completion
│     ├─ Drop successfully registered local declarations
│     ├─ Prepare exports
│     ├─ Drain remaining local expansion validation
│     ├─ Build the required final cumulative local generation
│     ├─ Compute FinalLocalEnv / FinalSkipIds / RetainIds
│     └─ Build FinalMacroRuntimeContext
├─ Shared Services
│  ├─ ExpansionValidator
│  │  ├─ Start every expansion from Original/FrozenForm
│  │  ├─ Explicit WhitelistControl: disabled / collect / verify
│  │  ├─ Observe original matches at match-before-invoke
│  │  ├─ Collect each Return AST once in process_macro_return
│  │  ├─ Merge and batch-verify ReturnObserved before replacement expansion
│  │  ├─ Same InputFingerprint -> reuse whitelist + result
│  │  ├─ Different InputFingerprint -> verify whitelist, then compare AST
│  │  └─ Maintain CanonicalWhitelist + CanonicalExpandedForm
│  ├─ DependencyScheduler
│  │  ├─ Accept NeedCallable from any phase
│  │  ├─ Compute minimal canonical cumulative boundary
│  │  └─ Reuse an already committed boundary
│  └─ GenerationCompiler
│     ├─ Read CanonicalExpandedForms only
│     ├─ Compile / safe-load cumulative module
│     └─ Commit generation atomically
└─ 2. Final Function Expansion
   ├─ Select retain functions and ordinary macro callers
   ├─ Exclude FinalSkipIds unless retained
   ├─ Filter local-closure env by canonical whitelist and use verify
   ├─ Expand ordinary functions with full FinalMacroRuntimeContext and disabled
   ├─ Compare against its last local-macro expansion result when one exists
   ├─ Materialize the accepted canonical result
   └─ Sort only at the established compiler boundary
```

## 4. 统一 MacroRuntimeContext

所有宏展开入口使用同一个逻辑类型：

```text
MacroRuntimeContext = {
  effective_macro_map,
  macro_options,
  inject_forms
}
```

`effective_macro_map` 按源码顺序维护。import/use/local declaration 使用同一个 checked update：

```text
update_effective_env(CurrentEnv, IncomingEntries, SourcePosition)
  -> UpdatedEnv | macro_override
```

- key 不存在：加入；
- 定义相同：幂等；
- 定义不同且 incoming 没有 `force_override`：在声明位置报错；
- 定义不同且 incoming 有 `force_override`：incoming 覆盖 existing。

上下文构造逻辑相同，但取值时点不同：

```text
DeclarationMacroRuntimeContext
  = context_at(pre-declaration PassedForms, pre-declaration EffectiveMacroMap)

CallSiteMacroRuntimeContext
  = context_at(call-site PassedForms, call-site EffectiveMacroMap)

FinalMacroRuntimeContext
  = context_at(completed attribute forms, final EffectiveMacroMap)
```

`inject_forms` 与宏映射必须来自同一个时点。完整 `ClosureSourceView = passed + current + remaining` 只用于静态闭包、冻结和模块结构物化，不属于 `MacroRuntimeContext`。

## 5. 同一 declaration 的成员语义

一个 declaration form 写入多个独立 FA 条目：

```text
LocalMacroEntry = {
  declaration_order,
  runtime_context_snapshot,
  closure_source_view,
  closure_ids,
  options
}
```

对于：

```erlang
-local_macro([foo/1, bar/1]).
```

`foo/1` 与 `bar/1` 的条目共享相同的 declaration order、MacroRuntimeContext 和 options。这个 MacroRuntimeContext 取自 declaration 前，因此按定义尚不包含本次新增的两个宏，无需专门排除 members：

```text
DeclarationMacroEnv = PreDeclarationEffectiveMacroMap
```

首次预展开把 declaration 前可匹配的 local entries 作为 `CandidateLocalEnv`，并传 `collect(FormId)`。原始 AST 在 `match_macro_call` 成功后观察实际 local FA；每个 macro 返回 AST 则由 `process_macro_return` 在既有规范化 traversal 中一次性收集 local macro presence，并以 `{ProcessedNode, ReturnObserved}` 返回。调用方把各批结果合并到 function-level accumulator。成功结果成为该 FormId 的 `canonical_whitelist`。因此 `bar/1` 中对 `foo/1` 的直接调用仍是普通 Erlang 本地调用和闭包边，因为 declaration 前 CandidateLocalEnv 尚不包含本次 members；反向同理。

后续 declaration context 使用 `verify(Expected)` 和当前 CandidateLocalEnv。每个 Return AST 完成收集后，调用方批量拒绝名单外 presence；同一返回 AST 的所有 unexpected FAs 只产生一个汇总错误，并且该 replacement 不进入递归展开。完整 function expansion 结束后再检查缺失项。final retained local closure 先以 canonical whitelist 过滤 FinalLocalEnv，再传 `verify(Expected)`；名单外同声明或后声明调用不匹配为 local macro，保持普通调用。final 的 external macros、options 和 inject forms 仍取 FinalMacroRuntimeContext。

不需要按 order、自身、同声明成员或 direct-call 集合做最终减法，也不得维护独立引用 scanner、owner whitelist union 或共享 helper 的运行时猜测。非 local-closure 普通 function 使用完整 FinalLocalEnv，并传 `disabled`。

各 FA 仍拥有各自的宏头、闭包根、retain 状态和 callable 状态。无需为共享 order/context 单独维护 group id、members map 或第二份声明状态。

## 6. ExpansionValidator

### 6.1 状态

```text
ExpansionRecord = {
  canonical_whitelist,
  canonical_result,
  results_by_input = #{
    InputFingerprint => #{whitelist, result}
  }
}
```

InputFingerprint 覆盖展开前可知的全部可观察输入：external macro map、候选 local descriptors/versions、macro options、解析后的 internal macro bindings 和 `inject_forms`。白名单是展开输出，不能作为首次 lookup 的唯一 key。internal bindings 既决定 MacroEnv 移除项，也决定 alias 本地调用改写到哪个原始远程函数，因此即使过滤后的 MacroEnv 相同也必须进入 key。final retained 使用 canonical whitelist 裁剪后的有效 MacroEnv 与 declaration internal bindings 计算 fingerprint，因此名单外 local descriptor/generation 不进入该 form 的 final input key。FormId 已在外层 cache key 中，不得再用单个 TargetFA 把共享的 declaration 快照切成不同环境。

### 6.2 统一操作

```text
expand_and_validate(FormId, OriginalForm, MacroRuntimeContext, WhitelistControl):
  results_by_input 命中 InputFingerprint
    -> 直接复用缓存的 whitelist + result

  未命中
    -> 始终从同一 OriginalForm 展开
    -> collect: 以 ObservedLocalFAs 建立 canonical whitelist
    -> verify: 校验 ObservedLocalFAs == canonical whitelist
       └─ 不同：conflicting_local_macro_whitelist
    -> whitelist 相同后比较 canonical/accepted expanded result
       ├─ 相同：缓存本 input 的 whitelist + result
       └─ 不同：conflicting_local_macro_closure_environment
```

非-final `verify` 对每个 `process_macro_return` 返回的完整 `ReturnObserved` 批量比较：同一返回 AST 的 unexpected FAs 只报告一次，冲突批次不进入 replacement 展开；missing FA 只能在完整递归展开结束后报告。不同 whitelist 即使产生相同 AST 也冲突；相同 whitelist 也不能免除 AST 一致性比较。

禁止在已经展开的 AST 上继续展开。local declaration、retain 和 Step 2 function 都必须从同一个 original/frozen form 开始。

原始 AST 的白名单观察接在共享的 macro match-before-invoke 点。`process_macro_return` 在规范化返回树、位置和变量的同一次 traversal 中收集该 Return AST 的 local FAs，但不校验或展开，并通过 `scoped_state_run` 返回 `{Node, ReturnObserved}`。调用方合并与批量校验后，accepted replacement 才进入原有 pre/post 递归路径。不得增加第二次 return-tree scan 或 whole-form rescan。

### 6.3 预展开

扫描到 local declaration 后立即注册依赖并尝试以 `collect` 或已有 canonical 时的 `verify` 预展开，但“声明出现”本身不要求编译这些 FA。预展开若只使用 external 或已可调用 local macros，完整 whitelist/result 直接进入 expansion record。

若原始或 replacement AST 的统一匹配点真正发现一个尚不可调用的 candidate local macro，则 expansion 返回 `needed_local_macros`。scheduler 可以当场通过 `NeedCallable(FA)` 编译最小必要依赖边界，随后从 frozen form 重试；部分 whitelist/result 不提交。这不是 declaration 编译策略或 whitelist 冲突，而是通用的依赖可调用性规则。

## 7. DependencyScheduler 与 GenerationCompiler

### 7.1 编译时机

编译时机不绑定 attribute，也不绑定 declaration 或 finalize：

```text
NeedCallable(FA)
  -> calculate canonical minimal cumulative boundary
  -> boundary 已提交：复用
  -> boundary 未提交：确保所需 canonical forms 已就绪，编译并提交
```

编译 boundary 的身份只有按声明顺序排列的累计 local macro members。没有引入新的
`local_macro`，就没有新的 boundary，也不得重新编译。MacroRuntimeContext、展开触发点、
`inject_attrs` 输入和宏环境变化只触发展开缓存命中或结果一致性比较，不进入编译身份。

例如：

```erlang
-local_macro([foo/1]).
-local_macro([bar/1]).
```

若 `bar/1` 的 function form 不实际依赖 `foo/1` 作为宏，则声明和预展开 `bar/1`
不会先编译 `{foo}`；首次真正需要可调用或 scan 收尾时直接编译累计 `{foo, bar}`。
只有 `bar/1` 的预展开真实需要调用尚不可用的 `foo/1` 时，才先产生 `{foo}` 中间代次。

`NeedCallable` 可以来自：

- declaration 预展开需要调用先声明 local macro；
- external/local attribute runtime 选中尚不可调用的 local target；
- retain 或 Step 2 function 展开需要 local macro；
- scan completion 构造最终 local generation。

只有真实 local macro 依赖产生中间编译边界。普通 Erlang direct call、闭包成员关系和同一 declaration 内成员调用不产生宏依赖边界。

### 7.2 编译输入

GenerationCompiler 的输入是：

```text
GenerationInput = {
  boundary_members,
  canonical_expanded_forms,
  stable_module_support_forms
}
```

它不得接收或解释每个 declaration 的 MacroRuntimeContext，不得遍历 expansion requests，也不得为了编译而按环境重放 function expansion。若 canonical form 尚未就绪，应先返回 ExpansionValidator/DependencyScheduler 完成准备，再进入 compiler。

其中 canonical 部分只包含该累计闭包所需的 function/spec FormIds；compiler 还从
boundary 最后一个 declaration 的冻结 source view 选择稳定的 module/record/type 等
编译支持 forms。普通无关函数、macro 控制 attributes 和触发点之后临时增加的 forms
不进入 local module。parse transform 的 compile options 是稳定的 compiler 参数，
既不是宏环境，也不属于 boundary identity。

成功 compile + safe load 后才更新 generation、callable status 和已提交 boundary。失败不得覆盖上一代模块或 canonical expansion records。

## 8. Attribute Scan-and-Splice

attribute runtime 对 external/local 完全通用：

```text
scan attribute
  -> capture CallSiteMacroRuntimeContext
  -> resolve macro target
  -> if selected local target unavailable: NeedCallable(target)
  -> build invocation from the captured context
  -> invoke / validate / splice
```

local 只可能在 invocation 前多出一次通用 `NeedCallable`，调用参数、alias、`inject_attrs` 和 passed forms 规则没有 local 分支。编译过程也不得用 call-site context 覆盖 local declaration 的预展开 context。

scan-and-splice 继续满足：

- splice forms 插到当前位置队首并立即扫描；
- 已通过 forms 不回扫；
- attribute injection 只读取调用点前 passed forms；
- import/use 成功后消费，macro_options 保留；
- 用户宏 traverse state 与扫描 state 隔离；
- frozen function/spec ID 被生成 splice 改写时拒绝该 mutation。

## 9. Scan Completion 与 Step 2 Function Expansion

attribute scan 结束后：

1. 删除成功注册的 local declaration forms，并执行 `prepare_exports`。
2. drain 尚未验证的 local declaration expansion；依赖未就绪时通过 `NeedCallable` 推进。
3. 按需要构造最终累计 local macro generation，得到 `FinalLocalEnv`。
4. 计算 `FinalSkipIds` 与 retain closure IDs，但此处不使用专用 retain 展开算法。
   对显式 `local_macro_retain` 中不存在的 FA 报
   `undefined_local_macro_retain`，对存在但未命中任何 closure 的 FA 报
   `ineffective_local_macro_retain`；两者均使用 attribute 位置，隐式 export roots 不报。
5. 从完整 attribute 输出构造唯一的 `FinalMacroRuntimeContext`。
6. 选择 retain functions 与普通 macro caller functions。
7. local frozen target 以 canonical whitelist 过滤有效 LocalEnv 并传 `verify`；普通 target 使用完整 FinalLocalEnv 并传 `disabled`，二者调用同一个 `expand_and_validate`。
8. 物化结果并在既定边界排序。

retain 与普通 function 的差异只有选择、生命周期与显式 whitelist control：属于 local frozen closure 的 retained target 使用 `verify`，并在 FinalMacroRuntimeContext 上重放 declaration 的 internal macro key 过滤与 alias-to-remote 改写；非 local-closure ordinary target 使用 `disabled`。两者共享展开实现和 FinalMacroRuntimeContext 构造规则。

如果某个 retained 或普通 function 曾作为 local macro closure form 展开：

- final input fingerprint 已缓存：直接复用该 input 的 whitelist/result；
- fingerprint 不同：从 original form 在按 canonical whitelist 过滤的 final context 下展开并执行 `verify`；
- observed whitelist 不同：报 `conflicting_local_macro_whitelist`；
- whitelist 相同但结果不同：报 `conflicting_local_macro_closure_environment`。

该规则适用于 retained local macro 宏头，不再存在“宏头跳过最终环境比对”的例外。

`PreparedFunctionIds` 可以保留为避免重复调度的优化，但不再是正确性边界：即使同一 form 再次进入 Step 2，相同 final context 也必须通过 ExpansionValidator 命中缓存，而不是在已展开 AST 上二次展开。

## 10. `__original__` 与 Spec 局部 Merge

scan-and-splice 层继续只对实际冲突的 `F/A` 做局部整理：

1. 生成 wrapper 调用 `__original__/A` 且存在原 `F/A` 时，为原函数选择唯一内部名字。
2. 重命名原 function 及必要自调用，并改写 wrapper 的 `__original__` 调用。
3. 没有生成 public spec 时保留原 `-spec F(...)`。
4. 存在生成 public spec 时由生成 spec 替换原 public spec。
5. 不把 public wrapper spec 复制给重命名后的内部函数。
6. 无关 forms 的相对顺序不变。

## 11. 错误与事务边界

```text
Form handler failure
  -> 回滚该 form 的 scanner state 提交，记录诊断并继续兄弟 forms

Expansion failure
  -> 不更新 ExpansionRecord，不提交 generation

Different observed local whitelist
  -> conflicting_local_macro_whitelist

Same whitelist + different expansion result
  -> conflicting_local_macro_closure_environment

Compile/load failure
  -> 保留上一代 callable module 和 boundary 状态
```

用户宏 computation 继续在 scoped state 中运行；formatter、position、warning 和 error 使用外层诊断管线。

## 12. 最终不变量

1. 所有宏环境都由同一个 `MacroRuntimeContext` builder 构造；local closure 的 local-macro 部分统一按完整递归展开观察得到的 canonical whitelist 过滤。
2. 一个 `-local_macro([...])` declaration 的全部 members 共享同一个 context，且成员之间不互为宏。
3. whitelist control 始终显式传入：local frozen function 使用 `collect/verify`，普通 function 与 attribute 使用 `disabled`。
4. 原始 AST 在统一 match-before-invoke 点观察；每个 macro 返回 AST 由 `process_macro_return` 在既有 traversal 中一次性收集，调用方随后合并和批量校验；该函数不承担校验或展开。
5. 每次 function-form 展开都从 original/frozen form 开始。
6. 相同 FormId、相同 input fingerprint 直接复用完整 whitelist/result；不同 input 必须先比较 whitelist、再比较展开结果。
7. 同一 FormId 只有一个已确认的 canonical whitelist 和 canonical expanded form 可以进入 local module generation。
8. GenerationCompiler 不读取 MacroRuntimeContext，也不执行按 request 环境展开。
9. 编译仅由真实匹配产生的 `NeedCallable` 和最终 generation 需求驱动，不绑定某种 form 或扫描阶段。
10. external/local attribute 调用使用相同 call-site runtime 规则。
11. retain 与 Step 2 function 使用相同 FinalMacroRuntimeContext 和 ExpansionValidator；普通 target 禁用 whitelist。
12. retained local macro 宏头同样参与最终 whitelist 与环境结果比对。
13. 编译、加载、whitelist/result cache 和 generation 提交原子化；失败保留上一代。
14. source-ordered override、scan-and-splice、state 隔离及局部 `__original__` merge 规则保持不变。
15. generation boundary identity 只由累计 local macro members 决定；未引入新 local macro 时不得重新编译。

## 13. 实现结果

### P0：声明条目与统一上下文

- 把同一个 `-local_macro([...])` 注册为共享 order/context 的逐 FA 条目。
- declaration 前环境自然不含本次 members；首次递归展开收集 canonical whitelist，后续 declaration/final 复用并校验，无需最终排除路径。
- 提供 attribute/local/retain/function 共用的 `MacroRuntimeContext` builder。

状态：已实现。

### P1：拆分展开验证与编译

- 将 request-specific expansion 与 `compile_boundary` 分离，由 `execute_plan` 仅按“准备后编译”顺序协调。
- 实现显式 `LocalMacroWhitelistControl`、带 whitelist/result 的 `ExpansionRecord` 和 `expand_and_validate`。
- 原始 AST 在共享 macro 发现—执行点观察；`process_macro_return` 返回 `{Node, ReturnObserved}`，调用方批量合并/校验后再展开 accepted replacement。
- GenerationCompiler 只消费 canonical expanded forms。

状态：已实现。

### P2：声明点预展开与通用 NeedCallable

- declaration 注册后尝试预展开。
- 预展开、attribute、retain、Step 2 和 finalize 共用 dependency scheduler。
- 用规范化 boundary cache 保证触发时点不制造额外编译。

状态：已实现；boundary key 是累计 members，独立连续声明只在需要时合并编译一次。

### P3：统一最终 function 路径

- retain 与普通 function 都使用 `FinalMacroRuntimeContext`。
- 删除 retain 宏头跳过比对的例外。
- 将 `PreparedFunctionIds` 降级为调度优化，并用缓存命中保证语义正确。

状态：已实现。
