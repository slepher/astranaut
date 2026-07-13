# Macro Passes 最终处理层级

> 本文以 [`MacroPassesHierarchy.md`](MacroPassesHierarchy.md) 的独立设计为起点，并吸收后续关于 declaration 预展开、依赖驱动编译、统一 `MacroRuntimeContext`、多环境结果比对和最终 function 展开的讨论结果。本文是 macro-passes 与 local-macro 协作关系的最终权威模型。

## 1. 最终结论

整个流程不再按“attribute 阶段展开并编译 local macro、finalize 再按 request 环境重放、Step 2 另行展开 function”划分责任，而是由三个正交组件组成：

```text
ExpansionValidator
  -> 使用某个 MacroRuntimeContext 从原始 form 展开
  -> 缓存环境与结果
  -> 验证同一 FormId 在不同环境下结果一致

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

四者使用相同的环境构造、宏匹配、调用参数、`inject_attrs` 和结果验证逻辑。local macro 的特殊性只剩 declaration-time 快照、declaration group 成员排除、闭包/依赖及 callable generation 生命周期。

## 2. 与当前实现对照

当前实现已经完成最终层级重构：

| 最终契约 | 当前实现证据 | 对比结论 |
|---|---|---|
| 统一 attribute runtime | `resolve_attribute_macro_target/2`、`need_callable/4`、`build_attribute_macro_invocation/2` | 已实现；local 只增加通用可调用性前置条件。 |
| declaration group 共享一个环境 | `register/8` 的 `declaration_groups`、`group_members` | 已实现；同 declaration 成员整体排除，成员间调用保持普通 Erlang 调用。 |
| declaration 时尽可能预展开 | `prepare_declaration/4` | 已实现；无真实 local 依赖时只预展开、不编译。 |
| 环境只控制展开与一致性 | `prepare_requests/4`、`expansion_records` | 已实现；环境 fingerprint 不参与 generation 身份。 |
| 编译仅消费 canonical forms | `compile_boundary/4`、`canonical_expanded_forms` | 已实现；compiler 不按 declaration context 重放展开。 |
| 编译由 `NeedCallable` 驱动 | `need_callable/4` | 已实现；仅真实 local 依赖可产生中间 boundary。 |
| retain 与 Step 2 共用最终上下文和验证器 | `expand_final_functions/5` | 已实现；两类目标走同一 final-context 路径。 |
| retain 宏头也比较最后一次 local 展开结果 | `verify_retained/2`、`expand_final_functions/5` | 已实现；宏头没有跳过例外。 |
| 相同最终环境直接复用 | `expansion_records.results_by_env` | 已实现；E1 → E2 → E1 可直接复用 E1。 |
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
│  │  │  ├─ Register one DeclarationGroup
│  │  │  ├─ Exclude every group member from the group MacroEnv
│  │  │  ├─ Freeze original function/spec forms and closure source view
│  │  │  ├─ Update static local dependency graph
│  │  │  └─ Pre-expand ready forms through ExpansionValidator
│  │  │     └─ NeedCallable(dependency) when expansion needs unavailable local macro
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
│  │  ├─ Same EnvFingerprint -> reuse
│  │  ├─ Different EnvFingerprint -> expand and compare last accepted result
│  │  └─ Maintain CanonicalExpandedForm
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
   ├─ Expand every selected function with FinalMacroRuntimeContext
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

## 5. DeclarationGroup 语义

一个 declaration form 对应一个不可变 group：

```text
DeclarationGroup = {
  members,                    % 例如 [foo/1, bar/1]
  declaration_order,
  runtime_context_snapshot,
  closure_source_view,
  closure_ids,
  referenced_local_macros,
  options
}
```

对于：

```erlang
-local_macro([foo/1, bar/1]).
```

`foo/1` 与 `bar/1` 共享完全相同的 declaration MacroRuntimeContext。group 的全部 members 整体从该 MacroEnv 排除：

```text
GroupMacroEnv = PreDeclarationEffectiveMacroMap - {foo/1, bar/1}
```

因此 `bar/1` 中对 `foo/1` 的直接调用是普通 Erlang 本地调用和闭包边，不是 local macro 调用；反向同理。不能因为当前展开 form 或 TargetFA 不同而为 group members 制造不同宏环境或不同环境 fingerprint。

group 内各 FA 仍可拥有各自的宏头、闭包根、retain 状态和 callable 状态，但这些生命周期字段不能改变共同的 declaration context。

## 6. ExpansionValidator

### 6.1 状态

```text
ExpansionRecord = {
  last_env_fingerprint,
  last_expanded_form,
  canonical_expanded_form,
  results_by_env_fingerprint   % 可选但推荐，用于避免环境来回切换时重复展开
}
```

环境 fingerprint 覆盖该次展开的全部可观察输入：有效宏映射及可调用 local 版本、macro options、`inject_forms` 和 declaration group 排除策略。FormId 已在外层 cache key 中，不得再用单个 TargetFA 把同 declaration group 切成不同环境。

### 6.2 统一操作

```text
expand_and_validate(FormId, OriginalForm, MacroRuntimeContext):
  Record 不存在
    -> 从 OriginalForm 展开
    -> 保存为 last 和 canonical result

  fingerprint(CurrentContext) == Record.last_env_fingerprint
    -> 直接复用 Record.last_expanded_form

  fingerprint 不同
    -> 始终从同一 OriginalForm 重新展开
    -> 与 Record.last_expanded_form 比较
       ├─ 相同：接受并更新 last record
       └─ 不同：conflicting_local_macro_closure_environment
```

每个新环境只需与上一次已接受结果比较；由于每次成功转换都要求结果相同，该关系能传递到 canonical result。保留 `results_by_env_fingerprint` 不改变语义，只避免 E1 → E2 → E1 时重复执行 E1。

禁止在已经展开的 AST 上继续展开。local declaration、retain 和 Step 2 function 都必须从同一个 original/frozen form 开始。

### 6.3 预展开

扫描到 local declaration 后立即注册依赖并尝试预展开，但“声明出现”本身不要求编译该 group。预展开若只使用 external 或已可调用 local macros，结果直接进入 expansion record。

若预展开真正需要执行一个尚不可调用的 local macro，则产生 `NeedCallable(FA)`。scheduler 可以当场编译最小必要依赖边界，随后恢复预展开。这不是 declaration 编译策略，而是通用的依赖可调用性规则。

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

只有真实 local macro 依赖产生中间编译边界。普通 Erlang direct call、闭包成员关系和 declaration group 内成员调用不产生宏依赖边界。

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
5. 从完整 attribute 输出构造唯一的 `FinalMacroRuntimeContext`。
6. 选择 retain functions 与普通 macro caller functions。
7. 对每个目标调用同一个 `expand_and_validate(..., FinalMacroRuntimeContext)`。
8. 物化结果并在既定边界排序。

retain 与普通 function 的差异只有选择和生命周期：retain 必须保留，普通 function 由 caller detection 决定是否进入目标集合。两者的展开、环境 fingerprint 和多环境结果比较完全相同。

如果某个 retained 或普通 function 曾作为 local macro closure form 展开：

- final fingerprint 与 last local fingerprint 相同：直接复用最后一次结果；
- fingerprint 不同：从 original form 在 final context 下展开，并与最后一次 local 结果比较；
- 结果不同：报 `conflicting_local_macro_closure_environment`。

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

Different environment + different expansion result
  -> conflicting_local_macro_closure_environment

Compile/load failure
  -> 保留上一代 callable module 和 boundary 状态
```

用户宏 computation 继续在 scoped state 中运行；formatter、position、warning 和 error 使用外层诊断管线。

## 12. 最终不变量

1. 所有宏环境都由同一个 `MacroRuntimeContext` builder 构造，差异仅来自源码时点和明确的 declaration group 排除。
2. 一个 `-local_macro([...])` declaration 的全部 members 共享同一个 context，且成员之间不互为宏。
3. 每次 function-form 展开都从 original/frozen form 开始。
4. 相同 FormId、相同 fingerprint 直接复用；不同 fingerprint 必须比较展开结果。
5. 同一 FormId 只有一个已确认的 canonical expanded form 可以进入 local module generation。
6. GenerationCompiler 不读取 MacroRuntimeContext，也不执行按 request 环境展开。
7. 编译仅由 `NeedCallable` 和最终 generation 需求驱动，不绑定某种 form 或扫描阶段。
8. external/local attribute 调用使用相同 call-site runtime 规则。
9. retain 与 Step 2 function 使用相同 FinalMacroRuntimeContext 和 ExpansionValidator。
10. retained local macro 宏头同样参与最终环境结果比对。
11. 编译、加载和 generation 提交原子化；失败保留上一代。
12. source-ordered override、scan-and-splice、state 隔离及局部 `__original__` merge 规则保持不变。
13. generation boundary identity 只由累计 local macro members 决定；未引入新 local macro 时不得重新编译。

## 13. 实现结果

### P0：DeclarationGroup 与统一上下文

- 把同一个 `-local_macro([...])` 注册为共享 context 的 group。
- group members 整体从 declaration MacroEnv 排除。
- 提供 attribute/local/retain/function 共用的 `MacroRuntimeContext` builder。

状态：已实现。

### P1：拆分展开验证与编译

- 将 request-specific expansion 与 `compile_boundary` 分离，由 `execute_plan` 仅按“准备后编译”顺序协调。
- 实现显式 `ExpansionRecord` 和 `expand_and_validate`。
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
