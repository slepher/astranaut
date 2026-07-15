# Macro Passes 独立处理流程设计

> 本文只依据 `openspec/changes/macro-passes` 的需求与规格建立处理模型，不依据当前源码结构反推设计。它是后续实现对比的独立基线。

## 1. 设计结论

宏处理采用“两阶段、一个收尾边界”的层级：

```text
Module Forms
└─ Attribute Phase
   ├─ Initialize Scan Context
   ├─ Forward Scan-and-Splice
   │  ├─ Environment Form Handling
   │  ├─ Local-Macro Workflow Delegation
   │  ├─ Attribute Macro Resolution / Invocation
   │  └─ Ordinary Form Preservation
   ├─ Local-Macro Finalization
   ├─ Final MacroRuntimeContext Construction
   └─ Minimal Merge + Compiler Ordering
      ↓
   Function Phase
   ├─ Select Retain + Eligible Target Functions
   ├─ Shared Expansion/Environment Validation
   └─ Materialize Accepted Canonical Results
      ↓
   Expanded Module Forms
```

这里的 local-macro finalization 是 Attribute Phase 的收尾，不构成第三个 pass。属性阶段只负责按源码位置决定“此刻能看见什么、此 form 如何进入后续流”；函数阶段才递归处理 function body。

## 2. 组件职责层级

### 2.1 Module Orchestrator

模块级协调器只负责阶段编排：

1. 初始化外部宏环境与不透明的 local-macro state。
2. 执行统一属性扫描。
3. 调用 local-macro 收尾并取得 `FinalLocalEnv`、retain IDs 和 `FinalSkipIds`。
4. 从完整 attribute 输出构造 `FinalMacroRuntimeContext`。
5. 让 retain 与普通目标 function 通过同一个展开/环境一致性验证器。

协调器不解释 local-macro 的冻结、缓存、编译 generation、retain 闭包或安全加载计划。

### 2.2 Attribute Scan Engine

扫描器维护以下逻辑状态：

```text
ScanContext = {
  external_env,       % 当前可见的外部宏及其 options
  local_state,        % local-macro 工作流的不透明状态
  passed_forms,       % 已完成处理并保留的 forms，保持输出顺序
  queue,              % 当前 form 之后仍待处理的精确队列
  diagnostics         % traverse 管线累计的错误、warning 与 formatter
}
```

`passed_forms` 与 `queue` 必须严格分离：前者是 attribute injection 的历史视图，后者只用于扫描调度和 remaining-source 观察。尚未真正处理的 splice 结果不能进入 injection 视图。

注册 local declaration 时必须构造唯一形状的 declaration-time `MacroRuntimeContext`，其中 `inject_forms = passed_forms`。完整的 `passed_forms ++ queue` 只用于闭包发现；它不能替代 context 中的注入视图。注册后立即尝试以该 context 预展开，编译只在某次展开或调用真正产生 `NeedCallable` 时发生。

### 2.3 Local-Macro Gateway

扫描器只依赖三个语义接口：

```text
register_and_preexpand(Declaration, RuntimeContext, LocalState, SourceView)
  -> LocalState' | Error

need_callable(LocalFA, LocalState)
  -> {CallableLocalEnv, LocalState'} | Error

finalize(AllScannedForms, LocalState)
  -> {FinalLocalEnv, RetainIds, FinalSkipIds} | Error
```

接口返回值可按实际类型调整，但职责边界不可改变。`need_callable` 是预展开、attribute 调用、最终 function 展开和 finalize 共用的依赖入口，不属于 attribute 专用接口。编译器只消费已由展开验证器确认的 canonical forms。

### 2.4 Macro Environment Manager

环境管理器负责：

- 解析并合并 `import_macro`；
- 将 `use_macro` 的选择、alias 和逐 key option 合并为有效宏映射；
- 以后值覆盖同名 key 的方式累计全局 `macro_options`；
- 合并外部与当前可调用的本地宏；
- 对宏 key 做冲突检查：相同定义幂等，不同定义仅允许 `force_override` 覆盖。

它不回溯更新已经处理的 forms。每次成功更新只替换当前 context 中的环境快照。

### 2.5 Shared Macro Expansion Core

共享核心提供单一 function 展开能力，并由调用方显式指定白名单控制：

```text
expand_and_validate(MacroRuntimeContext, OriginalForms, TargetFAs,
                    WhitelistControl, ExpansionRecords)
  -> Forms' | Error

WhitelistControl = disabled | collect(FormId) | verify(FormId, Expected)
```

共享核心从 original/frozen form 展开。local frozen function 首次使用 `collect`：原始 AST 在 `match_macro_call` 成功后观察真实 local FA；每个 macro 返回 AST 由 `process_macro_return` 在既有 traversal 中一次性收集 local FAs，并以 `{Node, ReturnObserved}` 交给调用方合并。后续 declaration/final context 使用 `verify`；普通 function 与 attribute 使用 `disabled`。

相同 input fingerprint 复用缓存的 whitelist/result；不同 input 先验证 canonical whitelist，再比较 expanded AST。final local closure 按 canonical whitelist 过滤 local-macro 部分，不再维护 final 排除规则。`process_macro_return` 规范化 AST、位置和变量并收集当前 Return AST 的 local macro presence，但不校验或执行 replacement；不得增加第二次 return-tree scanner、whole-form rescan 或 AST diff。

## 3. Attribute Phase 详细流程

### 3.1 初始化

```text
Queue         := OriginalForms
PassedForms   := []
ExternalEnv   := empty/existing imported environment
LocalState    := local workflow initial state
```

### 3.2 单步扫描

每一步从队首取出 `Form`，使用当前 context 处理，并且只产生以下三种调度结果：

```text
keep(Form1)
  -> PassedForms := PassedForms ++ [Form1]

consume
  -> 不加入 PassedForms

splice(NewForms)
  -> Queue := NewForms ++ Queue
```

`splice` 必须保留 `NewForms` 内部顺序，并立即在同一环境演进规则下重新处理。任何已 keep 或已 consume 的旧 form 都不重新入队，因此整个过程是单向扫描而不是 fixed-point。

### 3.3 Form 决策树

```text
Handle(Form)
├─ import_macro
│  ├─ 解析、冲突校验、更新 ExternalEnv
│  └─ consume
├─ use_macro
│  ├─ 选择/alias、合并 options、冲突校验、更新 ExternalEnv
│  └─ consume
├─ macro_options
│  ├─ 更新全局 options
│  └─ keep(Form)
├─ local_macro declaration
│  ├─ gateway.register_and_preexpand(...)
│  └─ 按 local-macro 契约决定 declaration 的保留/物化，不在扫描器内猜测
├─ attribute-shaped form
│  ├─ 用 ExternalEnv + CallableLocalEnv 按统一规则解析
│  ├─ 已可调用宏 -> invoke -> validate -> splice(GeneratedForms)
│  ├─ 已注册但未就绪 local 宏
│  │  └─ gateway.need_callable -> invoke at same position -> splice
│  ├─ 语法上要求执行但无法执行 -> 诊断一次 + keep(original)
│  └─ 非宏普通 attribute -> keep(Form)
└─ ordinary form
   └─ keep(Form)
```

属性宏产生的普通 function/spec 在这里先被保留，不作为普通 Step 2 target 提前展开。local declaration 是显式例外：注册时可从其 frozen originals 预展开以填充 ExpansionRecord。属性宏产生的环境 form 或 local declaration 因 splice 回队首，会自然经过同一决策树并只影响后续 forms。

### 3.4 Attribute Injection

调用属性宏时，注入输入只由当前 `passed_forms` 构造。不能读取：

- 当前 form；
- 尚未处理的原队列；
- 同一 splice 中排在当前 form 后面的生成 form；
- 未来才会生成或保留的 attribute。

函数宏的注入视图不同：它使用 attribute phase 完成后的完整保留 forms。

local macro forms 的预展开使用 declaration 前的 `MacroRuntimeContext`：宏名称、alias、调用参数、`inject_attrs` 配置和注入值都来自该时点。用于闭包发现的 remaining queue 不属于该 context。GenerationCompiler 不读取它；local attribute 与 external attribute 使用完全相同的调用点运行规则。

### 3.5 宏调用与状态隔离

宏调用流程应当是：

```text
Resolve Macro
  -> Build Call-Site Injection / Options
  -> Execute Macro in Private Traverse State
  -> Validate Returned Tree and Role
  -> Normalize Position / Variables / Formatter
  -> Return splice forms or recovery value
```

宏内部的 traverse `put/modify` 不能覆盖 `ScanContext` 或函数遍历 state；错误、warning、formatter、文件与位置信息仍沿外层 traverse 管线传播。返回 `astranaut_return` 的校验或 workflow 调用必须显式桥接到 traverse；所有 state 操作必须通过 bind/do 串联。

单次宏调用失败时，以原调用 form/tree 作为恢复值继续分析兄弟项，以便累计诊断。语法上要求执行但当前无法执行的 attribute 只在当前扫描点诊断一次。

## 4. Attribute Phase 收尾

扫描队列清空后按固定顺序收尾：

1. 将完整扫描结果与 `LocalState` 交给 local-macro gateway `finalize`。
2. 取得 `FinalLocalEnv`、retain 物化 forms、`FinalSkipIds`。
3. 合入物化结果，并从 function phase 输入中移除 `FinalSkipIds` 指定的 forms。
4. 对生成 function/spec 做最小冲突整理。
5. 最后执行一次编译器 form 排序。

扫描器不得在 finalize 前自行删除 local-macro 原始 forms，也不得自行推导最终可调用集合。

## 5. 生成 Function/Spec 的最小整理

生成 form 在扫描过程中始终保持当前位置。只有同时满足以下条件才允许局部改写：

1. 生成 function 与已有 function 同名同 arity；
2. 生成 function 调用了 `__original__/Arity`。

此时：

- 为原 function 选择不与现有同 arity function 冲突的新名字；
- 重命名原 function；
- 同步替换生成 function 中相关 `__original__` 调用；
- 只移动或合并解决该冲突所必需的 function/spec；
- 保持所有无关 forms 的相对顺序。

禁止把全部 forms 分为 Generated/Base 后全局重新插入，因为这会改变无冲突生成项的源码局部语义。

## 6. Function Phase 详细流程

```text
FinalMacroRuntimeContext := context_from(AttributeOutput, EffectiveMacroMap, FinalLocalEnv)
CandidateForms := AttributeOutput - FinalSkipIds
TargetFAs := RetainIds union functions_that_can_contain_macro_calls(CandidateForms, FinalMacroRuntimeContext)
Result := expand_and_validate(FinalMacroRuntimeContext, OriginalForms, TargetFAs,
                              per_form_whitelist_control, ExpansionRecords)
```

函数阶段：

- 只遍历未被跳过且实际可能包含宏调用的目标 function；
- retain 与普通 function 都使用 attribute phase 完成后的最终 `MacroRuntimeContext`；
- 曾参与 local-macro 展开的 form 先按 canonical whitelist 过滤 FinalLocalEnv、使用 `verify`，并在 whitelist 相同后比较 expanded result；
- 非 local-closure 普通 function 使用完整 FinalLocalEnv，并显式传 `disabled`；
- 沿用既有递归、`outer` / `inner` 与 `max_depth` 语义；
- 不再次排序 forms；
- 不让未编译或不在 `FinalLocalEnv` 中的 local macro 参与匹配。

## 7. 关键不变量

1. **单向性**：环境变化只影响队列中的后续 form，绝不回扫 `passed_forms`。
2. **当前位置语义**：splice 结果先于原剩余队列处理，内部相对顺序不变。
3. **历史/未来隔离**：attribute injection 只能看 `passed_forms`。
4. **阶段隔离**：attribute phase 不递归展开 function body。
5. **策略隔离**：展开验证、依赖调度和 generation 编译互相分离。
6. **状态隔离**：用户宏的 traverse state 不泄漏到框架 state。
7. **冲突显式性**：不同宏定义占用同一 key 必须显式 `force_override`。
8. **最小改写**：没有 `__original__` 合并需要时，不重排生成 function/spec。
9. **最终环境真实性**：retain 与普通 function phase 只能使用唯一的 `FinalMacroRuntimeContext`。
10. **诊断稳定性**：无法执行的宏 attribute 在其扫描位置只诊断一次。
11. **白名单真实性**：canonical whitelist 来自原始 AST 的真实 local match，以及每个未被 traversal skip 的 macro 返回 AST 在规范化 traversal 中收集的 local macro presence，不来自独立预扫描。
12. **冲突独立性**：whitelist 不同报告 `conflicting_local_macro_whitelist`；whitelist 相同但 AST 不同报告 closure-environment conflict。

## 8. 建议的验证矩阵

| 维度 | 必测行为 |
|---|---|
| 扫描顺序 | 外部/本地属性宏交错；生成属性立即重扫；旧结果不回扫 |
| 环境演进 | 生成 import/use/options；同 splice 后项可见；冲突与 force_override |
| injection | 只见 passed；不可见 remaining queue 与同 splice 后项 |
| local gateway | 逐 FA 注册/预展开、通用 NeedCallable、finalize、FinalSkipIds、未编译项过滤 |
| form 顺序 | 无冲突生成 function/spec 原地；`__original__` 仅局部整理 |
| 阶段边界 | 生成 function 的宏调用只在最终 function phase 展开 |
| 状态/错误 | 私有 traverse state；return/traverse 桥接；兄弟诊断累计；单次 invalid attribute |
| 共享语义 | attribute/local/retain/function 同一 context 逻辑；同 declaration 成员保持普通调用；编译只消费 canonical forms |
| whitelist | disabled/collect/verify 显式边界；replacement 递归观察；unexpected/missing；final env 过滤；普通 function 禁用 |
