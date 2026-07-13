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
   ├─ Retain Materialization / Skip Filtering
   └─ Minimal Merge + Compiler Ordering
      ↓
   Function Phase
   ├─ Build Final Macro Environment
   ├─ Select Eligible Target Functions
   └─ Shared Recursive Function Expansion
      ↓
   Expanded Module Forms
```

这里的 local-macro finalization 是 Attribute Phase 的收尾，不构成第三个 pass。属性阶段只负责按源码位置决定“此刻能看见什么、此 form 如何进入后续流”；函数阶段才递归处理 function body。

## 2. 组件职责层级

### 2.1 Module Orchestrator

模块级协调器只负责阶段编排：

1. 初始化外部宏环境与不透明的 local-macro state。
2. 执行统一属性扫描。
3. 调用 local-macro 收尾并取得 `FinalLocalEnv`、retain 物化结果和 `FinalSkipIds`。
4. 整理 attribute phase 输出，剔除跳过项并排序到编译器可接受的 form 顺序。
5. 使用最终环境执行唯一一次 function phase。

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

注册 local declaration 时还必须把 `passed_forms` 单独冻结为 `inject_forms_snapshot`。完整的 `passed_forms ++ queue` 只用于闭包发现；它不能替代注入快照。以后即使在 attribute 调用点按需编译 local macro forms，也必须使用 declaration-time MacroEnv 与该注入快照。

### 2.3 Local-Macro Gateway

扫描器只依赖三个语义接口：

```text
register(Declaration, LocalState, SourceView)
  -> LocalState' | Error

ensure_callable(AttributeFA, LocalState, SourceView)
  -> {CallableLocalEnv, LocalState'} | Error

finalize(AllScannedForms, LocalState)
  -> {FinalLocalEnv, MaterializedForms, FinalSkipIds} | Error
```

接口返回值可按实际类型调整，但职责边界不可改变。`ensure_callable` 负责让已注册但未就绪的本地属性宏在当前调用点可执行；扫描器不自行展开编译计划。`finalize` 必须在 function phase 之前完成。

### 2.4 Macro Environment Manager

环境管理器负责：

- 解析并合并 `import_macro`；
- 将 `use_macro` 的选择、alias 和逐 key option 合并为有效宏映射；
- 以后值覆盖同名 key 的方式累计全局 `macro_options`；
- 合并外部与当前可调用的本地宏；
- 对宏 key 做冲突检查：相同定义幂等，不同定义仅允许 `force_override` 覆盖。

它不回溯更新已经处理的 forms。每次成功更新只替换当前 context 中的环境快照。

### 2.5 Shared Macro Expansion Core

共享核心提供两项同构能力：

```text
expand_functions(MacroEnv, Forms, TargetFAs)
  -> Forms' | Error

resolve_local_references(CandidateLocalEnv, Forms, ClosureFAs)
  -> ReferencedFAs | Error
```

两者必须复用相同的调用匹配规则。共享核心只消费调用方构造好的 `MacroEnv`，不读取 local-macro 的 declaration order、generation、retain 或 `internal_function` 策略。展开某个 local target 时，从环境中移除 target 自身也是 local-macro 调用方的责任。

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
│  ├─ gateway.register(...)
│  └─ 按 local-macro 契约决定 declaration 的保留/物化，不在扫描器内猜测
├─ attribute-shaped form
│  ├─ 用 ExternalEnv + CallableLocalEnv 按统一规则解析
│  ├─ 已可调用宏 -> invoke -> validate -> splice(GeneratedForms)
│  ├─ 已注册但未就绪 local 宏
│  │  └─ gateway.ensure_callable -> invoke at same position -> splice
│  ├─ 语法上要求执行但无法执行 -> 诊断一次 + keep(original)
│  └─ 非宏普通 attribute -> keep(Form)
└─ ordinary form
   └─ keep(Form)
```

属性宏产生的 function、spec 和普通 forms 在这里都只是被保留；不得提前递归展开 function body。属性宏产生的环境 form 或 local declaration 则因为 splice 回队首，会自然经过同一决策树并只影响后续 forms。

### 3.4 Attribute Injection

调用属性宏时，注入输入只由当前 `passed_forms` 构造。不能读取：

- 当前 form；
- 尚未处理的原队列；
- 同一 splice 中排在当前 form 后面的生成 form；
- 未来才会生成或保留的 attribute。

函数宏的注入视图不同：它使用 attribute phase 完成后的完整保留 forms。

local macro forms 的编译具有唯一的 local 特殊规则：其宏名称、alias、调用参数、`inject_attrs` 配置和注入值都只来自 `-local_macro` declaration 前的 `passed_forms`。用于闭包发现的 remaining queue 不属于该编译上下文。编译完成后，local attribute 与 external attribute 使用完全相同的调用点运行规则。

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
FinalMacroEnv := merge_checked(ExternalEnv, FinalLocalEnv)
CandidateForms := AttributeOutput - FinalSkipIds
TargetFAs := functions_that_can_contain_macro_calls(CandidateForms, FinalMacroEnv)
Result := expand_functions(FinalMacroEnv, CandidateForms, TargetFAs)
```

函数阶段：

- 只遍历未被跳过且实际可能包含宏调用的目标 function；
- 使用 attribute phase 完成后的最终属性视图做 injection；
- 沿用既有递归、`outer` / `inner` 与 `max_depth` 语义；
- 不再次排序 forms；
- 不让未编译或不在 `FinalLocalEnv` 中的 local macro 参与匹配。

## 7. 关键不变量

1. **单向性**：环境变化只影响队列中的后续 form，绝不回扫 `passed_forms`。
2. **当前位置语义**：splice 结果先于原剩余队列处理，内部相对顺序不变。
3. **历史/未来隔离**：attribute injection 只能看 `passed_forms`。
4. **阶段隔离**：attribute phase 不递归展开 function body。
5. **策略隔离**：扫描器不解释 local-macro 生命周期；共享展开器也不解释 local 策略。
6. **状态隔离**：用户宏的 traverse state 不泄漏到框架 state。
7. **冲突显式性**：不同宏定义占用同一 key 必须显式 `force_override`。
8. **最小改写**：没有 `__original__` 合并需要时，不重排生成 function/spec。
9. **最终环境真实性**：function phase 只能使用 finalize 返回的可调用 local 环境。
10. **诊断稳定性**：无法执行的宏 attribute 在其扫描位置只诊断一次。

## 8. 建议的验证矩阵

| 维度 | 必测行为 |
|---|---|
| 扫描顺序 | 外部/本地属性宏交错；生成属性立即重扫；旧结果不回扫 |
| 环境演进 | 生成 import/use/options；同 splice 后项可见；冲突与 force_override |
| injection | 只见 passed；不可见 remaining queue 与同 splice 后项 |
| local gateway | 注册、按需可调用、finalize、FinalSkipIds、未编译项过滤 |
| form 顺序 | 无冲突生成 function/spec 原地；`__original__` 仅局部整理 |
| 阶段边界 | 生成 function 的宏调用只在最终 function phase 展开 |
| 状态/错误 | 私有 traverse state；return/traverse 桥接；兄弟诊断累计；单次 invalid attribute |
| 共享语义 | 普通/local function 同一展开器；引用解析同一匹配规则；target 自移除在调用方 |
