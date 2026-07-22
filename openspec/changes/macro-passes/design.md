# Macro Passes 设计

## 设计目标

1. 外部与本地属性宏在同一次 scan-and-splice 中按源码顺序处理。
2. 宏环境增长只影响后续 forms，不回扫已处理结果。
3. local macro 的专属生命周期委托给 [local-macro 设计](../local-macro/design.md)。
4. 最终函数体展开继续保留现有递归及 `outer` / `inner` 语义。
5. scan 的局部顺序、诊断信息和 traverse state 不因宏执行或 forms 整理被破坏。

## 统一 function 展开能力

`astranaut_macro_expander` 只维护一套 function-body 宏匹配和递归展开实现；
`astranaut_macro` 负责 attribute/function pass 编排，`astranaut_macro_scan` 负责
source-ordered scan-and-splice 与扫描期 traverse state，`astranaut_macro_registry`
负责环境更新和阶段化 macro environment 解析。
白名单是 local frozen function expansion 的可选观察/校验策略；调用方必须显式传入
控制值，展开器不得根据 MacroEnv、FormId 或阶段隐式推断：

```text
LocalMacroWhitelistControl =
    disabled
  | #{mode := collect, form_id := FormId}
  | #{mode := verify,
      form_id := FormId,
      expected := ordsets:ordset(FA)}

ExpandFunctions(Forms, #{FormId => #{
  form := OriginalOrFrozenForm,
  macro_map := ResolvedMacroMap,
  whitelist_control := WhitelistControl
}})
  -> #{forms := ExpandedForms,
       task_results := #{FormId => #{
         form := ExpandedTargetForm,
         local_macro_whitelist := disabled | ordsets:ordset(FA),
         needed_local_macros := ordsets:ordset(FA)
       }}}
  | Error
```

- `disabled` 不分配 accumulator、不观察 local match、不做完成检查，结果 whitelist 固定为 `disabled` 且 `needed_local_macros` 为空。
- `collect` 累计原始 function 中实际发现的 local FAs，以及每个 macro 返回 AST 在 `process_macro_return` 中一次性收集的 local FAs；成功后返回 canonical whitelist。
- `verify` 使用相同来源累计 FAs；每个返回 AST 收集完成后由调用方批量拒绝 expected 之外的集合，完整展开后检查缺失项。final retained context 先用 expected 过滤 LocalEnv，使名单外调用保持普通调用。

local declaration 预展开、retain 和最终 function pass 都调用该实现。首次 local frozen FormId 使用 `collect`，已有 canonical whitelist 后使用 `verify`；普通 Step 2 function、普通 retained function 与 attribute invocation 使用 `disabled`。每次从 original/frozen form 开始；环境相同直接复用完整结果，环境不同则先校验 whitelist、再与上一次已接受 AST 比较。local-macro generation 编译不调用展开器，只消费已经确认的 canonical expanded forms。

attribute 目标解析、调用参数构造和 function caller 检测也由同一 expander 提供，因此
attribute 与 function 路径不会各自维护一份 `find_macro` 或返回 AST 规范化实现。
`inject_attrs` 由扫描器的 `AttributeEnv` 在 declaration/call/final 边界预解析；expander
只读取 descriptor 中的 `attributes`。expander 不拥有扫描队列、宏环境更新或 local
generation 生命周期，也不存在单函数兼容门面。

function 调用闭包构造与宏 caller 检测共享 `FunctionCallAnalysis`。closure walk 在首次
访问某个可达 function 时，用对应 declaration MacroEnv 的同一次 AST traversal 同时
收集普通本地调用边、匹配到的 local macro FAs 和任意宏 presence；同一 declaration
的多个 roots 复用该结果。final caller 筛选只消费宏 presence，因此使用 FinalMacroEnv
生成一次 presence-only analysis，不构造未消费的本地调用集合。两种 mode 都携带原始
form；只有 form 一致且分析环境是实际展开环境的相同或安全超集时，expansion task 才能
把 presence 当作可信提示，否则回退到现场检查。

普通 attribute 使用随 `EffectiveMacroMap` 增量重建的 `AttributeMacroIndex`，先按
attribute name/arity 直接定位 descriptor，再只为该 descriptor 解析 `inject_attrs`。
declaration 与 final function 是批量展开边界，可能命中 macro map 中的任意 descriptor，
因此只在这两个边界统一解析完整 map；扫描普通 form 不遍历或解析 macro map。

原始 function 的白名单观察位于统一发现—执行路径；macro 返回 AST 则复用 `process_macro_return` 已有的完整返回树 traversal，一次性收集后再交回调用方：

```text
Original function match
  -> observe matched local FA
  -> unavailable: add needed_local_macros and do not invoke
  -> callable: invoke macro

process_macro_return(Return)
  -> normalize/update every returned node
  -> collect all local macro FAs in this Return AST
  -> record whether any macro call exists in this Return AST
  -> return {ProcessedNode, ReturnAnalysis}

expand_macro_with
  -> merge ReturnObserved into function-level Observed
  -> verify the completed ReturnObserved batch
  -> unexpected batch: emit one conflict and do not expand this replacement
  -> accepted batch with macro presence: continue replacement expansion
  -> accepted batch without macro presence: return the processed AST directly
```

`process_macro_return` 只收集，不校验也不调用 replacement 中的宏。它以
`scoped_state_run` 使用局部 analysis map state，同时携带 `local_macro_calls` 与
`has_macro_call`；调用方合并 local FAs 并执行批量校验。只有被接受且确实含宏的返回
AST 才沿既有 pre/post 路径进入 `transform_exprs`。不得增加第二次 return-tree scanner、
whole-form rescan 或 expanded/original AST diff。宏私有 State 与返回树分析 State 均不得
覆盖 function expansion 的外层 State。

展开操作返回 `astranaut_return` 结果。whitelist/result 必须通过该单一返回形状交回调用方，不得使用 callback、process dictionary 或另一份隐式 traverse state。统一扫描只在调用 local-macro 的注册、按需可调用和收尾接口时桥接 traverse/return monad，不在扫描器内执行或解释 local-macro 编译计划。

### 候选环境与启用边界

必须区分候选能力与真实观察结果：

```text
CandidateLocalEnv
  = declaration 时冻结、可能被该 local frozen function 匹配的 local entries

ObservedLocalFAs
  = 原始 function 的真实 local match
    ∪ 每个未被 traversal skip 的 macro 返回 AST 中一次性收集的 local macro presence
```

declaration `collect` 使用冻结的 CandidateLocalEnv；declaration/非-final `verify` 使用当前 CandidateLocalEnv，并在每个返回 AST 完成收集后批量拒绝 whitelist 外 presence；final retained `verify` 使用 `FinalEnv + canonical whitelist` 构造有效 local env。通用展开器不解释 declaration order、self、retain 或 generation。

同一 declaration 的 members 因 declaration 前 CandidateLocalEnv 尚不包含本次新增 entries 而保持普通调用。白名单上线后不再并存 final order/self/member 排除链、owner union 或共享 helper 的运行时猜测。普通 function 的 `disabled` 模式使用完整 FinalMacroEnv。

### 白名单与展开结果冲突

首次成功的 `collect` 原子建立：

```text
CanonicalWhitelist(FormId) = ObservedLocalFAs
```

非-final `verify` 采用非对称检查：`process_macro_return` 先完整收集当前 Return AST，再由调用方计算 `ReturnObserved - Expected`。非空时只报告一个包含该批全部 unexpected FAs 的 `conflicting_local_macro_whitelist`，并且不进入该 replacement 的递归展开。收集过程中不得校验或提前中断。原始 function 中直接发现的 unexpected match 仍在调用前拒绝。缺失项只能在完整递归展开结束后以 `Expected - Observed` 检查，因为后续 replacement AST 仍可能生成该调用。final retained 已先过滤环境，不产生 unexpected whitelist 错误。

错误携带：

```text
{conflicting_local_macro_whitelist,
 FormId,
 #{expected => Expected,
   observed => Observed,
   unexpected => Unexpected,
   missing => Missing}}
```

白名单相同后仍执行最终 AST 一致性比较：whitelist 不同即使 AST 相同也报告 `conflicting_local_macro_whitelist`；whitelist 相同但 AST 不同报告 `conflicting_local_macro_closure_environment`。任何冲突或宏执行失败都不得提交部分 ExpansionRecord、canonical form 或 generation。

### ExpansionRecord 与 fingerprint

```text
ExpansionRecord = #{
  canonical_whitelist := ordsets:ordset(FA),
  canonical_result := Form,
  results_by_input := #{InputFingerprint => #{
    whitelist := ordsets:ordset(FA),
    result := Form
  }}
}
```

InputFingerprint 覆盖展开前可知的 external macro map、候选 local descriptors/versions、macro options、解析后的 internal macro bindings 和 inject forms。internal bindings 必须进入 key，因为两个 alias 可能在移除宏映射后留下相同 MacroEnv，却需要改写到不同的远程函数。whitelist 是展开输出，不能作为首次 cache lookup 的唯一输入 key。final retained 已有 canonical whitelist 时，fingerprint 使用过滤后的有效 MacroEnv，并带上 declaration 的 internal bindings；名单外 local descriptor/generation 不进入该 form 的 input key。普通 function 为 `disabled`，继续使用普通 function fingerprint/展开规则。

### replacement 驱动 callable 调度

`collect` 或非-final `verify` 可能在 external/local replacement AST 中首次匹配尚不可调用的候选 local macro。该情况不是 whitelist 冲突：展开器返回 `needed_local_macros`，不调用该宏，也不提交部分 expansion；`astranaut_macro_local` 通过 `NeedCallable` 编译所需累计 boundary，再从 frozen form 重试。这样按真实匹配驱动最小编译，无需预编译全部 candidates。

## 两步模型

顶层只有两个 pass。local-macro 的收尾不是第三个 pass，而是 attribute pass 的收尾子步骤；它必须在 forms 交给 function pass 前完成。

```text
1. Attribute pass
   1.1 初始化 ExternalEnv、LocalMacroState、Queue 和 Output
   1.2 逐 form 统一 scan-and-splice
       - 外部与可调用本地属性宏按当前位置展开
       - import/use/macro_options 前向更新 ExternalEnv
       - local_macro declaration 注册逐 FA 条目、冻结 context/CandidateLocalEnv、更新依赖并尝试预展开及收集 canonical whitelist
       - 任意展开/调用需要未就绪 local macro 时产生通用 NeedCallable
   1.3 收尾 local-macro 工作流，取得 FinalLocalEnv、RetainIds 与 FinalSkipIds
   1.4 从完整 AttributeEnv 解析 FinalMacroEnvironment

2. Function pass
   - retain 与普通目标 function 使用相同 FinalMacroEnvironment
   - 通过共享 ExpansionValidator 展开或比较最后一次 local 结果
   - 物化 accepted canonical forms，并只在既定边界排序
```

## 统一属性扫描

扫描 state 持有当前 `EffectiveMacroMap`、已通过扫描的 `passed_forms`、增量 `AttributeEnv`，以及不透明的 `LocalMacroState`。local macro 的注册表、缓存和编译产物不在本变更中定义。扫描 local declaration 时，function forms 的预展开 `MacroEnvironment` 从 declaration 前的状态解析；另可交付 `passed_forms + 当前及剩余 queue` 作为结构性的闭包源码视图，但后者不是宏展开环境。

### 队列与输出模型

```text
Queue  = 输入 forms
Output = []

while Queue 非空:
  Form = pop_front(Queue)
  Result = handle(Form, CurrentState)
  case Result of
    keep(Form1)      -> Output 追加 Form1，并将它记为 passed
    consume          -> 不输出该 form
    splice(NewForms) -> NewForms 插入 Queue 前端，立即按当前环境重扫
  end
```

这是一轮按源码顺序的前向扫描，不是全模块 fixed-point。已经 `keep` 或已被 attribute 展开消费的旧 form 绝不因后续环境变化重新进入队列。

`passed_forms` 以输出顺序保存已处理 form，供 attribute injection 使用；新 splice form 在真正被处理前不属于 `passed_forms`。启用 queue state 时，扫描器可见的 remaining source view 是“当前 form + 精确剩余队列”，其中包含尚未处理的生成 forms，但这不会使它们提前成为 attribute injection 输入。

遇到 `local_macro` declaration 时，扫描器冻结一份 local function-form 预展开上下文，并提供一份独立的结构源码视图：

```text
DeclarationMacroEnvironment = {
  MacroMap     = declaration 前已生效并用 AttributeEnv 解析的宏描述符,
  MacroOptions = declaration 点 options
}
ClosureSourceView = passed_forms ++ 当前及剩余 queue
```

`ClosureSourceView` 只用于定位 function/spec 与计算静态闭包；预展开 frozen local macro forms 时，宏名称、alias、调用参数及已解析 attribute 值全部由 `DeclarationMacroEnvironment` 决定。generation 编译不读取该 environment。

| 扫描到的 form | 行为 |
|---|---|
| `-import_macro(...)` | 更新 ExternalEnv 并消费该 form。 |
| `-use_macro(...)` | 更新 ExternalEnv 并消费该 form。 |
| `-macro_options(...)` | 更新全局 options，保留该 form，并记入 `passed_forms`。 |
| `-local_macro(...)` | 注册共享 order/context 的逐 FA 条目、更新依赖并尝试预展开。 |
| 外部属性宏调用 | 用当前环境展开，结果 splice 回队列。 |
| 已可调用的本地属性宏 | 用当前环境展开，结果 splice 回队列。 |
| 已注册但尚不可调用的本地属性宏 | 产生通用 `NeedCallable`，成功后仍在原位置展开。 |
| 其他 form | 原样保留。 |

属性宏可生成新的 `import_macro`、`use_macro`、`macro_options` 或 `local_macro` form；这些 form 重新进入同一扫描流。`export_macro` 单独出现时不会使宏在定义模块内变为可调用的 local macro；其专属保留语义见 local-macro 文档。

宏定义 options 使用分层 validator：`export_macro` 和 `local_macro` 共享
`as_attr`、`order`、`inject_attrs`、`group_args`、`force_override`、`max_depth`；
只有 `local_macro` 接受本地闭包选项 `extra_functions` 与 `internal_function`；前者补充
函数闭包，后者解析 declaration 位点当前可见的宏 key 并把对应调用固化为普通函数；二者既不
属于模块级 `macro_options`，也不属于 `export_macro`。把闭包构造选项写在这两种
attribute 上应作为 unexpected option 报告并忽略，不得让全局配置或导出声明隐式参与
本地闭包构造。

### 宏环境 form 的处理

- `import_macro` 解析导入模块并更新可用外部宏映射。
- `use_macro` 基于已导入宏映射选择或别名宏，并将 option 合并结果写入 ExternalEnv。
- `macro_options` 以后者覆盖同名全局 option，未提及的 option 保留。
- `debug`、`debug_ast`、`max_depth` 是逐宏 global defaults，只在其后 import external
  macro 或声明 local macro 时复制进 macro descriptor，不反向更新已存在 descriptor，
  也不作为宏函数实参。definition `max_depth` 覆盖 global default；use 位点的
  `debug`/`debug_ast` 覆盖 global default。
- `debug_module`、`debug_module_ast` 只读取 scan 完成后的最终 global value，用于打印
  完整 transformed module，不控制单次宏调用。
- 同一 `use_macro` 再次配置同一宏时，后声明 option 覆盖同名 key，其他 key 保留。
- 相同宏定义可幂等合并；若新导入、alias 或本地映射占用已有宏 key 且定义不同，必须显式使用 `force_override`，否则报 `macro_override`。因此不存在无条件的“同名导入后者覆盖”。

环境 form 可以由属性宏生成；生成顺序决定其对后续属性的可见性。例如某属性先 splice `import_macro(macro_b)`，再 splice 依赖 `macro_b` 的属性时，后者必须在导入已生效后展开。

### 属性宏的判定、注入与展开

每次处理 attribute 时，从 `ExternalEnv + 当前可调用 LocalEnv` 构造执行宏映射，用当前增量 `AttributeEnv` 把 `inject_attrs` selector 解析为 descriptor 的 `attributes`，再按既有 `as_attr`、`exec_macro` 规则匹配。所有 attribute 宏无论来自 external 还是 local，都只看当前位置之前的 attributes；最终 function 宏使用 attribute pass 完成后的完整环境。

若当前 attribute 需要尚不可调用的 local macro，只产生与其他阶段相同的 `NeedCallable`。scheduler 使用已经确认的 canonical forms 编译必要 boundary；随后 attribute 仍按调用点 `MacroEnvironment` 运行。attribute 不拥有专用编译策略。

若命中外部或已就绪本地属性宏，使用当前映射展开并返回 `splice(NewForms)`。若 attribute 对应已注册但尚不可调用的 local macro，扫描器调用 local-macro 工作流的确保可调用接口；成功后仍在同一队列位置展开，不能延后到独立本地 pass。

未匹配的普通 attribute 与普通 forms 保持原样。属性宏生成的 function、spec 或其他普通 form 留在输出流中，不在属性扫描阶段提前执行函数体递归展开。语法上属于宏 attribute 调用但当前无法执行的 form 会在扫描位置产生一次 `invalid_macro_attribute` 诊断并保留，不得在收尾阶段重复诊断。

## 生成 forms 的顺序与最小整理

`splice(NewForms)` 保留 `NewForms` 内的相对顺序，并在剩余原队列之前处理。扫描结束前不得将所有生成 forms 与原始 forms 全局拆分后重插入。

生成的 function/spec 只携带用于最小合并的内部标记：

- 无同名同 arity 冲突时保持 splice 后的局部位置。
- 只有生成 function 调用 `__original__/Arity` 且存在同名同 arity function 时，才重命名原函数、替换相关调用并合并。
- 新名字必须避开现有同 arity function。
- 该整理不得移动无关 function、spec 或其他 forms。

attribute pass 全部收尾完成后可调用 `sort_forms/1` 生成 Erlang 编译器可接受的最终 form 顺序；排序发生在统一扫描之后，function pass 不再排序。

## Traverse、错误与宏执行 state

扫描使用 traverse state 携带环境，因此 `put`、`modify` 等操作必须在 do/bind 中串联，不能用普通逗号表达式丢弃 monad 值。

`used_macros`、校验和 local-macro 接口返回的 `astranaut_return` 必须通过 `astranaut:traverse_return/1` 桥接，以保留累积错误、formatter 和位置。单个宏调用失败时以原调用作为临时恢复值，使兄弟节点仍可继续分析和累计诊断。

用户宏返回的 traverse computation 在私有 state 中执行，但继承当前 traverse attribute；宏内部的 `put` 不得覆盖扫描环境或 function traversal state。宏返回树的校验、位置/变量整理及 formatter 处理仍在调用方的 traverse 管线中完成。

## Attribute pass 收尾与 function pass

扫描完成后调用 local-macro 收尾流程。该流程返回最终可调用的本地宏环境、RetainIds 及 `FinalSkipIds`；具体如何冻结、预展开、比较和编译 canonical forms 见 [local-macro 设计](../local-macro/design.md)。

function pass 从最终 attribute 输出解析唯一 `FinalMacroEnvironment`。retain 与普通目标 functions 作为任务表交给共享 ExpansionValidator，在一次保序 Forms 遍历中分别使用各自环境；若某个 form 曾在 declaration environment 中展开，final environment 不同时必须从 original form 展开并与最后一次结果比较。属于 frozen closure 的 retained form 在 FinalMacroEnvironment 上重放 declaration 的 internal macro key 过滤与 alias-to-remote 改写；该规则同样适用于 retained local macro 宏头。

扫描器在收尾前不自行删除 local macro 相关原始 forms，也不解释 local-macro 的编译计划；它只传递完整 forms 流和不透明状态，并消费工作流返回的最终环境、物化 forms 与跳过集合。
