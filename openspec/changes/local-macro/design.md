# Local Macro 设计

## 术语：本地宏闭包

本文的“闭包”不是函数式语言中捕获词法环境的 closure。它是一个静态函数集合：以 local macro 宏头函数为根，包含该函数及通过静态本地调用、`extra_functions` 等规则递归引用到的函数；这些函数对应的 `-spec` forms 也属于该闭包。

## 职责边界

`astranaut_local_macro` 管理逐 FA 声明条目、闭包、冻结、展开一致性记录、依赖调度、generation 编译、retain 和最终跳过集合。统一扫描器通过注册/预展开、通用 `NeedCallable` 及收尾接口与其协作；扫描和 splice 细节见 [macro-passes](../macro-passes/design.md)。

local macro function 的展开不使用另一套遍历器。`astranaut_macro` 负责统一的
`MacroRuntimeContext` 构造与 pass 编排；`astranaut_macro_expander` 负责宏引用匹配、
调用、返回 AST 规范化、递归 function 展开，以及显式 whitelist control 的观察结果。
local declaration 预展开、retain 及 Step 2 普通 function 展开共享该 expander；
generation compiler 不调用展开器，只消费已经通过多环境一致性校验的 canonical forms。

### 模块调用方向

```text
astranaut_macro
  ├─ 统一 scan、环境更新、attribute splice
  ├─ 构造阶段化 MacroRuntimeContext
  ├─ 调用 astranaut_macro_expander
  │    └─ 通用目标解析、宏调用、function 递归展开与展开期 monad state
  └─ 调用 astranaut_local_macro
       ├─ declaration 成员注册、依赖规划与状态转换
       ├─ 闭包、internal policy、冻结、ExpansionRecord、retain、FinalSkipIds
       └─ canonical forms 的累计编译与 local macro 模块加载
```

`astranaut_local_macro` 不拥有 scan 队列，也不直接实现 attribute handler。它通过
注册/预展开、`NeedCallable` 和收尾结果与 `astranaut_macro` 协作。计划由 local-macro
工作流驱动，但实际引用解析和 function 展开验证通过调用方提供的 `MacroOps` 直接指向
`astranaut_macro_expander`，从而复用统一错误上下文，并避免把 traverse monad 或扫描
队列耦合进 local-macro 模块。`astranaut_macro:expand_function/5` 仅保留为兼容入口。

### 同构 function 展开与验证接口

local macro 工作流调用的展开操作与最终普通 function pass 使用同一实现：

```text
ExpandAndValidate(MacroRuntimeContext, OriginalForms, TargetFAs, ExpansionRecords)
  -> {ExpandedForms, ExpansionRecords1} | Error
```

操作始终从 original/frozen form 展开。相同环境 fingerprint 直接复用；环境不同
则从 original form 重新展开并与最后一次已接受结果比较。它不解释 generation、
retain 或 declaration order。

实际 local 引用同样由 `astranaut_macro_expander` 的统一调用匹配能力识别：

```text
ResolveLocalReferences(CandidateLocalEnv, Forms, ClosureFAs) -> ReferencedFAs
```

`astranaut_local_macro` 提供候选环境和闭包，保存返回的 `ReferencedFAs` 并据此
规划累计边界；它不以“静态闭包中包含某 FA”替代真实的宏调用匹配。

## 显式数据接口

跨模块数据只允许下列命名形状：

```text
MacroRuntimeContext = #{
  macro_map := MacroMap,
  macro_options := Options,
  inject_forms := Forms
}

LocalMacroWorkflowContext = #{
  source_view := Forms,
  compile_opts := CompileOptions
}

ExpansionRequest = #{
  closure_ids,
  closure_fas,
  referenced_local_macros,
  runtime_context_snapshot,
  source_view,
  forms
}
```

`MacroRuntimeContext` 只描述某个源码时点的宏运行期环境；
`LocalMacroWorkflowContext` 只描述 local-macro scheduler/compiler 所需的模块构造输入。
两者不可用同一个模糊 `Context` 形状替代。扫描器必须通过统一构造器生成 workflow
context；注册时必须保存完整的 `runtime_context_snapshot`，不得同时保存
`env_snapshot`、独立 inject snapshot 或接受裸 MacroMap 的兼容形状。

`ExpansionRequest` 是 ExpansionValidator 与 planner 之间的显式接口。未被准备、依赖
分析或 compiler 消费的字段不得进入 request。

## 状态

```text
State = #{
  local_macros => #{FA => #{
    order => ScanSequence,
    runtime_context_snapshot => MacroRuntimeContext,
    source_view => MaterializedSourceAtDeclaration,
    options => Options,
    closure_ids => [FormId],
    closure_fas => [FA],
    referenced_local_macros => [FA],
    status => pending | compiled
  }},
  frozen_forms => #{FormId => OriginalForm},
  retain_roots => [FunctionId],
  retained_form_ids => [FormId],
  expansion_records => #{FormId => ExpansionRecord},
  canonical_expanded_forms => #{FormId => ExpandedForm},
  compiled_forms => #{FormId => CanonicalExpandedForm},
  committed_boundaries => #{BoundaryKey => Generation},
  local_macro_expanded_ids => [FormId],
  generation => non_neg_integer()
}
```

FA 注册表同时保存不可变声明快照和逐 FA 生命周期。多个 FA 出现在同一个 declaration form 时，由同一次注册写入相同的 `order`、`runtime_context_snapshot`、`source_view` 和 options；不为这一事实建立独立 group 对象。后声明 FA 可引用前面已注册的 FA 作为 local macro。重复 FA declaration 报 `duplicate_local_macro_declaration`。

### 状态不变量

- `local_macros` 的每个条目直接保存不可变 context、闭包与 callable 状态，不保留 `group_id` 或第二份声明表。
- 同一次 declaration 写入相同的 `order`；它只提供稳定扫描和累计编译顺序，不参与最终宏环境裁剪，也不得由 map 遍历顺序推断。
- `frozen_forms` 永远保存原始源码 form；任何环境下的展开都从该原始 form 开始。
- `canonical_expanded_forms` 是所有环境比对通过后的唯一编译候选；`compiled_forms` 是当前 `<Module>__local_macro` 已提交 generation 的完整累计源码。
- `local_macro_expanded_ids` 只记录已作为 local macro closure 完成 canonical 展开验证的 form ID；它与 `frozen_forms`、`retained_form_ids` 是不同集合。

## 注册规则

### 声明单位与 FA

`-local_macro([foo/1, bar/2])` 一次注册两个 FA 条目。`foo/1` 和 `bar/2` 分别拥有宏头、闭包根和 callable 状态，同时保存相同的扫描顺序、MacroRuntimeContext 快照和 options。

处理 declaration 时必须先检查全部 FA 是否已经存在；任一重复均以 `duplicate_local_macro_declaration` 失败，不能部分注册。

注册成功后，条目中的 `order` 与 declaration 当时的 context 快照不可变。逐 FA `status` 和 generation 可以随累计编译推进而更新，但不得以新的环境重写旧声明快照。

### 注册过程

```text
register_and_preexpand(LocalMacroAttribute, ClosureSourceView, DeclarationRuntimeContext, State):
  1. 校验 declaration options、FA 格式与全部 FA 的唯一性
  2. 对每个 FA 计算静态函数闭包
  3. 校验 extra_functions 存在，并把 internal_function 引用解析为声明点宏绑定
  4. 将闭包原始 function/spec forms 写入 frozen_forms
  5. 使用 declaration 前的完整 MacroRuntimeContext；该环境按构造时点自然不含本次新增 FA
  6. 从 MacroEnv 移除 internal macro keys，按 alias 来源改写本地调用，并调用统一引用解析取得实际引用的先前 local FA
  7. 为各 FA 建立带相同 order 和 context 快照的独立条目
  8. 将各 FA status 设为 pending，更新依赖图
  9. 对依赖已就绪的 frozen forms 调用统一 ExpansionValidator
```

这是原子操作：任一步失败都不得留下已注册 FA、已冻结 form 或部分 retain 元数据。

### 闭包发现

闭包计算的根是该 FA 的宏头函数。静态本地调用递归纳入 helper，`extra_functions` 作为显式补充。`extra_functions` 引用不存在的函数报 `invalid_extra_functions`。`internal_function` 不构造函数闭包；它选择 declaration 之前当前 MacroEnv 中的宏调用，并把这些调用固化为普通函数调用。

这里的静态调用只包括 AST 中形如 `helper(Args...)` 的直接本地 call。`fun helper/1`、
动态函数值、`apply/3` 或其他间接引用不会自动形成闭包边；需要参与冻结、编译和 retain
的间接 helper 必须由 `extra_functions` 显式加入。

同一个 function form 可出现在任意多个闭包中。闭包成员资格不表示宏依赖：先声明 A 的闭包包含后声明 B 时，B 只是 A 的 helper 成员，除非 B 自身的编译环境实际需要 A 作为宏。

闭包分析建立的是函数依赖图，而不是 declaration 依赖图：

```text
Macro FA ──静态本地调用──> Helper FA ──静态本地调用──> ...
          └─extra_functions──> Explicit Helper FA
```

图的可达函数集合就是闭包。函数可从多个根可达，因而其原始 form 可以同时属于多个闭包。闭包边不因为目标函数后来被声明为 local macro 而自动转为宏调用边。

### `internal_function` 的作用范围

`internal_function` 是宏定义闭包的宏环境策略：被选中的 key 必须在 declaration
之前的当前 MacroEnv 中存在。列表项 `{F,A}` 解析本地调用 key，
`{M,F,A}` 是 attribute term 中远程 `M:F/A` 的表示并解析远程调用 key。普通函数即使
存在，也不能满足该校验；未找到的项报 `undefined_internal_functions`。`false` 选择
空集，`true` 选择声明点当前可见的全部宏映射。

如果 `{F,A}` 命中 imported macro 的现有 `use_macro` `alias`，注册时保存
`{F,A} -> {M,F,A}` 来源。展开 frozen form 前先把 `F(Args)` 改写为普通远程
`M:F(Args)`，再从有效 MacroEnv 同时移除 alias key 与原始远程 key，防止改写后的调用
再次匹配宏。若命中先前 local macro，则只移除其宏 key，AST 中的本地调用保持不变，
由累计 local macro module 作为普通函数解析。直接 `{M,F,A}` 调用无需改写，只移除远程
宏 key。

不同 declaration 可以有不同的 `internal_function` 列表。没有共享 helper form 时名单
差异不是错误；两个 declaration 共享 helper form 且为其提供不兼容的 internal macro
环境时，报 `conflicting_internal_function_policy`。某个 local macro 自身的根 form 不参与
这种提前策略比较，因为其唯一展开基准来自定义点快照；后续复用仍由 canonical
fingerprint/result 校验保证一致。

`internal_function` 的解析、共享闭包冲突校验和有效环境裁剪全部属于
`astranaut_local_macro`。通用展开器不会读取该 option。

### 同一 declaration 的有效环境

同一个 declaration 的全部 members 使用同一个环境：

```text
DeclarationMacroEnv
  = PreDeclarationEffectiveMacroMap
  - InternalMacroKeys(Declaration)
```

对于 `-local_macro([foo/1, bar/1])`，declaration 前的环境按定义尚未注册
`foo/1` 与 `bar/1`，因此无需再执行“排除当前 members”的操作。`bar/1` 对
`foo/1` 的调用以及反向调用都是普通 Erlang 本地调用。form 扫描使用 declaration 前
候选环境识别实际 local macro 引用，并把结果保存为 `referenced_local_macros` 白名单。

declaration 预展开和最终展开都保留各自时点的 external macros，但 local-macro 部分只保留
该闭包白名单中的 FA；两条路径还必须应用同一 internal key 过滤和 alias-to-remote
改写。因而 self、同声明成员、internal ordinary calls 和后声明 local macros 都不会进入
宏匹配。目标本身是 local macro 宏头时，
必须只使用其自身 declaration 条目的白名单，避免后声明闭包把该宏重新加入自身环境；
仅作为 helper 且同时属于多个 local 闭包时，允许集合才取这些闭包扫描结果的并集。
不属于任何 local 闭包的普通最终 function 仍使用完整 FinalLocalEnv。

spec form 不执行 function-body 展开，但与对应 function 使用同一个 declaration
环境指纹参与冻结和一致性记录。

### 源码与环境视图

注册时的源码视图是当前已 materialize 的 forms 流：已 pass 的输出前缀加上当前尚未 pass 的队列。此前 splice 生成但尚未处理的 form 已经在该队列中，不是额外的第三类输入；未来尚未执行的 attribute splice 输出不属于源码视图。该视图只用于寻找函数与闭包。

环境快照严格取 declaration 前已经 pass 的完整 effective macro map、`macro_options` 和 inject forms；`referenced_local_macros` 另行记录 form 扫描得出的 local 允许集合。展开时再以该白名单过滤快照中的 local 部分。后续环境更新不会回溯改变已记录的 declaration 快照或白名单。

`use_macro` 的同名 option 采用后者覆盖前者，未提及的 option 保留；不同定义占用同一 key 时仍须通过统一 `force_override` 规则。该时点已生效的 checked effective map 直接写入每个声明条目的 runtime context snapshot。

local macro 的上下文特殊规则是：预展开其 function forms 时，`MacroRuntimeContext` 仅取 `-local_macro` declaration 之前的状态。宏名称、alias、调用参数、options 和 `inject_attrs` 实际值必须来自同一时点。generation 编译不读取该 context，只读取已经确认的 canonical expanded forms。

它必须与只用于结构分析的源码视图区分：

```text
DeclarationMacroRuntimeContext = {
  effective_macro_map = macro environment derived before declaration,
  macro_options       = options effective before declaration,
  inject_forms        = PassedFormsBeforeDeclaration
}
ClosureSourceView = PassedForms ++ CurrentAndRemainingQueue
```

后续任何 `NeedCallable` 都不能用触发点 context 覆盖 declaration 预展开 context。若所需 canonical form 尚未就绪，必须回到 ExpansionValidator 使用冻结的 declaration context；GenerationCompiler 本身不执行该展开。

执行 attribute 宏不属于 local-macro 的特殊规则。所有 attribute 宏都使用调用点 `MacroRuntimeContext`；选中尚不可调用的 local target 时只产生通用 `NeedCallable`。预展开、attribute、retain 和 Step 2 共用同一 dependency scheduler。

闭包实际引用的 local macro 可以是此前已注册但尚未编译的 FA。统一引用解析
基于候选宏描述而不是当前已加载代码，因此它们仍记录在 snapshot 中，并由
后续最小累计编译计划保证在调用点可用；不应因为尚未加载就从引用集合省略。

## 注册、源码视图与冻结

扫描遇到 `-local_macro(...)` 时：

1. 使用该时刻的完整 `closure_source_view` 计算闭包；它是已 pass 的输出前缀加上当前尚未 pass 的队列，不包含未来尚未 materialize 的 splice 输出。
2. 以 declaration 前的 effective map、options 和 passed forms 构造一个共同 `MacroRuntimeContext`。
3. 直接使用该 declaration 前的环境；它自然不含本次新增 members。解析并移除 `internal_function` macro keys，alias 本地调用同时按保存的来源改写为普通远程调用。
4. 将闭包的原始 function/spec forms 保存到 `frozen_forms`；任何展开都从该输入开始。
5. 建立带相同 order/context 的逐 FA 生命周期条目，更新实际 local macro 依赖。
6. 对依赖已就绪的 forms 立即预展开；需要尚不可调用 local macro 时产生 `NeedCallable`。

后续属性 splice 不得改写 `frozen_forms` 中的 form ID，否则报 `illegal_locked_form_mutation`。

冻结不等于从统一 scan 输出删除 form，也不等于自动跳过最终展开；它只锁定所有环境展开共同使用的原始输入。

### Form ID 与冻结保护

function form 的 ID 是 `{function, Name, Arity}`，spec form 的 ID 是 `{spec, Name, Arity}`。对 splice 输出进行保护时，只比较 function/spec ID；其他 attribute 或普通 form 不因名称相近而触发冻结错误。

冻结保护覆盖整个统一 scan：无论输出来自外部属性宏还是本地属性宏，只要 splice 输出试图生成相同的冻结 ID，就记录 `illegal_locked_form_mutation` 错误并丢弃该 attribute 的展开结果，不将该 splice 插入队列。扫描随后继续 pass 后续 forms；该规则不终止整轮扫描。它保护的是原始快照可重复展开的前提，不限制宏输出生成无关的新 function 或 spec。

## 多环境展开与缓存

每次展开从 `frozen_forms` 中的原始 form 开始。每个 FormId 保存一个 `ExpansionRecord`：最后一次环境/结果、canonical result，以及按 fingerprint 的结果缓存。fingerprint 包含裁剪后的有效宏映射与可调用 local 版本、macro options 和 `inject_forms`。

同一 form 可属于多个闭包：

- 与最后一次环境相同：直接复用最后一次结果。
- 环境不同：从原始 form 展开，与最后一次已接受结果比较；一致则更新 record，不一致则报 `conflicting_local_macro_closure_environment`。

### 环境指纹

`EnvFingerprint` 必须反映 `MacroRuntimeContext` 的所有可观察输入，不能只比较 external map。不得使用包含 remaining queue 的 `closure_source_view` 代替 `inject_forms`。FormId 已是 record 的外层 key；同 declaration 的成员共享快照，不能另造 TargetFA 维度改变声明环境。

### 冲突检测时机

当某个 form 首次进入第二个不同环境时立即比较；scan 收尾覆盖所有尚未预展开的 local closure，最终 retain 与 Step 2 function 则使用 `FinalMacroRuntimeContext` 做最后一次比较。环境不同本身不是错误，只有展开 AST 结果不同才失败。

比较结果前应先规范化为同一抽象 form 表示，并保留错误、warning、formatter 和 file/position 上下文。若任一环境展开失败，失败按该环境的宏展开错误传播；不能把失败当作“结果不同”的普通冲突。

### 共享 form 的累计模块表示

同一 FormId 的多个环境结果一致时，`canonical_expanded_forms` 只保存一份 ExpandedForm。GenerationCompiler 仅从该映射取输入；若结果不同，则不得提交新的累计模块。

## 最小累计编译

任何阶段的 `NeedCallable` 只触发计划，不决定计划顺序。计划按 declaration 顺序和实际宏依赖构造：

- 若 B 的闭包需要先声明 A 作为宏，首次调用 B 时先编译累计 `{A}`，再编译 `{A,B}`。
- 若 B 不需要 A，可直接编译累计 `{A,B}`。
- 每次编译包含此前已编译闭包和本次新增闭包；输入只取 canonical expanded forms。
- scan 收尾重新构造包含全部 local macro 的最终累计模块。

### 计划算法

设 `Requested` 为任意 `NeedCallable` 所需 FA，`Prefix` 为扫描至当前时刻已注册、且 declaration 顺序不晚于 Requested 的 FA。计划从 Requested 的实际 local macro 引用开始向前查找：

1. 找出 Requested 的闭包在预展开时真正需要作为宏调用的先声明 FA。
2. 对每个尚不可调用的先声明 FA，递归处理其更早的宏依赖；若 canonical form 未就绪，先调用 ExpansionValidator。
3. 以 declaration 顺序形成必要的累计边界；只有“下一个闭包预展开需要当前模块中已可调用宏”时才插入中间编译。
4. 每个边界都构造“此前已提交 forms + 本边界新增 forms”的完整模块。

因此，A 在 B 前但 B 不引用 A 时，A 与 B 可以一起编译；若 B 预展开调用 A，则必须先加载 A，再预展开并加载 A+B。闭包成员关系、普通 Erlang 调用和被 internalize 后不再作为宏匹配的调用均不单独产生该累计边界。

boundary identity 只取按 declaration 顺序排列的累计 local macro members。没有新增
local macro 就没有新 identity，任何阶段再次请求同一累计 members 都直接复用，不能因
MacroRuntimeContext、展开触发位置、注入 forms 或 compile options 再编译。特别地，连续
声明独立的 A、B 时，B 的注册和预展开不产生 `{A}` generation；首次需要可调用或收尾时
直接编译 `{A,B}`。

### 编译输入与输出

每一代模块的 forms 由下列部分组成：

- module attribute，模块名为 `<Module>__local_macro`；
- 当前累计闭包中已经确认的 canonical function/spec forms；
- local macro 所需的 export forms；
- 编译所需的非函数模块 forms，但不复制原模块的普通 export 声明。

canonical 输入只包含累计闭包相关的 function/spec FormIds；非函数支持 forms 从该
boundary 最后一个 declaration 冻结的 source view 选择，因此 attribute/finalize 的触发点
不会改变编译源码。普通无关函数和 macro 控制 attributes 不进入 local module。

编译前对 canonical forms 执行现有排序和合法性处理。compile options 在一次 parse
transform 内保持稳定，只作为 compiler 参数传入，不参与 boundary identity。编译成功、
加载成功后，才把本代输入写入 `compiled_forms`，记录累计 members boundary key，更新相关
FA 的 `status = compiled` 和 generation。编译失败不能覆盖上一代可调用模块。

### 计划与执行的分离

编译计划是纯数据：它指出须先可调用的 FA、规范化累计 boundary 及其所需
canonical FormIds。ExpansionValidator 负责在进入 compiler 前准备缺失 forms；
GenerationCompiler 不接收 declaration environment 或 expansion request，只负责
生成累计 forms、编译、安全加载并提交 generation。

若准备或编译失败，当前已加载的 local macro 模块和 `compiled_forms` 必须保持不变。ExpansionRecord 只提交完整成功的单次展开；generation 只有在全部 canonical forms 和 Erlang 编译均成功后提交。

### 最终累计编译

scan 收尾不是“只编译 pending 项”。它按注册表的 declaration 顺序重建包含全部 local macro 的累计模块，并复用缓存。该最终版本是最终函数体展开唯一可见的 local macro 模块版本。

若某个 FA 已在中间阶段编译，收尾仍会将它纳入最终模块；canonical forms 不因编译阶段或触发者不同而重新展开。最终模块只集中所有已确认结果，不重新解释 declaration environment。

所有累计版本均覆盖加载同一个 `<Module>__local_macro`。编译成功后才换码；加载前清理 old code。`code:soft_purge/1` 返回 `false` 时以 `local_macro_module_in_use` 失败，禁止 `code:purge/1`。调用使用完全限定调用或 `apply/3`，不得跨重载缓存 fun；换码过程以模块级互斥锁串行。

模块级互斥范围必须覆盖“读取当前 generation → 计算累计 forms → 编译 → soft purge → load → 提交 State”，避免并行编译相互覆盖。`code:load_binary/3` 成功后，后续完全限定调用进入 current code；旧代码只可由尚在执行它的进程使用。

安全加载的失败不是可忽略的性能问题。若 `soft_purge/1` 返回 false，继续加载可能导致旧代码进程被强制清理或加载失败；工作流必须停止并报告 `local_macro_module_in_use`，把当前 generation 保持为可用状态。

## Retain 与最终跳过集合

`local_macro_retain`、`export_macro` 和 `export` 均先解析为 retain 根。`retained_form_ids` 是所有 retain 根的完整闭包及对应 spec forms 的并集。

`export_macro` 在本工作流中仅提供隐式 retain 根；它不改变统一 scan 的宏环境。`export` 同样只作为 retain 信息来源。三类 retain 根没有优先级，任一命中即可使其闭包进入保留集合。

显式 `local_macro_retain` 的 FA 若在模块中不存在，报告
`undefined_local_macro_retain`；若函数存在但不是任何 frozen closure 的成员，则没有
额外生命周期效果，并报告 `ineffective_local_macro_retain`。这两种 warning 只针对
显式 `local_macro_retain`，不针对作为隐式 retain roots 的普通
`export`/`export_macro`。
retain roots 可在其闭包 declaration 之后出现，因此只在收尾阶段从完整 retain 根集合
计算 `retained_form_ids` 后才能准确诊断。

冻结 form 保存所有环境展开共用的原始输入，不直接决定最终跳过。最终跳过集合为：

```text
FinalSkipIds = local_macro_expanded_ids - retained_form_ids
```

保留的冻结 form 与普通 Step 2 function 一样使用 `FinalMacroRuntimeContext` 进入统一 ExpansionValidator。未 retain 的 local-only forms 进入 `FinalSkipIds`。

### 最终收尾顺序

```text
1. 完成全部 declaration 预展开与多环境结果比对
2. 以 canonical forms 构造并提交最终累计 generation
3. 收集 local_macro_retain / export / export_macro 的 retain roots
4. 计算 retained_form_ids（根的闭包及 spec forms）
5. 将 retained frozen forms 与普通 function 目标一并交给 FinalMacroRuntimeContext 展开验证
6. FinalSkipIds = local_macro_expanded_ids - retained_form_ids
7. 返回 FinalLocalEnv 与 FinalSkipIds 给统一扫描流程的最终展开阶段
```

所有 retained functions（包括 local macro 宏头）都参与第 5 步。属于 local 闭包的目标先按 `referenced_local_macros` 白名单过滤 FinalLocalEnv，并重放与 declaration 相同的 internal key 移除及 alias-to-remote 改写；不属于任何 local 闭包的普通 Step 2 function 使用完整 FinalLocalEnv。internal bindings 进入 input fingerprint，避免不同 alias 来源错误复用缓存。若所得 final fingerprint 与最后一次 local 展开环境相同则复用；否则从原始 form 展开，并与最后一次已接受 local result 比较。

`FinalSkipIds` 的计算不删除原始 forms；它是交给最终函数体遍历器的过滤条件。这样同一 forms 列表可继续用于 record、attribute injection 和诊断，而 local macro 已预展开且未 retain 的 functions 不会被再次递归展开。

## 错误模型

- `duplicate_local_macro_declaration`：同一 FA 被多次注册。
- `invalid_extra_functions`：显式 helper 不存在或无效。
- `undefined_internal_functions`：`internal_function` 引用在 declaration 位点没有对应宏。
- `undefined_local_macro_retain`（warning）：显式 retain FA 在模块中不存在。
- `ineffective_local_macro_retain`（warning）：显式 retain FA 不属于任何冻结闭包。
- `conflicting_internal_function_policy`：共享 helper form 的 internal macro 环境不兼容。
- `conflicting_local_macro_closure_environment`：同一原始 form 在不同环境下的展开结果不同，或 retain form 的最终环境比对不一致。
- `illegal_locked_form_mutation`：属性 splice 改写 frozen 原始 form。
- `local_macro_module_in_use`：安全换码时 old code 仍被引用。

## 建议接口

```erlang
register_and_preexpand(Declaration, SourceView, RuntimeContext, MacroOps, State).
expand_and_validate(FormIds, RuntimeContext, MacroOps, State).
need_callable(FA, WorkflowContext, MacroOps, State).
compile_boundary(BoundaryKey, State).
finalize(RetainRoots, WorkflowContext, MacroOps, State).
```

`register_and_preexpand` 负责 group 快照、闭包发现、冻结、依赖记录及就绪 forms
的预展开；`need_callable` 是所有阶段共享的最小累计计划入口；`finalize` 返回
`FinalLocalEnv`、RetainIds 与 `FinalSkipIds`。`MacroOps` 至少提供
`resolve_local_references` 和 `expand_and_validate`，两者都实现于
`astranaut_macro`，并以 `astranaut_return` 结果保留统一错误上下文。

`astranaut_local_macro` 自己调度 canonical boundary。缺失 canonical form 时先通过
MacroOps 进入 ExpansionValidator；compiler 不接触 EnvFingerprint。该模块不依赖
统一扫描队列或 traverse monad；扫描器在注册/预展开、NeedCallable 和收尾边界
桥接 `astranaut_return`。
