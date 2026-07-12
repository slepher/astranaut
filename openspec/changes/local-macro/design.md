# Local Macro 设计

## 术语：本地宏闭包

本文的“闭包”不是函数式语言中捕获词法环境的 closure。它是一个静态函数集合：以 local macro 宏头函数为根，包含该函数及通过静态本地调用、`extra_functions` 等规则递归引用到的函数；这些函数对应的 `-spec` forms 也属于该闭包。

## 职责边界

`astranaut_local_macro` 管理注册表、闭包、冻结、缓存、编译计划、retain 和最终跳过集合。统一扫描器仅调用其注册、确保可调用及收尾接口；扫描和 splice 细节见 [macro-passes-adjusted](../macro-passes-adjusted/design.md)。

local macro function 的展开不使用另一套遍历器。`astranaut_macro` 提供两个通用
能力：按既有宏匹配规则识别 function 闭包实际引用的 local macro，以及在给定
最终 `MacroEnv` 时展开指定 functions。`astranaut_local_macro` 决定候选和有效
环境，但不复制调用匹配、`outer` / `inner`、递归展开或错误上下文语义。

### 模块调用方向

```text
astranaut_macro
  ├─ 统一 scan、环境更新、attribute splice
  ├─ 通用宏引用匹配、function 展开与错误/monad 流
  └─ 调用 astranaut_local_macro
       ├─ 注册/规划/状态转换
       ├─ 闭包、internal policy、有效环境、冻结、缓存、retain、FinalSkipIds
       └─ 累计编译与 local macro 模块加载
```

`astranaut_local_macro` 不拥有 scan 队列，也不直接实现 attribute handler。它通过
注册、确保可调用和收尾结果与 `astranaut_macro` 协作。计划由 local-macro
工作流驱动，但实际引用解析和 function 展开通过调用方提供的 `MacroOps` 执行，
从而复用统一错误上下文，并避免把 traverse monad 或扫描队列耦合进该模块。

### 同构 function 展开接口

local macro 工作流调用的展开操作与最终普通 function pass 使用同一实现：

```text
ExpandFunctions(MacroEnv, Forms, TargetFAs) -> ExpandedForms | Error
```

展开器只解释 `MacroEnv` 中存在的宏，不知道某个目标是否为 local macro，也不
解释 `internal_function`、generation、retain 或 declaration order。

实际 local 引用同样由 `astranaut_macro` 的统一调用匹配能力识别：

```text
ResolveLocalReferences(CandidateLocalEnv, Forms, ClosureFAs) -> ReferencedFAs
```

`astranaut_local_macro` 提供候选环境和闭包，保存返回的 `ReferencedFAs` 并据此
规划累计边界；它不以“静态闭包中包含某 FA”替代真实的宏调用匹配。

## 状态

```text
State = #{
  local_macros => #{FA => #{
    order => ScanSequence,
    env_snapshot => EnvSnapshot,
    closure_ids => [FormId],
    referenced_local_macros => [FA],
    options => Options,
    status => pending | compiled
  }},
  frozen_forms => #{FormId => OriginalForm},
  retain_roots => [FunctionId],
  retained_form_ids => [FormId],
  expanded_forms => #{{FormId, EnvFingerprint} => ExpandedForm},
  compiled_forms => #{FormId => ExpandedForm},
  local_macro_expanded_ids => [FormId],
  generation => non_neg_integer()
}
```

注册表以 function/arity（FA）为 key。多个 FA 出现在同一个 declaration form 时，各自保存相同的扫描顺序和初始 declaration 信息。后声明的 FA 可引用前面已注册的 FA 作为 local macro。重复 FA declaration 报 `duplicate_local_macro_declaration`。

### 状态不变量

- `local_macros` 是整个 scan 期间的唯一注册事实来源；某个 FA 编译成功后只更新其 `status`，不得删除其 declaration 快照。
- `order` 是 declaration 的扫描顺序，不得由 map 遍历顺序推断。
- `frozen_forms` 永远保存原始源码 form；任何环境下的展开都从该原始 form 开始。
- `compiled_forms` 是当前 `<Module>__local_macro` 的完整累计源码，不是只包含最近一次新增闭包的增量。
- `local_macro_expanded_ids` 只记录已用于 local macro 编译的 form ID；它与 `frozen_forms`、`retained_form_ids` 是不同集合。

## 注册规则

### 声明单位与 FA

`-local_macro([foo/1, bar/2])` 在语法上是一个 declaration form，但注册表为 `foo/1` 和 `bar/2` 分别建立条目。二者共享 declaration 的扫描顺序、已 pass 环境快照和 options；每个 FA 仍拥有自己的宏头、闭包成员和编译状态。

处理 declaration 时必须先检查全部 FA 是否已经存在；任一重复均以 `duplicate_local_macro_declaration` 失败，不能部分注册。

注册成功后，FA 条目的 `order` 与 declaration 当时的环境快照不可变。`status` 和 `generation` 可以随累计编译推进而更新，但不得以新的外部环境重写旧 declaration 的环境。

### 注册过程

```text
register(LocalMacroAttribute, SourceView, ExternalEnv, State):
  1. 校验 declaration options、FA 格式与全部 FA 的唯一性
  2. 对每个 FA 计算静态函数闭包
  3. 校验 extra_functions 与 internal_function 策略
  4. 将闭包原始 function/spec forms 写入 frozen_forms
  5. 以已注册 local macro 的候选环境调用统一引用解析，取得闭包实际引用的 FA
  6. 为每个 FA 写入不可变的 order、env_snapshot、closure_ids 和 options
  7. 将 status 设为 pending
```

这是原子操作：任一步失败都不得留下已注册 FA、已冻结 form 或部分 retain 元数据。

### 闭包发现

闭包计算的根是该 FA 的宏头函数。静态本地调用递归纳入 helper，`extra_functions` 作为显式补充；`internal_function` 决定宏定义内部某些调用是否按普通直接调用处理。`extra_functions` 引用不存在的函数仍报 `invalid_extra_functions`。

同一个 function form 可出现在任意多个闭包中。闭包成员资格不表示宏依赖：先声明 A 的闭包包含后声明 B 时，B 只是 A 的 helper 成员，除非 B 自身的编译环境实际需要 A 作为宏。

闭包分析建立的是函数依赖图，而不是 declaration 依赖图：

```text
Macro FA ──静态本地调用──> Helper FA ──静态本地调用──> ...
          └─extra_functions──> Explicit Helper FA
```

图的可达函数集合就是闭包。函数可从多个根可达，因而其原始 form 可以同时属于多个闭包。闭包边不因为目标函数后来被声明为 local macro 而自动转为宏调用边。

### `internal_function` 的作用范围

`internal_function` 是宏定义闭包中的调用策略：被标记的调用在 local macro 定义内部按普通函数直接调用，而不是在预展开阶段作为宏调用展开。它不是声明身份策略，不因函数出现在 `local_macro` 或 `export_macro` attribute 中而自动生效。

不同 declaration 可以有不同的 `internal_function` 列表。只有某个具体函数同时出现在多个闭包中，且一方将其标为 direct-call、另一方没有时，才报 `conflicting_internal_function_policy`。没有共享函数时，名单差异不是错误。

`internal_function` 的解析、共享闭包冲突校验和有效环境裁剪全部属于
`astranaut_local_macro`。通用展开器不会读取该 option。

### 逐目标 function 的同构有效环境

每个冻结 function form 都使用逐目标构造的环境：

```text
EffectiveEnv(Declaration, TargetFA)
  = ExternalSnapshot
  + ReferencedLocalMacros
  - InternalFunctions(Declaration)
  - TargetFA
```

最后的 `- TargetFA` 是环境不变量，不是展开器特判：local macro 自身从不作为
展开其自身 function form 时的宏。因此 `foo/1` 定义中的 `foo/1` 调用自然保留
为累计模块内的普通 Erlang 递归调用。若 B 实际引用先声明的 A，则展开 B 时
A 仍在环境中；若 A 的 form 同时属于 B 的闭包，展开该 A form 时仍按目标 FA
规则移除 A。

spec form 不执行 function-body 展开，但与对应 function 使用同一个 declaration
环境指纹参与冻结、缓存和冲突比较。

### 源码与环境视图

注册时的源码视图是当前已 materialize 的 forms 流：已 pass 的输出前缀加上当前尚未 pass 的队列。此前 splice 生成但尚未处理的 form 已经在该队列中，不是额外的第三类输入；未来尚未执行的 attribute splice 输出不属于源码视图。该视图只用于寻找函数与闭包。

环境快照则严格取 declaration 前已经 pass 的 `import_macro`、`use_macro`、`macro_options`，再加上该闭包实际引用的 local macro。后续环境更新不会回溯改变已记录的 declaration 快照。

`use_macro` 的同名 option 采用后者覆盖前者，未提及的 option 保留；`import_macro` 对同名导入采用后者覆盖。这个合并后的外部环境才是写入 `env_snapshot` 的内容。

闭包实际引用的 local macro 可以是此前已注册但尚未编译的 FA。统一引用解析
基于候选宏描述而不是当前已加载代码，因此它们仍记录在 snapshot 中，并由
后续最小累计编译计划保证在调用点可用；不应因为尚未加载就从引用集合省略。

## 注册、源码视图与冻结

扫描遇到 `-local_macro(...)` 时：

1. 使用该时刻的完整源码视图计算闭包；源码视图为已 pass 的输出前缀加上当前尚未 pass 的队列，不包含未来尚未 materialize 的 splice 输出。
2. 编译环境只快照已 pass 的 ExternalEnv，加上闭包实际引用的 local macro。
3. 将闭包的原始 function/spec forms 保存到 `frozen_forms`；冻结仅表示 local macro 编译使用该原始输入。
4. 以 FA 注册元数据。先声明 A 调用后声明 B 时，B 是 A 闭包成员；即使 B 也是 local macro，也按 helper 的多环境规则处理。

后续属性 splice 不得改写 `frozen_forms` 中的 form ID，否则报 `illegal_locked_form_mutation`。

冻结不等于从统一 scan 输出删除 form，也不等于自动跳过最终展开；它只锁定 local macro 编译所使用的原始输入。

### Form ID 与冻结保护

function form 的 ID 是 `{function, Name, Arity}`，spec form 的 ID 是 `{spec, Name, Arity}`。对 splice 输出进行保护时，只比较 function/spec ID；其他 attribute 或普通 form 不因名称相近而触发冻结错误。

冻结保护覆盖整个统一 scan：无论输出来自外部属性宏还是本地属性宏，只要 splice 输出试图生成相同的冻结 ID，就记录 `illegal_locked_form_mutation` 错误并丢弃该 attribute 的展开结果，不将该 splice 插入队列。扫描随后继续 pass 后续 forms；该规则不终止整轮扫描。它保护的是原始快照可重复展开的前提，不限制宏输出生成无关的新 function 或 spec。

## 多环境展开与缓存

每次展开从 `frozen_forms` 中的原始 form 开始。缓存键为 `{FormId, EnvFingerprint}`；fingerprint 包含外部宏映射、实际引用的 local macro 版本、macro options，以及影响 `inject_attrs` 的 forms 上下文。

同一 form 可属于多个闭包：

- 相同环境直接复用缓存。
- 不同环境分别展开；结果一致则复用，结果不同报 `conflicting_local_macro_closure_environment`。

### 环境指纹

`EnvFingerprint` 必须反映所有可观察的展开输入，不能只比较 ExternalEnv。除外部宏映射、local macro 版本和 options 外，还必须包含影响 `inject_attrs` 的 forms 上下文。这样，同一 form 在不同声明位点或不同累计模块 generation 下不会错误复用缓存。

### 冲突检测时机

当某个 form 首次需要第二个不同环境的结果时立即比较；scan 收尾还必须覆盖全部 local macro 的闭包，确保从未被 attribute 调用的 declaration 也接受相同检查。环境不同本身不是错误，只有展开 AST 结果不同才失败。

比较结果前应先规范化为同一抽象 form 表示，并保留错误、warning、formatter 和 file/position 上下文。若任一环境展开失败，失败按该环境的宏展开错误传播；不能把失败当作“结果不同”的普通冲突。

### 共享 form 的累计模块表示

同一 FormId 的多个环境结果一致时，`compiled_forms` 只保存一份 ExpandedForm。若结果不同则根本不提交新的累计模块。由此确保最终 `<Module>__local_macro` 中一个 function/spec ID 永远只有一个确定的定义。

## 最小累计编译

属性调用只触发计划，不决定计划顺序。计划按 declaration 顺序构造：

- 若 B 的闭包需要先声明 A 作为宏，首次调用 B 时先编译累计 `{A}`，再编译 `{A,B}`。
- 若 B 不需要 A，可直接编译累计 `{A,B}`。
- 每次编译包含此前已编译闭包和本次新增闭包；相同 form/environment 的结果从缓存复用。
- scan 收尾重新构造包含全部 local macro 的最终累计模块。

### 计划算法

设 `Requested` 为当前 attribute 调用所需 FA，`Prefix` 为扫描至当前时刻已注册、且 declaration 顺序不晚于 Requested 的 FA。计划从 Requested 的实际 local macro 引用开始向前查找：

1. 找出 Requested 的闭包在预展开时真正需要作为宏调用的先声明 FA。
2. 对每个尚不可调用的先声明 FA，递归处理其更早的宏依赖。
3. 以 declaration 顺序形成必要的累计边界；只有“下一个闭包预展开需要当前模块中已可调用宏”时才插入中间编译。
4. 每个边界都构造“此前已提交 forms + 本边界新增 forms”的完整模块。

因此，A 在 B 前但 B 不引用 A 时，A 与 B 可以一起编译；若 B 预展开调用 A，则必须先加载 A，再预展开并加载 A+B。闭包成员关系、普通 Erlang 直接调用和 `internal_function` direct-call 均不单独产生该累计边界。

### 编译输入与输出

每一代模块的 forms 由下列部分组成：

- module attribute，模块名为 `<Module>__local_macro`；
- 当前累计闭包的展开后 function/spec forms；
- local macro 所需的 export forms；
- 编译所需的非函数模块 forms，但不复制原模块的普通 export 声明。

编译前对 forms 执行现有的 forms 排序和合法性处理。编译成功、加载成功后，才把本代 ExpandedForm 写入 `compiled_forms`，更新相关 FA 的 `status = compiled` 和 generation。编译失败不能覆盖上一代可调用模块。

### 计划与执行的分离

编译计划是纯数据：它指出须先可调用的 FA、待展开的原始 form/environment
组合以及下一份累计模块的成员。`astranaut_local_macro` 驱动计划，逐目标构造
`EffectiveEnv`，再调用 `astranaut_macro` 提供的同构 function 展开操作；随后
由前者验证、缓存、生成累计 forms、加载并提交 generation。

若计划或展开失败，当前已加载的 local macro 模块和 `compiled_forms` 必须保持不变。只有全部新增 form 展开、比较和 Erlang 编译成功后，才提交新的 generation。

### 最终累计编译

scan 收尾不是“只编译 pending 项”。它按注册表的 declaration 顺序重建包含全部 local macro 的累计模块，并复用缓存。该最终版本是最终函数体展开唯一可见的 local macro 模块版本。

若某个 FA 已在中间阶段编译，收尾仍会将它纳入最终模块；但其 form/environment 缓存命中时不重新展开。最终模块的目的不是改变 declaration 语义，而是把所有已确认结果集中到最终函数体展开可使用的单一模块版本。

所有累计版本均覆盖加载同一个 `<Module>__local_macro`。编译成功后才换码；加载前清理 old code。`code:soft_purge/1` 返回 `false` 时以 `local_macro_module_in_use` 失败，禁止 `code:purge/1`。调用使用完全限定调用或 `apply/3`，不得跨重载缓存 fun；换码过程以模块级互斥锁串行。

模块级互斥范围必须覆盖“读取当前 generation → 计算累计 forms → 编译 → soft purge → load → 提交 State”，避免并行编译相互覆盖。`code:load_binary/3` 成功后，后续完全限定调用进入 current code；旧代码只可由尚在执行它的进程使用。

安全加载的失败不是可忽略的性能问题。若 `soft_purge/1` 返回 false，继续加载可能导致旧代码进程被强制清理或加载失败；工作流必须停止并报告 `local_macro_module_in_use`，把当前 generation 保持为可用状态。

## Retain 与最终跳过集合

`local_macro_retain`、`export_macro` 和 `export` 均先解析为 retain 根。`retained_form_ids` 是所有 retain 根的完整闭包及对应 spec forms 的并集。

`export_macro` 在本工作流中仅提供隐式 retain 根；它不改变统一 scan 的宏环境。`export` 同样只作为 retain 信息来源。三类 retain 根没有优先级，任一命中即可使其闭包进入保留集合。

对非 frozen 函数的 `local_macro_retain` 没有额外效果，也不报错。retain roots 可在其闭包 declaration 之后出现，因此只在收尾阶段从完整 retain 根集合计算 `retained_form_ids`。

冻结 form 保存原始编译输入，不直接决定最终跳过。最终跳过集合为：

```text
FinalSkipIds = local_macro_expanded_ids - retained_form_ids
```

保留的冻结 form 需以最终环境完成比对；比对通过后参与最终递归展开。未 retain 的已展开 form 进入 `FinalSkipIds`，避免重复展开。

### 最终收尾顺序

```text
1. 以全部注册 FA 构造最终累计模块
2. 完成共享 form 的多环境展开比对并提交最终 generation
3. 收集 local_macro_retain / export / export_macro 的 retain roots
4. 计算 retained_form_ids（根的闭包及 spec forms）
5. 对 retained frozen forms 做 declaration 环境与最终环境比对
6. FinalSkipIds = local_macro_expanded_ids - retained_form_ids
7. 返回 FinalLocalEnv 与 FinalSkipIds 给统一扫描流程的最终展开阶段
```

local macro 宏头自身不参与第 5 步的最终环境比对，因为自身递归调用按普通函数调用处理；但若被 retain，仍不在 `FinalSkipIds` 中并参与最终函数体展开。

对于 retain 闭包中的非宏头 helper，最终环境比对比较“其所属 declaration 环境下已经确认的展开结果”与 FinalMacroEnv 下从原始 form 重新展开的结果。若共享 helper 属于多个 declaration，前序多环境比较已保证其各 declaration 结果相同，因此最终环境只需与该确定结果比较一次。

`FinalSkipIds` 的计算不删除原始 forms；它是交给最终函数体遍历器的过滤条件。这样同一 forms 列表可继续用于 record、attribute injection 和诊断，而 local macro 已预展开且未 retain 的 functions 不会被再次递归展开。

## 错误模型

- `duplicate_local_macro_declaration`：同一 FA 被多次注册。
- `invalid_extra_functions`：显式 helper 不存在或无效。
- `conflicting_internal_function_policy`：共享闭包函数的 direct-call 策略不一致。
- `conflicting_local_macro_closure_environment`：同一原始 form 在不同环境下的展开结果不同，或 retain form 的最终环境比对不一致。
- `illegal_locked_form_mutation`：属性 splice 改写 frozen 原始 form。
- `local_macro_module_in_use`：安全换码时 old code 仍被引用。

## 建议接口

```erlang
register(FA, Options, SourceView, ExternalEnv, CandidateLocalEnv, MacroOps, State).
ensure_available(FA, CompileContext, MacroOps, State).
compile_plan(NeededFA, State).
finalize(RetainRoots, FinalContext, MacroOps, State).
```

`register` 负责源码快照、闭包发现和冻结，并通过 `MacroOps` 的统一引用解析
记录实际 local 依赖；`ensure_available` 在首次 attribute 调用时执行最小累计
计划；`finalize` 返回 `FinalLocalEnv` 与 `FinalSkipIds`。`MacroOps` 至少提供
`resolve_local_references` 和 `expand_functions`，两者都实现于
`astranaut_macro`，并以 `astranaut_return` 结果保留统一错误上下文。

`astranaut_local_macro` 自己执行 `compile_plan/2`，对每个
`{FormId, EnvFingerprint}` 先查缓存，再以逐目标 `EffectiveEnv` 调用
`MacroOps.expand_functions`。它不依赖统一扫描器或 traverse monad；扫描器只在
注册、按需确保可调用和收尾三个边界桥接 `astranaut_return`。
