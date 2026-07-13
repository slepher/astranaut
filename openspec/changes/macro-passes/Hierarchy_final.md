# Macro Passes 最终处理层级

> 本文先以 [`MacroPassesHierarchy.md`](MacroPassesHierarchy.md) 的独立设计为基线，再对照当前 `astranaut_macro.erl`、`astranaut_local_macro.erl` 与 `astranaut.erl` 实现。最终方案吸收实现中已验证的工程细节，同时保留规范要求的源码顺序语义。

## 1. 对比结论

当前实现的主干结构与独立设计一致：统一 attribute scan-and-splice、local-macro 收尾、最终 function pass 已经形成清晰的两阶段模型；队列重扫、passed/remaining 分离、宏 state 隔离、retain/skip 处理以及共享 function 展开也都落在正确边界。

最终方案需要保留实现中的三项细化：

1. `prepare_exports` 位于扫描结束与 local finalize 之间。
2. retain forms 在 finalize 中按最终环境展开并物化，随后以 `PreparedFunctionIds` 阻止 function pass 二次展开。
3. scan handler 失败时由 splice 驱动层继续处理后续 forms，从而保留诊断累计能力。

同时有三个应在后续实现中收敛的差距：

1. **local macro function-form 编译上下文错误混入了闭包源码视图。** 当前 declaration-time `env_snapshot` 已正确冻结宏名称、alias、调用参数和 `inject_attrs` 配置，但 frozen forms 展开时把 `passed_forms + remaining queue` 的完整 `SourceView` 作为 `InjectForms`，使声明后的原始 attribute 可能进入编译上下文。最终设计要求整个 local function-form 编译上下文仅由 declaration 前 `passed_forms` 决定；完整 source view 只参与闭包发现。
2. **跨 External/Local 宏表的覆盖顺序不完全由源码顺序决定。** 当前 `scan_attribute/2` 使用 `maps:merge(ExternalMacroMap, ScanLocalMap)`，local 固定覆盖 external；最终又以 `merge_macro_maps(External, Local)` 合并。因此“local 声明后又出现带 `force_override` 的 import/use”并不能自然表达后声明 external 生效，冲突也可能延迟到 finalize 才报告。最终设计要求有效宏环境在每个声明位置按统一冲突规则事务性更新，独立的 external/local 表只用于所有权和生命周期记录。
3. **`__original__` 整理对 spec 的契约与测试证据不足。** 当前 splice 层会标记生成 spec，但重命名实现只显式改写 function 和 call；现有单测证明了 function 的局部位置与重命名，没有证明关联 spec 在冲突时应保留、归属生成 function，还是随原 function 重命名。最终设计把它定义成显式的局部 merge policy，并要求补足 spec 场景测试。

## 2. 独立设计与当前实现对照

| 设计契约 | 当前实现证据 | 结论 / 最终取舍 |
|---|---|---|
| 两阶段模型 | `run_attribute_pass/5`、`run_function_macro_pass/2` | 一致；finalize 是 attribute phase 收尾，不是第三个 pass。 |
| 单向 scan-and-splice | `scan_attribute_forms/5` 调用 `astranaut:map_forms_splice/3` | 一致；`splice` 插到队首，已处理项不回扫。 |
| passed 与 remaining 分离 | `passed_forms`、`remaining_forms`、`queue_state => true` | 一致；最终方案沿用双视图。 |
| import/use 消费，options 保留 | `scan_env_form/2` | 一致；consume 用 `{splice, []}` 表达。 |
| local declaration 原位注册 | `scan_local_macro/2`、`register_local_declaration/8` | 一致；实现先记入 passed，扫描后由 `drop_local_declarations` 清理。最终方案允许这一规范化步骤，但必须保证注册快照已完成。 |
| 未就绪 local 属性宏按需编译 | `ensure_local_attribute_macro/2` | 一致；在原调用位置调用 `ensure_available` 后继续展开。 |
| attribute injection 只看历史 | `inject_macro_attributes(..., passed_forms(State))` | 一致。 |
| local macro forms 使用声明点注入快照 | request 保存完整 declaration `SourceView`，`expand_request_form/4` 将它作为 `InjectForms` | 不一致；必须拆分 `closure_source_view` 与 `inject_forms_snapshot`。 |
| 用户宏 state 隔离 | `invoke_macro_function/1` 使用 `scoped_state/2` | 一致；保留外层 Attr 与诊断管线。 |
| return/traverse 正确桥接 | `astranaut:traverse_return/1` 用于校验与 local workflow | 一致。 |
| 局部生成顺序与最小整理 | `map_forms_splice_reorder/1` | function 主路径一致；spec 契约需明确和补测。 |
| finalize 后过滤 local env | `compiled_local_macro_map/2` | 一致；未编译 local FA 不进入 function pass。 |
| retain 物化与 skip | `astranaut_local_macro:finalize/4`、`materialize_forms/2`、`remove_final_skip_forms/2` | 实现比独立方案更具体；全部吸收。 |
| 避免 retain form 二次展开 | `prepared_function_ids` 参与 `find_function_macro_callers` | 独立方案遗漏；吸收到最终方案。 |
| function/local 共用展开器 | `expand_function/4`、`expand_functions/3` | 一致；local 策略由调用方构造环境。 |
| local 引用复用调用匹配 | `resolve_local_references/2`、`call_find_macro` | 一致。 |
| 宏 key 按源码顺序冲突/覆盖 | 单表内 `merge_macro_maps_pure/2`，跨表使用固定 merge 顺序 | 部分一致；最终方案采用统一的有效环境更新规则。 |
| 失败后累计兄弟诊断 | `map_forms_splice_loop` 的 `catch_on_error`；`recover_macro_call/2` | 一致；区分 form 级失败恢复和 macro-call 级原节点恢复。 |

## 3. 最终层级

```text
Module Macro Pipeline
├─ 1. Attribute Phase
│  ├─ 1.1 Initialize
│  │  ├─ EffectiveMacroEnv
│  │  ├─ ExternalRegistry
│  │  ├─ LocalMacroState / LocalRegistry
│  │  ├─ PassedForms
│  │  └─ Queue
│  ├─ 1.2 Forward Scan-and-Splice
│  │  ├─ Publish Remaining Source View
│  │  ├─ Dispatch Current Form
│  │  │  ├─ import_macro / use_macro
│  │  │  ├─ macro_options
│  │  │  ├─ local_macro declaration
│  │  │  │  └─ Freeze LocalCompileContext from pre-declaration PassedForms
│  │  │  ├─ generic attribute macro runtime (external / local)
│  │  │  │  ├─ Resolve with CallSiteMacroEnv
│  │  │  │  ├─ If selected local macro is unavailable
│  │  │  │  │  └─ Ensure Local Macro Compiled
│  │  │  │  │     ├─ Expand frozen function forms with LocalCompileContext
│  │  │  │  │     └─ Compile / load cumulative local macro module
│  │  │  │  ├─ Return to generic runtime path
│  │  │  │  ├─ Build call arguments from call-site configuration
│  │  │  │  ├─ Inject CallSitePassedForms
│  │  │  │  └─ Invoke compiled macro
│  │  │  └─ ordinary form
│  │  ├─ Keep / Consume / Splice
│  │  └─ Local __original__/spec Merge Policy
│  ├─ 1.3 Scan Normalization
│  │  ├─ Drop already-registered local declarations
│  │  └─ Prepare exports
│  ├─ 1.4 Local-Macro Finalization
│  │  ├─ Build / execute final generation
│  │  ├─ Compute FinalLocalEnv
│  │  ├─ Expand and verify retained forms
│  │  └─ Compute FinalSkipIds
│  ├─ 1.5 Materialize Function Input
│  │  ├─ Replace retained forms
│  │  ├─ Record PreparedFunctionIds
│  │  ├─ Remove FinalSkipIds
│  │  ├─ Build final checked macro environment
│  │  └─ Find remaining macro callers
│  └─ 1.6 Sort Once for Erlang Compiler
└─ 2. Function Phase
   ├─ Exclude skipped and prepared functions
   ├─ Expand selected functions through shared core
   ├─ Preserve outer / inner / max_depth semantics
   └─ Return without another form sort
```

## 4. 最终状态模型

逻辑扫描状态定义为：

```text
ScanContext = {
  effective_macro_env,       % 当前调用点唯一可信的执行映射
  external_registry,         % 外部来源、模块和 options 元数据
  local_registry,            % local macro 描述到执行宏记录的映射
  local_macro_state,         % 冻结、generation、缓存、retain 等不透明状态
  global_macro_opts,
  passed_forms,              % 正序语义，具体可反向存储
  remaining_forms,
  scan_local_declarations,
  diagnostics
}
```

每个成功注册的 local declaration 还必须保存不可变的 function-form 编译上下文，并与结构源码视图区分：

```text
LocalCompileContext = {
  env_snapshot,           % 从 declaration 前 passed forms 得到的名称、alias、参数和 options
  inject_forms_snapshot   % 同一份 declaration 前 passed forms
}
ClosureSourceView          % passed forms + 当前及 remaining queue；仅结构分析
```

`env_snapshot` 与 `inject_forms_snapshot` 是同一个 `LocalCompileContext` 的实现分解，不代表两个宏上下文。`ClosureSourceView` 不是宏上下文。按需编译的触发点只能提供累计模块的物化/加载信息，不能改写 request 的 declaration-time 编译上下文。

`effective_macro_env` 是最终方案相对当前实现最重要的收敛点。ExternalRegistry 和 LocalRegistry 可以继续分开，但每次 import/use/local declaration 都必须通过同一个更新操作修改有效环境：

```text
update_effective_env(CurrentEnv, IncomingEntries, SourcePosition)
  -> UpdatedEnv | macro_override
```

更新规则是：

- key 不存在：加入；
- 定义完全相同：幂等；
- 定义不同且 incoming 没有 `force_override`：在当前声明位置报 `macro_override`；
- 定义不同且 incoming 有 `force_override`：incoming 覆盖 existing。

这样 local 与 external 的先后顺序都由源码位置决定，不依赖最终 `maps:merge/2` 的参数顺序。finalize 返回的 `FinalLocalEnv` 只负责过滤 local 可调用性；它不得重新解释已经决定的覆盖顺序。过滤后应重新验证最终映射中引用的 local FA 均可执行。

## 5. Forward Scan-and-Splice

### 5.1 调度循环

```text
scan(Queue, Context, Output):
  Queue = []
    -> {Output, Context}

  Queue = [Form | Rest]
    -> Context1 = Context#{remaining_forms => [Form | Rest]}
    -> Decision = handle(Form, Context1)
    -> case Decision of
         keep(Form1, Context2)
           -> scan(Rest, note_passed(Form1, Context2), Output ++ [Form1])
         consume(Context2)
           -> scan(Rest, Context2, Output)
         splice(NewForms, Context2)
           -> scan(NewForms ++ Rest, Context2, Output)
       end
```

实现可以继续用 traverse monad 和反向 accumulator，以上只是语义模型。`remaining_forms` 必须包含当前 form，以便 local declaration snapshot 与按需编译获得精确 source view。

### 5.2 Form 处理表

| Form | 状态更新 | 调度结果 |
|---|---|---|
| `import_macro` | 解析外部定义并更新 external registry、effective env | consume |
| `use_macro` | 基于已导入定义选择/alias，合并逐宏 options，更新 effective env | consume |
| `macro_options` | 后值覆盖同名全局 option | keep，并加入 passed |
| `local_macro` | 校验、冻结 source view、注册 state、构造 local entries、更新 effective env | keep 到扫描结束；随后 normalization 删除 declaration |
| 可执行 attribute macro | 注入 passed attributes，在私有 state 中执行并校验 | splice generated forms |
| 已注册但未就绪 local attribute | `ensure_available`，成功后在同一位置执行 | splice generated forms |
| 要求执行但无法执行的 macro attribute | 当前点诊断一次 | keep original |
| 普通 attribute/form | 无 | keep original |

local declaration 的校验应产生一次语义结果并同时供注册与映射构造使用。若现有 traverse 事务模型要求“无诊断的准备检查 + 有诊断的正式校验”两步实现，应封装为单一 gateway 行为，避免未来两份校验规则漂移。

### 5.3 Injection 与 source view

必须区分 local function-form 编译输入、结构源码视图和所有 attribute 共用的运行期视图：

```text
LocalCompileContext = {
  MacroEnv    = derived from DeclarationPassedForms,
  InjectForms = DeclarationPassedForms
}
LocalClosureSourceView = DeclarationPassedForms ++ CurrentAndRemainingForms
AttributeRuntimeView   = CallSiteMacroEnv + CallSitePassedForms
```

- 属性宏不能看当前及未来 attribute。
- local macro frozen forms 使用 declaration-time MacroEnv；其中 `use_macro` 等确定的名称、alias、调用参数和 `inject_attrs` 配置不会被后续环境更新覆盖。
- local macro frozen forms 的实际注入值只来自 `LocalInjectFormsSnapshot`，不能看到 declaration 自身或 remaining queue。
- local-macro 工作流可以使用 `LocalClosureSourceView` 查找函数、计算闭包和冻结原始 forms，但不能把它传给 `inject_macro_attributes`。
- 更晚 attribute 触发按需编译时，local forms 使用 `LocalCompileContext`；编译完成后的 attribute 与 external attribute 一样进入统一 `AttributeRuntimeView` 规则，这不是 local 特例。
- 同一 splice 的后项只有真正 keep 后，才进入后续 attribute 调用的 injection view。

### 5.4 错误恢复层级

```text
Form handler failure
  -> 记录诊断，终止该 form 的状态提交，继续扫描后续 forms

Resolved macro execution/validation failure
  -> 以原 call/form 作为临时恢复值，继续遍历兄弟节点

Syntactic macro attribute cannot execute
  -> 当前扫描位置诊断一次并保留原 form
```

环境 state 的 `put/modify` 必须通过 do/bind 串联。所有 `astranaut_return` 结果通过 `astranaut:traverse_return/1` 进入 traverse；用户宏 computation 使用 `scoped_state/2`，不能修改框架扫描或函数遍历 state。

## 6. Scan Normalization 与 Finalization

队列清空后执行以下固定顺序：

1. 删除已经成功注册并由 `scan_local_declarations` 记录的 `local_macro` declaration。失败或未注册的 form 不得被静默当作已注册项删除。
2. 执行 `prepare_exports`，使 export/export_macro 的编译器 forms 就绪；`export_macro` 本身不把 FA 加入本模块 local 执行环境。
3. 将规范化 forms、最终 external registry/context 与 local state 交给 local-macro finalize；每个 request 展开 frozen forms 时仍必须使用自身的 declaration-time `env_snapshot` 与 `inject_forms_snapshot`。
4. finalize 执行完整最终 generation，返回：
   - `FinalLocalEnv`；
   - `FinalSkipIds`；
   - 已按最终有效环境展开并验证的 retained forms；
   - 最终 local state。
5. 将 retained forms 物化回 forms 流，并记录这些 function 的 `PreparedFunctionIds`。
6. 删除 `FinalSkipIds` 指定的 function/spec；同步清理由此变空的 `nowarn_unused_function` 项。
7. 以最终可调用 FA 过滤 local registry，构造最终执行环境并进行一次冲突/一致性校验。
8. 基于最终 forms 和 macro env 查找仍需展开的 function callers，排除 skip 与 prepared IDs。
9. 只在此处调用一次 `sort_forms/1`。

`PreparedFunctionIds` 是必要集合：retained function 已经在 finalize 中按最终 local 环境展开，若再次进入 function phase，会造成重复展开或改变深度语义。

## 7. `__original__` 与 Spec 的最终局部策略

scan-and-splice 层继续给新生成的 function/spec 加内部 tag，但不得做全局 Generated/Base partition。

当生成 function `F/A` 调用 `__original__/A` 且已存在 `F/A` 时：

1. 在全模块已有 `*/A` 名字中选择唯一原函数名，例如 `F_1/A`。
2. 把被包装的原 `F/A` 重命名为新名字，并改写其必要的自调用。
3. 把生成 wrapper 中的 `__original__/A` 改写为新名字。
4. spec 采用明确归属规则：
   - 原有 `-spec F(...)` 默认描述公开 wrapper，保留 `F/A`；
   - 若系统需要为重命名后的原函数保留 spec，必须复制并改写为新名字，而不能只移动旧 spec；
   - 生成的 `-spec F(...)` 与原 spec 冲突时，必须走显式去重/覆盖规则，不能留下重复 spec 交给最终排序偶然处理。
5. 除参与该 `F/A` 合并的 function/spec 外，所有 forms 相对顺序不变。

建议至少新增三类测试：原函数带 spec、wrapper 自带 spec、原/生成 spec 同时存在。

## 8. Function Phase

最终目标集合为：

```text
EligibleIds = DetectedMacroCallerIds
              - FinalSkipFunctionIds
              - PreparedFunctionIds
```

然后调用共享核心：

```text
expand_functions(FinalMacroEnv, AttributeForms, EligibleFAs)
```

共享核心只负责：

- 使用统一 call matcher 找到宏调用；
- 对目标 function 执行递归展开；
- 保留 `outer` / `inner` / `max_depth`；
- 处理宏返回校验、位置、变量与 formatter。

它不负责：

- local target 自身移除；
- `internal_function` direct-call 集合；
- declaration snapshot、generation、retain 或 skip；
- forms 排序。

local reference resolution 使用同一个 call matcher；每个 target 的 CandidateEnv 由 local-macro 工作流预先裁剪。

## 9. 最终不变量与验收条件

1. 环境更新按源码顺序立即生效，且只影响后续 forms。
2. external/local 的不同来源不会绕过统一冲突与 `force_override` 规则。
3. splice 结果保持局部顺序并立即重扫；旧结果永不回扫。
4. attribute injection 只读取调用点 passed forms；local frozen forms injection 只读取 declaration 前 passed forms；local closure source view 才包含 current + remaining。
5. attribute phase 不递归展开普通 function body。
6. local declaration 的冻结/注册发生在声明点，生命周期计划只由 local-macro 模块解释。
7. retained function 只展开一次；skip 和 prepared 项不进入最终 function pass。
8. 用户宏 state 与框架 state 隔离，但诊断、位置与 formatter 继续传播。
9. `__original__` 只触发局部整理，spec 归属有确定规则且有测试证明。
10. attribute phase 末尾只排序一次，function phase 不排序。
11. 无法执行的宏 attribute 只诊断一次；单个失败不妨碍兄弟诊断累计。
12. 普通与 local function 共用展开器和调用匹配语义，共享核心不含 local 专属策略。

## 10. 实现收敛优先级

### P0：限制 local macro function-form 编译上下文

保证 local frozen function 的全部宏展开上下文只来自 declaration 前 `passed_forms`。实现可把它分解为 `env_snapshot` 与 `inject_forms_snapshot`；`closure_source_view` 只用于闭包发现和累计模块结构物化。按需编译完成后，attribute 按 external/local 共用的运行期规则执行。

必须覆盖以下测试：

- declaration 前后存在同名目标 attribute，local forms 只注入前者；
- declaration 后 `use_macro` 改 alias、调用参数或 `inject_attrs`，不影响 frozen local forms；
- 后续 attribute 触发按需编译时，local function forms 仍只使用 declaration 前 passed forms，随后 attribute 使用通用运行期规则；
- remaining queue 中 helper 可进入闭包，但其中尚未 pass 的 attributes 不进入注入。

### P1：统一跨来源有效环境的顺序语义

引入 `effective_macro_env` 或等价的有序更新机制；为以下顺序补测试：

- external → local，无 force / local force；
- local → external，无 force / external force；
- 生成的 import/use 与 local declaration 交错；
- 冲突发生后不得执行使用错误 winner 的后续属性宏。

### P2：明确并验证 spec merge

为 `map_forms_splice_reorder` 的 spec 行为建立契约与测试，再决定是否扩展重命名/复制逻辑。

### P3：封装 local declaration 单次语义校验

保持现有“坏声明不回滚先前注册、诊断不重复”的行为，同时让注册和 local map 构造共享同一份成功校验结果。
