# Macro Passes 设计

## 设计目标

1. 外部与本地属性宏在同一次 scan-and-splice 中按源码顺序处理。
2. 宏环境增长只影响后续 forms，不回扫已处理结果。
3. local macro 的专属生命周期委托给 [local-macro 设计](../local-macro/design.md)。
4. 最终函数体展开继续保留现有递归及 `outer` / `inner` 语义。
5. scan 的局部顺序、诊断信息和 traverse state 不因宏执行或 forms 整理被破坏。

## 统一 function 展开能力

`astranaut_macro` 只维护一套 function-body 宏匹配和递归展开实现：

```text
ExpandFunctions(MacroEnv, Forms, TargetFAs) -> ExpandedForms | Error
```

最终 function pass 和 local-macro 累计编译都调用该实现。它只根据传入的 `MacroEnv` 工作，不判断目标是否为 local macro，也不读取 `internal_function`、declaration order、generation、retain 或冻结状态。

`astranaut_macro` 还提供与展开器相同调用匹配语义的引用解析操作：

```text
ResolveLocalReferences(CandidateLocalEnv, Forms, ClosureFAs) -> ReferencedFAs
```

该操作负责判断闭包中的调用是否实际匹配某个 local macro。静态函数闭包、候选环境、`internal_function` direct-call 集合以及逐目标 FA 的环境裁剪由 `astranaut_local_macro` 决定。特别地，展开 TargetFA 时传入的最终环境不包含 TargetFA；这是调用方构造环境的规则，不是通用展开器中的递归特判。

两个操作均返回 `astranaut_return` 结果。统一扫描只在调用 local-macro 的注册、按需可调用和收尾接口时桥接 traverse/return monad，不在扫描器内执行或解释 local-macro 编译计划。

## 两步模型

顶层只有两个 pass。local-macro 的收尾不是第三个 pass，而是 attribute pass 的收尾子步骤；它必须在 forms 交给 function pass 前完成。

```text
1. Attribute pass
   1.1 初始化 ExternalEnv、LocalMacroState、Queue 和 Output
   1.2 逐 form 统一 scan-and-splice
       - 外部与可调用本地属性宏按当前位置展开
       - import/use/macro_options 前向更新 ExternalEnv
       - local_macro declaration 委托 local-macro 工作流注册
       - 未就绪本地属性宏请求工作流确保可调用后在原位置展开
   1.3 收尾 local-macro 工作流，取得 FinalLocalEnv 与 FinalSkipIds
   1.4 物化 retain 结果并从 function-pass 输入中剔除 FinalSkipIds
   1.5 对 attribute pass 的最终输出排序

2. Function pass
   - 使用最终 ExternalEnv + FinalLocalEnv
   - 只遍历 attribute pass 输出的保留 forms
   - 不再排序 forms
```

## 统一属性扫描

扫描 state 持有当前 `ExternalEnv`、已通过扫描的 `passed_forms`，以及不透明的 `LocalMacroState`。local macro 的注册表、缓存和编译产物不在本变更中定义。扫描 local declaration 时，local macro function forms 的编译上下文冻结为 declaration 前的 `passed_forms`；另可交付 `passed_forms + 当前及剩余 queue` 作为结构性的闭包源码视图，但后者不是宏展开上下文。

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

遇到 `local_macro` declaration 时，扫描器冻结一份 local function-form 编译上下文，并提供一份独立的结构源码视图：

```text
LocalCompileContext = {
  MacroEnv    = declaration 前已生效的 import/use/options 与可引用 local 宏,
  InjectForms = declaration 前的 passed_forms
}
ClosureSourceView = passed_forms ++ 当前及剩余 queue
```

`ClosureSourceView` 只用于定位 function/spec 与计算静态闭包；编译 frozen local macro forms 时，宏名称、alias、调用参数及 `inject_attrs` 配置和实际注入值全部由 `LocalCompileContext` 决定。更晚 attribute 触发的 `ensure_available` 不得用触发点环境覆盖该编译上下文。

| 扫描到的 form | 行为 |
|---|---|
| `-import_macro(...)` | 更新 ExternalEnv 并消费该 form。 |
| `-use_macro(...)` | 更新 ExternalEnv 并消费该 form。 |
| `-macro_options(...)` | 更新全局 options，保留该 form，并记入 `passed_forms`。 |
| `-local_macro(...)` | 调用 local-macro 注册流程。 |
| 外部属性宏调用 | 用当前环境展开，结果 splice 回队列。 |
| 已可调用的本地属性宏 | 用当前环境展开，结果 splice 回队列。 |
| 已注册但尚不可调用的本地属性宏 | 请求 local-macro 工作流确保可调用，再在原位置展开。 |
| 其他 form | 原样保留。 |

属性宏可生成新的 `import_macro`、`use_macro`、`macro_options` 或 `local_macro` form；这些 form 重新进入同一扫描流。`export_macro` 单独出现时不会使宏在定义模块内变为可调用的 local macro；其专属保留语义见 local-macro 文档。

### 宏环境 form 的处理

- `import_macro` 解析导入模块并更新可用外部宏映射。
- `use_macro` 基于已导入宏映射选择或别名宏，并将 option 合并结果写入 ExternalEnv。
- `macro_options` 以后者覆盖同名全局 option，未提及的 option 保留。
- 同一 `use_macro` 再次配置同一宏时，后声明 option 覆盖同名 key，其他 key 保留。
- 相同宏定义可幂等合并；若新导入、alias 或本地映射占用已有宏 key 且定义不同，必须显式使用 `force_override`，否则报 `macro_override`。因此不存在无条件的“同名导入后者覆盖”。

环境 form 可以由属性宏生成；生成顺序决定其对后续属性的可见性。例如某属性先 splice `import_macro(macro_b)`，再 splice 依赖 `macro_b` 的属性时，后者必须在导入已生效后展开。

### 属性宏的判定、注入与展开

每次处理 attribute 时，从 `ExternalEnv + 当前可调用 LocalEnv` 构造执行宏映射，并按既有 `as_attr`、`exec_macro` 规则匹配。宏声明的 `inject_attrs` 在调用时注入，而不是导入时固化：所有 attribute 宏无论来自 external 还是 local，都只看当前位置之前的 `passed_forms`；最终 function 宏则看 attribute pass 完成后的完整 forms。

若当前 attribute 触发某个 local macro 的按需编译，先用该 local declaration 的 `LocalCompileContext` 编译其 function forms；随后对 attribute 的解析和运行仍进入上述通用 attribute 规则。调用点规则不是 local macro 特例，local macro 的唯一特例只是 function-form 编译上下文被限制在 declaration 前的 `passed_forms`。

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

扫描完成后调用 local-macro 收尾流程。该流程返回最终可调用的本地宏环境及 `FinalSkipIds`；具体如何冻结、保留、比较和构建该集合见 [local-macro 设计](../local-macro/design.md)。

function pass 使用 `ExternalEnv + FinalLocalEnv`，并从输入剔除 `FinalSkipIds`。`FinalLocalEnv` 中不存在的未编译 local macro 不得残留在最终执行映射中。function pass 只遍历实际包含宏调用且未被跳过的目标 functions，保留当前递归展开、`outer` / `inner` 和 `max_depth` 规则。

扫描器在收尾前不自行删除 local macro 相关原始 forms，也不解释 local-macro 的编译计划；它只传递完整 forms 流和不透明状态，并消费工作流返回的最终环境、物化 forms 与跳过集合。
