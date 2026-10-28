# Macro Passes 设计

> 2026-10-28 已采用首次绑定与延迟展开（策略 A）。审查背景：[2026-10-27 宏子系统复杂度与 local_macro 职责边界审查](complexity-review.md)。该文保留历史建议；现行契约以本文与 local-macro 设计为准。

## 设计目标

1. 外部与本地属性宏在同一次 scan-and-splice 中按源码顺序处理。
2. 宏环境增长只影响后续 forms，不回扫已处理结果。
3. local macro 的专属生命周期委托给 [local-macro 设计](../local-macro/design.md)。
4. 最终函数体展开继续保留现有递归及 `outer` / `inner` 语义。
5. scan 的局部顺序、诊断信息和 traverse state 不因宏执行或 forms 整理被破坏。

provider 用于隔离本地宏内部策略。主流程显式识别本地宏声明、初始化其处理模块并调用生命周期接口，属于正常编排职责；无需通用插件注册入口。`capability` 是内部启用状态，不是用户可配置的插件机制。

## 统一 function 展开能力

`astranaut_macro_expander:expand_functions/2` 接收 Forms 与 `#{FormId => #{form, macro_map, has_macro_call => boolean()}}` 任务表，返回 `#{forms => ExpandedForms, task_results => #{FormId => ExpandedForm}}` 的 return monad。属性与函数共用目标解析、调用参数构造和返回 AST 处理。每个函数隔离整数重命名计数器；用户宏私有 state 不覆盖外层 state。

展开器只消费已可调用的 descriptors，不维护 observation、whitelist、needed_macro_ids 或编译回调。返回 AST 的单次规范化 traversal 同时检测宏 presence；有宏时继续既有 outer/inner 递归，无宏时直接返回。保留位置、变量卫生、record 校验和诊断恢复。

local provider 在首次冻结时绑定声明前环境，延迟展开一次 canonical form。含宏闭包采用策略 A，在展开前确保快照中的前序本地候选全部可调用。闭包分析由 provider 负责，复用展开器的宏匹配接口。最终 retained forms 直接物化 canonical，普通函数使用最终环境；详见 [local-macro](../local-macro/design.md)。

普通 attribute 使用增量 AttributeMacroIndex，只解析所选 descriptor 的 inject_attrs；声明冻结与最终函数环境构造仍是完整 map 的解析边界。

## 两步模型

顶层只有两个 pass。local-macro 的收尾不是第三个 pass，而是 attribute pass 的收尾子步骤；它必须在 forms 交给 function pass 前完成。

```text
1. Attribute pass
   1.1 初始化 ExternalEnv、LocalMacroState、Queue 和 Output
   1.2 逐 form 统一 scan-and-splice
       - 外部与可调用本地属性宏按当前位置展开
       - import/use/macro_options 前向更新 ExternalEnv
       - local_macro declaration 注册逐 FA 条目、冻结首次声明前环境、登记闭包及首次绑定，不展开、不编译
       - 本地属性调用由 provider 在执行前确保目标累计 prefix 可调用
   1.3 收尾 local-macro 工作流，编译最终累计 generation、删除 local-only forms、物化 retained canonical forms，并返回 ExpansionSkipIds
   1.4 从完整 AttributeEnv 解析 FinalMacroEnvironment

2. Function pass
   - 从 caller 集合排除 ExpansionSkipIds
   - 其余普通目标使用 FinalMacroEnvironment 与共享 expander 展开
```

## 统一属性扫描

Registry 分别保存按 provider 分组的 `imported_macro_maps` 和扁平的 `effective_macro_map`。导入描述符只在 import 时规范化；use 直接访问已有分组，不再从扁平表重建分组或重新规范化全部描述符。分组表保留尚未被覆盖的导入调用键及 alias 历史，不能替换成永不更新的原始导出元数据，否则重复 use 的选项继承与冲突行为会变化。跨 provider 的 `force_override` 同步移除旧来源的同名键，本地 definitions 只更新 effective 表。use 提交时仍将临时分组表扁平化一次，沿用完整导入环境的 checked merge 语义；单个 FA 的冲突检查只查目标调用键。

扫描 state 持有当前 `EffectiveMacroMap`、已通过扫描的 `passed_forms`、增量 `AttributeEnv`，以及不透明的 `LocalMacroState`。local macro 的注册表、缓存和编译产物不在本变更中定义。扫描 local declaration 时，function forms 的延迟展开 `MacroEnvironment` 从 declaration 前的状态解析；另可交付 `passed_forms + 当前及剩余 queue` 作为结构性的闭包源码视图，但后者不是宏展开环境。

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

遇到 `local_macro` declaration 时，扫描器冻结一份 local function-form 延迟展开上下文，并提供一份独立的结构源码视图：

```text
DeclarationMacroEnvironment = {
  MacroMap     = declaration 前已生效并用 AttributeEnv 解析的宏描述符,
  MacroOptions = declaration 点 options
}
ClosureSourceView = passed_forms ++ 当前及剩余 queue
```

`ClosureSourceView` 只用于定位 function/spec 与计算静态闭包；延迟展开 frozen local macro forms 时，宏名称、alias、调用参数及已解析 attribute 值全部由 `DeclarationMacroEnvironment` 决定。generation 编译不读取该 environment。

| 扫描到的 form | 行为 |
|---|---|
| `-import_macro(...)` | 更新 ExternalEnv 并消费该 form。 |
| `-use_macro(...)` | 更新 ExternalEnv 并消费该 form。 |
| `-macro_options(...)` | 更新全局 options，保留该 form，并记入 `passed_forms`。 |
| `-local_macro(...)` | 注册共享 order/context 的逐 FA 条目、登记闭包和首次绑定。 |
| 外部属性宏调用 | 用当前环境展开，结果 splice 回队列。 |
| 已可调用的本地属性宏 | 用当前环境展开，结果 splice 回队列。 |
| 已注册但尚不可调用的本地属性宏 | 调用 provider 的 `prepare_target` 确保累计 prefix 可调用，成功后仍在原位置展开。 |
| 其他 form | 原样保留。 |

属性宏可生成新的 `import_macro`、`use_macro`、`macro_options` 或 `local_macro` form；这些 form 重新进入同一扫描流。`export_macro` 单独出现时不会使宏在定义模块内变为可调用的 local macro；其专属保留语义见 local-macro 文档。

宏定义 options 使用分层 validator：`export_macro` 和 `local_macro` 共享
`as_attr`、`order`、`inject_attrs`、`group_args`、`force_override`、`max_depth`；
只有 `local_macro` 接受补充函数闭包的 `closure_roots`。该 option 既不属于模块级
`macro_options`，也不属于 `export_macro`；写在这两种 attribute 上应作为 unexpected
option 报告并忽略，不得让全局配置或导出声明隐式参与本地闭包构造。

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

每次处理 attribute 时，从 `ExternalEnv + 当前可调用 LocalEnv` 构造执行宏映射，用当前增量 `AttributeEnv` 把 `inject_attrs` selector 解析为 descriptor 的 `attributes`，再按既有 `as_attr`、`exec_macro` 规则匹配。所有 attribute 宏无论来自 external 还是 local，都只看当前位置之前的 attributes；普通最终 function 宏使用 attribute pass 完成后的完整环境；retained frozen forms 复用 canonical 结果。

若当前 attribute 需要尚不可调用的 local macro，由 provider 在调用前确保目标可调用。scheduler 使用已经确认的 canonical forms 编译必要 boundary；随后 attribute 仍按调用点 `MacroEnvironment` 运行。attribute 不拥有专用编译策略。

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

扫描完成后，orchestrator 移除成功注册的 declarations 并统一准备 exports。provider 收尾编译全部本地宏，删除 local-only forms，将 retained frozen forms 替换为 canonical forms，返回 `{Forms, ExpansionSkipIds, ProviderState, Warnings}`。

orchestrator 构造唯一 FinalMacroEnvironment，执行普通 caller analysis，并从 caller 集合排除 ExpansionSkipIds，格式化 warnings 后排序 forms。function pass 不再携带 provider state 或调用 preparation/completion 钩子，只展开非闭包目标。

scanner 仍只合并 provider 返回的 definitions delta，不解释编译计划。local 专属诊断由 provider formatter 负责，共享校验和宏执行诊断继续沿原 formatter 管线传播。
