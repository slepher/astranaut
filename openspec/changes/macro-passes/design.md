# Macro Passes 设计

## 当前行为

uniform macro expansion 已经提供了统一的外部宏环境，以及函数级递归展开能力。这解决了“已知宏集合内部”的导入顺序问题，但模块级环境时序仍未完全定义。

仍然缺失的规则包括：

- 外部属性宏展开与它们所生成环境更新之间的时序
- 新生成的外部宏导入或 alias 是否会回溯影响更早的属性
- 本地宏快照边界在何时建立
- 本地宏或本地属性宏是否允许继续修改宏环境
- 用户显式声明的 helper 闭包如何并入闭包分析

## 设计目标

该 Pass 模型应保证：

1. 外部属性宏可以为后续外部属性宏完成引导
2. 外部宏环境变化不会导致已扫描属性被重新展开
3. 本地宏行为始终基于一个稳定快照编译
4. 本地宏与本地属性宏不能重新打开宏环境发现过程
5. 最终普通代码展开仍然保持递归和统一环境

## Pass 模型

```text
1. 发现初始外部宏环境
2. 按 forms 顺序展开外部属性宏
   - 新生成的外部宏环境项会更新当前外部环境
   - 新生成的 forms 会立即插回当前扫描流
   - 生成出的属性会继续参与同一轮外部属性阶段
   - 环境更新只影响后续 forms
   - 不回扫已处理的旧 forms
   - 该步骤使用输入 forms 的当前顺序，不在阶段中途做 forms 重排
3. 基于步骤 2 的结果发现本地宏
4. 计算本地宏快照闭包
   - 静态 helper 扫描
   - 合并显式 extra_functions
   - 执行 internal_function 策略一致性检查
5. 仅使用步骤 2 结束后的最终外部宏环境，预展开本地宏快照
6. 编译并加载临时本地宏模块
7. 展开本地属性宏
   - 允许修改普通 forms
   - 不允许修改宏环境
   - 不允许修改已锁定的本地宏快照区域
8. 构造最终宏环境 = 外部宏 + 已加载本地宏
9. 对非本地快照区域执行最终递归展开
```

## 外部属性阶段

只有外部属性宏阶段允许宏环境增长。

扫描模型为“按顺序扫描 + 立即插回生成结果”：

```text
Queue = Forms
Output = []
ExternalEnv = #{
  global_macro_opts,
  module_macro_maps,
  macro_map
}

while Queue 非空:
  Form = pop_front(Queue)
  if Form 是宏环境 attribute:
    ExternalEnv = update(Form, ExternalEnv)
  else if Form 是外部属性宏调用:
    Expanded = expand(Form, ExternalEnv.macro_map)
    Queue = insert_forms(Expanded, Queue)
  else:
    输出 Form 到 Output
```

关键语义：

- 只有这一阶段允许外部宏环境增长
- 环境增长是前向生效的
- 生成出来的 forms 会在当前位置重新进入扫描流
- 已经展开完成的旧属性不会被重新回扫
- 这不是模块级 fixed-point 循环

它更接近“词法顺序驱动的环境增长”，而不是“全模块收敛回扫”。

实现命名应反映这一职责：外部阶段是 `run_external_macro_pass` / `scan_external_macro_pass`，而不是单纯的 attribute transform。该阶段的主状态是 `ExternalEnv`，不能再用一个预烘焙的 `MacroMap` 作为 pass 参数来替代环境状态。

## 外部属性阶段与 forms 重排

“按 forms 顺序展开外部属性宏”中的顺序，指的是进入该阶段时的 forms 顺序，以及该阶段内部通过 scan-and-splice 生成出来的局部顺序。

这一阶段的目标是保留顺序语义，因此：

- 不应在扫描过程中调用会整体调整 form 次序的整理逻辑
- 不应在扫描中途把已处理前缀重新并回队列后再继续扫描

如果后续实现仍然需要使用现有 forms 整理能力，例如：

- `astranaut_syntax:sort_forms/1`
- `astranaut_syntax:reorder_updated_forms/1`

那么这些整理步骤只能发生在外部属性阶段完成之后，而不能打断当前阶段的顺序扫描语义。

`astranaut_syntax:insert_forms/2` 不属于这里排除的“阶段后整理”。它本身就是一种受规则约束的 forms 插入机制，允许作为当前阶段生成 forms 的插回手段使用。

因此，这里的限制应理解为：

- 允许使用 `insert_forms/2` 将新生成 forms 按现有插入规则并回当前序列
- 插回目标是当前位置之后的剩余队列，已处理前缀只保存在输出累积中
- 不允许在扫描中途调用 `sort_forms/1`、`reorder_updated_forms/1` 或其他整体重排 forms 的逻辑

也就是说，本阶段的规则是：

1. 先按当前 forms 顺序完成外部属性扫描与环境更新
2. 阶段完成后，如有必要，再进入后续全局整理步骤

这是一条补充性的实现约束，不改变既有的宏环境规则，只是明确“顺序扫描”优先于“forms 归位整理”。

## 外部属性阶段对生成 forms 的处理

生成的 forms 不能一概而论，必须区分对待。

### 生成的宏环境相关 forms

如果外部属性宏生成了：

- `-import_macro(...)`
- `-use_macro(...)`
- `-local_macro(...)`
- `-export_macro(...)`
- `-macro_options(...)`

则这些 forms 会被插回扫描流，并在被扫描到时生效。它们对后续 forms 立即可见，但绝不会回溯影响已经输出的旧 forms。

### 生成的外部属性宏调用

如果外部属性宏生成了新的外部属性宏调用，则该调用会在同一外部属性阶段中立即继续扫描和展开。

这支持如下链式引导：

```text
属性 A 展开后导入宏模块 X
属性 A 或其生成结果中再生成属性 B
属性 B 依赖 X
```

整个过程不需要全局 fixed-point。

### 生成的普通 forms

如果外部属性宏生成的是普通 forms，例如函数定义、普通 attribute 或函数体中仍带宏调用的代码，那么这些 forms 会被插入 forms 流并保留下来，但不会在此阶段提前执行“最终函数体宏展开”。

该阶段只负责：

- 外部属性扫描
- 外部宏环境更新

真正的递归宏展开仍然发生在后续本地宏快照边界建立之后。

### 生成的非环境 attribute

如果外部属性宏生成了“非环境 attribute”，该 attribute 同样会重新进入当前扫描流。

如果它在当前外部环境下能解析为外部属性宏调用，则继续按同样的 scan-and-splice 规则递归展开。

如果不能解析为外部属性宏调用，则它只是一个普通 attribute，留待后续阶段处理。

## 宏环境变更黑名单

实现层面采用黑名单模型，而不是白名单模型。

至少以下 forms 必须被视为“宏环境变更项”：

- `-import_macro(...)`
- `-use_macro(...)`
- `-local_macro(...)`
- `-export_macro(...)`
- `-macro_options(...)`

外部属性宏允许在本地宏快照边界之前生成这些 forms。

因此，在外部属性阶段中，生成出的 forms 可以扩展：

- `-import_macro(...)`
- `-use_macro(...)`
- `-local_macro(...)`
- `-export_macro(...)`
- `-macro_options(...)`

只要这些扩展发生在外部属性阶段，并且影响的是后续的宏发现流程即可。

## 本地宏快照边界

外部属性阶段结束后，外部宏环境被冻结。

随后进入本地宏阶段，识别：

- 由 `-local_macro(...)` 或 `-export_macro(...)` 声明的宏函数
- 它们静态可发现的 helper 闭包
- 通过 `extra_functions` 显式补充的 helper

这些集合中的本地宏闭包构成锁定的本地宏快照。

锁定区域包括：

- 快照中的 function forms
- 附属于这些函数的 `-spec` forms

`-export_macro(...)` 也要参与 helper 闭包扫描，但“导出宏闭包”不会仅因导出身份而进入锁定区。
它参与闭包分析，是为了统一执行宏定义策略，而不是为了快照冻结。

## 静态 helper 扫描与 `extra_functions`

静态 helper 扫描保持保守，仅识别可以通过静态本地调用分析得到的 helper。

对于静态扫描无法覆盖的情况，`-local_macro(...)` 的定义选项允许写：

```erlang
{extra_functions, [helper1/2, helper2/1]}
```

规则：

- `extra_functions` 与静态扫描结果取并集
- 多个 `extra_functions` 声明之间也按集合并集处理
- 每个引用到的函数都必须存在
- 每个被引用函数都进入同样的快照、预展开和锁定边界
- 如果 `extra_functions` 中某个函数本身也是 local macro，允许存在，最终仍按并集处理

这是对本地宏定义元数据的扩展，不新增独立 attribute。

## 宏定义闭包分析

helper 闭包分析并不只针对 local_macro。它同时适用于：

- `-local_macro(...)`
- `-export_macro(...)`

对每个宏定义，都要计算一个宏定义闭包，来源包括：

- 宏函数自身
- 静态扫描得到的 helper 调用
- `extra_functions`

这些闭包有两种用途：

- local macro 闭包进入锁定本地快照
- export macro 闭包不锁定，但仍参与宏定义策略分析

之所以要区分，是因为 export macro 的闭包扫描目的不是冻结源码，而是决定“宏定义内部调用该如何解释”。

## `internal_function` 策略

宏定义可以声明一种内部直接调用策略：
当宏定义内部遇到某些宏函数调用时，将它们按普通函数直接调用，而不是按宏调用展开。

该策略可以：

- 作用于宏定义内部的全部宏函数调用
- 或者只作用于一个显式函数列表

它的判断范围不是只看宏头函数，而是要对宏定义闭包中的函数一起生效。

## `internal_function` 冲突检测

不同宏定义可以有不同的 `internal_function` 声明，这本身不是错误。

只有当某个具体函数同时属于多个宏定义闭包，并且这些宏定义对该函数的 internal/direct-call 处理不一致时，才构成冲突。

例如：

- 宏 `A` 将 `helper/1` 视为 internal direct-call
- 宏 `B` 不将 `helper/1` 视为 internal direct-call
- `helper/1` 同时属于 `A` 与 `B` 的宏定义闭包

则这是一个编译错误。

因此，冲突检测必须发生在：

- 静态 helper 扫描之后
- `extra_functions` 合并之后
- local/export 宏闭包构造完成之后

并且早于：

- 本地快照锁定
- 最终宏展开

这是一条“按具体函数检查”的规则，而不是“宏名单必须完全一致”的规则。

## 本地宏约束

本地宏行为比外部属性宏更严格。

### 本地宏函数

本地宏函数及其 helper：

- 允许使用已经冻结的外部宏环境
- 不允许继续引入新的宏环境
- 不允许默认展开同模块本地宏
- 在本地宏模块编译完成后，会被最终展开阶段跳过

宏定义内部的调用解释遵循 `internal_function` 策略。
是否按直接调用处理，并不由“该函数是否出现在 `-export_macro(...)` / `-local_macro(...)` 声明里”单独决定；声明身份只参与闭包分析，真正的直接调用行为由宏定义策略本身控制。

### 本地属性宏

本地属性宏：

- 可以生成或改写普通 forms
- 生成 forms 会插回当前位置之后的剩余队列，并在同一本地属性阶段继续扫描
- 不允许生成任何宏环境变更项
- 不允许生成 `-local_macro(...)`
- 不允许生成 `-export_macro(...)`
- 不允许生成 `-import_macro(...)`
- 不允许生成 `-use_macro(...)`
- 不允许生成 `-macro_options(...)`
- 不允许改写已经锁定的本地快照 forms

违反任一条都应立即编译失败。

## 最终展开

本地宏模块加载完成后：

```text
FinalMacroEnv = FrozenExternalEnv + LoadedLocalMacroEnv
```

最终递归展开只作用于锁定本地快照之外的 forms。

这一阶段保留现有递归行为：

- 展开结果生成的宏调用会继续展开
- `outer` 宏在 pre 阶段展开
- `inner` 宏在 post 阶段展开
- `max_depth` 仍然限制单条宏展开链深度

## 错误模型

这一设计需要为跨 Pass 违规行为引入明确的新错误名。

建议新增：

- `illegal_macro_environment_mutation`
- `illegal_local_macro_definition_mutation`
- `illegal_dynamic_local_macro_environment`
- `invalid_extra_functions`
- `illegal_local_macro_function_style_call`
- `conflicting_internal_function_policy`

建议含义：

- `illegal_macro_environment_mutation`
  本地展开阶段生成了宏环境变更 attribute
- `illegal_local_macro_definition_mutation`
  本地属性宏试图改写已锁定的本地快照 form
- `illegal_dynamic_local_macro_environment`
  本地宏函数体或本地属性试图在冻结点后继续引入宏环境变化
- `invalid_extra_functions`
  `extra_functions` 语法非法，或引用了不存在的函数
- `conflicting_internal_function_policy`
  同一具体函数同时属于多个宏定义闭包，且 `internal_function` 处理方式冲突

对于新的 Pass 边界错误，应优先使用新的显式错误名，而不是复用旧的宏展开错误名。

## 为什么不做全局 fixed-point

该设计刻意避免引入全模块重新发现循环。

原因：

- 外部引导能力仍然通过源码顺序的外部属性宏得到保留
- 生成出的属性会在当前位置立即继续扫描
- 本地宏仍然只针对一个稳定环境编译
- 整体语义可以解释为“一次前向扫描 + 一次本地快照边界”

这比全局 fixed-point 更适合 Astranaut，因为它保留了真正有价值的外部属性引导能力，同时避免本地宏行为漂移。
