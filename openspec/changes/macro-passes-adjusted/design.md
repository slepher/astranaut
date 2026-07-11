# Macro Passes Adjusted 设计

## 设计目标

1. 外部与本地属性宏在同一次 scan-and-splice 中按源码顺序处理。
2. 宏环境增长只影响后续 forms，不回扫已处理结果。
3. local macro 的专属生命周期委托给 [local-macro 设计](../local-macro/design.md)。
4. 最终函数体展开继续保留现有递归及 `outer` / `inner` 语义。

## 两步模型

顶层只有两个 pass。local-macro 的收尾不是第三个 pass，而是 attribute
pass 的收尾子步骤；它必须在 forms 交给 function pass 前完成。

```text
1. Attribute pass
   1.1 初始化 ExternalEnv、LocalMacroState、Queue 和 Output
   1.2 逐 form 统一 scan-and-splice
       - 外部与可调用本地属性宏按当前位置展开
       - import/use/macro_options 前向更新 ExternalEnv
       - local_macro declaration 按 declaration-time source view 注册
       - 未就绪本地属性宏按需请求累计编译后在原位置展开
   1.3 收尾 local-macro 工作流，取得 FinalLocalEnv 与 FinalSkipIds
   1.4 从 function-pass 输入中剔除 FinalSkipIds；retain forms 保留
   1.5 对 attribute pass 的最终输出排序

2. Function pass
   - 使用最终 ExternalEnv + FinalLocalEnv
   - 只遍历 attribute pass 输出的保留 forms
   - 不再排序 forms
```

## 统一属性扫描

扫描 state 持有当前 `ExternalEnv`、已 pass forms，以及不透明的 `LocalMacroState`。local macro 的注册表、缓存和编译产物不在本变更中定义。

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

`passed_forms` 以输出顺序保存已处理 form，供现有 attribute injection 使用；新 splice form 在真正被处理前不属于 passed forms。

| 扫描到的 form | 行为 |
|---|---|
| `-import_macro(...)` / `-use_macro(...)` / `-macro_options(...)` | 更新 ExternalEnv，仅对后续 forms 生效。|
| `-local_macro(...)` | 调用 local-macro 注册流程，传入当前源码视图和当前 ExternalEnv。|
| 外部属性宏调用 | 用当前环境展开，结果 splice 回队列。|
| 已可调用的本地属性宏 | 用当前环境展开，结果 splice 回队列。|
| 已注册但尚不可调用的本地属性宏 | 请求 local-macro 工作流完成必要编译，再展开该属性。|
| 其他 form | 原样保留。|

属性宏可生成新的 `import_macro`、`use_macro`、`macro_options` 或 `local_macro` form；这些 form 重新进入同一扫描流。`export_macro` 不属于宏环境变更项，单独出现时不会使宏在定义模块内变为可调用的 local macro；其 retain 语义由 local-macro 工作流处理。`local_macro` 与 `export_macro` 可标记同一 FA：前者使用 declaration-time 本地宏环境，后者发布原模块的导出宏，二者保持独立。

### 宏环境 form 的处理

`import_macro`、`use_macro` 和 `macro_options` 在当前位置被消费或保留的具体形式沿用既有宏 attribute 约定，但状态更新必须在处理下一 form 前完成。

- `import_macro` 解析导入模块并更新可用外部宏映射。
- `use_macro` 基于已导入宏映射选择或别名宏，并将 option 合并结果写入 ExternalEnv。
- `macro_options` 更新后续导入和展开使用的全局 macro options。

环境 form 可以由属性宏生成；生成顺序决定其对后续属性的可见性。例如某属性先 splice `import_macro(macro_b)`，再 splice 依赖 `macro_b` 的属性时，后者必须在导入已生效后展开。

### 属性宏的判定与展开

每次处理 attribute 时，从 `ExternalEnv + 当前可调用 LocalEnv` 构造执行宏映射，并按既有 `as_attr`、`exec_macro` 规则匹配。若命中外部或已就绪本地属性宏，使用当前映射展开并返回 `splice(NewForms)`。

若 attribute 对应已注册但尚不可调用的 local macro，扫描器先向 local-macro 工作流请求最小累计编译计划；计划成功提交后重新以更新后的 LocalEnv 展开该 attribute。该过程仍在同一队列位置完成，不能把 attribute 延后到独立本地 pass。

未匹配的普通 attribute 与普通 forms 保持原样。属性宏生成的 function、spec 或其他普通 form 留在输出流中，不在属性扫描阶段提前执行函数体递归展开。

## 环境更新

- `use_macro` 对同一 option key 使用后者覆盖前者的合并规则，未提及的 option 保留。
- `import_macro` 的同名导入由后者覆盖。
- 本地属性宏生成的环境变更同样只影响后续 forms。

### 生成 form 的局部顺序

`splice(NewForms)` 保留 `NewForms` 内的相对顺序，并在剩余原队列之前处理。扫描结束前不得将所有生成 forms 与原始 forms 全局拆分后重插入，否则会破坏源码位置语义。已有 function/spec 合并或 `__original__/Arity` 整理只可在其既有的最小适用范围执行，不能改写本轮扫描的前向顺序。

### 本地属性宏的环境增长

本变更取消“本地属性宏不得生成宏环境变更”的阶段边界。一个已经可调用的本地属性宏若生成 `import_macro`、`use_macro` 或 `macro_options`，这些 form 与外部属性宏生成的同类 form 完全一样，立即 splice 并仅影响后续扫描。

它不影响该 local macro 的 declaration 环境或已经完成的累计编译；这些属于 local-macro 工作流的不可变快照。

## Attribute pass 收尾与 function pass

扫描完成后调用 local-macro 收尾流程。该流程返回最终可调用的本地宏环境及 `FinalSkipIds`；具体如何冻结、保留、比较和构建该集合见 [local-macro 设计](../local-macro/design.md)。

`FinalSkipIds` 是应从 function pass 输入中剔除的 forms，而不是 retain
集合。其定义为 `local_macro_expanded_ids - retained_form_ids`：已经为
local macro 编译且未 retain 的 form 不进入 function pass；被
`local_macro_retain`、`export` 或 `export_macro` retain 的闭包 form 保留。

function pass 使用 `ExternalEnv + FinalLocalEnv`，且只接收 attribute pass
收尾后的保留 forms。保留 form 仍遵循当前递归展开、`outer` / `inner` 和
`max_depth` 规则。

扫描器在收尾前不删除 local macro 相关的原始 forms；它只传递完整 forms 流及不透明 LocalMacroState。这样 retain declaration 即使出现在闭包 form 之后，也能由 local-macro 工作流基于完整扫描结果正确处理。
