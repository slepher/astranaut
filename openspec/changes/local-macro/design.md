# Local Macro 设计

2026-10-28：采用首次绑定、延迟展开与策略 A（含宏闭包前序就绪）。本设计替代多环境重放及 canonical whitelist 方案；审查背景见 [复杂度审查](../macro-passes/complexity-review.md)。

## 职责边界

provider 是将 `local_macro` 专属逻辑从主流程中分离的内部模块边界。扫描器可以显式识别 `local_macro` 并指定 `astranaut_macro_local`，主流程负责在既定阶段委托处理；闭包、首次绑定、编译调度和 retain 策略由该模块拥有。设计不要求动态插件注册、第三方扩展或可替换 provider。`capability` 仅表示本地宏处理是否已启用及其内部状态。

`astranaut_macro_local` 管理声明校验、静态闭包、首次环境绑定、canonical forms、累计编译计划及 retain。`astranaut_macro_local_compile` 管理唯一临时模块、BEAM 编译加载及 generation 提交。扫描器拥有 queue、splice 和 registry；provider 返回 definitions delta，由 scanner 合并。

provider 的生命周期接口为 `init/1`、`handle_form/3`、`prepare_target/3`、`validate_generated/2`、`remove_declarations/2`、`finish_attribute_pass/3`。最终函数 pass 不再调用 provider。无 local declaration 时 capability disabled，外部宏不要求 provider 存在。

`astranaut_macro_expander` 共享宏匹配、调用、返回 AST 规范化和递归展开；不接收白名单、可调用标记或编译回调。闭包调用边分析归 provider，通过展开器的通用匹配接口判断 presence。本地宏 descriptor 显式携带 diagnostic_mfa 和 display_module，通用展开器据此展示调用身份及异常栈，不推断临时模块含义。局部诊断由 `astranaut_macro_local:format_error/1` 负责；共享声明校验和宏执行诊断保留各自 formatter。

## 注册与首次绑定

闭包是从宏头和 `closure_roots` 出发，经直接本地调用递归可达的函数及对应 spec 集合。`fun helper/1`、动态调用与 `apply/3` 不形成静态闭包边，须显式补充 closure_roots。不存在的补充根报错。同一声明的全部 FA 先检查重复，成功后共享 order；不另设 group 对象。

注册只校验、计算闭包、保存快照和 definitions；不展开函数，也不编译。源码视图为 `passed_forms ++ current_and_remaining_queue`，仅包含已经 materialize 的 forms。宏环境取声明前的有效映射和 options，`inject_attrs` 已用声明前 AttributeEnv 解析为 descriptor 的 attributes；remaining queue 不进入注入值。

每个 FormId 首次进入闭包时永久绑定原始 form、声明前 macro map、order 与 record 上下文，并保存 has_macro_call。后续声明复用已有绑定，不覆盖共享 helper，也不因 helper 后来被声明为宏而重新绑定。同声明新成员与后声明宏尚不在该快照内，调用自然保持普通 Erlang 调用。若同名 key 已由先前外部或本地宏占用，仍遵守快照中的匹配及 force_override 规则。

```text
State = #{
  declarations => #{DeclarationForm => {Pos, FAs}},
  local_macros => #{FA => #{order, closure_ids, closure_fas, source_view}},
  frozen_forms => #{FormId => #{
    original_form, macro_map, order, has_macro_call, record_forms
  }},
  canonical_forms => #{FormId => ExpandedForm},
  retained_form_ids => [FormId],
  compiler => CompilerState
}
```

function ID 为 `{function, Name, Arity}`，spec ID 为 `{spec, Name, Arity}`。任何属性宏生成相同 frozen ID 均报 `illegal_locked_form_mutation` 并丢弃该次 splice，扫描继续。未来 splice 的函数不能预先成为闭包成员。

## 单次 canonical 展开

每个 frozen form 只在首次绑定环境下成功展开一次。spec 和 has_macro_call=false 的 function 直接使用原始 form；含宏 function 使用共享 `expand_functions/2`。成功结果写入 canonical_forms；后续声明、编译或 retain 均复用，不做环境指纹或结果比较。

record forms 从首次绑定的 source view 提取，与绑定一起复用。每个任务只处理目标 function 与所需 record 定义，不扫描无关函数。展开失败不缓存半成品；宏错误、warning 和源位置沿 return/traverse 管线保留。

## 延迟累计编译（策略 A）

需要字节码的入口只有属性调用的 `prepare_target` 和扫描收尾的 `finish_attribute_pass`。前者确保目标声明 order 及之前的累计 members 可调用，后者确保全部声明可调用。同声明 members 必须完整进入同一个 prefix。

计划按 frozen form 的首次绑定 order 处理：

1. 没有宏调用的 form 直接成为 canonical，不产生中间编译。
2. 含宏调用且绑定 macro map 包含先前本地宏时，在展开前确保该绑定 order 之前的完整累计 prefix 已编译。更早的边界依次先处理。
3. 所有候选本地宏已可调用后，展开该 function。外部或本地 replacement 动态生成的前序本地宏调用沿普通递归路径直接执行，不挂起、不重试。
4. 准备目标 prefix 的全部 canonical forms，再编译累计模块。已有更大累计 generation 时复用，不回退加载较小 prefix。

因此，连续独立且无宏调用的声明在收尾只编译一次；B 调用前序 A 时编译 A 后再编译 A+B。B 仅调用外部宏但快照仍含前序 A 时，也会先编译 A，这是策略 A 为去掉展开器调度协议接受的成本。不存在前序本地候选时无需中间编译。请求 B 的 prefix 中任何其他成员需要前序宏时，同样必须满足其展开依赖。

每个 boundary 以稳定声明顺序的累计 members 为身份，相同 boundary 不因触发位置、注入值或 options 重编译。编译计划只消费 frozen bindings，不按后声明环境重新发现依赖。

### 编译输入与输出

`astranaut_macro_local_compile:ensure/5` 接收累计 members、编译专用 snapshots、
canonical forms、编译 context 和自身的 CompilerState。snapshot 仅含
`closure_ids`、`closure_fas`、`source_view`；context 仅含 compile options 与 formatter
闭包信息。compiler 不读取声明环境，不回调 local 或 expander。

compiler 自行检查 boundary 是否已提交、选择最后一个声明的 source view、确认所需
canonical forms 齐备、合并累计源码、加入 formatter 函数并生成 exports，随后编译加载。
只有成功加载后才一次更新 compiled forms、boundary、generation 和逐 FA versions。
local 仅替换返回的 compiler 子状态，通过 module/is_callable/compiled_ids 查询
编译事实，不直接写内部字段。module_name/1 也归 compiler；不会重新引入共享名字锁。

最终 retain 仍由 local 决定删除哪些 FormIds；compiler 的 remove_forms/2 执行源码过滤，
并清理指向已删除函数的 nowarn_unused_function 编译属性。

每一代模块的 forms 由下列部分组成：

- module attribute，模块名为 `<Module>__local_macro__<Unique>`，其中 suffix 在每次 parse transform 入口分配且只供该次编译使用；
- 当前累计闭包中已经确认的 canonical function/spec forms；
- local macro 所需的 export forms；
- 编译所需的非函数模块 forms，但不复制原模块的普通 export 声明。

canonical 输入只包含累计闭包相关的 function/spec FormIds；非函数支持 forms 从该
boundary 最后一个 declaration 冻结的 source view 选择，因此 attribute/finalize 的触发点
不会改变编译源码。普通无关函数和 macro 控制 attributes 不进入 local module。

编译前对 canonical forms 执行现有排序和合法性处理。compile options 在一次 parse
transform 内保持稳定，只作为 compiler 参数传入，不参与 boundary identity。编译成功、
加载成功后，才把本代输入写入 `compiled_forms`，记录累计 members boundary key，更新相关
FA 的可调用 versions 和 generation（均由 compiler 提交）。编译失败不能覆盖上一代可调用模块。


## Retain 与最终函数 pass

扫描收尾确保全部 local members 的最终累计 generation，然后从 `local_macro_retain`、`export`、`export_macro` 收集 roots。根命中任一闭包成员时，保留该完整闭包及 specs；非闭包显式 retain 报 `ineffective_local_macro_retain`，不存在的显式 root 报 `undefined_local_macro_retain`，隐式 export roots 不报这两类 warning。

```text
RemovedIds = compiled_ids(CompilerState) - retained_form_ids
ExpansionSkipIds = frozen_ids(State)
```

RemovedIds 用于物理移除 local-only forms，并清理 nowarn_unused_function。剩余 frozen forms 在 attribute pass 收尾替换为 canonical forms。provider 返回 ExpansionSkipIds，orchestrator 从最终 caller 集合中排除它们。retained 宏头、共享 helper 和 specs 均复用首次结果；普通非闭包函数仍用完整 FinalMacroEnvironment 展开。不得把“删除 form”与“保留但不再展开”合并为同一过滤操作。

## 诊断与提交

保留 duplicate_local_macro_declaration、invalid_closure_roots、illegal_locked_form_mutation 及 retain warnings。移除 conflicting_local_macro_whitelist 和 conflicting_local_macro_closure_environment：后续环境变化不再构成冲突。

canonical 只在完整展开成功后提交，generation 只在编译和加载成功后提交；错误不覆盖上一代可调用代码。临时模块继续使用每次 parse transform 唯一的名字，安全 soft purge，不引入全局锁或进程字典调度。
