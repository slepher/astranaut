# 宏子系统复杂度与 local_macro 插件边界审查

记录日期：2026-10-27。

来源：用户提供的宏子系统代码走查记录。本次整理保存审查观点、代码定位和候选方案，不代表重新完成了代码审计，也不表示已接受或实现这些建议。现行契约仍以 [Macro Passes 设计](design.md)、[Local Macro 设计](../local-macro/design.md) 和 [处理层级](Hierarchy_final.md) 为准。

原审查中的“插件化约完成 80%”“静态分析覆盖 99%”“可删除 400～500 行／600+ 行”等数字属于估计，没有配套测量方法。六模块 5,058 行、本地宏相关约 2,150 行等统计也未绑定可复现的 revision；此处不将它们作为架构决策依据。以下用文件与函数名定位，避免沿用会随代码变更失效的行号。

## 审查结论

原审查认为，外部宏的主干流程复杂度基本合理：源码序 scan-and-splice 负责生成 forms 的即时处理及环境前向生效，统一 `MacroEnvironment` 让属性宏与函数宏共享匹配和执行规则。

`local_macro` 已具备按需加载和独立生命周期，但核心模块仍承担只被本地宏使用的观察、调度、闭包分析和诊断逻辑。因此，“可选 capability provider”与“核心完全不感知本地宏策略的插件”应分别评价。原审查将多环境重放、canonical whitelist 和挂起重试列为主要维护成本，并建议通过更简单的语义降低复杂度。这是设计取舍建议；相关场景是否低频、成本是否超过收益，仍需用实际用例与测试验证。

## 已形成的模块边界

- `astranaut_macro_scan:scan_local_macro_form/2` 在首次遇到 `local_macro` 声明时加载 provider；未使用本地宏时 capability 保持 disabled。原审查也指出 `rebar.config` 已移除对 local provider 的 `erl_first_files` 要求。
- 编排器与扫描器通过 `init/1`、`handle_form/3`、`prepare_target/3`、`validate_generated/2`、`remove_declarations/2`、`finish_attribute_pass/3`、`prepare_function_pass/3`、`finish_function_pass/3` 驱动生命周期，共八个接口。
- `astranaut_macro_local_compile` 独立管理临时模块命名、generation、BEAM 编译和加载；原审查认为该拆分消除了早期全局模块锁的需要。

## 仍需评估的概念耦合

下表保留原审查的关注点。“只有本地宏消费”不自动意味着应当搬迁；需要判断该能力是否有稳定、独立的通用契约。

| 位置 | 审查关注点 |
| --- | --- |
| `astranaut_macro_expander` 的 `observation_control`、`observe_macro/2`、`whitelist_conflict_detail/4` | `disabled / collect / verify` 虽使用通用命名，目前服务于本地宏 canonical whitelist 的收集和校验。 |
| `expand_without_pending_dependency/2`、`expand_or_request_macro/4` | `callable => false`、`needed_macro_ids` 与短路遍历将本地宏的可调用调度需求带入展开器。 |
| `function_call_analysis/3`、`analyze_function_calls/3` | closure 模式收集本地调用边与宏依赖；外部宏 caller 筛选仅消费 presence。 |
| `macro_mfa/1`、`macro_exception/5` | 根据执行模块和源模块的差异还原本地宏身份及异常栈。 |
| `astranaut_macro_scan:scan_form/1`、`scan_local_macro_form/2`、`map_forms_splice_note_queue/2` | 扫描器识别 `local_macro` 并指定 provider；remaining queue 快照服务于闭包源码视图。应核实 disabled 时维护快照的必要性。 |
| `astranaut_macro:format_error/1` | 主模块持有 `invalid_closure_roots`、`conflicting_local_macro_whitelist`、`illegal_locked_form_mutation` 等本地宏专属错误文案。 |

## 候选简化一：首次声明环境绑定

现行 `ExpansionRecord` 保存 `canonical_whitelist`、`canonical_result` 和按输入环境索引的 `results_by_input`。同一 frozen function 首次以 collect 展开，后续环境以 verify 重放原始 AST，既比较白名单，也比较展开结果；final retained context 还按 canonical whitelist 过滤本地宏环境。

原审查建议：一个闭包函数的宏展开永久绑定首次声明时的环境快照，首次成功展开后得到 canonical form；后续闭包复用以及 retain/export 到最终模块时直接使用该 form，不在新环境下重放。

预期可取消多环境结果缓存、collect/verify 观察协议，以及 `conflicting_local_macro_whitelist` 和 `conflicting_local_macro_closure_environment` 两类冲突。它会改变现行可观察语义：此前因环境差异失败的程序可能成功，retained function 也不再按最终环境重新展开并校验。需要先明确共享 helper 的首次绑定时点、同声明成员的普通调用规则，以及与 retain/export 的关系，再同步规范与测试。

`process_macro_return/3` 现有返回树遍历同时负责规范化、位置/变量处理和宏 presence 分析。现行设计明确要求复用一次 traversal，并非独立的“白名单二次扫描”。简化时仅可移除不再需要的 observation 工作，不能据此删除返回树处理或递归展开所需的 presence 检查。

## 候选简化二：前缀编译替代挂起重试

现行 `prepare_declaration/3` 先处理静态已知依赖；当宏返回的 AST 动态引入未就绪的候选本地宏时，展开器报告 `needed_macro_ids` 并停止该次展开。provider 经 `need_callable/3` 编译所需 boundary 后，从 frozen form 重试，不提交半成品。

原审查建议：需要某个本地宏可调用时，按声明顺序确保其之前已注册的本地宏前缀全部就绪。若展开所能匹配的候选宏均已 callable，可删除 function expansion 中的挂起、短路和重试状态。

这一方案以提前编译换取较简单的控制流，可能增加不实际使用宏的编译成本，并提前暴露原本不会触发的编译错误。实施前需明确前缀边界、同声明成员排除规则、动态生成声明的处理，以及 canonical forms 尚未准备完成时的依赖顺序。还需证明 replacement 能发现的候选宏均被覆盖。删除 function expansion 挂起协议不等于可以删除属性调用点确保目标可调用的生命周期接口。

## 候选简化三：收紧 provider 边界

1. 将本地宏专属 `format_error/1` 子句下沉到 `astranaut_macro_local`，诊断使用对应 formatter，并保留错误位置与 monad 桥接语义。
2. 将 closure 调用图分析内聚到 local provider，复用通用 AST traversal 和宏匹配能力；expander 保留 caller presence 检查。迁移前确认宏匹配接口边界，避免复制匹配规则或引入第二套函数展开器。
3. 在 descriptor 中显式携带 `display_module` 等诊断身份，让通用展开器使用描述符信息。仅新增字段不足以自动解决异常栈映射，仍需明确执行模块与展示模块的映射契约。
4. 核查 capability disabled 时的 queue snapshot 成本，再决定是否按需维护；是否进一步抽象属性到 provider 的注册入口，取决于实际扩展需求。

## 候选简化四：registry 数据结构与展开状态

`astranaut_macro_registry` 的 `module_macro_maps_from_uniform/1`、`used_macros/4` 与 `uniform_imported_macro_map/1` 在扁平映射和按模块分组的映射之间转换。原审查建议分别保存按模块索引的原始导入表与扁平的 `effective_macro_map`，直接从导入表处理 use/alias/options，减少反复重建。

这项重构应保持重复 import/use、option 覆盖、alias、`force_override` 及跨来源冲突的现有行为；能否删除转换和减少多少代码应以实际实现为准。

另一个局部问题是 `initial_expansion_state(disabled)` 使用整数，而观察模式使用 map，使状态访问需要整数/map 分支。若保留 observation，可评估统一状态形状；若取消该机制，优先删除随之失效的状态分支，无需另建抽象。

## 建议顺序与验收边界

原审查建议优先讨论首次声明环境绑定和前缀编译，其次收紧 provider 边界，最后清理 registry 转换。前两项属于语义变更，应先决定契约；后两项可分别评估，不必捆绑成一次大重构。

进入实施前，应覆盖共享 helper 跨声明环境、retain/export、replacement 动态引用本地宏、同声明成员调用、源码序注入、导入冲突和诊断归属等案例。保留 scan-and-splice 的前向语义、统一递归展开及 `outer / inner` 行为、用户宏 state 隔离和 generation 提交的原子性。

接受方案后同步修改 macro-passes、local-macro 的 design/spec、处理层级与相关测试，再以实际代码量和编译行为评估收益。本记录不创建已完成任务，也不授权或执行代码重构。
