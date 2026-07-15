# Review: Macro Passes 实现与文档状态

## 2026-07-13 合并结论

后续调整内容已合并回本变更。旧文档中的分离式“外部属性 pass → 冻结 → 本地属性 pass”模型已删除，当前规范与实现均采用：

```text
统一 attribute scan-and-splice
  -> local-macro 工作流收尾
  -> 最终 function pass
```

local macro 的闭包、冻结、缓存、累计编译、retain 和安全加载只在 [local-macro](../local-macro/design.md) 中定义；本变更仅保留扫描器与该工作流的接口边界，以及共享 function 展开能力。

## 当前实现核对

| 契约 | 实现位置 | 状态 |
|---|---|---|
| 外部与本地属性宏统一顺序扫描 | `scan_attribute_forms/5`, `scan_form/1` | 已实现 |
| splice 结果在当前位置立即重扫 | `astranaut:map_forms_splice/3` | 已实现 |
| import/use/options 前向更新且不回扫 | `scan_env_form/2` | 已实现 |
| passed forms 与 remaining queue 分离 | `note_passed_form/2`, `queue_state => true` | 已实现 |
| attribute runtime 先解析、确保可调用，再统一构造与执行 | `resolve_attribute_macro_target/2`, `ensure_attribute_target_callable/2`, `build_attribute_macro_invocation/2` | 已实现 |
| attribute injection 使用调用点 passed forms | `attribute_runtime_context/1`, `build_attribute_macro_invocation/2` | 已实现 |
| local macro forms 使用 declaration-time context | `runtime_context_snapshot`, `prepare_declaration/4`, `prepare_requests/3` | 已实现 |
| declaration 预展开不强制编译 | `prepare_declaration/4`, `need_callable/4` | 已实现；仅真实 local 依赖产生中间代次 |
| generation 按累计 members 去重 | `generation_boundary_key/1`, `committed_boundaries` | 已实现；未新增 local macro 不重新编译 |
| compiler 只消费 canonical forms | `compile_boundary/3`, `canonical_expanded_forms` | 已实现 |
| 跨来源宏映射按源码顺序冲突/覆盖 | `effective_macro_map`, `merge_macro_maps_pure/2` | 已实现 |
| 生成 function/spec 仅最小整理 | `map_forms_splice_reorder/1`, `map_forms_splice_merge_specs/1` | 已实现 |
| 用户宏 traverse state 隔离 | `invoke_macro_function/1` + `scoped_state/2` | 已实现 |
| FinalLocalEnv 过滤并接入 function pass | `compiled_effective_macro_map/2`, `finalize_attribute_macro_pass/8` | 已实现 |
| FinalSkipIds 在 function pass 前剔除 | `remove_final_skip_forms/2` | 已实现 |
| local 与普通 function 共用展开器 | `expand_functions/4`, `expand_function/5` | 已实现 |
| whitelist control 显式区分 disabled/collect/verify | `local_macro_whitelist_control/0`, `whitelist_control/2` | 已实现 |
| 原始与 replacement AST 在统一发现点观察 local match | `observe_local_macro/2`, `expand_macro_recursive/4` | 已实现 |
| whitelist/result 共同进入 ExpansionRecord | `cache_expanded/4`, `results_by_input` | 已实现 |
| final local closure 按 canonical whitelist 过滤 | `final_whitelist_control/2`, `final_allowed_local_fas/2`, `keep_allowed_local_fas/2` | 已实现 |

## 从实现补入规范的细节

本轮检查额外记录了以下原 adjusted 文档没有完整表达的行为：

1. `inject_attrs` 是调用位点语义：attribute 宏只看已经通过扫描的 forms，不能看队列后方的 attribute；function 宏使用 attribute pass 完成后的 forms。
2. `import_macro` 和 `use_macro` 在扫描成功后被消费，`macro_options` 被保留并进入 passed 视图。
3. 不同来源的定义占用同一宏 key 时，在其源码位置统一裁决：必须声明 `force_override` 才能覆盖，否则当场报 `macro_override`；相同定义仍可幂等合并。
4. 用户宏返回的 traverse computation 在私有 state 中执行，以防覆盖 attribute scan 或 function traversal state；formatter、position 与错误仍沿外层管线传播。
5. 生成 function 只有在同名同 arity 冲突且调用 `__original__/Arity` 时才触发重命名；生成 public spec 若存在则替换原 public spec，否则保留原 spec；无关 forms 不参与全局重排。
6. 最终宏映射按 `FinalLocalEnv` 过滤，未编译或不可用的 local FA 不得进入 function pass。

## 2026-07-13 声明位点注入快照复核与实现结果

实现只冻结一份唯一形状的 declaration-time `runtime_context_snapshot`，其中同时包含 `macro_map`、`macro_options` 与 `inject_forms`。因此声明后的 `use_macro` 不会改变 local macro forms 中宏的名称、alias、调用参数和 `inject_attrs` 配置，声明后的 attributes 也不会进入 frozen forms 的注入视图。完整 `closure_source_view` 仍可用于查找 remaining queue 中的 helper，但不再作为宏上下文。

attribute 调用则始终使用调用点的 `effective_macro_map` 和 `passed_forms`。重构后的控制流先解析目标，只有选中且未就绪的 local 目标才执行 availability prerequisite；随后 external/local 都回到同一个 invocation 构造、注入和执行路径。这个运行期规则对所有宏通用，不是 local-macro 特例。

## 对初次 review 的处理

初次 review 中列出的 `FinalLocalEnv` 未接入、重复注册、忽略累计计划、未调用 retained 校验及缺少 function 延后测试，均已由后续 local-macro 重构和测试解决。历史问题不再作为当前待办保留。

## 验证

- `openspec validate macro-passes --strict`：通过。
- `openspec validate local-macro --strict`：通过；本轮已将旧规格标题转换为标准 delta 结构。
- `git diff --check`：通过。
- `powershell -ExecutionPolicy Bypass -File scripts\rebar3_sandbox.ps1 ct`：314/314 通过。

## 2026-07-13 最终 MacroRuntimeContext 层级实现

后续讨论确定的最终契约现已实现：

1. local declaration 注册后应尽可能预展开，而不是等编译 boundary 执行 request 时才展开。
2. 同一个 `-local_macro([...])` declaration 的 members 共享一个 context；首次完整递归展开观察到的 local matches 是 declaration/final 共用 canonical whitelist，不再构造 group、独立引用 scanner 或 final 排除环境。
3. ExpansionValidator 负责 input fingerprint、canonical whitelist/result 与 per-input cache；GenerationCompiler 只消费 canonical forms，不再载入每个 declaration 环境重放展开。
4. 编译由所有阶段共用的 `NeedCallable` 驱动，不绑定 attribute 调用点。
5. retain 与 Step 2 ordinary function 都使用 `FinalMacroRuntimeContext`，并与最后一次 local expansion result 比较；retain 宏头不再跳过。

此外，编译 boundary identity 已收紧为按声明顺序排列的累计 local macro members。
`-local_macro([foo/1]). -local_macro([bar/1]).` 中，若 bar 的 function form 不实际依赖
foo 作为宏，bar 的注册和预展开不会产生中间编译；首次需要可调用或收尾时直接编译
`{foo,bar}`。MacroRuntimeContext、注入 forms、触发阶段和 compile options 均不制造新代次。

最终目标与完成任务分别见 [`Hierarchy_final.md`](Hierarchy_final.md) 与 [`tasks.md`](tasks.md)。

## 2026-07-14 Local Macro Whitelist 合并

原 `local-macro-white-list` change 已合并进本 change。权威模型现在明确：

1. 通用 function 展开入口必须显式接收 `disabled`、`collect` 或 `verify(Expected)`；普通 function/attribute 不隐式参与白名单。
2. canonical whitelist 来自原始 frozen AST 的真实 local match，以及每个 macro 返回 AST 在 `process_macro_return` 既有 traversal 中一次性收集的 local macro presence；不增加独立 form scan。
3. non-final verify 在每个 Return AST 收集完成后批量报告该批全部 unexpected FAs，并阻止该 replacement 展开；missing FA 在完整递归展开结束后报告；final retained 先过滤环境。
4. whitelist conflict 与 expanded-form conflict 分别诊断，且失败不提交部分 ExpansionRecord、canonical form 或 generation。
5. replacement 首次匹配未 callable local macro 时返回调度请求，经 `NeedCallable` 编译后从 frozen form 重试。

合并后的 proposal、design、spec、tasks 与两份 hierarchy 文档均使用上述模型；原独立 change 目录不再保留。
