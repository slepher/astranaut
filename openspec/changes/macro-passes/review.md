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
| attribute injection 使用 passed forms | `inject_macro_attributes/2` 调用点 | 已实现 |
| local macro forms 使用 declaration-time inject forms | `expand_request_form/4` 当前传入 declaration `SourceView` | 未实现：会包含 remaining queue |
| 宏映射冲突需 `force_override` | `merge_macro_maps/2`, `merge_macro_maps_pure/2` | 已实现 |
| 生成 function/spec 仅最小整理 | `map_forms_splice_reorder/1` | 已实现 |
| 用户宏 traverse state 隔离 | `invoke_macro_function/1` + `scoped_state/2` | 已实现 |
| FinalLocalEnv 过滤并接入 function pass | `compiled_local_macro_map/2`, `finalize_attribute_macro_pass/7` | 已实现 |
| FinalSkipIds 在 function pass 前剔除 | `remove_final_skip_forms/2` | 已实现 |
| local 与普通 function 共用展开器 | `expand_functions/3`, `expand_function/4` | 已实现 |

## 从实现补入规范的细节

本轮检查额外记录了以下原 adjusted 文档没有完整表达的行为：

1. `inject_attrs` 是调用位点语义：attribute 宏只看已经通过扫描的 forms，不能看队列后方的 attribute；function 宏使用 attribute pass 完成后的 forms。
2. `import_macro` 和 `use_macro` 在扫描成功后被消费，`macro_options` 被保留并进入 passed 视图。
3. 不同定义占用同一宏 key 时不会自动后者覆盖；必须声明 `force_override`，否则报 `macro_override`。相同定义仍可幂等合并。
4. 用户宏返回的 traverse computation 在私有 state 中执行，以防覆盖 attribute scan 或 function traversal state；formatter、position 与错误仍沿外层管线传播。
5. 生成 function 只有在同名同 arity 冲突且调用 `__original__/Arity` 时才触发重命名；无关 forms 不参与全局重排。
6. 最终宏映射按 `FinalLocalEnv` 过滤，未编译或不可用的 local FA 不得进入 function pass。

## 2026-07-13 声明位点注入快照复核

进一步核对确认：当前实现正确冻结了 declaration-time `env_snapshot`，因此声明后的 `use_macro` 不会改变 local macro forms 中宏的名称、alias 和调用参数；attribute 调用也正确使用调用点前的 `passed_forms` 注入。后者是 external/local attribute macro 共用的运行期规则，不是 local-macro 特例。

但 local macro frozen forms 展开时，`expand_request_form/4` 将 declaration 注册时的完整 `SourceView = passed_forms ++ remaining queue` 作为 `ExpandFunction` 的 `InjectForms`。这使 declaration 后方尚未 pass 的原始 attributes 可能进入 `inject_attrs`，不符合“local macro forms 使用 declaration 前 passed forms 注入快照”的契约。现有测试只覆盖 attribute 调用点与最终普通 function pass 的 injection，没有覆盖该边界；相关实现和测试任务已追加到 tasks，而不是视为已完成。

## 对初次 review 的处理

初次 review 中列出的 `FinalLocalEnv` 未接入、重复注册、忽略累计计划、未调用 retained 校验及缺少 function 延后测试，均已由后续 local-macro 重构和测试解决。历史问题不再作为当前待办保留。

## 验证

- `openspec validate macro-passes --strict`：通过。
- `git diff --check`：通过。
- `powershell -ExecutionPolicy Bypass -File scripts\rebar3_sandbox.ps1 ct`：296/296 通过。
