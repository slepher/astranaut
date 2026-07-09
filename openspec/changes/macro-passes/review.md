# Macro Passes 审核

对比 `src/astranaut_macro.erl` 未提交 diff 与 openspec 文档（proposal.md / design.md / specs/macro-expansion/spec.md）。

---

## 已实现 — 与文档一致

### 外部属性阶段

| 需求 | 来源 | 实现 |
|---|---|---|
| 从空环境起步，逐行扫描 | design L57-70 | `run_external_macro_pass` L856-862 → `new_external_env` 起始 `#{}` |
| 按源码顺序展开外部属性宏 | spec | `scan_external_macro_pass` L885-899 + `scan_external_macro_form` L901-924 |
| 环境形式优先路由 | design "外部属性阶段" | `is_external_env_form` L926-929 → `apply_external_env_form` L945-999 |
| 生成 forms 立即插回重扫 | spec | `splice_generated_forms` L1046-1049 把生成 attributes 前置 + `insert_forms` 其余 |
| 环境增长前向生效，不回扫旧 form | spec | `Acc` 单向累积，旧 form 不重访 |
| 外部宏环境冻结后传入本地阶段 | spec | `ExternalEnv` 由 `scan_external_macro_pass` 返回后不再变 |
| import_macro / use_macro / macro_options 均在扫描中即时更新环境 | design "外部属性阶段" | `apply_external_env_form` L945-999 三条子句分别处理 |
| 外部属性阶段结束后 environment 可见最终 GlobalMacroOpts | spec | `external_env_global_macro_opts` L870 传入后续阶段 |

### ExternalEnv 结构

| 字段 | 用途 |
|---|---|
| `global_macro_opts` | 可变的全局宏选项（由 `macro_options` 即时更新） |
| `module_macro_maps` | 按模块组织的宏导入映射（由 `import_macro` / `use_macro` 即时更新） |
| `macro_map` | `module_macro_maps` 的 uniform 视图（派生字段，惰性更新） |

关键：`GlobalMacroOpts` 不再在加载时烘焙进各宏条目。`import_macro_form` L1005-1036 仍然做 `maps:merge(GlobalMacroOpts1, ...)`，但此时 `GlobalMacroOpts1` 是**当时扫描点的瞬时值**，而非预设的全局常量。

### extra_functions

| 需求 | 来源 | 实现 |
|---|---|---|
| 作为 local_macro / export_macro 定义选项 | spec | `macro_definition_validator` L526 |
| FA 格式校验 | spec | `validate_function_with_arity/1` L529-532 |
| 并集语义 | spec | `local_macro_extra_functions/1` L676-681 `ordsets:union` |
| 未定义函数报 `invalid_extra_functions` | spec | `local_macro_extra_functions/2` L662-668 |
| 按 source 过滤（仅 local_macro 的 extra_functions 进锁定集） | - | `local_macro_extra_functions/2` L670-676 with `Source` atom filter |
| 并入本地宏相关函数集合 | design | `uniform_macro_context` L654-655 |

### internal_function

| 需求 | 来源 | 实现 |
|---|---|---|
| 策略语法校验 | spec | `validate_internal_function_policy/1` L539-553 |
| 按具体 FA 检测冲突 | spec | `assert_internal_function_policies/2` L683-704 |
| true 作用于全闭包，list 作用于指定 FA | spec | `internal_direct_functions/2` L706-710 |
| 在 local_macro 加载完成后立即校验 | spec "冲突检测必须在…早于本地快照锁定" | `load_local_macro_attributes` L197 — 每步 `update_local_macros` L284 均调用 |
| 错误名 `conflicting_internal_function_policy` | design | L699 |

### 本地阶段保护

| 需求 | 来源 | 实现 |
|---|---|---|
| 本地属性宏展开后检查环境 mutation | spec | `assert_no_macro_environment_mutation/1` L1052-1059 |
| 黑名单 | design | `is_macro_environment_form/1` L1061-1066 |
| 锁定快照仅包含 local_macro 闭包（排除 export） | spec L89-91 | `LockedLocalMacroRelatedFunctions` 由 `local_macro_functions(LocalMacroMap, local_macro)` 计算 |
| 锁定快照追踪 | spec | `local_macro_snapshot_form_ids/2` L799-812 |
| 改写锁定 form 报错 | spec | `assert_no_locked_snapshot_mutation/2` L1068-1075 |
| 错误名 `illegal_macro_environment_mutation` | design | L1057 |
| 错误名 `illegal_local_macro_definition_mutation` | design | L1073 |

### 宏定义闭包分析分离

| 集合 | 来源 | 用途 |
|---|---|---|
| `MacroDefinitionFunctions` | `local_macro_functions(LocalMacroMap)` — 全部（export + local） | 编译本地宏模块 |
| `LocalMacroFunctions` | `local_macro_functions(LocalMacroMap, local_macro)` — 仅 local | 锁定快照 |
| `MacroDefinitionRelatedFunctions` | `MacroDefinitionFunctions` + 全部 `extra_functions` 的闭包 | 编译、排除最终展开 |
| `LockedLocalMacroRelatedFunctions` | `LocalMacroFunctions` + local_macro `extra_functions` 的闭包 | 锁定保护 |

export_macro 的 helper 闭包进入 `MacroDefinitionRelatedFunctions`（用于编译和 internal_function 冲突检测），但**不进入** `LockedLocalMacroRelatedFunctions`（不锁定），与 spec 一致。

### 本地属性宏扫描改为顺序

`transform_local_attribute_macros` L1038-1043 → `scan_local_attribute_macros` L1045-1049 → `scan_local_attribute_form` L1051-1070。逐 form 处理，生成结果 `splice_generated_forms` 插回，与外部阶段一致的顺序语义。不再使用旧的 `astranaut:map_m` 批量遍历。

### 测试

| 测试用例 | 验证内容 | 对应 spec/tasks |
|---|---|---|
| `test_macro_pass_generated_import` | 外部属性宏生成 import_macro → 后续属性可见 | spec "后续属性能看到新导入的外部宏" |
| `test_macro_pass_no_backscan` | 较早的属性不会被后续导入回溯展开 | spec "更早的属性不会被回溯重展开" |
| `test_macro_pass_local_attribute_chain` | 本地属性宏生成另一个本地属性宏 → 链式展开 | tasks L54 |
| `test_macro_pass_generated_macro_options` | 外部属性宏生成 macro_options+import → 影响后续宏行为（max_depth） | spec "外部属性阶段中生成的普通 forms 不做最终展开" + macro_options 生效 |
| `test_macro_pass_export_helper_unlocked` | export_macro 的 helper 不被锁定，可被本地属性重写 spec | spec "导出宏 helper 闭包被分析但不锁定" |
| `test_macro_pass_internal_function_conflict` | 共享 helper 在不同宏间 internal_function 策略冲突 | spec "共享函数处理冲突时报错" |
| `test_macro_pass_local_environment_mutation_errors` | 本地属性生成 import_macro / local_macro → illegal_macro_environment_mutation | spec "本地属性宏生成 import_macro" / "本地属性宏生成 local_macro" |
| `test_macro_pass_locked_snapshot_mutation_error` | 本地属性改写锁定 helper → illegal_local_macro_definition_mutation | spec "本地属性改写锁定 helper" |

---

## 偏差与问题

### 1. macro_options 仍被烘焙进宏条目（延迟问题）

`import_macro_form` L1005-1036 在加载时即执行 `maps:merge(GlobalMacroOpts1, MacroOptions)` 将当前 GlobalMacroOpts 烘焙进各宏条目。虽然 `GlobalMacroOpts` 现在随扫描即时更新（L991-997），但已入 MacroMap 的条目仍绑定的是**加载当时的 GlobalMacroOpts 快照值**。

例如：
```
-macro_options({debug, true}).         → GlobalMacroOpts = #{debug => true}
-import_macro(A).                      → A 条目 baked: #{debug => true}
[展开生成] -macro_options({max_depth, 50}).  → GlobalMacroOpts = #{debug => true, max_depth => 50}
[展开生成] -import_macro(B).           → B 条目 baked: #{debug => true, max_depth => 50}  ✓
```

B 能看到完整选项，因为它在 max_depth 之后才加载。但 A 的条目仍是加载当时的快照——这就是 design 要求的「瞬时 Union」效果。**实际影响有限**：同一个模块只导入一次，后续 use_macro 可以覆盖选项；且 GlobalMacroOpts 变化主要影响**之后**加载的模块。

结论：当前实现已满足「后加载的模块看到即时 GlobalMacroOpts」的语义，瓶颈仅在同一个模块不会被后续 GlobalMacroOpts 变化重新染色——这在设计上合理。

### 2. global_macro_update_validator 与 global_macro_validator 重复

两个 validator 功能重叠，`global_macro_update_validator` L517-524 是 `global_macro_validator` L506-513 的子集。可合并为一个。

### 3. import_macro 失败时 Continue 仍被调用

`apply_external_env_form` L957: `import_macro_form` 返回 `{error, Error}` 时，走 `formatted_error` + `Continue(Env)` 分支。这意味着 import 失败不会中断扫描，只会产生错误信息——合理，但需确认是否为预期行为。

---

## 未完成的 tasks.md 项目

| # | 项目 | 状态 |
|---|---|---|
| 测试 L53 | use_macro alias 前向可见 | 未实现 |
| 测试 L56-61 | extra_functions 系列（补充 helper、未定义函数报错、并集合并） | 未实现 |
| 测试 L63 | 无冲突的 internal_function 编译成功 | 未实现 |
| 测试 L65 | internal_function direct-call 生效 | 未实现 |
| 测试 L69 | locked spec mutation 报错 | 未测试 |
| 测试 L71 | 最终展开跳过锁定快照 | 未测试 |
| 规格 L1-24 | 各项 spec "记录" 条目 | 未勾选 |

---

## 总结

| 项目 | 状态 |
|---|---|
| 外部属性阶段（单 pass、空环境起步、顺序扫描、即时回插） | ✓ |
| ExternalEnv 结构（global_macro_opts 随扫描即时更新） | ✓ |
| macro_options 在外部属性扫描中生效 | ✓ |
| extra_functions | ✓ |
| internal_function 校验 + 冲突检测 | ✓ |
| 导出宏闭包不锁定 | ✓ |
| 锁定快照仅 local_macro 闭包 | ✓ |
| 本地属性宏顺序扫描 + splice | ✓ |
| 本地 phase mutation 保护 | ✓ |
| format_error 新错误名 | ✓ |
| macro_options 烘焙延迟（同模块不重新染色） | 设计合理 |
| global_macro_update_validator 重复 | 轻度冗余 |
| 全部测试 (235/235) | 通过 |
