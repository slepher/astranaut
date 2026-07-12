# Review: macro-passes-adjusted 实现与测试质量

## 2026-07-12 后续状态

本文件下文保留首次 review 的历史结论。local-macro 后续重构已经解决其中的
未完成项：

- scan 只调用 `register`、`ensure_available` 和 `finalize`，不再自行执行或忽略
  compile plan；最小累计 boundary 与最终强制全量 generation 由
  `astranaut_local_macro` 驱动。
- `FinalLocalEnv` 已过滤并接入最终 function pass。
- retained frozen forms 以最终逐目标环境重新展开并调用 `verify_retained`；目标
  FA 自身和 `internal_function` 由 local 模块从有效环境裁剪。
- local macro function 与普通 function 共用 `astranaut_macro` 的统一引用匹配和
  `expand_functions` 实现，通用展开器没有 local 专属递归分支。
- 新增依赖边界、多环境冲突、缓存、retained helper 与同构环境测试；完整
  Common Test 为 296/296 通过。

## 总体结论

**基本完成。** `tasks.md` 中 6 项实现任务 4 项已完成，2 项部分完成；6 项测试任务中 5 项已覆盖，1 项缺失。整体 spec 覆盖率 12/12 场景。发现 5 个问题。

## 实现对照 (tasks.md)

| # | 任务 | 状态 | 位置 |
|---|------|------|------|
| 1 | 重构 attribute pass，使其携带 ExternalEnv 与 LocalMacroState | ✅ | `run_attribute_macro_pass/5` (line 130) |
| 2 | local_macro 注册和按需可调用性检查接入 | ⚠️ 部分 | 注册 ✅；on-demand 编译忽略 `_Plan`，每次全量编译所有 local macro |
| 3 | scan-and-splice 保留当前位置重扫语义 | ✅ | `map_forms_splice` + `queue_state => true` |
| 4 | 生成的 import/use/macro_options 在处理下一 form 前更新 ExternalEnv | ✅ | env form splice `{splice, []}` 立即生效 |
| 5 | attribute pass 收尾剔除 FinalSkipIds，再运行 function pass | ✅ | `finalize_attribute_macro_pass/6` → `remove_final_skip_forms/2` |
| 6 | FinalLocalEnv 接入 function pass | ⚠️ 未完成 | `FinalLocalEnv` 被 `_FinalLocalEnv` 捕获未使用，function pass 实际通过旧 `LocalMacroMap` 加载 |

## 测试对照 (tasks.md)

| # | 测试项 | 状态 | 证据 |
|---|--------|------|------|
| 1 | 外部与已就绪本地属性宏按同一源码顺序扫描 | ✅ | `test_macro_pass_scan_local_attribute` |
| 2 | 本地属性宏生成环境变更后向前可见、不回扫 | ✅ | `test_macro_pass_local_no_backscan`, `test_macro_pass_generated_local_attribute` |
| 3 | 尚未就绪的本地属性宏调用触发 local-macro 工作流 | ✅ | `ensure_scan_attribute_local_macro` 通过 `macro_pass_scan_local_attr_test.erl` 隐式覆盖 |
| 4 | 属性宏生成 import 按 splice 顺序展开 | ✅ | `test_macro_pass_local_generated_import` |
| 5 | 生成的普通 function 延后到最终函数体展开 | ❌ 缺失 | 无显式测试 |
| 6 | 重新运行现有 uniform macro 套件 | ✅ | 51/51 通过 |

## Spec 场景覆盖率

### 属性宏统一参与 scan-and-splice (4/4)

- 本地属性宏在统一扫描中展开 → `handle_external_attribute` → `ensure_scan_attribute_local_macro`
- 外部属性宏生成后续外部属性调用 → `map_forms_splice` splice 语义
- 普通生成 forms 延后到最终函数体展开 → function/spec 不在 attribute scan 阶段递归展开
- 尚不可调用的本地属性宏触发工作流 → `ensure_scan_attribute_local_macro` 调用 `ensure_available`

### 宏环境前向生效 (5/5)

- 本地属性宏生成 import → `macro_pass_local_generated_import_test.erl`
- 已处理属性不回扫 → `macro_pass_local_no_backscan_test.erl`
- 生成的 import 对同一 splice 后续属性可见 → `handle_external_env_form` splice `{splice, []}` 后立即更新 state
- 生成的 local_macro declaration 进入同一扫描 → `macro_pass_scan_local_attr_test.erl`
- use_macro 同名 option 后者覆盖 → 已有 use_macro merge 逻辑

### 最终展开使用 local-macro 收尾结果 (1/1)

- 最终展开跳过 FinalSkipIds → `remove_final_skip_forms` + function pass 调用链

## 当前流程

```text
parse_transform
  └─ run_attribute_macro_pass
       ├─ run_unified_attribute_scan       (统一直搧: 外部+本地属性宏, local_macro 注册, on-demand 编译)
       ├─ prepare_local_macro_declarations (export_macro 变换, local_macro 元数据采集)
       └─ finalize_attribute_macro_pass    (编译加载 local macro 模块, 收尾 FinalSkipIds, local attribute pass)
  └─ run_function_macro_pass              (function pass: 最终函数体展开, 跳过 FinalSkipIds)
```

## 发现的问题

### 1. `FinalLocalEnv` 未接入 function pass (`src/astranaut_macro.erl:870`)

```erlang
{_FinalLocalEnv, FinalSkipIds, _FinalLocalState} =
    astranaut_local_macro:finalize(retain_roots(Forms), LocalState1)
```

`FinalLocalEnv` 捕获为 `_FinalLocalEnv`（未使用）。function pass 实际通过旧 `LocalMacroMap` → `FinalMacroMap` 使用 local macro。

### 2. `-local_macro` 双重处理

Phase 1 `handle_local_macro_declaration` 通过 `astranaut_local_macro:register/5` 注册，Phase 2 `prepare_local_macro_declarations` 通过 `forms_with_attribute` 再次采集同一属性。

### 3. 缺少"function 延后展开"显式测试

spec 场景: "属性宏展开后生成包含宏调用的 function form → 不在属性扫描阶段递归展开其函数体" 无专门测试用例。

### 4. On-demand 编译忽略增量计划 (`src/astranaut_macro.erl:337`)

```erlang
_Plan <- case astranaut_local_macro:ensure_available({Function, Arity}, LocalState) of
```

`ensure_available/2` 返回最小累计编译计划，但 `_Plan` 被忽略。每次 on-demand 编译全量加载所有 `LocalFunctions`，不按 declaration 顺序分步构建。与 local-macro design 中"按声明顺序最小累计编译"不符。

### 5. `verify_retained/2` 已实现但未调用

`astranaut_local_macro:verify_retained/2` 已实现并通过单元测试，但 `astranaut_macro.erl` 中从未调用。local-macro design 收尾步骤 5 要求"对 retained frozen forms 做 declaration 环境与最终环境比对"，此校验缺失。
