# Astranaut `format_error` strict coverage 改造计划

## 规划来源与执行协议

本计划由只读 `sol_planner_reviewer` 基于实际源码、测试和当前提交 `4d31096` 生成，dispatcher 已核对其关键路径后持久化。

执行链固定为：

1. Sol 生成或更新本总规划，列出有序任务、边界、不变量和验证标准。
2. 每个待实施任务单独写入 `task-N.md`，一个文件只描述一个封闭实施任务。
3. Luna coding worker 只实现当前 `task-N.md`，不得自行扩展架构范围。
4. Luna runner 执行定向验证并返回命令、状态和证据。
5. dispatcher 将实际 diff 和验证证据交给 Sol 审核。
6. 每次审核结果写入对应的 `task-review-N.md`；有实质问题时先修复，再重新审核同一任务。
7. dispatcher 完成最终验收、提交或状态维护。

当前已生成：

- [task-1.md](task-1.md)：`astranaut_rebinding` strict formatter 迁移。

当前尚未生成 `task-review-1.md`，因为任务 1 尚未实施，也没有可供 Sol 审核的实际 diff。

## 总目标

- 保持现有 `format_error/1` 的默认 fallback 行为。
- 增加 `format_error/2` strict 模式；`#{default => throw}` 下未覆盖 reason 必须抛出。
- 具体 formatter 子句使用 `format_error_1/1`，公共 dispatcher 使用 `astranaut_lib:format_error/4`。
- formatter 内部发生的 `function_clause` 必须保留原堆栈重新抛出；只有 formatter 自身顶层不匹配才触发 fallback。
- local macro 生成模块可以携带 formatter 入口及其普通本地 helper，但 formatter 不得成为 local macro compilation boundary 的成员或闭包状态。
- 测试 helper 对导出的 `/2` 执行 strict 覆盖检查；只有 `/1` 的 legacy formatter 保持兼容但不伪称已完成 strict coverage。

## 已确认的架构边界

### Formatter 协议

- `/1` 是 Erlang compiler formatter 的身份锚点。
- `/2` 是 Astranaut strict coverage 扩展。
- 只有 `/2` 没有 `/1` 时，不得把模块选为 formatter；继续使用 `astranaut_macro` 或其他既有 fallback。
- `astranaut_disable_tco` 只产生普通转换结果，没有结构化 error/warning 路径，因此保留 `/1` 是合理的，不为接口一致性强加 `/2`。
- `astranaut_rebinding` 会产生 `{invalid_rebinding_fun, Other}`，并会通过通用 validator 产生结构化 reason，因此必须迁移到 `/1` + `/2`。

### Local macro 生成边界

必须严格区分：

- `Members`：真正声明、计划、冻结、编译和提交的 local macro 函数。
- `FormatterExports`：生成模块需要导出的 `format_error/1`，以及存在时的 `format_error/2`。
- `FormatterRelated`：formatter 入口调用到的私有 `format_error_1/1` 和普通本地 helper 的传递闭包。

formatter 只允许进入临时生成模块的 forms 和 export forms，不得进入：

- `Boundary.members`
- request 的 `closure_ids` / `closure_fas`
- `candidate_local_macros`
- `frozen_forms`
- whitelist observation
- generation boundary key
- `commit_compiled/3` 的 `Members`
- retain roots、retained IDs、callable 状态或 macro environment fingerprint

formatter 信息必须从同一声明时间点的 `SourceView` 推导，不能向后扫描未来源码。

## 有序任务

### Task 1：rebinding strict formatter

文件：

- `src/astranaut_rebinding.erl`
- `test/astranaut_rebinding_SUITE.erl`
- rebinding invalid function/option fixtures

内容：导出 `/2`，`/1` 包装 `/2`，用 `astranaut_lib:format_error/4` 只覆盖 `{invalid_rebinding_fun, Function}`，通用 validator reason 委托 `astranaut:format_error/2`。验证 strict 未知 reason 抛出、真实 warning 归属不变。

详细实施契约见 [task-1.md](task-1.md)。

完成后：

```bash
rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl
git diff --check
```

通过后由 dispatcher 复核，再生成 [task-review-1.md](task-review-1.md)。

### Task 2：收紧测试 helper 的 formatter 协议

文件：

- `test/astranaut_test_lib.erl`
- 对应 helper 协议测试 suite

实施边界：

- 同时有 `/1`、`/2`：调用 `/2(Error, #{default => throw})`，要求返回非空字符列表。
- 只有 `/1`：调用 `/1` 并验证非空字符列表，标记为 legacy，不宣称 strict coverage。
- 只有 `/2` 或没有 `/1`：测试失败，因为 compiler formatter 入口不可用。
- 不改变 `astranaut_lib:format_error/4` 的 nested `function_clause` 传播语义。

完成标准：helper 不再静默跳过 `/1`，并能区分 legacy、strict、无效 formatter 协议。

### Task 3：local formatter 数据建模与生成模块组装

文件：

- `src/astranaut_macro_local.erl`
- `test/astranaut_macro_local_SUITE.erl`

实施边界：

1. 使用 `function_clauses_map/2` 识别 `/1`、可选 `/2`，建立 `formatter_protocol` / `local_formatter_info`，三态为 `none`、`legacy`、`strict`。
2. `local_macro_definitions/5` 只以 `/1` 判断是否将生成模块设为 formatter；只有 `/2` 时继续使用 `astranaut_macro`。
3. `compile_boundary/3` 保持原始 `Members` 和 `commit_compiled(Members, ...)`；单独计算 formatter exports 与 related closure。
4. `load_local_macro_forms` 可扩展为携带 formatter info 的参数；只有存在真正 `Members` 时才编译生成模块。
5. `select_local_macro_forms/3` 选择 `Members ∪ MacroRelated ∪ FormatterRelated`；`local_macro_exports/1` 只导出 `Members ∪ FormatterExports`。
6. 私有 `format_error_1/1` 和普通 helper 必须通过现有 `astranaut_macro_local` 的 `forms_id_map/1`、`closure/5`、`analyze_closure_function/4` 传递选入，不得新造第二套 AST call walker。
7. 删除或停用无法表达三种集合的 `maybe_add_formatter/2`；若 `has_function/3` 无其他调用则删除。

必须保护声明快照、boundary 合并、canonical cache、retain、callable 和 fingerprint 语义。formatter 是生成模块附属入口，不是 macro environment 输入。

完成标准：formatter `/1`、`/2` 能进入正确生成模块，私有 helper 可用且不导出；compile plan、closure、frozen、generation 和 retain 不被污染。

### Task 4：三态 local formatter 集成测试

文件：

- `test/astranaut_macro_SUITE_data/macro_local_formatter_legacy_test.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_only_v2_test.erl`
- `test/astranaut_macro_error_SUITE.erl`
- `test/astranaut_macro_local_SUITE.erl`

必须覆盖：

- only `/1`：生成模块导出 `/1`，不要求 `/2`。
- `/1` + `/2`：生成模块导出两者，strict 调用成功。
- `/2` 调用私有 `format_error_1/1`：成功但 helper 不导出。
- formatter helper 再调用普通 helper：传递闭包完整。
- only `/2`：不选本地 formatter，回退到 `astranaut_macro`。
- compile plan 的 `members`、request closure 和 frozen IDs 不包含 formatter。

完成标准：三态协议和 local macro 生命周期不变量都有可失败的具体断言。

### Task 5：迁移真实 local macro diagnostic fixtures

文件：

- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`
- `test/astranaut_macro_error_SUITE.erl`

将 fixture 的自定义 reason 移到私有 `format_error_1/1`，增加 `/1` 包装和 `/2` dispatcher；保持现有 `noop`、`bar`、macro exception、sibling error 的消息、位置、数量和 reason 不变。测试必须证明真实生成 local module 的 `/2` 被 strict helper 调用。

### Task 6：完整回归与最终验收

执行：

```bash
rebar3 ct
git diff --check
git status --short
```

验收：所有 Common Test 通过；无新的 local module 编译 warning；formatter 不改变 generation、retain、callable、fingerprint、诊断顺序或旧 `/1` 行为。最终 review 结果写入相应 `task-review-N.md`，再由 dispatcher 决定提交。

## 未决项

- `rebinding_all` 非法值的 validator payload 必须由 Task 1 的首次定向测试确认，不能在计划中凭空固定内部 tuple。
- 当前没有公开 formatter-closure API；Task 3 应复用 `astranaut_macro_local` 的现有 closure 分析函数，而不是扩展公开接口。
- formatter helper 内尚未展开的 macro call 不在本计划范围；现有 `/1` 路径也有同一限制。
