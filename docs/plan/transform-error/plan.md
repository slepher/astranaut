# transform-error 总计划

## Goal

`transform-error`：以 `openspec/changes/transform-error/` 中当前 proposal、design、tasks 和 capability spec 为产品约束，在 `astranaut_return:to_compiler/1` 统一把领域 diagnostic 包装成 `astranaut_lib` compiler adapter，让领域 `format_error/1` 保持纯 clauses，并为缺失用户 macro formatter 提供一次性 warning。

initiative 路径只用于 workflow 持久化；OpenSpec 是独立规格输入。`docs/plan/format-error-migration/`、根 `status.md`、其他旧计划和任何旧 strict `/2` 兼容语义均不属于本 Goal。

## 当前事实与技术判断

- 事实：`astranaut_lib` 当前公开 `dispatch_error/3` 和 `format_default_error/2`，并通过 stack frame 区分顶层 no-match 与 formatter 内部 `function_clause`（`src/astranaut_lib.erl:24`, `src/astranaut_lib.erl:612-652`）。
- 事实：多个生产 formatter 仍公开 `/2`，通过 `format_error_1/1` 和 `#{default => throw}` 实现 strict dispatch，例如 `src/astranaut.erl:592-614`、`src/astranaut_macro.erl:18-55`、`src/astranaut_do.erl:36-50`。
- 事实：local macro formatter 仍建模为 `none | legacy | strict`，strict closure 会复制并导出 `/1` 和 `/2`（`src/astranaut_macro_local.erl:31-38`, `src/astranaut_macro_local.erl:795-846`）。
- 事实：测试 helper 优先调用 formatter `/2` 并传入 throw option（`test/astranaut_test_lib.erl:115-148`）。
- 事实：external registry 已检查 `/1` 并在缺失时选择 `astranaut_macro`，但不发 warning（`src/astranaut_macro_registry.erl:355-391`）；local protocol 同样会 fallback，但没有所需诊断。
- 事实：`astranaut_return:to_compiler/1` 当前只调用 `astranaut_error:realize/1` 并原样输出 `{Pos, Formatter, Reason}`；它是内部 diagnostic 到 compiler return 的统一边界。
- 判断：最初 OpenSpec 错误地要求每个领域 `/1` callback 内调用 shared dispatcher，当前 Task 1 product diff 也实现了该错误结构。修订后 Task 1 必须返工为 `to_compiler` adapter + 纯领域 callbacks；缺失 formatter warning仍作为后续独立行为任务。

## 全局约束与不变量

- `astranaut_error:realize/1` 的内部 `{Pos, DomainFormatter, Reason}`、reason 所有权、文件、位置和 parse-transform 行为不变。
- `astranaut_return:to_compiler/1` 输出 `{Pos, astranaut_lib, {DomainFormatter, Reason}}`；文件分组、顺序和 error/warning 分类不变。
- shared helper 捕获 formatter 执行动态范围内任意 `error:function_clause` 并 fallback；不检查 stack frame。其他 class/reason 必须连同原 stacktrace 传播。
- 默认格式化仅为：deep character list 原样返回，否则 `io_lib:write/1`；没有 options、throw mode 或公开 default helper。
- `astranaut_lib:format_error/1` 是 compiler adapter callback，`format_error/2` 接受 Reason 与一元 formatter fun。
- production 领域 formatter surface 只有纯 `format_error/1` clauses；callback 不调用 shared helper，不保留通用 `format_error_1/1`、generic catch-all 或 `/2`。
- 动态 formatter module 调用统一适配为 `fun Module:format_error/1`，不增加 module-specific dispatch 分支。
- local generated formatter 只复制/导出 `/1` 及其真实 closure 依赖；仅 `/2` 等同缺失。
- missing formatter warning 使用 `{missing_macro_formatter, Module}`，formatter 固定为 `astranaut_macro`；每个 provider 在一次 source module 编译中最多一次，且不阻断注册或展开。
- 每个 task 的 coding self-tests 由 coding worker 执行；Independent Verification 由另一个 `luna_runner` 独立执行。Sol 不运行这些命令。
- 不修改本 initiative 的 `status.md`；不读取或复用相似历史计划。

## 有序任务

### Task 1 — 原子切换 to_compiler adapter 与纯 `/1` 协议

- 目标：完成 OpenSpec 1.1–1.4、2.1–2.4、3.1–3.3 和 5.1，使 `to_compiler/1` 统一包装 compiler diagnostics，领域 callbacks 保持纯粹，并移除 strict `/2`、options、throw mode、`dispatch_error/3` 和通用 `format_error_1/1`。
- Owned area：`astranaut_return` compiler conversion；`astranaut_lib` adapter API；所有 production formatter callback；local formatter protocol/closure；formatter 测试 helper、相关 suites 和 fixtures。
- 行为边界：不加入 missing formatter warning；仅 `/2` provider 在本任务后只能被视为没有 formatter 并继续使用 framework formatter。
- 前置条件：当前 OpenSpec 四个文档保持一致；没有待归属的产品 diff。
- 验证：内部 realize ownership、to_compiler wrapper、shared helper 语义、纯 formatter API/closure contract、相关 CT suites 和静态残留审计。
- 完成标准：新 API 和所有实际调用方在同一变更中可用；生产源码无被删除协议；领域消息和未知 fallback 均有精确断言；专项与完整的 Task 1 自测层通过。

### Task 2 — 缺失 macro formatter warning 与最终验收

- 目标：完成 OpenSpec 4.1–4.4 和 5.2–5.4，在 external/local 检测边界发出一次性 warning，并完成 initiative 全量验证。
- Owned area：`astranaut_macro_registry` 的 external provider 检测与 source-compilation 去重状态；`astranaut_macro_local` 的首次 formatter protocol 检测；`astranaut_macro` warning 文案；macro scan/error/local/pass/uniform suites 及必要 fixtures。
- 行为边界：external 缺失 `/1` 时 descriptor 继续使用 `astranaut_macro`；local warning identity 必须是声明 source module，不是 generation module；仅 `/2` 必须告警；重复 import/use/call 或多个 local declarations 不重复告警。
- 前置条件：Task 1 已提交并彻底移除 strict protocol；不能为了减少 warning 而恢复 `/2` 兼容。
- 实现约束：warning 必须在现有 traverse/return monad 链内串联并由节点边界补位置；不得以普通逗号丢弃 monadic action。去重状态归属于一次 source module compile，不得使用 process-global 状态。
- 验证：external、local、only-v2、重复 provider 场景；既有 macro 注册/展开继续成功；完整 CT、xref、dialyzer、OpenSpec strict validation 与 diff check。
- 完成标准：所有规格场景均有断言，warning 次数、identity、formatter 和 continuation 行为准确；全量验证通过；无 strict formatter 残留。

## 依赖顺序与结束条件

Task 1 必须先按修订合同返工、验证并提交；旧匿名-dispatch callback 实现及其既有自测结果不构成验收证据。Task 2 不得与 Task 1 混合提交。Task 2 通过独立验证和 Sol review 后，本 Goal 完成。

出现以下任一情况立即停止并返回 Sol：规格要求兼容旧 `/2`；需要改变内部 `astranaut_error:realize/1` tuple、reason 或位置；warning 去重需要跨 source compilation 的全局状态；发现未列入当前 task 且无法安全归属的产品改动；测试要求与 OpenSpec 的 adapter/fallback 语义相冲突。
