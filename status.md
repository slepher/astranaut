# Astranaut `format_error` 改造状态

更新时间：2026-08-06（Asia/Shanghai）
工作目录：`/home/slepher/project/astranaut`
当前提交：`95661f0 Separate local formatter dependency closure`
计划目录：`docs/plan/format-error-migration/`
当前阶段：`task_planning`
当前任务：Task 3（local formatter dependency closure）

## 目标与已确认约束

- 保持原有 `format_error/1` 行为：未匹配错误仍然兜底格式化，不抛异常。
- 新增 `format_error/2`；传入 `#{default => throw}` 时，未匹配错误必须抛出，以检测 formatter 覆盖遗漏。
- 具体匹配子句改名为 `format_error_1/1`。
- 公共 dispatcher 与默认 fallback 位于 `astranaut_lib`。
- 默认 fallback 名称为 `format_default_error/2`。
- dispatcher 接受可注入的 `FallbackFun/2`，且只把 formatter 自身无匹配的 `function_clause` 当作未覆盖；formatter 内部发生的 `function_clause` 必须保留原堆栈重新抛出。
- `astranaut_test_lib` 仅在发现 formatter 导出 `format_error/2` 时用 `/2` 严格检查；只有 `/1` 时跳过检查。对实际 error 的格式化应确认不抛异常。
- `local_macro` 编译阶段应检测 `format_error` 闭包覆盖情况，不能把这些 formatter 函数一同塞入编译闭包。
- 每个小任务由 Luna 实现，主代理使用 `audit-implementation-simplicity` 两阶段审核；审核不通过则退回 Luna 修改，通过测试后逐步提交。

## 已提交内容

1. `870f574 Add strict format error fallback control`
   - 建立 `format_error/2` 严格模式，同时保持 `/1` 兜底语义。
2. `deb4d00 Check strict formatter coverage in tests`
   - 测试工具仅对存在的 `/2` formatter 执行严格覆盖检查。
3. `44f5a23 Add strict macro error formatting`
   - `astranaut_macro` 迁移至 `/1`、`/2`、`format_error_1/1`。
4. `f37ba34 Add strict do error formatting`
   - `astranaut_do` 完成相同迁移。
5. `f46b1e5 Add strict compile error formatting`
   - compile-meta 与 compile-options 路径完成迁移。
6. `33db3b2 Add strict quote error formatting`
   - `astranaut_quote` 完成迁移；审核发现伪覆盖后已返工为显式匹配子句。
7. `29a01d3 Centralize format error dispatch`
   - 新增 `astranaut_lib:format_error/4` 与 `astranaut_lib:format_default_error/2`。
   - dispatcher 支持传入 formatter 与 `FallbackFun/2`，并区分顶层无匹配和 formatter 内部异常。
8. `1ab6770 Use shared format error dispatcher`
   - macro、do、compile-meta、quote 改用共享 dispatcher，删除重复的 try/catch dispatcher。

9. `4d31096 Add strict struct error formatting`
   - struct formatter 完成 `/1`、`/2` 迁移，并补充严格覆盖测试。
10. `c5b6558 add files`
   - 持久化 format-error-migration 的总计划、Task 1 契约和本状态文件。

上述各阶段均经过对应审核与定向 Common Test；最近一次已提交迁移的定向结果包括 macro 12/12、design 21/21、quote 73/73。

## 当前工作树与 Task 1 / Task 2 证据

- struct 变更已在 `4d31096` 提交；原先关于 struct 未提交的记录已过时。
- coding worker 已修改 Task 1 声明的四个路径，未提交或暂存。
- Coding Self-Test：`rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl`
  退出码 0，21/21 通过。
- Coding Self-Test：`git diff --check` 退出码 0。
- Independent Verification：`rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl`
  退出码 0，21/21 通过；范围、strict contract 和真实诊断归属均通过。
- Review 1：Sol verdict `changes_required`；已写入
  `docs/plan/format-error-migration/task-1-code-review-1.md`。
- 必须修复：保留 `{invalid_rebinding_fun, Function}` 的历史
  `io_lib:write/1` 输出，并锁定精确兼容性断言；清理或中性重命名测试
  accessor 的重复/误导命名。
- Review 2：Sol verdict `passed`；已写入
  `docs/plan/format-error-migration/task-1-code-review-2.md`。
- Review 2 独立验证：rebinding CT 21/21，`git diff --check` 退出码 0，
  无 material finding。
- 已核对 staged 变更：恰为 Task 1 声明的四个实现路径；
  `git diff --cached --check` 退出码 0。
- 提交成功：`72992b3 Add strict rebinding error formatting`，退出码 0。
- Task 1 实现路径已提交；status/review 文档仍是待整理的 workflow metadata。
- Continuity：Sol 选择 `Next Task: task-2`、`Next Sol: reuse`；理由是 Task 2
  直接延续 formatter protocol，且 shared dispatcher、formatter 和测试
  helper 上下文仍然有效。
- Task 2 尚未生成 `task-2.md`，当前阶段为 Task 2 planning。
- Task 2 契约已由 Sol 生成并由 dispatcher 核对落盘；Task 2 已提交。
- Task 3 契约已由新的 write-authorized Sol 直接写入
  `docs/plan/format-error-migration/task-3.md`，当前任务边界已接受。
- Coding Self-Test：`rebar3 compile` 退出码 0。
- Coding Self-Test：`rebar3 ct --suite=test/astranaut_macro_local_SUITE.erl`
  退出码 0，42/42 通过。
- Coding Self-Test：`rebar3 ct` 退出码 0，439/439 通过。
- Coding Self-Test：`git diff --check` 退出码 0；范围恰为 Task 3 两个路径。
- 下一步：独立 Luna runner 重跑 Task 3 验证，随后交给 Sol 审核。
- Independent Verification：compile、local suite 42/42、完整 CT 439/439、
  `git diff --check` 均退出码 0；但发现
  `local_formatter_info/1` 在 `local_macro_definitions/5` 与
  `compile_boundary/3` 重复计算，违反 Task 3 的 derive-once/thread invariant。
- 下一步：Sol 写入 Task 3 Code Review 1 与 retrospective，解释设计为何未阻止
  重复计算；随后 Luna 返工并重跑两层测试。
- Review 1：Sol verdict `changes_required`；已直接写入
  `task-3-code-review-1.md` 与 `task-3-code-review-1-retrospective.md`。
- 必须修复：`local_formatter_info/1` 只推导一次并在线程中复用；补充直接
  证明 generation-key/callable-state 不受 formatter 污染的测试或证据。
- 下一步：Luna 依据 Review 1 返工并重跑 Coding Self-Tests。
- Rework Coding Self-Test：compile 退出码 0；5 个 focused formatter case
  均 1/1；local suite 42/42；完整 CT 439/439；`git diff --check` 均通过。
- `local_formatter_info/1` 当前仅在 `src/astranaut_macro_local.erl:802`
  推导一次，boundary/callable identity assertions 通过。
- 下一步：独立 Luna runner 重跑修复后的 Task 3 验证。
- 修复后 Independent Verification：compile、local suite 42/42、完整 CT
  439/439、`git diff --check` 均退出码 0；但 runner 发现
  `formatter_info_for_source/2` 仍在 `handle_form/3`、
  `finish_attribute_pass/3`、`ensure_formatter_info/1` 多路径推导，
  derive-once/thread invariant 仍未满足。
- 下一步：Sol 写入 Task 3 Code Review 2 和 retrospective，解释首轮返工为何
  仍未消除重复推导；随后再次返工。
- Review 2：Sol verdict `changes_required`；已直接写入
  `task-3-code-review-2.md` 与 `task-3-code-review-2-retrospective.md`。
- Skill 改进的历史决策已分离为
  `task-3-code-review-2-skill-change-spec.md`；当前规则以本地 `SKILL.md` 为准。
- 必须修复：消除 `handle_form/3`、`finish_attribute_pass/3`、
  `ensure_formatter_info/1` 的多阶段重复推导，确保单一声明时间点产出并
  线程传递同一 `local_formatter_info`。
- 下一步：Luna 第二次返工并重跑 Coding Self-Tests。
- 第二次返工 Coding Self-Test：compile 退出码 0；focused formatter CT
  5/5；local suite 42/42；完整 CT 439/439；`git diff --check` 退出码 0；
  scope 恰为两个 Task 3 路径。
- 下一步：独立 Luna runner 重跑第二次返工后的 Task 3 验证。
- 第二次返工 Independent Verification：compile、local suite 42/42、完整 CT
  439/439、`git diff --check` 均退出码 0；runner 确认单一 guarded producer
  和线程传递通过，无 staged/deletion。
- Task 3 Review 3：Sol verdict `passed`，已直接写入
  `task-3-code-review-3.md`；未创建 retrospective 或 skill-change-spec artifact。
- staged scope 核对：恰为 Task 3 声明的两个产品路径；
  `git diff --cached --check` 退出码 0。
- 提交成功：`95661f0 Separate local formatter dependency closure`，退出码 0。
- Continuity：Sol 选择 `Next Task: task-4`、`Next Sol: reuse`；理由是 Task 4
  直接验证 Task 3 的 formatter protocol、closure 和 boundary invariants。
- 下一步：复用 Sol 直接写入 `docs/plan/format-error-migration/task-4.md`。
- Coding Self-Test：`rebar3 ct --suite=test/astranaut_SUITE.erl` 退出码 0，
  37/37 通过。
- Coding Self-Test：`rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl`
  退出码 0，21/21 通过。
- Coding Self-Test：`rebar3 ct` 退出码 0，434/434 通过。
- Coding Self-Test：`git diff --check` 退出码 0。
- Independent Verification 已由独立 Luna runner 完成：`astranaut_SUITE`
  37/37、rebinding 21/21、完整 CT 434/434，均退出码 0；
  `git diff --check` 退出码 0；无 staged 路径；无 `src/` 变更。
- 流程改进：历史状态曾记录 Sol 写入 Task 2 retrospective 并更新
  `local-workflow/SKILL.md`，明确 Sol 不执行 CT/测试/构建/验证，
  独立验证命令只由 Luna runner 执行；当前工作树中没有可迁移的
  Task 2 retrospective 文件。
- Task 2 Review 1：Sol verdict `passed`，已直接写入
  `docs/plan/format-error-migration/task-2-code-review-1.md`；无 material finding。
- staged scope 核对：恰为 Task 2 声明的三个产品路径；
  `git diff --cached --check` 退出码 0。
- 提交成功：`9fe3c3d Tighten formatter protocol checks`，退出码 0。
- Continuity：Sol 选择 `Next Task: task-3`、`Next Sol: fresh`；理由是任务转向
  local formatter 数据建模与生成模块组装，需新的决定性上下文。
- 下一步：Luna runner 收集 Task 3 Evidence Focus，随后 fresh Sol 直接写
  `task-3.md`。

## 后续步骤

1. 交给 Luna 实现 Task 2 并运行 Coding Self-Tests。
2. 按新 code-review/retrospective/skill-change-spec 分工完成 Sol
   裁决、必要返工和提交。

## 代理状态

- `/root/migrate_shared_dispatcher`：已完成。
- `/root/struct_shared_formatter`：已完成。
- recovery runner：已完成；无当前运行中的子任务。

## Token 路由备注

当前会话根代理是 Sol，无法在会话中交换父子身份。若要由 Luna 作为 dispatcher、Sol 仅作为架构或审核 worker，需要新建以 Luna 为根代理的会话，并以 `fork_turns = "none"` 给 Sol 传递精简任务。预计可减少约 50%–90% 的 Sol token，但多代理总 token 不一定下降。
