# Task 1 Code Review 1 Retrospective

## Status

Completed.

## Triggering Verdict

`changes_required`

## Root Cause

Task 1 的设计与合同已经明确要求分别覆盖成功 warning 和失败 errors+warnings：`task-1.md` Ordered Step 1 要求 `to_compiler/1` adapter assertions 覆盖失败态，OpenSpec scenario 也以“return monad 失败”为前提。实现同时修改了 `astranaut_return:to_compiler/1` 的 `?RETURN_OK` 与 `?RETURN_FAIL` clauses，但新增测试只通过 `astranaut_return:ok/2` 构造输入。

因此问题不是设计遗漏或合同歧义，而是实现交付未逐项映射合同场景。机械验证门只能证明现有 443 个测试通过；runner 明确没有做 assertion semantics 审查，所以不能发现一个从未进入测试的分支。

## Exact Evidence

- `src/astranaut_return.erl:180-195`：adapter 分别存在于 `?RETURN_OK` 和 `?RETURN_FAIL` 两个 clauses；失败态在约第 190 行进入独立 clause。
- `test/astranaut_SUITE.erl:530-552`：`test_to_compiler_adapter/1` 的 error+warning 断言使用 `astranaut_return:ok([form], ErrorStruct)`；后续 warning-only 断言仍使用 `ok/2`，没有 `astranaut_return:fail(ErrorStruct)` 断言。
- `docs/plan/transform-error/task-1.md` Ordered Step 1：要求 adapter assertions 覆盖“失败 errors+warnings”。
- runner-authored packet：专项 suites 与完整 CT 均 exit 0、443 tests passed；packet 同时声明未进行 semantic source review/code judgment，说明该门按职责完成但不覆盖此类缺失场景。

## Smallest Recurrence-Prevention Correction

实现层只需在现有 `test_to_compiler_adapter/1` 中增加一条失败 return-monad 断言：以同一个包含多文件 errors+warnings 的 `ErrorStruct` 调用 `astranaut_return:fail(ErrorStruct)`，期望 `{error, WrappedErrors, WrappedWarnings}`。不需要修改产品代码、OpenSpec、plan 或 task contract。

## Workflow Assessment

现有 workflow 已要求 Sol 审查 assertion semantics，并成功在本轮发现缺口；没有可复用的 skill gap。因此不写 skill-change specification，也不修改 local workflow skill。

## Routing

下一 worker：`luna_coding_worker`。仅修改 `test/astranaut_SUITE.erl` 添加上述断言，随后按 Task 1 合同重跑 Coding Self-Tests；再由独立 `luna_runner` 重跑 Independent Verification，最后提交 Task 1 Code Review 2。
