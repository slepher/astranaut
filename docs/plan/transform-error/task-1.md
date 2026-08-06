# Task 1 — 原子切换 to_compiler adapter 与纯 `/1` formatter 协议

## Goal 与范围

Goal：`transform-error`。

本任务按已修订的 `openspec/changes/transform-error/` 返工现有未提交实现：由 `astranaut_return:to_compiler/1` 统一适配 compiler diagnostics，领域 formatter 恢复为纯 `format_error/1` clauses，`astranaut_lib` 独占 compiler callback、shared dispatch 和 fallback。

不得实现 `{missing_macro_formatter, Module}` warning；该行为属于 Task 2。不得复用 `docs/plan/format-error-migration/` 或旧版 `transform-error` 的匿名-dispatch callback 设计。

## Decisive Evidence

- `astranaut_error:realize/1` 返回按文件分组的内部 `{Pos, DomainFormatter, Reason}`，保留诊断所有权。
- `astranaut_return:to_compiler/1` 当前只将 realize 结果装配为 parse-transform return，是内部诊断进入 OTP compiler 的统一边界。
- OTP 对 compiler diagnostic `{Pos, Module, Payload}` 直接调用 `Module:format_error(Payload)`。
- 因而中央 fallback 与纯领域 callback 能同时成立的结构是：`to_compiler/1` 输出 `{Pos, astranaut_lib, {DomainFormatter, Reason}}`；`astranaut_lib:format_error/1` 再以 `fun DomainFormatter:format_error/1` 调用 shared `/2`。
- 当前 worktree 中多个 formatter 已按旧合同改成 callback 内 `astranaut_lib:format_error/2` + anonymous fun。该实现是明确的返工对象，不是可保留的兼容形式。
- 当前已记录的 Task 1 自测发生在设计修正之前，全部失效；返工后必须重新执行完整 Coding Self-Tests。

## Approach

先用测试固定内部协议、compiler adapter 协议和最终文本协议三层边界，再返工产品代码：

```text
astranaut_error:realize/1
  → {Pos, DomainFormatter, Reason}

astranaut_return:to_compiler/1
  → {Pos, astranaut_lib, {DomainFormatter, Reason}}

OTP compiler
  → astranaut_lib:format_error({DomainFormatter, Reason})
  → astranaut_lib:format_error(Reason, fun DomainFormatter:format_error/1)
```

领域 formatter 只保留直接 reason clauses。未知 reason 直接调用领域 `/1` 时抛 `function_clause`；经 adapter 调用时由 shared dispatcher fallback。不要在领域 callback 中内联 anonymous dispatcher、添加 catch-all，或恢复 `_1`。

## Owned Files / Modules

预期 tracked paths（只允许按实际需要修改其中路径）：

- `src/astranaut_return.erl`
- `src/astranaut_lib.erl`
- `src/astranaut.erl`
- `src/astranaut_macro.erl`
- `src/astranaut_macro_local.erl`
- `src/astranaut_quote.erl`
- `src/astranaut_do.erl`
- `src/astranaut_rebinding.erl`
- `src/astranaut_struct_transformer.erl`
- `src/astranaut_compile_meta_transformer.erl`
- `src/astranaut_compile_opts.erl`
- `src/astranaut_disable_tco.erl`
- `src/astranaut_struct.erl`
- `test/astranaut_return_SUITE.erl`
- `test/astranaut_test_lib.erl`
- `test/astranaut_SUITE.erl`
- `test/astranaut_design_SUITE.erl`
- `test/astranaut_macro_error_SUITE.erl`
- `test/astranaut_macro_local_SUITE.erl`
- `test/astranaut_rebinding_SUITE.erl`
- `test/astranaut_struct_SUITE.erl`
- `test/disable_tco_SUITE.erl`
- `test/astranaut_SUITE_data/sample_transformer_1.erl`
- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl`
- `test/astranaut_macro_SUITE_data/macro_uniform_a.erl`
- `test/astranaut_macro_SUITE_data/macro_validator_slots.erl`

若 repository 没有 `test/astranaut_return_SUITE.erl` 或现有 return tests 的正确 seam 在其他既有 suite，可在上述已有 test files 中放置 adapter assertions，不为此新建平行 harness。

`sample_transformer_only_v2.erl` 与 `macro_local_formatter_only_v2_test.erl` 保持 only-v2 negative fixtures，不得改成有效 `/1` provider。允许的新 untracked 产品路径和授权文件删除均为无。

已有 `.codex/skills/local-workflow/*` 修改不属于 coding scope；不得覆盖、调整或暂存。不得修改 OpenSpec、workflow 文档、`status.md`、staging 或 commits。

## Invariants

- `astranaut_error:realize/1` 继续返回原始 DomainFormatter 和 Reason，不做 adapter 包装或文本格式化。
- `to_compiler/1` 对 errors 与 warnings 逐项包装，并保持文件分组、顺序、位置、分类与 payload identity。
- `astranaut_lib:format_error/1` 只接受 `{Module, Reason}` adapter payload；`/2` 第二参数为一元 fun。
- shared dispatch 内任意 `error:function_clause` fallback；其他异常保留 class、reason 与 stacktrace。
- deep character list fallback 原样返回；其他 term 等价于 `io_lib:write/1`。
- 领域 formatter 只含直接 `/1` clauses：不调用 `astranaut_lib:format_error`，不导出 `/2`，不定义通用 `format_error_1/1`，不实现 generic catch-all。
- internal ownership assertions 检查 `astranaut_error:realize/1`；compiler-boundary assertions 检查 `to_compiler/1` wrapper；最终消息 assertions 经 `astranaut_lib` adapter。
- local formatter closure roots/exports 仅含 `{format_error,1}`，真实领域 helper 保持 private。
- only-v2 negative fixtures继续被视为缺失 `/1`。

## Ordered Steps

1. 在现有 return/error 测试 seam 添加 `to_compiler/1` adapter assertions，覆盖成功 warnings、失败 errors+warnings、多个文件、顺序和位置；保留 `realize/1` 原形断言。
2. 更新 shared formatter tests，覆盖 `astranaut_lib:format_error/1` wrapper、`/2` match、no-match、内部 helper `function_clause` fallback、character-list fallback、dynamic remote fun 和其他异常传播。
3. 实现 `astranaut_lib:format_error/1,2` 与 private default helper；删除旧 dispatch/default/stack-frame APIs。
4. 在 `astranaut_return:to_compiler/1` 的唯一 compiler conversion 路径包装 errors 与 warnings；不要修改 `astranaut_error:realize/1`。
5. 把所有 production formatter 从旧版 anonymous shared-dispatch callback 返工为直接 `/1` clauses；保持 clause 顺序、guards、reason 和消息文本。
6. 对共享 reason 审计 formatter ownership。优先在产生处绑定真正 formatter；若模块确实拥有组合 reason，使用精确 `/1` clause，不得 catch-all delegation。
7. 收敛 local macro formatter protocol/closure 到纯 `/1` 和真实 helper；此任务不发 missing formatter warning。
8. 返工 fixtures 和测试 helper：领域精确消息直接调用 `/1`；安全 fallback 与 compiler 消息经 `astranaut_lib:format_error/1,2`；删除 throw-option 与 `/2` assumptions。
9. 静态审计 production source，确认领域 callback 内没有 `astranaut_lib:format_error`、旧 symbols、options/throw、通用 `_1` 或 formatter `/2`。
10. 执行全部 Coding Self-Tests并返回原始命令、exit status、CT counts、残留命中和生成 artifacts。

## Stop Conditions

- 需要改变 `astranaut_error:realize/1` 内部 tuple、reason、位置、文件或诊断顺序。
- 无法在 `to_compiler/1` 唯一边界完成 adapter 包装，必须在每个 parse-transformer 重复实现。
- 任何领域 callback 必须调用 shared dispatcher、恢复 `/2`、通用 `_1`、options 或 throw mode才能通过。
- 需要提前实现 Task 2 missing formatter warning。
- 需要修改 Owned Files 之外的产品路径，或无法区分现有返工 diff 与不相关用户改动。

## Coding Self-Tests

由 `luna_coding_worker` 在返工后重新执行；旧结果无效：

1. `rebar3 compile`
2. `rebar3 ct --suite test/astranaut_SUITE`
3. `rebar3 ct --suite test/astranaut_design_SUITE`
4. `rebar3 ct --suite test/astranaut_macro_error_SUITE`
5. `rebar3 ct --suite test/astranaut_macro_local_SUITE`
6. `rebar3 ct --suite test/astranaut_quote_SUITE`
7. `rebar3 ct --suite test/astranaut_rebinding_SUITE`
8. `rebar3 ct --suite test/astranaut_struct_SUITE`
9. `rebar3 ct --suite test/disable_tco_SUITE`
10. `rebar3 ct`
11. `rg -n 'dispatch_error|format_default_error|default\s*=>\s*throw|format_error_1|format_error/2' src`
12. `rg -n 'format_error\([^)]*\)\s*->\s*astranaut_lib:format_error|astranaut_lib:format_error' src`
13. 对 tests 运行旧协议残留搜索，并逐项报告 only-v2 negative fixtures 或断言文本中的合法命中。
14. `git diff --check`

第 11 条预期 exit 1。第 12 条允许 `astranaut_return`/显式 adapter 消费位置和 `astranaut_lib` 自身命中，但领域 formatter callback 内不得命中。完整 CT 使用至少 120 秒真实 timeout。

## Independent Verification

Coding Self-Tests 完成后，由独立 `luna_runner` 执行：

1. `git status --short`
2. `git diff --stat`
3. `git diff --check`
4. `rebar3 compile`
5. 上述八个专项 CT suites
6. `rebar3 ct`
7. 重复第 11–13 条残留审计并原样报告命中

runner 只报告命令、完成状态、exit status、CT counts、raw status/diff 和 artifacts，不作代码审查。

## Commit Subject

`Adapt compiler diagnostics through astranaut_lib`

commit 仅由 dispatcher 在独立验证和 Sol review passed 后执行。

## Completion Criteria

- `to_compiler/1` 是 compiler adapter 的唯一生产边界，内部 `realize/1` 协议不变。
- `astranaut_lib` 公开 `/1` compiler callback 与 `/2` shared dispatcher，旧 strict APIs 已移除。
- 所有 production 领域 formatter 均为纯 `/1` clauses，无 callback 内 shared dispatch。
- local generated formatter 只导出 `/1`，真实 helper closure 完整。
- 专项、完整 CT、静态审计、独立验证和 Sol review 全部通过。
- changed paths 是 declared scope 的子集，无删除、意外 untracked 产品文件、staging 或 commit。
# Task 1 Contract Amendment — quote-suite ownership

This amendment resolves the Task 1 workflow-contract inconsistency without replacing or deleting any existing contract text. Where this amendment is more specific, it controls Task 1 execution.

## Additional Expected/Owned Product Test Path

- `test/astranaut_quote_SUITE.erl`

Ownership of this file is narrow: update only compiler-boundary assertions that still expect the legacy compiler diagnostic tuple `{Position, astranaut_quote, Reason}`. When the asserted value is returned through the compiler-facing error seam, the expected tuple must be `{Position, astranaut_lib, {astranaut_quote, Reason}}`.

Before changing each candidate assertion (including the currently reported regions near lines 584, 597, 607, 622, 633, 651, 667, 865, 881, 897, and 920), classify it by its producer:

- If the assertion observes a compiler/API compile result, migrate only the tuple ownership and wrapping described above.
- If the assertion directly exercises `astranaut_quote:quoted/1` or otherwise tests quote-internal formatter ownership, preserve the existing `astranaut_quote` ownership expectation unchanged.
- Do not change warning behavior or any Task 2 assertion.

## Scope and Stop Conditions

- All previously excluded files remain excluded. In particular, do not edit `docs/plan/transform-error/status.md`, `docs/plan/transform-error/plan.md`, OpenSpec files, `.codex/skills/local-workflow/**`, product source outside the existing Task 1 ownership, or unrelated tests.
- Existing unrelated worktree changes must be preserved and must not be staged, reverted, reformatted, or folded into Task 1.
- Stop and return to Sol if a failing quote assertion cannot be classified unambiguously as compiler-boundary versus quote-internal ownership, or if resolution requires warning/Task 2 behavior.

## Validation and Completion Delta

- The existing `rebar3 ct --suite test/astranaut_quote_SUITE` Coding Self-Test is now backed by explicit ownership of its source file.
- Task 1 is not complete until the quote suite and full CT no longer fail on legacy compiler-boundary quote tuple assertions, while direct quote-internal ownership assertions remain unchanged and passing.
- Independent Verification remains runner-owned; this amendment does not authorize Sol to run commands or edit verification/status artifacts.
