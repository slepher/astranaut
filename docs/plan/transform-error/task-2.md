# Task 2 — 缺失 macro formatter warning 与最终验收

## Goal 与前置边界

Goal：`transform-error`。

本任务完成 `openspec/changes/transform-error/` 的 4.1–4.4 与 5.2–5.4：在 external 和 local macro formatter protocol 检测边界报告一次性 `{missing_macro_formatter, Module}` warning，同时继续注册和展开，并完成 initiative 的最终验证。

Task 1 已由提交 `ae32f6c`（`Adapt compiler diagnostics through astranaut_lib`）完成，handoff 状态提交为 `a8b030c`。Task 1 的 `to_compiler` adapter、纯领域 `format_error/1`、local `/1` closure、shared fallback，以及删除 strict `/2`、options、throw mode 的结果均是本任务不可回退的前置条件。

除在 `astranaut_macro:format_error/1` 增加一个精确的 `{missing_macro_formatter, Module}` clause 外，本任务不得修改 Task 1 已提交的 formatter protocol。若实现或测试要求恢复领域 `/2`、`default => throw`、options、`format_error_1/1`、generic catch-all 或 callback 内 shared dispatch，立即停止并返回 Sol。

## Decisive Evidence

- `src/astranaut_macro_registry.erl:56-65` 的 `new/3` 为每次 source module compile 创建 registry state；这是 external provider warning 去重的正确生命周期，不需要 process-global 状态。
- `src/astranaut_macro_registry.erl:85-108` 的 `apply_directive/2` 已在 return monad 中处理 `-import_macro` 并返回更新后的 registry；`src/astranaut_macro_registry.erl:355-390` 已读取 provider exports，并在缺少 `{format_error,1}` 时把 descriptor formatter 设为 `astranaut_macro`，但当前没有 warning。
- `src/astranaut_macro_scan.erl:292-386` 对每个 attribute 使用 `astranaut:traverse_form/2`，并通过 `astranaut:traverse_return/1` 桥接 registry/local return monad。pending warning 因而会由当前 import/local-macro attribute 节点边界补位置，并使用 scan 的 `astranaut_macro` formatter；业务层不应手工构造 formatted warning。
- `src/astranaut_macro_local.erl:73-102` 的 `handle_form/3` 是 local declaration 的 monadic入口；`src/astranaut_macro_local.erl:795-829` 在 source view 首次建立 `formatter_info`，protocol 仅为 `present | missing`，`missing` descriptor 已继续选择 `astranaut_macro`。同一 capability state 中 `formatter_info` 只初始化一次，正好提供“多个 local declarations 只警告一次”的 source-compilation-local gate。
- `handle_form/3` 的 Context 已携带声明 source module identity；local warning 必须使用该 source module，不能使用 `local_macro_module` generation identity。
- `lessons.md` 明确规定 monadic warning 必须在 `do`/`then`/`bind` 链中串联；普通逗号会丢失 action。当前节点诊断应使用 pending `astranaut_return:warning/1`，由 traversal boundary 补 formatter 与位置。
- `test/astranaut_macro_error_SUITE.erl:107-164` 已有 local `/1` 与 only-v2 integration seams；`test/astranaut_macro_local_SUITE.erl:710-829` 已固定 `present | missing`、`{format_error,1}` root 与 private closure；macro scan/pass/uniform suites覆盖 source-ordered、generated、重复使用和 continuation 行为。
- 当前 HEAD 为 `a8b030c`，其 Task 1 产品边界已提交。相对 HEAD 没有未提交 product source/test diff；现有 status、OpenSpec 和 local-workflow metadata 修改均不属于 Task 2 coding scope，必须原样保留且不得暂存。

## Approach

### External provider

保留 `formatter_opts/3` 只以 `{format_error,1}` 判定 descriptor formatter；让 import detection 同时把 `present | missing` 结果传回 `apply_directive/2`。在 registry state 中维护一个按 provider module 标识的 `ordsets` 去重集合，初值属于 `new/3` 创建的单次 source-compilation state。

一次 import 成功合并并准备返回更新 state 时：

- `present`：直接返回，不发 warning；
- `missing` 且 provider 尚未记录：先把 provider 加入返回的 registry state，再在同一 return-monad 链中附加 `warning({missing_macro_formatter, Provider})`；
- `missing` 且 provider 已记录：直接返回，不重复 warning。

重复 import、generated import、后续 use 或多次 macro call 都共享同一 registry state。检测只发生在 provider import boundary；use/call 不建立第二套检测或去重机制。provider 仅导出 `/2` 时仍为 `missing`，不得检查或兼容 `/2`。

### Local provider

在 `handle_form/3` 进入首个有效 local declaration 时，记录传入 State 是否尚未包含 `formatter_info`。`register_return/5` 建立 formatter info 后，如果这是首次检测且 protocol 为 `missing`，在继续 `prepare_declaration/3` 之前把 pending `warning({missing_macro_formatter, SourceModule})` 串入同一 return-monad `do` 链；`SourceModule` 从 Context 的 `module` 取得。

后续 local declaration 因 state 已有 `formatter_info` 而不再 warning。warning action 必须排在后续可能失败的 monadic preparation 前，确保后续失败不会丢掉已发现的 warning。纯 `register/5` helper 仍负责状态/closure 计算，不手工附位置或引入 process-global side effect。

### Message and tests

在 `astranaut_macro:format_error/1` 增加精确 clause：

```erlang
format_error({missing_macro_formatter, Module}) ->
    io_lib:format(
      "macro provider ~p does not export format_error/1; using astranaut_macro formatter.",
      [Module]).
```

测试必须断言完整 tuple、精确 provider identity、首次 attribute 位置、每 provider 次数、descriptor formatter 和 expansion continuation；不得只搜索 warning 文本或放宽为“存在某 warning”。

## Owned Files / Modules

生产实现仅允许按实际需要修改：

- `src/astranaut_macro_registry.erl`
- `src/astranaut_macro_local.erl`
- `src/astranaut_macro.erl`

测试实现仅允许按实际需要修改：

- `test/astranaut_macro_scan_SUITE.erl`
- `test/astranaut_macro_error_SUITE.erl`
- `test/astranaut_macro_local_SUITE.erl`
- `test/astranaut_macro_pass_SUITE.erl`
- `test/astranaut_macro_uniform_SUITE.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_only_v2_test.erl`

允许新增以下 test fixtures；名称若与现有 module 冲突则停止，不得自行换成更宽范围：

- `test/astranaut_macro_SUITE_data/macro_missing_formatter_provider.erl`
- `test/astranaut_macro_SUITE_data/macro_only_v2_formatter_provider.erl`
- `test/astranaut_macro_SUITE_data/macro_missing_formatter_external_test.erl`
- `test/astranaut_macro_SUITE_data/macro_missing_formatter_local_test.erl`

不得修改 `test/astranaut_SUITE_data/sample_transformer_only_v2.erl`；它是 Task 1 的非-macro negative fixture。不得通过给既有无 formatter provider 添加虚假 catch-all `format_error/1` 来压制新 warning。若既有 suites 的精确 warning assertions 因规范要求的新 warning 改变，只更新上列 suite assertions，并保持对所有原诊断与新诊断的精确检查。

## Invariants

- external warning tuple 为 `{ImportPos, astranaut_macro, {missing_macro_formatter, ProviderModule}}`；local tuple 为 `{FirstLocalDeclarationPos, astranaut_macro, {missing_macro_formatter, SourceModule}}`。
- 每个缺失 formatter provider 在一次 source module compile 中最多一次 warning；不同 provider 各一次；下一次独立 source module compile 重新允许一次。
- external 去重 state 归属 `astranaut_macro_registry:new/3` 创建的 registry；local 去重归属 capability state 的首次 `formatter_info` detection。不得使用 process dictionary、ETS、`persistent_term`、application env 或其他跨编译全局状态。
- 只有导出/定义 `format_error/1` 才是 `present`。仅 `/2` 与完全缺失等价，descriptor formatter 都是 `astranaut_macro` 并产生 warning。
- external provider 使用实际 provider module identity；local provider 使用声明 source module identity，绝不使用动态 generation module。
- warning 不阻断 import、registration、local generation、use 或 expansion；descriptor fallback 保持 `astranaut_macro`。
- warning 必须是 pending return-monad diagnostic，并在 `do`、`then` 或 `bind` 中串联；不得用普通逗号丢弃 action，不得在 registry/local 业务分支手写 position 或 `formatted_warning`。
- warning 顺序遵循 source order。首次缺失 external import/local declaration 的 warning 出现在该节点；重复 directive/declaration 不增加副本。
- Task 1 formatter protocol保持不变：`astranaut_lib` adapter、`to_compiler` wrapper、领域纯 `/1`、local closure roots/exports 仅 `/1`，以及 only-v2 negative semantics 都不得回退。
- 既有 errors/warnings 的 formatter、reason、位置、顺序和恢复语义保持不变；测试不得通过过滤所有 warning 隐藏回归。

## Ordered Implementation

1. 先在 focused tests 中固定 external 行为：无 `/1` provider、only-v2 provider、同 provider 重复 import/use/call、不同缺失 providers 各一次、warning 精确位置/identity/formatter，以及 macro expansion 仍产出预期结果。
2. 在 local integration tests 中固定完全缺失与 only-v2 两类；至少一个 fixture 包含多个 `-local_macro` declarations 并调用多个 local macros，断言只有首次 declaration 产生一个 source-module warning，descriptor 仍为 `astranaut_macro` 且展开继续。
3. 保留并加强有 `/1` 的 local/external control case，断言不产生 `{missing_macro_formatter, _}`；不得把已有领域 warning误计为 missing warning。
4. 为 `astranaut_macro:format_error/1` 添加上述唯一精确 message clause，并在 formatter contract test 中断言结果是 deep character list 且文本精确。
5. 扩展 registry state 的 per-compilation dedup 集合，并让 import analysis 返回 formatter presence；在成功 import 的现有 return-monad路径中串联一次性 warning 和更新后的 state。
6. 在 local `handle_form/3` 中以首次 `formatter_info` detection 为 gate，从 Context 读取 source module，并在后续可能失败的 preparation 前串联一次性 pending warning；保持 `register/5`、closure roots 和 generation identity 语义不变。
7. 审计 generated imports、重复 directives 和多个 local declarations 都沿同一 state 生命周期；删除任何重复检测分支，不在 use/call 或 expansion runtime 增加第二套 warning。
8. 更新受新规范 warning 影响的 macro scan/error/local/pass/uniform 精确 assertions。保留原 errors/warnings，显式加入预期 missing warning；不得广泛忽略 warnings，也不得为测试 provider 恢复兼容协议。
9. 静态审计 changed product source：没有 process-global 去重，没有普通逗号丢 monadic warning，没有手写 formatted position，没有 `/2` 检测或 Task 1 protocol残留。
10. coding worker 执行全部 Coding Self-Tests并返回原始命令、exit status、suite counts、timeout/interruption 状态、status/diff outputs 和 artifacts；失败留在 coding 层修复，不得交给 runner 代替。

## Stop Conditions

- 需要恢复或修改 Task 1 的 adapter、领域 `/1` purity、shared fallback 或 local closure protocol，超出新增一个精确 `astranaut_macro` reason clause。
- formatter presence 无法仅由 `/1` 判断，或测试/规格要求 `/2` 兼容。
- 去重需要跨 source module compile 的 state、process-global storage 或外部 side effect。
- warning 无法通过现有 return/traverse bridge在 import/local attribute 节点补位置，必须手工改写 `astranaut_error:realize/1`、`to_compiler/1` 或 traversal diagnostic protocol。
- local source identity 无法从现有 capability Context 确定，必须使用 generation module 猜测。
- 发现需要修改 Owned Files 之外的产品路径、需要删除文件，或当前未提交 product diff 无法归属本任务。
- 既有 diagnostic assertion 与 OpenSpec 要求冲突，且无法通过精确加入 missing warning 保留原行为。

## Expected Paths, Deletions, and Untracked Scope

- Expected tracked modifications：上述 3 个 production files、5 个 suite files，以及 existing only-v2 local fixture的实际必要子集。
- Permitted new untracked product-test paths：仅上述 4 个明确命名的新 fixtures；实现完成后它们属于 Task 2 commit scope。
- Authorized deletions：无。
- `docs/plan/transform-error/task-2.md` 是 Sol-owned workflow artifact；coding worker不得修改。
- 当前已存在的 `.codex/skills/local-workflow/**`、`docs/plan/transform-error/status.md` 与 `openspec/changes/transform-error/**` 修改均在 coding scope 之外；保留其内容，不得 stage、revert、format 或折入 Task 2。
- 不得修改 `plan.md`、任何 review/retrospective、其他 workflow 文档、产品文档、skill、staging 或 commits。

## Coding Self-Tests

以下命令全部由 `luna_coding_worker` 在实现后直接执行；Sol 不执行：

1. `rebar3 compile`
2. `rebar3 ct --suite test/astranaut_macro_scan_SUITE`
3. `rebar3 ct --suite test/astranaut_macro_error_SUITE`
4. `rebar3 ct --suite test/astranaut_macro_local_SUITE`
5. `rebar3 ct --suite test/astranaut_macro_pass_SUITE`
6. `rebar3 ct --suite test/astranaut_macro_uniform_SUITE`
7. `rebar3 ct --suite test/astranaut_SUITE`
8. `rebar3 ct --suite test/astranaut_design_SUITE`
9. `rebar3 ct --suite test/astranaut_quote_SUITE`
10. `rebar3 ct --suite test/astranaut_rebinding_SUITE`
11. `rebar3 ct --suite test/astranaut_struct_SUITE`
12. `rebar3 ct --suite test/disable_tco_SUITE`
13. `rebar3 ct`，使用至少 120 秒的真实 timeout
14. `rebar3 xref`
15. `rebar3 dialyzer`
16. `openspec validate transform-error --strict`
17. `rg -n 'dispatch_error|format_default_error|default\s*=>\s*throw|format_error_1' src`，预期 exit 1、无匹配
18. `rg -n 'missing_macro_formatter' src test`，逐项报告 source clause、external/local detection 和精确测试命中
19. `git status --short`
20. `git diff --stat`
21. `git diff --check`

所有 CT 结果必须报告 suite test count。`xref` 或 OpenSpec strict validation 若因环境/工具缺失而不能启动，报告原始错误并停止；不得静默跳过或以其他命令替代。

### Known Dialyzer Baseline

Task 2 coding self-test 已在 Erlang/OTP 29.0.4 上完成 `rebar3 dialyzer`，命令 exit 1 且仅报告：

```text
src/astranaut_syntax_schema.erl:699:14: Guard test is_list
         (Node1 :: erl_anno:anno()) breaks the opacity of its argument
```

artifact：`_build/default/29.0.4.dialyzer_warnings`。

Sol 已以只读 Git evidence确认 `src/astranaut_syntax_schema.erl` 相对当前 HEAD、`ae32f6c..HEAD` 和 Task 1 implementation boundary 均无 diff；该 guard 来自早于本 initiative 的 schema history，且该 module 不在 Task 2 owned paths。因此，只有在以下条件全部满足时，这一个已知 warning 是非 Task 2 gating baseline：

- `rebar3 dialyzer` 确实完成，原始 exit status 和 artifact 被保留；
- 输出仅有上述同一文件、行、opaque `erl_anno:anno()` reason 的一个 warning，没有任何 Task 2 source/test path 或其他 warning；
- `git diff -- src/astranaut_syntax_schema.erl` 为空，worker/runner 未修改该 module；
- 当前 Task 2 source/test/scope gates 继续保持，不因 baseline 放宽。

满足时，worker 必须明确记录 Dialyzer 为“exit 1，accepted pre-existing baseline”，然后继续执行 OpenSpec strict validation、两项 residual audits、最终 status/stat/diff-check，不得提前停止，也不得修改或 suppress `astranaut_syntax_schema.erl`。任一细节不同、出现新增 warning、schema 有 diff、Dialyzer 中断或不能启动，均仍是 gating failure并立即停止返回 Sol。

## Independent Verification

Coding Self-Tests 全部完成后，由新的独立 `luna_runner`（`fork_turns = none`）针对同一 worktree执行；Sol 和 coding worker不得代跑：

1. `git status --short`
2. `git diff --stat`
3. `git diff --check`
4. `rebar3 compile`
5. 上述五个 macro focused suites
6. `rebar3 ct --suite test/astranaut_SUITE`
7. `rebar3 ct --suite test/astranaut_design_SUITE`
8. `rebar3 ct --suite test/astranaut_quote_SUITE`
9. `rebar3 ct --suite test/astranaut_rebinding_SUITE`
10. `rebar3 ct --suite test/astranaut_struct_SUITE`
11. `rebar3 ct --suite test/disable_tco_SUITE`
12. `rebar3 ct`，使用至少 120 秒的真实 timeout
13. `rebar3 xref`
14. `rebar3 dialyzer`
15. `openspec validate transform-error --strict`
16. 重复 Coding Self-Tests 的两项 `rg` audit，并原样报告匹配
17. 最终 `git status --short`、`git diff --stat` 和 `git diff --check`

Independent Verification 对 Dialyzer 使用完全相同的 Known Dialyzer Baseline gate：runner 必须实际执行命令、原样报告 exit 1 和唯一 warning，并确认 schema diff 为空；仅在四项 baseline 条件全部满足时继续后续 strict validation、audits 和 final checks。runner 不得把 exit 1 改报为 exit 0 或笼统的 pass。

runner 只返回每个命令的完成/中断状态、exit status、CT counts、raw status/diff/audit outputs 和 generated artifacts，不作源码、测试语义、scope 或架构判断，也不修改、stage 或 commit。

## Commit Subject

`Warn on missing macro formatters`

commit 仅由 dispatcher 在 coding self-tests、Independent Verification 和 Sol code review 全部通过后执行，并且必须显式 stage accepted Task 2 paths，排除既有 status/OpenSpec/skill 工作树修改。

## Completion Criteria

- external provider 缺少 `/1` 时，在首次成功 import 节点产生一个 `{missing_macro_formatter, Provider}` warning，descriptor 继续为 `astranaut_macro`，注册/use/call/展开继续。
- local source 缺少 `/1` 时，在首次有效 local declaration 节点产生一个 `{missing_macro_formatter, SourceModule}` warning；多个 declarations/macros 仍仅一次，generation descriptor 继续为 `astranaut_macro`。
- external 与 local only-v2 cases 都被明确视为 missing；没有任何 `/2` compatibility path。
- 同 provider 重复 import/use/call 不重复 warning，不同 providers 分别 warning，独立 source compilations 不共享去重状态。
- warning tuple 的 formatter、reason、identity、位置、顺序、次数和最终 message均有精确 assertions；有 `/1` 的 control provider 不 warning。
- 所有 continuation assertions 证明 registration 与 expansion 未被 warning 阻断；既有 diagnostics 保持原语义。
- Task 1 protocol residual audit 仍通过，且没有 process-global state、手写 formatted position或丢弃的 monadic action。
- Dialyzer 必须无新增 warning；允许的唯一非 gating 结果是 Known Dialyzer Baseline 中精确记录的 untouched schema warning，且 coding worker 与 independent runner 都必须原样报告并继续完成其余 gates。
- changed product/test paths 是 declared scope 的子集；无删除、无意外 untracked path、无 staging、无 worker commit。
- Coding Self-Tests、独立 runner verification、Task 2 Sol review 全部通过；dispatcher 随后以指定 subject提交 Task 2，本 `transform-error` Goal 才可标记完成。
