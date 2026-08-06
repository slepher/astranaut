# Task 4 — Exercise Three-State Local Formatter Integration

## Objective

Add fixture-backed integration coverage for the local formatter protocol implemented and committed by Task 3 at `95661f0e3bc8b6186803ba713fda03147001d382` (`Separate local formatter dependency closure`). Prove through the existing parse-transform diagnostic harness that:

- a local `format_error/1` formatter is selected and exported as a legacy formatter;
- local `format_error/1` plus `format_error/2` is selected as a strict formatter;
- strict formatter entry points can reach a private `format_error_1/1` and a second ordinary private helper;
- local `format_error/2` without `format_error/1` does not establish formatter identity and diagnostics use `astranaut_macro`;
- formatter entry points and helpers remain absent from compile-plan `members`, request closure, and frozen IDs.

This is a test-only slice. No production-source change is authorized. If the committed Task 3 behavior cannot satisfy these tests without changing production source, stop and return the exact failing behavior and source evidence for clarification.

## Prerequisite and Workflow Note

- Task 3 is accepted by `docs/plan/format-error-migration/task-3-code-review-3.md` and is reported committed at HEAD `95661f0`.
- `docs/plan/format-error-migration/status.md` is absent from the current workflow directory. The dispatcher must restore or initialize that dispatcher-owned checkpoint before implementation begins; the coding worker and Sol must not create or edit it.

## Decisive Evidence

- `docs/plan/format-error-migration/plan.md:132-149` defines Task 4 as the three-state integration-test slice and requires coverage of legacy `/1`, strict `/1` plus `/2`, private and transitive formatter helpers, `/2`-only fallback, and formatter-free compile-plan state.
- `docs/plan/format-error-migration/task-3-code-review-3.md:13-20` records the accepted producer, protocol, closure, selection/export, and members-only compilation paths. Lines 56-59 record focused unit coverage and independent verification of the committed implementation.
- `src/astranaut_macro_local.erl:795-799`, `formatter_options/3`, selects the generated local module only for protocol `strict`; `legacy` and `none` retain `astranaut_macro`.
- `src/astranaut_macro_local.erl:811-846`, `local_formatter_info/1`, `formatter_protocol/1`, and `formatter_info_with_closure/2`, gives `/1` identity precedence, excludes `/2`-only roots, and computes formatter dependencies through the existing closure machinery.
- `src/astranaut_macro_local.erl:856-875`, `load_local_macro_forms/7`, compiles only when real macro members exist, selects `Members ∪ MacroRelated ∪ FormatterRelated`, and exports `Members ∪ FormatterExports`.
- `src/astranaut_macro_local.erl:907-959`, `compile_boundary/3` and `generation_boundary_key/1`, load formatter information separately while committing and keying only the original `Members`.
- `src/astranaut_macro_local.erl:1400-1426`, `plan_boundary/3` and `request_for_entry/2`, construct plan members and request closure solely from registered macro entries and frozen member forms.
- `test/astranaut_macro_local_SUITE.erl:697-817` already provides the in-memory local-macro harness and checks protocol selection, formatter roots, member-only `members`, `closure_ids`, and frozen IDs, private exports, transitive helper invocation, and generation/callable equivalence. Task 4 should extend this case only where an explicit request `closure_fas`/`forms` exclusion assertion is missing; it must not build a second state-machine harness.
- `test/astranaut_macro_error_SUITE.erl:14-18` routes this suite to `test/astranaut_macro_SUITE_data` and loads fixtures with `astranaut_test_lib`; lines 37-92 show the existing warning/error realization pattern and generated-local-module assertion.
- `test/astranaut_test_lib.erl:115-178`, `assert_formatted_messages/1`, calls `/2` with `#{default => throw}` when both formatter arities are exported, calls `/1` for a legacy formatter, and rejects `/2`-only formatter modules. The integration tests must reuse this protocol check.
- The three Task 4 fixture paths do not currently exist. Existing `macro_with_warnings.erl` and `macro_with_error.erl` remain Task 5 migration fixtures and are not to be edited in this task.

## Invariants

- `format_error/1` remains the formatter identity anchor. `/2` alone never selects the generated local module.
- A strict fixture exports both formatter entry points from the generated local module; a legacy fixture exports only `/1`.
- `format_error_1/1` and every ordinary helper reachable only from formatter roots are compiled into the strict generated module but are not exported.
- The strict fixture's `/1` is a compatibility wrapper around `/2`; `/2` uses `astranaut_lib:format_error/4` with the fixture's private formatter and `astranaut_macro:format_error/2` fallback.
- The strict custom reason is handled by the private formatter path under `#{default => throw}`. A passing default-format fallback must not masquerade as strict local coverage.
- The `/2`-only fixture emits a reason known to `astranaut_macro`; the realized diagnostic formatter must be exactly `astranaut_macro`, not the source module or a generated local module.
- Formatter roots and helper IDs/FAs remain disjoint from plan `members`, every request's `closure_ids`, `closure_fas`, and `forms`, and `astranaut_macro_local:frozen_ids/1`.
- Existing diagnostic ordering, positions, reason terms, and non-empty character-list requirements remain unchanged.
- Reuse `astranaut_test_lib:test_module_forms/2`, `compile_test_forms/1`, `realize_with_baseline/2`, `assert_formatted_messages/1`, and the existing local-module prefix assertion. Do not introduce a parallel compiler, fixture loader, formatter-protocol detector, or local-macro state harness.
- Task 4 does not migrate `macro_with_warnings.erl`, `macro_with_error.erl`, or `macro_sibling_errors_test.erl`; that remains Task 5.

## Ownership

### Owned test paths

- `test/astranaut_macro_SUITE_data/macro_local_formatter_legacy_test.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_only_v2_test.erl`
- `test/astranaut_macro_error_SUITE.erl`
- `test/astranaut_macro_local_SUITE.erl`

### Non-owned paths

- All files under `src/`, including `src/astranaut_macro_local.erl`.
- All other tests and fixtures, including the real diagnostic fixtures reserved for Task 5.
- `docs/plan/format-error-migration/plan.md`, dispatcher-owned `status.md`, Task 3 artifacts, and all workflow documents except this Task 4 contract.
- `project-workflow-local/SKILL.md`, build configuration, dependencies, generated output, staging, commits, and external state.

Any required change to a non-owned path is scope expansion and must stop for clarification.

## Test Design

- `macro_local_formatter_legacy_test.erl`: define one real local macro that returns a valid AST plus one custom warning reason; define and export only `format_error/1` for that reason. The integration assertion must capture the generated local formatter from the warning, verify its source-module-derived name, verify `/1` is exported and `/2` is not, and pass the warning through `assert_formatted_messages/1` so the legacy path is exercised.
- `macro_local_formatter_strict_test.erl`: define one real local macro that emits a distinct custom warning reason; export `/1` and `/2`; make `/1` call `/2`; make `/2` dispatch through `astranaut_lib:format_error/4` to private `format_error_1/1`, and have that helper call one further ordinary private helper that returns the expected non-empty message. Assert the warning names the generated local module, strict formatting succeeds under `#{default => throw}`, both entry points are exported, and neither private helper is exported.
- `macro_local_formatter_only_v2_test.erl`: define one real local macro that emits a reason already covered by `astranaut_macro`; export only local `format_error/2` with a distinguishable result that would reveal accidental selection. Assert the realized warning formatter is exactly `astranaut_macro` and `assert_formatted_messages/1` succeeds through the global strict formatter.
- `astranaut_macro_local_SUITE.erl`: extend `formatter_closure_is_private_and_identity_free/1` rather than creating a new synthetic harness. In addition to its exact `members`, `closure_ids`, and frozen-ID assertions, assert the request's `closure_fas` and `forms` contain only the real macro member and exclude `{format_error,1}`, `{format_error,2}`, `format_error_1/1`, and the transitive private helper.

Use stable, fixture-specific reason atoms/tuples and exact expected messages so each protocol branch can fail independently. Keep each fixture to one diagnostic unless an additional diagnostic is necessary to distinguish the selected formatter.

## Ordered Implementation Steps

1. Confirm the dispatcher has restored the missing `status.md` checkpoint and recorded Task 4 as the active task. Do not edit that file.
2. Read the current versions of the five owned test paths or their containing harnesses, plus the Task 3 implementation symbols cited above. Preserve the current test naming, baseline normalization, and fixture compilation conventions.
3. Add the legacy fixture and an `astranaut_macro_error_SUITE` case that compiles it through the existing data-directory harness, matches its single diagnostic, identifies the generated local formatter, verifies `/1`-only exports, and invokes `assert_formatted_messages/1`.
4. Add the strict fixture and suite case. Use the repository's current `/1` wrapper, `/2` dispatcher, private `format_error_1/1`, and fallback pattern. Assert the exact custom message via the generated formatter under strict options, both formatter exports, and private helper non-exports.
5. Add the `/2`-only fixture and suite case. Emit a globally supported macro reason, assert the formatter in the diagnostic tuple is exactly `astranaut_macro`, and run the shared formatted-message assertion. Do not accept a generated-local formatter or fixture module as equivalent.
6. Extend the existing `formatter_closure_is_private_and_identity_free/1` state-machine case with explicit `closure_fas` and request `forms` assertions. Keep the current exact `members`, `closure_ids`, frozen IDs, export isolation, and transitive runtime checks intact.
7. Review the real diff against the five-path ownership boundary. Remove redundant helpers or duplicate protocol logic in tests; use the existing harness functions.
8. The coding worker runs every Coding Self-Test below and reports exact commands, exit statuses, case counts, and relevant output. It does not stage, commit, or edit workflow status.
9. After coding self-tests pass, the dispatcher assigns all Independent Verification to a fresh, separate `luna_runner`. Sol does not execute any command in either layer.

## Expected Paths and Authorized Deletions

Expected added paths:

- `test/astranaut_macro_SUITE_data/macro_local_formatter_legacy_test.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl`
- `test/astranaut_macro_SUITE_data/macro_local_formatter_only_v2_test.erl`

Expected modified paths:

- `test/astranaut_macro_error_SUITE.erl`
- `test/astranaut_macro_local_SUITE.erl`

Permitted untracked product/test paths: only the three expected new fixtures before they are staged by the dispatcher.

Authorized file deletions: none. Authorized symbol/test-case deletions: none.

`docs/plan/format-error-migration/task-4.md` is the Sol-owned workflow artifact and is not part of the product/test implementation scope.

## Coding Self-Tests

The `luna_coding_worker` owns these commands after implementation and after every rework. It must follow the repository's platform-specific Erlang instructions and return blockers to the dispatcher rather than bypassing them.

1. Run the repository's normal compile check.
2. Run the three new `astranaut_macro_error_SUITE` cases directly if the Common Test selector supports an unambiguous multi-case invocation; otherwise run the complete `astranaut_macro_error_SUITE`.
3. Run `formatter_closure_is_private_and_identity_free` directly, then run the complete `astranaut_macro_local_SUITE`.
4. Run the complete `astranaut_macro_error_SUITE` even if focused cases were run separately, so existing diagnostic ordering and formatter coverage remain protected.
5. Run the repository whitespace/diff check and inspect scope: exactly the five owned test paths may differ for Task 4, there must be no deletion, and no path may be staged.

The report must identify every command, exact exit status, completed test count, and any generated/crash artifacts. Interrupted, skipped, timed-out, or inconclusive commands are not passes.

## Independent Verification

Only after the coding self-test packet is complete, the dispatcher assigns a fresh, separate `luna_runner` with this contract and the real Task 4 diff. Independent Verification is mechanical only. The runner must:

1. Execute the same compile, focused and complete `astranaut_macro_error_SUITE`, focused and complete `astranaut_macro_local_SUITE`, whitespace/diff, and status/scope command set already listed under Coding Self-Tests. Use real command timeouts appropriate to compilation and Common Test; do not use a short yield timeout as the command timeout.
2. For every command, record the exact command, completion state, exact exit status, completed test count where reported, and whether it was interrupted, skipped, timed out, or otherwise inconclusive.
3. Return the raw status and diff outputs produced by those commands, without converting them into a scope, architecture, design, or correctness judgment.
4. Report every generated or crash artifact exposed by the command outputs or mechanical status results.
5. Return this runner-authored raw evidence packet without editing source, tests, workflow documents, status, staging, or commits.

The runner must not inspect or audit fixture contents, source code, test assertion semantics, architecture, design, conceptual scope, path legitimacy, protocol coverage, fallback behavior, formatter selection, export meaning, or compile-plan invariants. It reports mechanical command failures and repository outputs only. Passing runtime suites do not replace Sol's audit, and dispatcher summaries do not replace the runner-authored packet.

## Sol Review Requirements

The dispatcher must supply Sol with the completed runner-authored raw mechanical packet, the coding self-test packet, the real Task 4 diff including untracked fixture contents, this contract, the initiative plan, and the decisive surrounding source and test paths. Sol must not start or finalize review if the runner packet is missing, interrupted, still running, or omits required command facts.

After receiving that packet, Sol independently inspects the real diff and surrounding code and owns every source, assertion, design, architecture, and semantic-scope judgment. Sol must not rely on a runner or dispatcher conclusion and must not execute compile, test, CT, lint, build, or verification commands. The review must:

1. Audit the real change set against the exact five-path product/test scope: three added fixtures and two modified suites, with no production source, non-owned test, dependency, deletion, staged path, generated output, or unrelated behavior included.
2. Inspect each fixture and its assertions to establish that the three protocol branches are genuinely distinct: `/1` only, `/1` plus `/2`, and `/2` only.
3. Establish that strict `/2` coverage uses `#{default => throw}`, reaches the fixture-specific private `format_error_1/1` and its second ordinary private helper, cannot pass through a generic fallback, and asserts that both helpers are absent from generated-module exports.
4. Establish that the `/2`-only diagnostic records exactly `astranaut_macro` as formatter and does not accept the fixture's distinguishable `/2` result as evidence of local selection.
5. Inspect the local-suite plan assertions and surrounding implementation to establish that formatter entry points and helpers are absent from `members`, every request's `closure_ids`, `closure_fas`, and `forms`, and frozen IDs.
6. Audit every invariant in this Task 4 contract, including diagnostic ordering/positions/reasons, non-empty messages, reuse of the existing harness, absence of duplicate protocol or state-machine machinery, and preservation of the Task 5 boundary.
7. Correlate the raw command results with the inspected implementation and report actionable findings first, or explicitly state that no material findings remain.

## Stop Conditions

Stop and return `Clarification required` with exact evidence if:

- `docs/plan/format-error-migration/status.md` has not been restored before implementation starts;
- any integration case requires a change under `src/` or any other non-owned path;
- the committed Task 3 implementation selects a generated local formatter for `/2` only, fails to include a strict private/transitive helper, exports a formatter helper, or contaminates compile-plan membership/request/frozen state;
- a fixture cannot produce a deterministic diagnostic through the existing `astranaut_test_lib` harness without changing product behavior;
- satisfying Task 4 would require migration of `macro_with_warnings.erl`, `macro_with_error.erl`, or `macro_sibling_errors_test.erl` before Task 5;
- test assertions would depend on generated module names beyond the established source-module prefix contract, nondeterministic load ordering, or unrelated global module state;
- any expected path has unrelated pre-existing edits that cannot be safely separated;
- a command is blocked, interrupted, timed out, or produces ambiguous evidence.

## Proposed Commit Subject

```text
Cover local formatter protocol integration
```

The dispatcher owns staging and commit creation only after coding self-tests, independent verification, and Sol review pass. The product/test commit scope is exactly the five owned test paths.

## Completion Criteria

- All three fixtures exist and exercise real local macros through the existing parse-transform diagnostic harness.
- The legacy diagnostic uses the generated local module, exports `/1` but not `/2`, and produces its exact non-empty message through the legacy helper path.
- The strict diagnostic uses the generated local module; strict `/2` formatting succeeds with `#{default => throw}`; `/1` and `/2` are exported; private `format_error_1/1` and its ordinary helper are compiled, invoked, and not exported.
- The `/2`-only diagnostic uses `astranaut_macro`, never the generated local module or source fixture, and formats successfully through the global strict formatter.
- Compile-plan `members`, request `closure_ids`, `closure_fas`, request `forms`, and frozen IDs exclude every formatter entry point and formatter-only helper.
- No production source, non-owned test, dependency, generated output, deletion, staged path, or unrelated behavior is included.
- Coding Self-Tests complete successfully with exact evidence.
- A separate `luna_runner` completes Independent Verification successfully and supplies its own evidence packet.
- Sol review reports no material findings; only then may the dispatcher update `status.md`, stage the five test paths, and commit.
