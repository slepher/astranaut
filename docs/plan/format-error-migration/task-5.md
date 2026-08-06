# Task 5 — Migrate Real Local-Macro Diagnostic Formatters

## Objective

Migrate the three existing local-macro diagnostic fixtures to the strict
formatter protocol introduced by Task 3, while preserving their existing
diagnostic behavior. Add `/1` compatibility wrappers, `/2` dispatchers using
`astranaut_lib:format_error/4`, and specific private `format_error_1/1`
clauses for fixture-owned reasons. Prove through the existing
`astranaut_macro_error_SUITE` harness that real generated local modules invoke
their strict formatter paths.

This is a test-fixture and test-suite slice. No production-source change is
authorized. If the committed Task 3 behavior cannot satisfy the contract
within the four owned test paths, stop and return the exact behavior and
source evidence for clarification.

## Baseline and decisive evidence

- The initiative plan defines Task 5 at `docs/plan/format-error-migration/plan.md:149-158`: migrate the three real fixtures, add `/1` wrappers and `/2` dispatchers, move custom reasons into private `format_error_1/1`, preserve `noop`, `bar`, macro-exception, sibling-error messages/positions/counts/reasons, and prove generated-local `/2` strict coverage.
- The current committed baseline is `72b05232004cbdf7f0eadab77ddaf460412eb109` (`72b0523`, `Cover local formatter protocol integration`). Task 4 is committed and accepted according to `docs/plan/format-error-migration/status.md`.
- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl:17-25,55-81` exports only `/1`, emits local `noop_function` and `noop` warnings, and formats only `noop` specially; the current fallback for `noop_function` is generic `astranaut_macro` formatting.
- `test/astranaut_macro_SUITE_data/macro_with_error.erl:16-27,38-50` exports only `/1`, produces local `macro_exception` and `bar` diagnostics, and formats only `bar` specially.
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl:9-29` exports only `/1`, emits `macro_exception`, `sibling_return_error`, and `invalid_macro_return` sibling failures, and currently delegates all formatting to `astranaut_macro`.
- `test/astranaut_macro_error_SUITE.erl:40-95` records the existing warnings/errors, formatter identities, positions, reasons, ordering, and runtime behavior for the first two fixtures. Lines `358-402` record the sibling count, accepted diagnostic shapes, reason checks, and shared formatting assertion.
- `src/astranaut_lib.erl:613-628` owns formatter no-match fallback and preserves the original options; nested formatter `function_clause` errors must be re-raised. `src/astranaut_macro.erl:45-53,89-155` supplies the global strict formatter and handles `macro_exception`/`invalid_macro_return`, but does not handle `noop_function` or `sibling_return_error` under `#{default => throw}`.
- `test/astranaut_test_lib.erl:115-178` calls `/2` with `#{default => throw}` for modules exporting both formatter arities. Consequently, after migration, every local diagnostic that reaches a generated local formatter must either match a specific private clause or be covered by the global strict fallback. `noop_function` and `sibling_return_error` are not covered by that global strict fallback and require explicit private clauses preserving their current generic messages.
- Task 4 Review 2 and its improvement artifact establish the required boundary: use `astranaut_lib:format_error/4`, do not add private catch-all fallback clauses, preserve options, and prove both custom and unmatched behavior through independent assertions.
- `src/astranaut_macro_local.erl:795-846` selects the generated local formatter only for local `/1` identity with strict `/1` + `/2`, while retaining formatter-related forms separately from macro members. This task must consume that behavior, not modify it.

## Owned paths

Only these existing product/test paths may change:

- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`
- `test/astranaut_macro_error_SUITE.erl`

Non-owned paths:

- All files under `src/`.
- `test/astranaut_test_lib.erl`, `test/astranaut_macro_local_SUITE.erl`, all
  other fixtures/suites, dependencies, build configuration, generated output,
  workflow documents, `status.md`, root `status.md`, staging, commits, and
  external state.

The dispatcher owns status maintenance, staging, and commit creation. The
coding worker owns only the four product/test paths above and must not edit
workflow status. Sol does not edit product/test paths or execute commands.

## Protocol and behavior invariants

### Common formatter protocol

- Each fixture exports `format_error/1` and `format_error/2` after migration.
- `/1` is a compatibility wrapper calling `/2` with `#{}`.
- `/2` calls:

  ```erlang
  astranaut_lib:format_error(
    Error, Options, fun format_error_1/1,
    fun astranaut_macro:format_error/2)
  ```

- `format_error_1/1` contains only specific fixture-owned clauses. No private
  catch-all may reproduce fallback policy or discard `Options`.
- Existing global reasons continue through the global `astranaut_macro`
  fallback. The migration must not change nested `function_clause` propagation,
  message type, or default/strict option semantics.
- Private `format_error_1/1` clauses and any private helpers are available in
  the generated local module but are not exported. The suite must assert both
  formatter entry points are exported and private formatter functions are not.

### `macro_with_warnings.erl`

- Preserve the existing seven warning records, their order, positions, file,
  formatter ownership, reasons, and `test_attributes/0` result.
- Preserve `noop`'s exact existing message `"oops, noop"`.
- Preserve `noop_function`'s existing generic message. Because the shared
  helper calls strict `/2` with `#{default => throw}`, add a specific private
  clause whose result is equivalent to the previous ordinary fallback
  (`io_lib:write(noop_function)`), rather than relying on a global strict
  fallback that will throw.
- The suite must identify the generated local formatter, assert `/1` and `/2`
  exports, assert private formatter non-export, invoke
  `Local:format_error(noop, #{default => throw})` for the exact custom message,
  invoke the `noop_function` strict path for the preserved generic message,
  and retain `assert_formatted_messages/1`.

### `macro_with_error.erl`

- Preserve all ten existing errors, order, positions, formatter ownership,
  reason terms, macro-exception stack payload matching, and the existing
  `bar` message `"oops, bar"`.
- Move `bar` into private `format_error_1/1`; `macro_exception` must continue
  through `astranaut_macro:format_error/2` under strict options.
- The suite must identify the generated local formatter used by the local
  diagnostics, assert `/1` and `/2` exports and private non-export, invoke
  `Local:format_error(bar, #{default => throw})` for the exact custom message,
  and retain `assert_formatted_messages/1` for every diagnostic.

### `macro_sibling_errors_test.erl`

- Preserve the three sibling failures, their order/positions, reason terms,
  and the existing accepted diagnostic representation. Do not alter macro
  expansion or recovery behavior.
- Add `/1` and `/2` with the common dispatcher. Add a specific private clause
  for `sibling_return_error` whose output preserves the old generic message
  (`io_lib:write(sibling_return_error)`); `macro_exception` and
  `invalid_macro_return` continue through the global strict formatter where
  applicable.
- Extend `test_macro_sibling_errors/1` only enough to locate the generated
  local formatter in the existing direct or `local_macro_diagnostic` shape,
  assert its `/1` and `/2` exports/private non-export, and call its `/2` with
  `sibling_return_error` under `#{default => throw}`. Keep all existing sibling
  reason/count/shape assertions and the shared formatted-message assertion.

## Test design and capability reuse

- Reuse `astranaut_test_lib:test_module_forms/2`,
  `compile_test_forms/1`, `get_baseline/2`,
  `realize_with_baseline/2`, `assert_formatted_messages/1`, and the existing
  `assert_local_macro_module/2` helper.
- Extend the existing three suite cases in `astranaut_macro_error_SUITE.erl`;
  do not create a second fixture loader, formatter protocol detector, compiler,
  error realization path, or local-macro state harness.
- Keep exact custom-message assertions separate from the shared non-empty
  message check. A generic fallback producing a character list must not be
  accepted as evidence that the fixture-specific private formatter ran.
- Use `astranaut_lib:format_error/4` and existing `astranaut_macro` behavior for
  fallback; do not call the global formatter directly from a private catch-all.
- Do not alter `test_macro_local_formatter_*` Task 4 fixtures or migrate any
  Task 6/future fixture in this task.

## Ordered implementation steps

1. Confirm the current worktree is attributable to Task 5 and read the current
   versions of the four owned paths plus the source/helper anchors above. Do
   not edit workflow status or unrelated existing changes.
2. Convert `macro_with_warnings.erl` to `/1` wrapper + `/2` dispatcher and
   specific private clauses for `noop` and `noop_function`. Preserve the old
   message values and do not add a catch-all.
3. Convert `macro_with_error.erl` to the same protocol, move `bar` into
   private `format_error_1/1`, and retain global formatting for known macro
   reasons.
4. Convert `macro_sibling_errors_test.erl` to the same protocol and add the
   specific `sibling_return_error` clause needed for strict coverage while
   preserving its old generic message.
5. Update only the three corresponding cases in
   `astranaut_macro_error_SUITE.erl`: preserve existing diagnostics and runtime
   assertions, add generated-local `/2` identity/export/private assertions,
   and add exact strict custom-path assertions for `noop`, `bar`, and
   `sibling_return_error`.
6. Review the real diff for the four-path boundary, absence of catch-all
   formatter clauses, no changed fixture declarations/macros unrelated to
   formatting, no changed diagnostic order/positions/reasons/messages, and no
   accidental Task 4 or Task 6 scope.
7. The coding worker runs all Coding Self-Tests below and returns exact raw
   command results, counts, statuses, and generated-artifact information. It
   does not stage, commit, edit status, or delegate.
8. After coding self-tests pass, the dispatcher assigns the exact Independent
   Verification command set to a separate `luna_runner`. The runner reports
   mechanical results only; it does not audit fixture semantics or scope.

## Stop conditions

Stop and return `Clarification required` with exact path/symbol evidence if:

- Any required correction falls outside the four owned paths, especially into
  `src/`, `astranaut_test_lib.erl`, Task 4 fixtures, or workflow metadata.
- The generated local module is not selected for the migrated real local
  diagnostics, cannot expose `/2`, or cannot reach the private formatter path
  without changing production code.
- A local diagnostic reason cannot be covered under strict `/2` without
  changing its established message, position, order, count, or reason.
- Preserving strict behavior appears to require a private catch-all, direct
  global bypass, changed `astranaut_lib` semantics, or a new formatter/closure
  mechanism.
- The sibling diagnostics cannot be matched through their existing direct or
  `local_macro_diagnostic` representation while proving the generated local
  formatter path without altering product behavior.
- Existing assertions or source evidence conflict with the required fallback,
  nested `function_clause`, `/1` compatibility, or strict `#{default => throw}`
  semantics.
- Any expected path has unrelated edits that cannot be safely separated, a
  deletion appears, staging is non-empty, or a command is blocked,
  interrupted, timed out, skipped, or otherwise inconclusive.

## Coding Self-Tests

The `luna_coding_worker` owns these commands after implementation and after
every rework. Sol must not execute them.

1. Compile the repository with the normal project compile command:
   `rebar3 compile`.
2. Run each migrated case directly:

   ```text
   rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl --case=test_macro_with_warnings
   rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl --case=test_macro_with_error
   rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl --case=test_macro_sibling_errors
   ```

3. Run the complete acceptance suite:
   `rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl`.
4. Run `git diff --check`.
5. Inspect scope mechanically with `git status --short`,
   `git diff --name-only`, and `git diff --cached --name-only`. The product
   change set must be exactly the four owned paths, with no product deletion
   and no staged path; workflow metadata remains dispatcher-owned.
6. Report every command, exact exit status, completed count, timeout or
   interruption state, and generated/crash artifact. A command that is
   skipped, ambiguous, or interrupted is not a pass.

## Independent Verification

Only after the coding self-test packet is complete, a separate
`luna_runner` executes the following mechanically against the current worktree:

1. `rebar3 compile`.
2. The three focused Common Test commands listed above.
3. `rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl`.
4. `rebar3 ct` as a risk-based full-regression check for changes to real
   diagnostic fixtures.
5. `git diff --check`, `git status --short`,
   `git diff --name-only`, and `git diff --cached --name-only`.

The runner must return, for each command, the exact command, completion state,
exit status, test count where available, timeout/interruption/skipped state,
raw status/diff outputs, and generated/crash artifacts. It must not inspect or
judge fixture contents, formatter semantics, diagnostic assertions,
architecture, capability reuse, path legitimacy, or conceptual scope. It must
not edit source, tests, workflow documents, status, staging, or commits.

If any runner command fails or is inconclusive, the dispatcher routes the raw
failure to coding/rework; Sol does not substitute a local command or infer a
pass from another suite.

## Sol semantic review requirements

The later Task 5 review must receive the real diff including all four owned
paths, this contract, the coding packet, and the completed runner-authored raw
packet. Sol must not execute verification commands. Sol owns these audits:

1. Confirm the product commit scope is exactly the three existing fixtures and
   `astranaut_macro_error_SUITE.erl`, with no source/dependency/non-owned test,
   generated, deletion, or staged product path.
2. Verify each fixture exports `/1` and `/2`, `/1` delegates to `/2`, `/2`
   uses `astranaut_lib:format_error/4`, private formatter clauses are specific,
   and fallback remains `fun astranaut_macro:format_error/2`.
3. Verify strict coverage for every local diagnostic reason that the shared
   formatter assertion exercises: `noop` and `noop_function`; `bar` and
   `macro_exception`; and `sibling_return_error` plus the globally supported
   sibling reasons. Confirm no private catch-all masks fallback or discards
   options.
4. Verify exact preservation of diagnostic order, positions, counts, reason
   terms, existing messages, macro runtime results, and accepted sibling error
   shapes. Confirm the new exact strict assertions prove private formatter
   execution rather than merely non-empty fallback output.
5. Verify generated local formatter identity, `/1` and `/2` exports, private
   helper non-exports, and use of the existing prefix/module-loaded assertion.
6. Audit reuse and simplicity: existing fixture loader, compiler, realization,
   formatter assertion, and global dispatcher are reused; no duplicate test
   harness, protocol detector, fallback policy, closure walker, or unrelated
   helper is introduced.
7. Correlate the raw runner packet with the diff without treating runner
   output as a semantic conclusion. Write exactly one review verdict,
   `passed` or `changes_required`, with actionable findings first when needed.

## Expected paths and authorized deletions

Expected modified paths:

- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`
- `test/astranaut_macro_error_SUITE.erl`

Expected added product paths: none.

Permitted untracked product paths: none.

Authorized deletions: none.

`docs/plan/format-error-migration/task-5.md` is the Sol-owned workflow
artifact and is not part of the product/test commit. Dispatcher-owned
initiative/status documents and pre-existing workflow entries remain outside
the Task 5 product scope.

## Proposed commit subject

```text
Migrate real local macro formatters
```

The dispatcher stages only the four accepted product/test paths and creates
the commit after coding self-tests, independent verification, and a passed Sol
review.

## Completion criteria

- All three real fixtures expose strict `/1` + `/2` formatters with `/1`
  compatibility wrappers and `astranaut_lib:format_error/4` dispatchers.
- Fixture-specific private clauses cover every local reason exercised under
  strict `#{default => throw}`; no private catch-all or fallback bypass exists.
- `noop`, `bar`, and `sibling_return_error` retain their prior messages, while
  `noop_function` retains its prior generic output and global macro reasons
  retain their existing output/exception behavior.
- The suite preserves existing diagnostic order, positions, count, reasons,
  macro-exception data, sibling shapes, and runtime assertions, and proves the
  generated local module's `/2` custom path directly.
- Private formatter functions are not exported from generated local modules;
  both formatter entry points are exported and the existing shared formatter
  assertion passes.
- Only the four declared product/test paths differ; no deletion, staging,
  source/dependency/workflow edit, or generated product artifact is included.
- Coding Self-Tests and separate Independent Verification complete with exact,
  unambiguous evidence and no interruption or timeout.
- Sol review reports no material findings before dispatcher commit.
