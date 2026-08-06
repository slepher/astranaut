# Task 5 Replan — Accept the Committed `dispatch_error/3` Migration

## Objective and execution state

Task 5 is now a post-commit acceptance task. Commit `6308e25`
(`Switch formatter callers to dispatch_error`) already contains the intended
fixture and suite changes, so no product implementation remains in this task.
The worker stages are limited to read-only self-test/verification execution;
any source or test edit is a scope violation and must stop for clarification.

The acceptance objective is to establish that the committed migration:

- uses `astranaut_lib:dispatch_error/3`, never the removed four-argument API;
- preserves `/1` default behavior and provides strict `/2` behavior through
  specific formatter clauses and strict options;
- preserves the real fixture diagnostics and proves generated local `/2`
  formatter execution;
- preserves nested `function_clause` propagation and global formatting behavior;
- has no uncommitted product diff, deletion, staging, or duplicate commit
  after `6308e25`.

The old unaccepted `task-5.md` is superseded by this clearly named replan. It
must remain immutable for audit history. The dispatcher must update its
dispatcher-owned status checkpoint to point to this artifact; Sol does not edit
`status.md`.

## Decisive evidence

- `git show 6308e25` shows the commit changed the shared callers from
  `astranaut_lib:format_error/4` to `astranaut_lib:dispatch_error/3` and changed
  the three Task 5 fixtures plus `astranaut_macro_error_SUITE.erl`.
- `src/astranaut_lib.erl:612-637` defines the committed API:
  `dispatch_error(Error, Options, FormatterFun)` invokes the one-argument
  formatter; on a top-level formatter no-match it calls
  `format_default_error(Error, Options)`; `#{default => throw}` throws the
  original reason, while ordinary options return the original char list or
  `io_lib:write(Error)`.
- `src/astranaut_lib.erl:619-626` preserves nested `function_clause` errors by
  checking the stack frame against the formatter fun and re-raising when the
  failure is internal rather than a top-level no-match.
- `src/astranaut_macro.erl:45-52` now uses `dispatch_error/3`; its specific
  clauses at `:55-155` handle macro diagnostics, while unmatched reasons use
  `format_default_error/2` rather than an injected fallback.
- `src/astranaut.erl:592-599` now uses `dispatch_error/3` and therefore owns
  generic option-validation fallback through the shared default formatter.
- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl:21-36` already
  exports `/1` and `/2`; its anonymous formatter has exact clauses for `noop`
  and `noop_function`, whose private results preserve `"oops, noop"` and
  `io_lib:write(noop_function)`.
- `test/astranaut_macro_SUITE_data/macro_with_error.erl:16-27` already exports
  `/1` and `/2`; its anonymous formatter has exact `bar` coverage and an
  explicit strict delegation for known `macro_exception` reasons.
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl:9-13` already
  exports both arities; its exact `sibling_return_error` clause preserves the
  prior generic message and its `macro_exception` clause delegates to the
  global strict formatter.
- `test/astranaut_macro_error_SUITE.erl:40-109` preserves the existing warning
  and error tuples and adds generated-local export/private assertions and
  strict calls for `noop`, `noop_function`, and `bar`.
- `test/astranaut_macro_error_SUITE.erl:372-457` preserves the three sibling
  diagnostic shapes and adds `sibling_local_formatter/1`, generated-module
  identity, export/private assertions, and strict custom-path coverage.
- `test/astranaut_test_lib.erl:118-178` calls `/2` with `#{default => throw}`
  for strict formatter modules, so every local diagnostic reaching these
  generated modules must be covered by an exact clause or by a known formatter
  clause that returns under strict options.
- `src/astranaut_macro_local.erl:795-846` remains the committed producer and
  selection boundary. Task 5 must verify its generated local formatter
  behavior, not alter production code.

## Exact API semantics to verify

No `format_error/4` or injected fallback is permitted in the Task 5 paths.
The committed callers use this shape:

```erlang
format_error(Error) ->
    format_error(Error, #{}).

format_error(Error, Options) ->
    astranaut_lib:dispatch_error(Error, Options, FormatterFun).
```

The formatter fun is intentionally fixture-specific:

- warnings: clauses for `noop` and `noop_function` calling private
  `format_error_1/1`;
- error: a `bar` clause calling private `format_error_1/1`, plus a known
  `macro_exception` clause delegating to
  `astranaut_macro:format_error(MacroError, #{default => throw})`;
- siblings: a `sibling_return_error` clause calling private
  `format_error_1/1`, plus the same known `macro_exception` delegation.

For a formatter fun no-match, `dispatch_error/3` uses
`format_default_error/2`: ordinary `/1` options produce the existing generic
character-list behavior, while strict `#{default => throw}` raises the
original reason. Known `macro_exception` delegation is a specific clause, not
an injected fallback. The review must confirm that the committed direct
delegation is limited to the known reason and does not swallow or rewrite
nested formatter failures.

## Owned paths and scope state

The four Task 5 paths already changed in `6308e25` are:

- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`
- `test/astranaut_macro_error_SUITE.erl`

They are reference paths for acceptance, not expected new working-tree edits.

The following are explicitly outside this Task 5 acceptance boundary:

- all product source, including the already committed `src/astranaut_lib.erl`
  and other caller migrations;
- `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl`,
  which was also adapted to `dispatch_error/3` by `6308e25` as a Task 4
  compatibility change;
- all other tests/fixtures, dependencies, build configuration, generated
  output, workflow documents except this replan and the plan update,
  `status.md`, root `status.md`, staging, commits, and external state.

## Invariants

- `HEAD` is exactly `6308e25` or a dispatcher-approved descendant that retains
  this commit; the four reference paths have no uncommitted product edits.
- No Task 5 path calls `astranaut_lib:format_error/4`, defines an injected
  fallback callback, or relies on a private catch-all to emulate fallback.
- `/1` delegates to `/2` with ordinary `{}` options. Strict `/2` custom clauses
  return the existing messages under `#{default => throw}`.
- `noop`, `noop_function`, `bar`, and `sibling_return_error` retain their
  established reason terms and messages. `macro_exception` and
  `invalid_macro_return` retain their existing global formatter behavior.
- Existing diagnostic order, positions, file association, counts, stack-payload
  matching, sibling error representation, and runtime assertions remain intact.
- Generated local modules are the formatter identities for the real local
  diagnostics; both `/1` and `/2` are exported, while private
  `format_error_1/1` is not.
- The shared formatter assertion remains the authority for non-empty
  character-list/strict protocol validation; no duplicate protocol checker is
  introduced.
- The commit's shared caller migration is accepted only together with its
  `dispatch_error/3` API behavior: top-level no-match uses
  `format_default_error/2`, and nested `function_clause` is re-raised.
- No source/test/workflow edits, deletion, staging, or new commit is part of
  this acceptance task.

## Ordered work steps (implementation delta: none)

1. Dispatcher confirms `HEAD` and worktree attribution, updates its status
   checkpoint to this replan, and supplies the committed source/path packet.
2. Coding worker reads the four reference paths and the committed API, makes
   no edits, and runs the Coding Self-Tests below. Any failed semantic
   expectation is reported as evidence; the worker does not patch it under
   this contract.
3. A separate `luna_runner` executes the Independent Verification commands
   mechanically and returns raw command results only.
4. Sol receives the real committed diff/history context and both packets,
   audits API semantics, fixture assertions, diagnostics, scope, and reuse,
   and writes exactly one Task 5 review verdict.
5. If review passes, dispatcher records acceptance of the existing `6308e25`
   boundary and proceeds to Task 6. If review finds a product defect, stop;
   the dispatcher must obtain a new authorized rework contract because this
   replan authorizes no product edits.

## Stop conditions

Return `Clarification required` with exact evidence if:

- `HEAD` is not `6308e25` or the four reference paths differ from the committed
  migration in an unattributable way.
- Any required correction would edit source, tests, the four reference paths,
  another product path, workflow status, staging, or a commit.
- Any caller or Task 5 reference path still uses `format_error/4`, an injected
  fallback, or a private catch-all that changes `dispatch_error/3` semantics.
- The committed exact formatter clauses do not cover all local diagnostics
  under strict `#{default => throw}`, or known global reasons no longer return
  their established messages.
- Diagnostics differ in count, order, positions, file, reason, message,
  macro-exception payload, sibling shape, or runtime behavior.
- Generated local formatter identity, `/2` invocation, or private export
  isolation cannot be proven through the existing suite without a product/test
  implementation change.
- A command is blocked, skipped, timed out, interrupted, or inconclusive.

## Coding Self-Tests

The coding worker owns these post-commit acceptance commands. They are not
implementation commands and must not edit product or workflow files. Sol must
not run them.

1. `rebar3 compile`.
2. Focused Task 5 cases:

   ```text
   rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl --case=test_macro_with_warnings
   rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl --case=test_macro_with_error
   rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl --case=test_macro_sibling_errors
   ```

3. `rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl`.
4. Run the directly affected caller/regression suites:

   ```text
   rebar3 ct --suite=test/astranaut_SUITE.erl
   rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl
   ```

5. `git diff --check`.
6. `git status --short`, `git diff --name-only`, and
   `git diff --cached --name-only`. Product paths must have no uncommitted
   changes, no deletions, and no staged paths; workflow metadata may remain
   dispatcher-owned and must be reported raw.
7. Report exact command, completion state, exit status, counts, timeout or
   interruption state, and generated/crash artifacts. A pass is not inferred
   from a different suite or from the commit message.

## Independent Verification

After the coding packet is complete, a separate `luna_runner` executes
mechanically:

1. `rebar3 compile`.
2. The three focused Task 5 commands above.
3. The complete `astranaut_macro_error_SUITE`.
4. The complete `astranaut_SUITE` and `astranaut_rebinding_SUITE`.
5. `rebar3 ct` as the full regression check for the shared caller migration.
6. `git diff --check`, `git status --short`,
   `git diff --name-only`, and `git diff --cached --name-only`.

The runner returns only raw mechanical evidence: exact commands, exit status,
completion/counts, timeout/interruption/skipped state, raw status/diff output,
and generated/crash artifacts. It must not inspect or judge fixture contents,
formatter clauses, assertion meaning, architecture, scope legitimacy, or
semantic correctness, and it must not edit source, tests, workflow documents,
status, staging, or commits.

## Sol semantic review requirements

The dispatcher supplies Sol with this replan, `plan.md`, the real committed
diff/history, the four reference paths, coding evidence, and the completed
runner-authored raw packet. Sol must not execute any command. Sol audits:

1. API migration: exact `dispatch_error/3` call shape, exported API, default
   fallback behavior, strict `#{default => throw}`, and nested
   `function_clause` preservation in `src/astranaut_lib.erl` and callers.
2. Fixture clauses: `noop`/`noop_function`, `bar`/known `macro_exception`, and
   `sibling_return_error`/known `macro_exception`; no format_error/4 or
   injected fallback remains in the reference paths.
3. Diagnostic assertions: exact existing warning/error sequences, positions,
   counts, reason terms, messages, stack payload matching, sibling shapes,
   generated-local identity, `/1` + `/2` exports, private non-exports, and
   direct strict custom-path assertions.
4. Capability reuse and simplicity: shared `dispatch_error/3`,
   `format_default_error/2`, `astranaut_macro`, fixture loader, realization,
   and formatter assertion are reused; no duplicate fallback/protocol/harness
   mechanism was introduced.
5. Scope: the four reference paths are already in `6308e25`; no current product
   diff, deletion, staging, or unauthorized path is accepted. The Task 4
   strict fixture change in the same commit is prerequisite context, not Task 5
   scope.
6. Evidence: runner output is treated as mechanical evidence only. Sol writes
   exactly one review verdict and reports any semantic finding independently.

## Expected paths and authorized deletions

Reference paths already committed by `6308e25`:

- `test/astranaut_macro_SUITE_data/macro_with_warnings.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`
- `test/astranaut_macro_error_SUITE.erl`

Expected new product paths for this replan: none.

Expected modified product paths for this replan: none.

Permitted untracked product paths: none.

Authorized deletions: none.

The old unaccepted `task-5.md` remains an immutable superseded contract.
`plan.md` and `task-5-replan.md` are Sol-owned workflow artifacts; `status.md`
is dispatcher-owned and must not be edited by Sol.

## Proposed commit subject

No new commit is authorized or required. The accepted migration boundary is
already committed as:

```text
6308e25 Switch formatter callers to dispatch_error
```

The dispatcher must not create a duplicate Task 5 commit after acceptance.

## Completion criteria

- The committed `6308e25` API migration is mechanically verified with no
  failure, interruption, timeout, or ambiguous result.
- Sol confirms `dispatch_error/3` semantics and all exact fixture clauses and
  strict-option paths described above.
- Existing diagnostics and assertions remain semantically unchanged except
  for the intended strict entry-point/export coverage already in the commit.
- Generated local `/2` execution and private formatter isolation are proven by
  the existing suite assertions.
- The four reference paths have no additional working-tree/staged changes and
  no deletion.
- Sol review passes; dispatcher records acceptance of `6308e25` and proceeds
  to Task 6 without creating a new Task 5 product commit.
