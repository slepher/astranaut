# Task 2 — Preserve the struct formatter and expose the public default API

## Dispatch decision

Route this task through the normal `luna_coding_worker` path, followed by a
separate `luna_runner` and Sol review. The staged simple path is not eligible:
the task includes coordinated product and regression-test edits. This is a
source-and-test implementation task.

The user decision is resolved authority. Do not reopen whether the struct
formatter should be removed, whether a warning is acceptable, or whether the
diagnostic count may change.

## Decisive evidence

- Task 1 is committed as `bd38f1d`; its review passed.
- `src/astranaut_struct.erl:21-76` exports `format_error/1`, but every current
  clause proxies to `astranaut_macro:format_error/1`. The export must remain and
  the clauses must be replaced by exactly one universal clause.
- `src/astranaut_lib.erl:15-24,612-632` exports `format_error/1,2`; its private
  `default_format_error/1` leaves deep character lists unchanged and otherwise
  calls `io_lib:write/1`. The public API must expose exactly that behavior, and
  `/2` must call the public helper.
- `src/astranaut_macro_registry.erl:401-419` treats an exported
  `format_error/1` as a present formatter and only emits
  `{missing_macro_formatter, Module}` for a missing export. Keeping the struct
  export therefore preserves the no-warning path without registry changes.
- `src/astranaut_struct_transformer.erl:14-46` owns the direct
  struct-transformer formatter clauses. That module is a verification surface,
  not an implementation target.
- Existing fallback coverage is in
  `test/astranaut_SUITE.erl:501-517,581-619`; existing struct formatter and
  compile coverage is in `test/astranaut_struct_SUITE.erl:230-294,296-353`.
  The struct suite's `init_per_suite/1` already loads
  `astranaut_struct_test` through `astranaut_test_lib:load_data_modules/2`,
  whose warning assertion is a direct no-warning gate.

## Frozen end state

### Product behavior

1. In `src/astranaut_lib.erl`, add `format_default_error/1` to the export list,
   rename the current private implementation to that exact public function, and
   preserve its exact branches:

   - a deep character list is returned unchanged;
   - every other term is returned as `io_lib:write/1`.

   `format_error/2` must call `format_default_error/1` when its formatter call
   raises `error:function_clause`. Do not add options, throw modes, or
   `format_default_error/2`.

2. In `src/astranaut_struct.erl`, retain `-export([format_error/1]).`, remove
   every proxy and reason-specific clause, and leave exactly:

   ```erlang
   format_error(Msg) ->
       astranaut_lib:format_default_error(Msg).
   ```

   There must be no struct `/2`, reason dispatch, fallback chain, registry
   special case, or private forwarding helper.

3. Keep `astranaut_macro_registry` unchanged. Its normal export detection must
   observe `astranaut_struct:format_error/1` as present, so compilation emits no
   `{missing_macro_formatter, astranaut_struct}` warning.

4. Preserve `astranaut_macro` ownership for framework-produced diagnostics and
   preserve `astranaut_struct_transformer` ownership and messages for
   struct-transform reasons. The task must not alter Task 1's macro exception
   ownership or introduce any reason-shape ownership inference.

### Regression behavior

Modify only the existing tests in the two owned test suites; no new fixture is
needed.

- In `test/astranaut_SUITE.erl`, extend the existing public/shared fallback
  cases so both branches are tested directly through
  `astranaut_lib:format_default_error/1`, and assert that
  `astranaut_lib:format_error/2` returns the same values for a deep character
  list and a non-character-list term. Keep the existing unknown-reason and
  nested-`function_clause` behavior assertions.
- In `test/astranaut_struct_SUITE.erl`, replace the old equality asserting that
  the struct formatter matches `astranaut_macro` with assertions that the
  export exists and that both a deep character list and an unknown term use the
  public default helper's exact result. Keep the five direct
  `astranaut_struct_transformer` reason/message checks and all compile-failure
  ownership/count checks.
- Add a named struct-suite regression case to the existing `all/0` list that
  compiles the existing `astranaut_struct_test` forms with
  `astranaut_test_lib:compile_test_forms/1` and asserts the resulting
  `astranaut_error:warnings/1` list contains no warning, specifically no
  `{missing_macro_formatter, astranaut_struct}`. Retain the existing
  `load_data_modules/2` no-warning assertion as an independent compile-path
  gate; do not add a new fixture.
- The unchanged `test/astranaut_macro_error_SUITE.erl` remains part of Task 2
  verification. Its existing assertions must continue to prove framework
  formatter identity, provider/generated-local identity for successful returned
  diagnostics, and Task 1 payload/recovery behavior.

## Invariants and forbidden alternatives

The implementation and tests must preserve:

- the public `astranaut_struct:format_error/1` export and callable universal
  fallback;
- exact default formatting semantics, `format_error/1,2` adapter behavior, and
  `format_default_error/1` arity/API;
- framework formatter identity (`astranaut_macro`) and struct-transformer
  formatter identity (`astranaut_struct_transformer`);
- diagnostic classification, file, position, reason, order, and count,
  including the absence of a struct missing-formatter warning;
- Task 1's macro exception class/reason/stack payload/MFA/arguments, AST results,
  failed-call recovery, and sibling recovery.

Forbidden: removing the struct export; retaining any proxy clause; adding a
reason-specific struct clause; adding `/2`, reason dispatch, fallback chains,
registry exceptions, formatter inference, or unrelated transform-error changes;
changing `astranaut_macro` or `astranaut_struct_transformer`; changing warning
deduplication; changing AST or diagnostic traversal semantics; or
adding/deleting fixtures.

## Owned paths and scope

The coding worker owns exactly these tracked paths:

Product and tests:

- `src/astranaut_struct.erl`
- `src/astranaut_lib.erl`
- `test/astranaut_struct_SUITE.erl`
- `test/astranaut_SUITE.erl`

`src/astranaut_macro_registry.erl`,
`src/astranaut_struct_transformer.erl`,
`test/astranaut_macro_error_SUITE.erl`, all fixture directories, and workflow
artifacts are not owned. No optional fixture path exists for this task.

## Ordered implementation steps

1. Confirm the worktree contains no overlapping edits on the four owned paths;
   preserve unrelated worktree edits. Confirm Task 1 commit `bd38f1d` without
   changing it.
2. Implement the `astranaut_lib` public helper/export and route `/2` to it,
   copying the current private helper behavior exactly.
3. Replace the complete `astranaut_struct` formatter block with the one frozen
   universal clause while retaining the export and all unrelated macro APIs.
4. Update the two owned suites with the exact direct helper, struct universal
   fallback, no-warning, transformer-ownership, and existing diagnostic
   preservation assertions. Do not edit the macro-error suite or fixtures.
5. Run all Coding Self-Tests below. Stop before handoff if any command fails,
   times out, is interrupted, or generates an out-of-scope path.

## Stop conditions

Stop and report the exact conflict without inventing an alternative if:

- an owned path has an overlapping change not attributable to Task 2;
- the public helper would change default rendering, `/2` fallback, exception
  propagation, or arity beyond the frozen API;
- keeping the struct export still produces a missing-formatter warning, or
  preserving it requires a registry special case;
- any diagnostic count/order, framework ownership, transformer ownership, AST,
  or recovery invariant changes;
- the implementation needs a path outside the owned list; or
- Coding Self-Tests or Independent Verification are missing, stale, interrupted,
  or failing.

## Coding Self-Tests — `luna_coding_worker`

Run exactly:

```text
rebar3 compile
rebar3 ct --suite=test/astranaut_SUITE.erl
rebar3 ct --suite=test/astranaut_struct_SUITE.erl
rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl
rebar3 xref
git diff --check
```

The coding report must include raw command completion/interruption state, exit
codes, test counts and generated artifacts, plus status and names of all changed
paths. The worker must not stage or commit.

## Independent Verification — separate `luna_runner`

After the coding worker passes, route a fresh runner to repeat every command in
the Coding Self-Tests block against the unchanged worktree. The runner must
also report:

```text
git status --short
git diff --name-only
git diff --name-only -- <all four owned paths>
```

The targeted list must contain only the four owned paths. No new fixture,
registry, struct-transformer, workflow, staging, or commit path may appear in
the Task 2 implementation diff. Report all test counts, validation results,
artifacts, timeout/interruption status, and any unrelated pre-existing
worktree state separately.

## Expected worktree paths and deletions

Expected Task 2 implementation changes are modifications to exactly the four
owned paths listed above. No new file is expected and no fixture is optional.
The Sol-authored `docs/plan/macro-error/task-2.md` artifact is workflow input,
not a coding-worker path; the worker must not edit it. The current separate
`docs/plan/macro-error/status.md` modification remains outside the task.

Authorized deletions: none. In particular, do not delete old task/review
artifacts or fixtures.

## Commit and completion

Proposed dispatcher commit subject:

```text
Preserve struct formatter with public default fallback
```

Completion requires the exact source/API end state, focused tests, no-warning
and ownership invariants, passing Coding Self-Tests, fresh Independent
Verification, a passed Sol review, and a dispatcher commit containing only the
four owned Task 2 paths. The dispatcher
updates `status.md` and performs staging/commit only after review; Sol does not
run tests, builds, lint, xref, acceptance, staging, or
commits.

Exact next action: route this contract to `luna_coding_worker`; after its full
self-test packet passes, route the unchanged worktree to a fresh
`luna_runner`, then route Sol review round 1.
