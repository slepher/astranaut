# macro-error Workflow Plan

## Goal

`macro-error`

Make each macro diagnostic record the formatter that semantically owns its reason: macro-framework diagnostics use `astranaut_macro`, while diagnostics deliberately returned by a successful user macro use that macro descriptor's registry formatter. Remove the non-owning `astranaut_struct` formatter facade without changing diagnostic content, position, ordering, payload, or recovery.

## Inputs and Authority

- Initiative: `docs/plan/macro-error/`.
- Specification inputs:
  - `openspec/changes/macro-error/proposal.md`
  - `openspec/changes/macro-error/design.md`
  - `openspec/changes/macro-error/specs/macro-error-ownership/spec.md`
  - `openspec/changes/macro-error/tasks.md`
- Committed formatter protocol dependency:
  - `openspec/changes/transform-error/proposal.md`
  - `openspec/changes/transform-error/design.md`
  - `openspec/changes/transform-error/specs/transform-error-formatting/spec.md`
- Repository instructions: `AGENTS.md` and `lessons.md`.
- Baseline: `e8c1acab5d7a9b80e54efb074d30c39ec9a4c08c` (`e8c1aca`, `Align transform-error specs and workflow`). Initial runner reported no product diff and only the untracked initiative directory.

The selected macro-error OpenSpec is authoritative for formatter ownership, reason/payload preservation, sibling recovery, and removal of the struct facade. The later committed transform-error capability is authoritative for formatter invocation protocol: pure domain `format_error/1`, `astranaut_lib:format_error/1,2` shared adapter, no public formatter `/2`, no `dispatch_error/3`, no public `format_default_error/2`, and no throw-mode fallback.

## Constraints

- Process one task at a time through coding self-test, independent runner verification, Sol review, and dispatcher commit before starting the next task.
- Sol may write initiative planning/review artifacts only. Sol must not edit product source, tests, `status.md`, staging, or commits, and must not run test/build/lint/acceptance commands.
- Coding workers may modify only paths explicitly owned by the active task and must run that task's Coding Self-Tests.
- Independent Verification is executed only by a separate `luna_runner` after coding self-tests complete. The runner reports raw commands, completion state, exit status, CT counts, status/diff outputs, and artifacts; it does not perform semantic review.
- The dispatcher alone maintains `docs/plan/macro-error/status.md`, stages explicit accepted paths, and commits after a passed Sol review.
- Preserve unrelated worktree changes. No task authorizes broad formatting, dependency installation, generated artifact commits, staging, commits, delegation, or child spawning by Sol/coding/runner workers.
- No formatter fallback chain or reason-shape inference may be introduced. Ownership is fixed when the diagnostic is produced.
- Preserve existing macro AST semantics, registry selection, recursive expansion, local-module generation, exception reason/MFA/arguments/stack payload, position/file attachment, sibling ordering, and failed-call recovery.
- `lessons.md` applies: `invoke_macro_function/1` returns traverse computations; state isolation remains `scoped_state/2`; traversal diagnostics rely on the traversal boundary for position and formatter attachment.

## Dependencies and Reconciliation Boundary

The macro-error OpenSpec predates the completed transform-error migration and is mechanically stale:

- `openspec/changes/macro-error/proposal.md` and the ownership spec retain `dispatch_error/3`, `format_default_error/2`, nested-`function_clause` propagation, `/2`, and throw-mode requirements that directly conflict with `openspec/changes/transform-error/specs/transform-error-formatting/spec.md:26-83,128-141` and current `src/astranaut_lib.erl:612-632`.
- The macro-error design/task text requests deletion of `astranaut_struct:format_error/1,2`, but current `src/astranaut_struct.erl:18-76` exports and defines only `/1`.
- `openspec/changes/macro-error/tasks.md` names `macro_error.md`, but no such tracked or working-tree file exists. The repository's maintained macro documentation is the bilingual `# Macro` section in `README.md:606-621` and `README.zh.md:399-414`.

Therefore Task 1 is a prerequisite specification reconciliation. It may change protocol wording only; it must not weaken formatter ownership, diagnostic preservation, sibling recovery, or facade-removal behavior.

## Ordered Tasks

### Task 1 — Align macro-error specifications with the committed formatter adapter

- Objective: make all four macro-error OpenSpec documents executable against HEAD by replacing superseded formatter-protocol assumptions with the committed transform-error protocol and by resolving the nonexistent documentation target to the bilingual README macro sections.
- Owned area: the four files under `openspec/changes/macro-error/` listed in Inputs and Authority.
- Behavior boundary: documentation/specification only; no product source, test, workflow status, or unrelated OpenSpec edits.
- Prerequisite: none.
- Verification: strict macro-error OpenSpec validation; residual searches for superseded API requirements and nonexistent `macro_error.md`; exact diff/status scope checks.
- Completion: the OpenSpec remains complete for framework/user ownership, external/local/sibling scenarios, struct facade removal, and diagnostic preservation while depending explicitly on the transform-error adapter contract.
- Detailed contract: `docs/plan/macro-error/task-1.md`.

### Task 2 — Fix macro exception ownership and prove framework/user separation

- Objective: bind `macro_exception` to `astranaut_macro` at the catch branch that creates it, without extending that formatter override to successful user-returned computations.
- Owned area: `src/astranaut_macro_expander.erl`; `test/astranaut_macro_error_SUITE.erl`; the existing local fixtures `test/astranaut_macro_SUITE_data/macro_with_error.erl` and `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`; one narrowly named external-provider fixture and one caller fixture if existing fixtures cannot prove both outcomes without contaminating unrelated suites.
- Behavior boundary: local and external thrown exceptions use `astranaut_macro`; deliberately returned user error/warning uses the descriptor formatter; invalid returns and other framework reasons remain `astranaut_macro`.
- Prerequisite: Task 1 committed.
- Verification: focused macro-error CT with exact formatter/reason/position/payload/order/recovery assertions; macro local/uniform suites as affected-neighbor checks; compile, full CT, xref, strict OpenSpec validation, residual proxy search, and scope/diff checks.
- Completion: no user formatter contains a `macro_exception` proxy; local and external exception paths are framework-owned; user-domain diagnostics retain local/generated or provider identity; sibling recovery and all payloads are unchanged.

### Task 3 — Remove the struct formatter facade, document ownership, and complete acceptance

- Objective: remove the non-owning `astranaut_struct:format_error/1` export and forwarding clauses, prove registry fallback for struct macros and continued transformer ownership, and document migration and ownership in both maintained README macro sections.
- Owned area: `src/astranaut_struct.erl`, `test/astranaut_struct_SUITE.erl`, any existing focused registry/macro suite assertion strictly needed to inspect the `astranaut_struct` descriptor, `README.md`, and `README.zh.md`.
- Behavior boundary: `astranaut_struct` macro descriptors naturally select `astranaut_macro`; `astranaut_struct_transformer` remains owner of struct-transform reasons; only the facade API is removed.
- Prerequisite: Task 2 committed.
- Verification: struct and macro-error focused CT, compile, full CT with a real timeout of at least 120 seconds, xref, strict macro-error OpenSpec validation, residual export/proxy searches, bilingual documentation inspection, and exact scope/diff checks.
- Completion: the facade is absent, transformer diagnostics retain exact formatter/reason/message behavior, documentation gives direct migration guidance, every OpenSpec requirement is covered, all required checks pass, and no undeclared path is changed.

## Verification Boundaries

- Coding Self-Tests are contract-specific and run by the assigned coding worker after each implementation or rework.
- Independent Verification repeats task acceptance mechanically on the same worktree through a fresh runner. No Sol-authored claim or coding-worker summary substitutes for the runner packet.
- Sol review reads the real diff, surrounding code, assertions, OpenSpec, both evidence layers, and scope outputs. It checks patch correctness, semantic ownership, capability reuse, and absence of fallback/proxy reinvention without executing commands.
- Full CT belongs to product Tasks 2 and 3. Task 1 is specification-only and is gated by strict OpenSpec validation and exact scope checks; it does not require unrelated product tests.
- Any interrupted command is reported as interrupted, not failed or passed. Full CT uses a real timeout of at least 120 seconds.

## Initiative Completion Criteria

- The macro-error OpenSpec agrees with the committed transform-error formatter adapter and validates strictly.
- Every macro-framework reason is produced with `astranaut_macro`; the changed exception branch has focused local and external proof.
- Successful user-returned error/warning computations retain the registry formatter, including generated local formatter identity.
- `macro_exception`, user-domain error/warning, and `invalid_macro_return` sibling diagnostics preserve exact position, reason, payload, order, count, and recovery behavior.
- No user macro fixture proxies framework reasons and no formatter fallback chain or reason-shape routing exists.
- `astranaut_struct` no longer exports or defines `format_error/1`; registry fallback is `astranaut_macro`; `astranaut_struct_transformer` remains the struct-domain formatter.
- `README.md` and `README.zh.md` document ownership-at-production and the struct facade migration consistently.
- All task self-tests, independent verification, Sol reviews, and dispatcher commits complete with exact declared scope; final compile, focused suites, full CT, xref, strict OpenSpec validation, and diff checks satisfy their contracts.

## Stop Conditions

Stop and return to Sol/user before expanding scope if any of the following occurs:

- reconciliation would change transform-error's committed adapter semantics rather than make macro-error depend on them;
- ownership requirements conflict after Task 1, or an ownership decision cannot be made at the reason's production point;
- implementation requires changing diagnostic shapes, AST semantics, registry selection, local generation, recovery, or the shared adapter;
- a required path outside the active contract must change, an undeclared deletion appears, or worktree changes cannot be attributed safely;
- OpenSpec validation or required tests cannot start, are interrupted, or fail in a way that requires contract expansion;
- external approval, dependency installation, staging, commit, or another consequential authority is required.
