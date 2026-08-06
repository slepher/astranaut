# Task 6 — Final Post-Commit Acceptance

## Objective and execution state

Task 6 is the final acceptance task for the format-error migration. The
implementation boundary is already committed at HEAD 6308e25
(Switch formatter callers to dispatch_error). There is no implementation
delta, product edit, new fixture, deletion, staging operation, or new commit
authorized by this task.

The objective is to establish, from a complete regression run plus a final
Sol semantic audit, that the committed migration:

- preserves all existing diagnostics, ordering, positions, reasons, messages,
  stack payloads, counts, and formatter identities, plus the shared
  `format_error/1` default behavior defined by `dispatch_error/3`; caller-
  specific `astranaut:format_error/2` fallback chains are intentionally not
  preserved;
- consistently uses the exported astranaut_lib:dispatch_error/3 API and its
  shared default/strict/nested-function_clause semantics;
- keeps formatter forms outside local-macro generation members, request
  closures, frozen IDs, retain roots, generation boundary keys, callable
  state, and environment fingerprints;
- preserves the intended generated local formatter exports and private
  helper isolation;
- introduces no duplicate fallback, protocol, closure, lifecycle, or
  diagnostic machinery; and
- leaves the committed product boundary clean and the initiative ready for
  final completion.

The prior Task 5 review passed at
docs/plan/format-error-migration/task-5-review-1.md. Its accepted
dispatch_error/3 and four-fixture boundary are prerequisites, not work to
repeat or modify.

## Decisive evidence and relevant paths

HEAD must remain 6308e25 or a dispatcher-approved descendant that retains it.
The accepted commit changed the shared API and caller set:

- src/astranaut_lib.erl:15-24,612-656 exports dispatch_error/3 and
  format_default_error/2; top-level formatter function_clause invokes the
  shared fallback, strict #{default => throw} throws the original reason, and
  nested function_clause failures are re-raised with their original stack.
- src/astranaut.erl:592-599 and src/astranaut_macro.erl:45-52 use the shared
  dispatcher.
- src/astranaut_do.erl:36-50,
  src/astranaut_compile_meta_transformer.erl:35-45,
  src/astranaut_quote.erl:991-1005,
  src/astranaut_rebinding.erl:34-49, and
  src/astranaut_struct_transformer.erl:37-52 use the same /1-to-/2 wrapper
  and dispatch contract. src/astranaut_struct.erl:25-29 remains the intentional
  delegating facade to astranaut_macro.

The final API and diagnostics evidence is in:

- test/astranaut_SUITE.erl:128-145,220-235,286-300,330-346,498-578 for
  diagnostics, old default formatting, strict throwing, protocol behavior,
  shared fallback, and nested function_clause propagation;
- test/astranaut_rebinding_SUITE.erl:125-135,283-332 for strict/local and
  generic delegated diagnostics;
- test/astranaut_macro_error_SUITE.erl:23-109,131-174,372-457 for the real
  macro warnings/errors, three formatter protocols, local identity, strict
  entry points, private exports, and sibling diagnostic shapes;
- test/astranaut_test_lib.erl:115-178 for the shared formatter protocol
  assertion; and
- test/astranaut_design_SUITE.erl:107-113 for the committed public
  astranaut_lib export set.

The generation and lifecycle implementation is concentrated in
src/astranaut_macro_local.erl:

- :31-38 models formatter protocol and related closure metadata;
- :174-200 selects the generated formatter module without changing macro
  member identity;
- :207-224 performs final retain processing;
- :449-475 plans only declared macro members;
- :550-580 advances generation only after compilation and computes retained
  IDs;
- :599-616 exposes frozen/retained/non-closure state;
- :680-707 and :990-1019 derive expansion fingerprints and cache inputs;
- :808-846 derives formatter roots/closure from the declaration SourceView;
- :860-875 assembles formatter-related forms and exports separately from
  Members;
- :905-959 uses Members as the generation boundary key and commits only
  Members; and
- :1104-1116 marks only compiled local macros callable.

The focused lifecycle assertions at
test/astranaut_macro_local_SUITE.erl:9-50,62-222,589-695,697-830 cover
static closure/frozen IDs, cache and environment conflicts, retain skip IDs,
generation transitions, callable dependency state, fingerprint inputs,
protocol selection, formatter closure exclusion, formatter-only state
identity, generated exports, and private helper isolation.

## Scope and invariants

This is a read-only acceptance task. The expected product state is exactly
the committed state at 6308e25:

- no current product/source/test diff;
- no staged path;
- no product deletion;
- no additional product untracked path;
- no generated or dependency change attributable to Task 6; and
- no new commit.

Workflow metadata already present in the working tree, including initiative
status artifacts, plan artifacts, project-workflow-local/, and root status.md,
is dispatcher-owned and must be reported raw rather than treated as a Task 6
product change. Sol must not edit status.md, root status.md, or the local
workflow skill.

The following semantic invariants are acceptance gates:

1. API migration. Executable source and tests use dispatch_error/3; the
   removed astranaut_lib four-argument API is not exported or called. The
   shared fallback returns an existing character list or io_lib:write(Error)
   under ordinary options, throws Error under #{default => throw}, and
   re-raises nested formatter function_clause failures.
2. Formatter protocol. Every strict migrated formatter has /1 delegating to
   /2 with ordinary options. Specific clauses retain their existing messages;
   known delegation to astranaut or astranaut_macro remains specific and does
   not become a private catch-all. Legacy-only formatter behavior remains
   intentionally legacy.
3. Diagnostics. Full suites retain exact existing warning/error counts,
   order, file association, positions, reasons, messages, macro exception
   payloads, sibling representations, and runtime assertions. Existing
   `format_error/1` calls use the shared default behavior: deep char lists
   remain unchanged and other unmatched reasons use `io_lib:write/1`;
   caller-specific fallback is not an invariant.
4. Generated formatter identity. Real local diagnostics use the generated
   local module where the existing protocol requires it; strict local modules
   export /1 and /2, while format_error_1/1 and ordinary private formatter
   helpers remain unexported.
5. Generation boundary. Formatter roots and related forms may be selected
   into the temporary generated module, but formatter functions/helpers are
   absent from Members, request closure_ids, request closure_fas, request
   forms, candidate local macros, frozen IDs/forms, retain roots, retained
   IDs, generation boundary keys, committed Members, callable macro state,
   and macro-environment fingerprints.
6. Lifecycle. Formatter-only forms do not alter macro environment snapshots,
   cache fingerprints, committed boundaries, compiled generation, status,
   local_macro_expanded_ids, or callable lifecycle state. Compilation advances
   generation only after successful generated-module compilation/loading.
7. Capability reuse. The final state continues to use the shared dispatcher,
   default formatter, local closure analysis, fixture loader, realization
   path, and formatter protocol assertion. No replacement mechanism is
   introduced by the acceptance work.

## Ordered workflow

1. The dispatcher confirms HEAD, current worktree attribution, and the
   accepted Task 5 review; it updates its dispatcher-owned status checkpoint
   to Task 6 without changing accepted history.
2. A coding worker receives this no-op contract, makes no source/test edits,
   and runs the Coding Self-Tests. It reports exact raw command outcomes and
   artifacts.
3. After the coding packet is complete, a separate luna_runner repeats the
   Independent Verification mechanically against the same worktree. It does
   not rely on the coding packet and does not edit any file.
4. Sol receives the real HEAD/history, current scope, both raw packets, and
   the decisive source/test paths. Sol performs the final semantic,
   assertion, architecture, reuse, and scope audit without running commands.
5. Sol writes task-6-review-1.md with exactly one passed or changes_required
   verdict. A product finding stops completion; this no-op contract does not
   authorize rework. A passed review hands the initiative to the dispatcher
   for final completion, not another product commit.

## Stop conditions

Stop and return Clarification required, with exact evidence, if:

- HEAD is not 6308e25 or an attributable descendant;
- any product/source/test path is modified, untracked, deleted, or staged;
- the worktree contains a product change not explained by the accepted commit;
- a required check is skipped, blocked, failed, timed out, interrupted, or
  has incomplete/ambiguous output;
- the full Common Test regression does not complete successfully;
- any executable caller still uses the removed four-argument dispatcher or
  bypasses the shared fallback semantics;
- any diagnostic count/order/position/file/reason/message/stack payload,
  sibling shape, formatter identity, shared /1 default behavior, or strict
  semantic changes;
- formatter data enters a generation, retain, callable, frozen, closure, or
  fingerprint boundary covered by the lifecycle invariants;
- the existing assertions cannot establish the lifecycle and API invariants
  without editing product/test source; or
- any worker attempts source/test/workflow edits, staging, deletion, commit,
  delegation, or semantic conclusions outside its role.

## Coding Self-Tests

The coding worker owns this no-op acceptance layer. These commands are
mechanical only; the worker must not patch or otherwise change the worktree.
Use real timeouts, report exact exit status/counts, and report timeout,
interruption, and generated/crash artifacts explicitly:

1. timeout 180s rebar3 compile
2. timeout 180s rebar3 ct --suite=test/astranaut_macro_local_SUITE.erl
3. timeout 180s rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl
4. timeout 180s rebar3 ct --suite=test/astranaut_SUITE.erl
5. timeout 180s rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl
6. timeout 300s rebar3 ct
7. git diff HEAD^ HEAD --check
8. git diff --check
9. git rev-parse HEAD
10. git log -1 --format='%H %s'
11. git show --format= --name-only HEAD
12. git status --short
13. git diff --name-only
14. git diff --cached --name-only

The coding packet must state that no files, staging, or commits were changed.
It must distinguish raw workflow metadata from the absence of product paths.

## Independent mechanical verification

After the Coding Self-Tests pass, a separate luna_runner executes the same
fourteen commands mechanically. The runner returns only:

- exact command text and exit status;
- completion, test counts, failures, skips, timeout, interruption, and
  whether every requested command ran;
- raw output for status, name-only diffs, committed HEAD/path listing, and
  diff checks; and
- generated, coverage, log, and crash artifacts.

The runner must not inspect or judge source clauses, assertion meaning,
architecture, generation state, scope legitimacy, or semantic correctness.
It must not edit source, tests, workflow files, status, staging, or commits.
It must not call a failure a product defect; Sol owns that conclusion.

## Sol final review requirements

Sol must not execute CT, tests, builds, lint, or verification commands. Sol
must inspect the committed source, tests, history, and real scope and write
one immutable review artifact covering:

1. dispatch_error/3 export, call shape, ordinary fallback, strict throw, and
   nested function_clause stack preservation;
2. consistency of all migrated callers and intentional delegating/legacy
   formatter facades;
3. exact diagnostic assertions for warnings, errors, sibling failures,
   positions, counts, reasons, messages, stack payloads, and shared /1
   default behavior;
4. local formatter protocol selection, generated identity, /1 and /2
   exports, private helper isolation, and no local formatter for /2-only;
5. generation, retain, callable, frozen, closure, boundary-key, cache,
   fingerprint, environment snapshot, and compiled-generation invariants;
6. capability reuse and absence of duplicate fallback, protocol, closure,
   lifecycle, or test-harness machinery;
7. the distinction between accepted 6308e25 product state and workflow-only
   uncommitted files; and
8. the supplied coding and independent packets as mechanical evidence only,
   never as a substitute for semantic reasoning.

If no material finding remains and both packets are complete, the review
verdict is passed. Any material semantic, assertion, API, design, or scope
finding is changes_required and blocks final completion; no improvement
contract or product rework is implied by this no-op Task 6 plan.

## Expected paths and authorized deletions

Expected new product paths: none.

Expected modified product paths: none.

Permitted untracked product paths: none.

Authorized deletions: none.

The only artifact authored for this planning turn is:

- docs/plan/format-error-migration/task-6.md

The future review artifact task-6-review-1.md is created only by Sol during
the later review phase. No status.md, root status.md, plan history, source,
test, skill, staging, or commit may be changed by this task.

## Proposed commit and final handoff

No new commit is authorized or required. The accepted product boundary
remains:

6308e25 Switch formatter callers to dispatch_error

After a passed Task 6 review, the dispatcher records the final initiative
acceptance and sets the initiative phase to complete. The final routing
decision is:

- Next Task: none
- Next Sol: none
- Reason: Task 6 is the final full-regression and semantic-acceptance gate;
  no implementation delta or eligible follow-up task remains.

## Completion criteria

Task 6 is complete only when:

- compile and the complete Common Test regression pass without timeout,
  interruption, skip, or ambiguous result;
- the directly affected suites and local lifecycle suite pass in the same
  acceptance layer;
- committed and working-tree diff checks pass;
- HEAD and raw scope output confirm no Task 6 product changes, staging,
  deletion, or duplicate commit;
- Sol passes the API, diagnostic, lifecycle, generation, reuse, and scope
  audit described above;
- no worker edited source, tests, status, root status, skill, staging, or
  commits; and
- the dispatcher records initiative completion and returns the final
  acceptance summary.
