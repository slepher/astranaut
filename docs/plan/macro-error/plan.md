# Goal: macro-error

## Authority and repository state

The user-authoritative end state is fixed:

- keep `astranaut_struct:format_error/1` exported;
- remove every current clause that proxies to `astranaut_macro:format_error/1`;
- implement exactly one universal clause:

  ```erlang
  format_error(Msg) ->
      astranaut_lib:format_default_error(Msg).
  ```

- establish the public `astranaut_lib:format_default_error/1` API used by that
  clause; and
- preserve diagnostic classification, order, and count, including no
  `{missing_macro_formatter, astranaut_struct}` warning.

This decision supersedes the macro-error OpenSpec text that removes the struct
facade. It is resolved authority, not a clarification gate.

The supplied mechanical baseline is HEAD
`b1499cfbd9c86614cedafc5899ab5c815ce1e542` (`Align macro error specs with
formatter adapter`, 2026-08-06). The worktree already contains edits to this plan,
`status.md`, the repository-local workflow skill, and four macro-error OpenSpec
files, plus deletions of old task/review artifacts. Those changes are pre-existing
input and are not silently attributable to any implementation task.

The four modified macro-error OpenSpec files are specifically unattributed until
the dispatcher confirms their scope:

- `openspec/changes/macro-error/proposal.md`
- `openspec/changes/macro-error/design.md`
- `openspec/changes/macro-error/tasks.md`
- `openspec/changes/macro-error/specs/macro-error-ownership/spec.md`

No task may stage or commit those existing edits merely because this plan names the
files. Task 2 may edit them only after explicit dispatcher attribution and scope
confirmation.

## Current facts and protocol reconciliation

### Source facts

- `astranaut_macro_expander:expand_macro_with/3` applies the descriptor formatter
  around invocation, while `invoke_macro_function/1` currently returns an
  unformatted `macro_exception` failure from its catch branch
  (`src/astranaut_macro_expander.erl:561-608`). A catch-local
  `astranaut_traverse:update_pos(Pos, astranaut_macro, ...)` can bind framework
  ownership without changing successful user-return ownership.
- External registry selection treats an exported `format_error/1` as a present
  formatter protocol; otherwise it selects `astranaut_macro` and emits the
  once-per-provider warning (`src/astranaut_macro_registry.erl:401-419`). Keeping the
  struct export therefore prevents `missing_macro_formatter` without a registry
  exception.
- `astranaut_struct` exports `format_error/1` and currently has only specific proxy
  clauses to `astranaut_macro` (`src/astranaut_struct.erl:21-76`).
- `astranaut_struct_transformer` independently exports direct clauses for its own
  struct-transform reasons (`src/astranaut_struct_transformer.erl:14-46`).
- `astranaut_lib` exports `format_error/1,2`; the `/2` fallback currently calls the
  private `default_format_error/1`, whose behavior is deep character list unchanged,
  otherwise `io_lib:write/1` (`src/astranaut_lib.erl:15-24,612-632`).
- Existing focused formatter coverage is in `test/astranaut_SUITE.erl:581-619` and
  `test/astranaut_struct_SUITE.erl:269-294`. Existing macro exception proxy fixtures
  include `test/astranaut_macro_SUITE_data/macro_with_error.erl:18-21` and
  `macro_sibling_errors_test.erl:11-14`.

### Narrow committed-protocol change

The committed transform-error design says `default_format_error/1` is
library-private (`openspec/changes/transform-error/design.md:46-67`). The new public
API intentionally changes only that statement and its API surface:

- rename/export the helper as `astranaut_lib:format_default_error/1`;
- preserve its exact default-format behavior;
- make `astranaut_lib:format_error/2` call the public helper for caught
  `error:function_clause`; and
- document `astranaut_struct:format_error/1` as an explicit compatibility/default
  formatter using that primitive, not as a proxy, ownership fallback chain, or
  reason-shape dispatcher.

The committed compiler tuple `{Position, astranaut_lib, {DomainFormatter, Reason}}`,
`format_error/1,2` dispatch semantics, dynamic `function_clause` handling, internal
formatter identity, and missing-provider warning protocol remain unchanged. The
transform-error capability already forbids public `format_default_error/2`, not the
new `/1`; no `/2` default-format API is introduced
(`openspec/changes/transform-error/specs/transform-error-formatting/spec.md:26-59,
101-130`).

### Behavior boundaries and invariants

1. Formatter identity is fixed at diagnostic production; final formatting does not
   infer ownership from reason shape.
2. External and generated-local macros that successfully return domain error/warning
   computations retain their registry formatter.
3. `macro_exception`, `invalid_macro_return`, and other framework-produced reasons
   remain owned by `astranaut_macro`. Exception class, reason, stacktrace, MFA,
   arguments, position, failure state, AST recovery, and sibling behavior are
   preserved.
4. `astranaut_struct:format_error/1` remains exported and contains exactly the one
   universal clause fixed above: no guards, proxy clauses, reason-specific clauses,
   `/2`, or private forwarding layer.
5. Because the export remains, registry behavior remains `present`: no
   `missing_macro_formatter` warning for `astranaut_struct`, and diagnostic count and
   order do not change.
6. The struct facade's callable formatting behavior becomes universal default
   rendering only. It does not make `astranaut_struct` the producer/owner of framework
   diagnostics.
7. `astranaut_struct_transformer` remains the formatter recorded for
   struct-transform reasons and retains its direct domain clauses and exact messages.
8. `format_default_error/1` is a public primitive with exactly the current private
   helper semantics. Existing `astranaut_lib:format_error/1,2` behavior is unchanged
   except that `/2` calls the newly public helper.
9. Apart from the required formatter identities and callable API behavior, preserve
   file, position, class, reason, payload, count, order, returned AST, and sibling
   recovery.
10. The four current macro-error OpenSpec edits remain unstaged/uncommitted input
    until explicitly attributed. No documentation-only first task is permitted.

## Ordered tasks

### Task 1 — Implement macro exception ownership with focused local/external regressions

**Objective:** Bind unexpected external and local macro exceptions to
`astranaut_macro` at the catch boundary while preserving provider/generated-local
ownership for successfully returned domain diagnostics and preserving sibling
recovery.

**Owned paths:**

- `src/astranaut_macro_expander.erl`
- `test/astranaut_macro_error_SUITE.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`
- `test/astranaut_macro_SUITE_data/macro_error_external_provider.erl` (new if needed)
- `test/astranaut_macro_SUITE_data/macro_error_external_test.erl` (new if needed)

No OpenSpec, README, registry, struct, library-adapter, local-generation, status, or
skill path is owned by Task 1.

**Behavior boundary:** Wrap only the catch-produced
`fail(macro_exception(...))` with
`astranaut_traverse:update_pos(Pos, astranaut_macro, ...)`. Keep the outer descriptor
formatter boundary for successful returns. Remove exception-only formatter proxy
clauses from focused fixtures. Add focused external and local assertions for raw
formatter identity, returned domain errors/warnings, invalid returns, exception
payload, position, classification, count/order, recovered call tree, and final
adapter-produced messages. Describe exceptions as unexpected fault containment;
expected domain failures are returned computations.

**Prerequisites:** The dispatcher records the pre-existing worktree paths as outside
Task 1 and confirms no unattributed edit overlaps the six owned paths. The struct/API
work is not a prerequisite.

**Coding Self-Tests (coding worker only):**

- `rebar3 compile`
- `rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl`
- `rebar3 ct --suite=test/astranaut_macro_local_SUITE.erl`
- `git diff --check`

**Independent Verification (separate `luna_runner` only):** Repeat the compile,
focused suites, and diff check against the completed Task 1 worktree; report raw
commands, completion state, exits, test counts, status, and diff names.

**Completion criteria:** External/local exceptions are raw
`astranaut_macro` diagnostics; successfully returned user error/warning diagnostics
retain external/generated-local formatter identity; invalid returns remain framework
owned; exception-only fixture proxies are gone; payload, positions, diagnostic
count/order, AST recovery, and messages meet the invariants; both evidence layers
pass; Sol review passes.

**Stop conditions:** Stop if the catch-local change would alter successful-return
ownership, payload/order/recovery cannot be preserved, an owned path has
unattributed overlapping edits, implementation needs registry/struct/adapter/OpenSpec
changes, or either evidence layer is missing, interrupted, stale, or failing.

### Task 2 — Preserve the struct formatter with universal default behavior and expose the default API

**Objective:** Keep the public struct formatter protocol present, replace its proxy
implementation with the exact universal fallback, expose the required library API,
prove warning-count preservation and formatter ownership boundaries, and reconcile
the specifications as part of the behavior change.

**Product/test owned paths:**

- `src/astranaut_struct.erl`
- `src/astranaut_lib.erl`
- `test/astranaut_struct_SUITE.erl`
- `test/astranaut_SUITE.erl`

Registry and `astranaut_struct_transformer` source are verification surfaces, not
owned implementation paths.

**Required specification paths:**

- `openspec/changes/transform-error/design.md`
- `openspec/changes/transform-error/specs/transform-error-formatting/spec.md`
- the four macro-error OpenSpec files listed in the authority section, but only after
  the dispatcher explicitly attributes their existing edits and confirms Task 2 may
  rework them.

**Frozen specification edit map:**

- Transform-error design: change only the private-default-helper statement/API
  example to public `format_default_error/1`, retaining the same semantics and
  `format_error/1,2` dispatch behavior.
- Transform-error capability: add the public `/1` default-format primitive and its
  deep-character-list/`io_lib:write/1` behavior; retain the prohibition on
  `format_default_error/2` and all adapter, fallback, ownership, and warning rules.
- Macro-error proposal: replace facade-removal/breaking-removal claims with preserved
  export, universal default behavior, and unchanged warning count.
- Macro-error design: replace the facade-removal decision, migration step, risk, and
  rollback text with the exact one-clause struct implementation, public library API,
  no-warning consequence, and ownership boundary.
- Macro-error ownership spec: replace the requirement/scenario that removes the
  export with a requirement that the export remains, proxy clauses are absent, the
  universal default clause is exact, registry emits no missing warning, framework
  reasons remain `astranaut_macro`, and struct-transform reasons remain
  `astranaut_struct_transformer`; retain exact diagnostic count/order preservation.
- Macro-error tasks: replace removal work with the implementation/API/tests/spec
  work above and update later README wording to describe behavior preservation rather
  than removal.

No worker may broadly rewrite the committed transform-error protocol, normalize
unrelated OpenSpec prose, or treat the four existing macro-error diffs as stageable
without attribution.

**Behavior boundary:** Export `format_default_error/1` and rename the current private
helper implementation to that public name; make `format_error/2` call it. In
`astranaut_struct`, preserve the export, delete all proxy clauses, and add exactly the
universal clause fixed by the user. Do not alter registry selection or warning code.
Tests must directly assert the public helper's two branches, equivalence of shared
fallback, the struct facade's universal rendering, export presence, absence of a
missing-formatter warning for struct imports, unchanged diagnostic count/order,
`astranaut_macro` ownership for framework-produced reasons, and
`astranaut_struct_transformer` ownership/messages for struct-transform reasons.

**Prerequisites:** Task 1 passed and was committed. Before Task 2 coding starts, the
dispatcher must (a) confirm attribution and edit permission for all four currently
modified macro-error OpenSpec paths, and (b) confirm no unattributed edits overlap
the product/test or transform-error specification paths. This is a worktree-scope
gate, not a product-design question.

**Coding Self-Tests (coding worker only):**

- `rebar3 compile`
- `rebar3 ct --suite=test/astranaut_SUITE.erl`
- `rebar3 ct --suite=test/astranaut_struct_SUITE.erl`
- `rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl`
- `rebar3 xref`
- `openspec validate macro-error --strict`
- `openspec validate transform-error --strict`
- `git diff --check`

**Independent Verification (separate `luna_runner` only):** Repeat every Task 2
self-test/validation command and return raw completion, exits, test counts, generated
artifacts, status, and diff names.

**Completion criteria:** The struct export remains; its implementation is exactly one
universal clause and contains no macro proxy; `format_default_error/1` is exported and
implements the prior private helper semantics; `format_error/1,2` behavior remains
compatible; struct registry protocol is present with no missing warning or count/order
change; framework and struct-transform ownership are proven at the raw diagnostic
boundary; the exact specification map is complete; both evidence layers pass; Sol
review passes.

**Stop conditions:** Stop if the four OpenSpec edits remain unattributed, an owned
path has conflicting user changes, the API requires changing default-rendering or
adapter semantics, preserving the export still produces a missing warning, a test
requires a registry special case, ownership/count/order changes, scope expands beyond
the listed paths, or either evidence layer is missing, interrupted, stale, or failing.

### Task 3 — Update public documentation and run initiative acceptance

**Objective:** After both behavior tasks are committed, update public documentation
and perform final whole-initiative acceptance. This later task does not make product
decisions.

**Owned paths:** `README.md`, `README.zh.md` only, plus its workflow artifacts. No
product, test, OpenSpec, status, or skill path is owned.

**Behavior boundary:** Document returned computations as the supported user-domain
diagnostic protocol; caught `error/throw/exit` as unexpected fault containment owned
by `astranaut_macro`; shared compiler adaptation through
`astranaut_lib:format_error/1,2`; public default rendering through
`format_default_error/1`; preserved `astranaut_struct:format_error/1` universal
default behavior; no missing struct warning; and continued
`astranaut_struct_transformer` ownership. Do not describe the struct API as removed or
recommend exceptions as domain diagnostics.

**Prerequisites:** Tasks 1 and 2 passed and were committed; Task 2 specifications are
attributed, reconciled, committed, and strictly valid; no unrelated edit overlaps the
README paths.

**Coding Self-Tests (coding worker only):**

- `rebar3 compile`
- focused macro/local/struct/Common Test suites from Tasks 1-2
- full `rebar3 ct` with a real timeout of at least 120 seconds
- `rebar3 xref`
- `openspec validate macro-error --strict`
- `openspec validate transform-error --strict`
- `git diff --check`

**Independent Verification (separate `luna_runner` only):** Repeat the complete Task
3 acceptance set and return raw commands, completion/interruption state, exits, test
counts, generated artifacts, status, and diff names.

**Completion criteria:** Both README languages match implemented and specified
behavior; focused/full regression, xref, both strict OpenSpec validations, and diff
checks pass in both evidence layers; final Sol review has no material findings; each
task has an exact-scope dispatcher commit.

**Stop conditions:** Stop if documentation exposes a new behavior ambiguity, differs
from committed code/specification, requires product/test/OpenSpec changes, overlaps
unattributed edits, or any required evidence is missing, interrupted, stale, or
failing.

## Workflow and verification gates

- Sol freezes each task's decisions in `task-M.md` before coding and does not edit
  product source, tests, `status.md`, staging, or commits.
- The coding worker implements the frozen contract and runs every Coding Self-Test.
- Only after those pass, a separate `luna_runner` performs Independent Verification.
  Sol never runs compile, CT, build, lint, xref, OpenSpec validation, or acceptance
  commands; it consumes the completed runner-authored packet.
- Sol reviews the real diff, surrounding source, assertions, contract, coding
  evidence, and runner packet. Rework repeats coding self-tests and independent
  verification. No task passes without a `passed` Sol review.
- The dispatcher alone updates `status.md`, confirms worktree attribution, stages
  explicit accepted paths, checks commit scope, and commits each passed task. Existing
  unrelated modifications/deletions are never swept into a task commit.
- On Windows Codex sandbox, workers follow `AGENTS.md`: direct escalated `rebar3`
  compile/CT commands, not `scripts/rebar3_sandbox.ps1`; full CT receives a real
  timeout of at least 120 seconds.

## Initiative completion criteria

The initiative is complete only when all three tasks pass in order; external/local
exception ownership and user-return ownership are proven; the struct export and exact
universal fallback are present; the public default API and narrow specification
change are complete; no struct missing-formatter warning or diagnostic-count change
occurs; struct-transform ownership remains intact; public documentation agrees; all
self-test, independent-verification, review, and exact-commit gates complete.

## Initiative stop conditions

Stop and return exact evidence without inventing a design when required work expands
beyond a task's owned paths, an unattributed worktree edit overlaps required scope,
the dispatcher has not attributed the four macro-error OpenSpec edits before Task 2,
source contradicts a frozen contract, preserving diagnostic content/count/order or
ownership proves impossible, or required evidence is missing/interrupted/failing.
Do not ask for another struct warning/count decision: the answer is fixed as no new
warning and unchanged count.

## Exact next action

The dispatcher updates `docs/plan/macro-error/status.md` to record this accepted plan
revision and sets the singular next action to: route Task 1 to Sol to write
`docs/plan/macro-error/task-1.md` from the Task 1 contract above, while preserving all
pre-existing non-Task-1 worktree changes as out of scope.
