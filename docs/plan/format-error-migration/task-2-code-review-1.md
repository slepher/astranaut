# Task 2 Code Review 1 — Tighten the Test Helper Formatter Protocol

## Scope and baseline

Task scope is the committed Task 1 baseline plus the Task 2 changes in:

- `test/astranaut_test_lib.erl`
- `test/astranaut_SUITE.erl`
- `test/astranaut_SUITE_data/sample_transformer_only_v2.erl`

Production formatter dispatch, including `astranaut_lib:format_error/4`, is
outside Task 2 ownership. Coding Self-Tests reported the focused main suite,
focused rebinding suite, full Common Test suite, and diff check passing. The
separate runner then supplied the completed authoritative packet below.

## Result

Verdict: passed

No material finding remains. The coding self-test layer and independent runner
verification layer both satisfy the Task 2 contract. The implementation is
limited to the declared test-helper protocol, its protocol tests, and its
only-`/2` fixture.

## Independent runner evidence

The following is the runner-authored packet, reproduced without substitution:

> Status
>
> PASS for Task 2 implementation; scope caveat noted.
>
> Result
>
> All required commands completed, none interrupted:
>
> - `rebar3 ct --suite=test/astranaut_SUITE.erl` — exit 0, 37/37.
> - `rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl` — exit 0, 21/21.
> - `rebar3 ct` — exit 0, 434/434.
> - `git diff --check` — exit 0.
> - `git status --short` — exit 0.
> - `git diff --name-only` — exit 0.
> - `git diff --cached --name-only` — exit 0, no staged paths.
>
> Evidence
>
> - Strict `/1`+`/2` invokes `/2(Error, #{default => throw})` at `test/astranaut_test_lib.erl:120-123`.
> - Legacy `/1` invokes `/1(Error)` at lines 124-127.
> - Only-`/2` and no-`/1` reject with `missing_format_error_1` at lines 132-145.
> - Results require non-empty deep character lists at lines 167-178.
> - Protocol tests cover `astranaut`, `sample_transformer_1`, only-`/2`, and no formatter at `test/astranaut_SUITE.erl:520-526`.
> - Only-`/2` fixture exports solely `format_error/2`: `test/astranaut_SUITE_data/sample_transformer_only_v2.erl:4-9`.
> - 45 current helper call sites were inventoried across test suites.
> - No `src/` files appear in the diff.
> - Dispatcher implementation remains unchanged; nested `function_clause` propagation is intact at `src/astranaut_lib.erl:613-628`.
>
> Changes
>
> None. No files were edited, staged, committed, reset, or deleted.
>
> Caveats
>
> The Task 2 product scope matches the two tracked test files plus one fixture. The worktree also contains unrelated workflow metadata changes/untracked files: `plan.md`, `status.md`, review artifacts, and `project-workflow-local/`.

## Responsibility map

| Responsibility | Owner | Review result |
|---|---|---|
| Establish a valid compiler formatter identity by requiring `format_error/1` | `test/astranaut_test_lib.erl` | Correct; only-`/2` and no-`/1` modules fail explicitly. |
| Exercise strict formatter behavior when both arities exist | `test/astranaut_test_lib.erl` | Correct; `/2` receives the exact `#{default => throw}` contract. |
| Preserve legacy formatter compatibility | `test/astranaut_test_lib.erl` | Correct; `/1`-only formatters are checked as legacy, without claiming strict coverage. |
| Validate formatter output shape | `test/astranaut_test_lib.erl` | Correct; both paths share the non-empty deep-character-list requirement. |
| Prove protocol branches | `test/astranaut_SUITE.erl` and fixture | Correct; strict, legacy, only-`/2`, and absent-formatter identities are represented. |
| Own production formatting and nested exception semantics | Existing formatter modules and `astranaut_lib:format_error/4` | Unchanged and outside this task. |
| Execute independent acceptance commands | Separate `luna_runner` | Completed; evidence packet preserved above. |
| Audit correctness and simplicity | Sol | Two passes completed from source inspection and runner evidence. |

## Pass 1 — patch correctness

The protocol boundary is ordered correctly: formatter loading/identity is
resolved before capability selection; `/1` remains mandatory; `/2` is selected
only when the valid formatter also exports it. This prevents an only-`/2`
module from being mistaken for a compiler formatter.

The strict branch uses the exact options map required by the migration and does
not silently fall back when strict formatting throws. The legacy branch invokes
only `/1` and records legacy coverage rather than presenting it as strict. Both
branches pass through one output-shape check, so empty output, improper textual
data, and non-character deep lists cannot pass merely because the formatter
function returned normally.

The four protocol identities are covered by real modules or the explicit
absence case. The only-`/2` fixture is minimal and proves the identity rule
rather than another formatting behavior. Existing helper consumers remain on
the shared helper path; the inventory of all 45 call sites and the full-suite
result show that tightening the common boundary did not strand a caller.

No production source changed. The test helper still reaches real formatter
owners instead of duplicating formatter text, and nested `function_clause`
behavior remains owned by the unchanged shared production dispatcher.

## Pass 2 — capability reuse

| Capability needed | Existing/reused capability | Assessment |
|---|---|---|
| Load and inspect formatter modules | Existing Erlang module-loading/export introspection in the helper | Reused at the protocol boundary; no registry or parallel dispatcher added. |
| Select strict versus legacy arity | Existing shared formatter-checking path | One necessary capability branch, not duplicated at 45 callers. |
| Enforce textual output | One shared deep-character-list validator | Reused by strict and legacy paths; no branch-specific duplicate assertion. |
| Report invalid formatter identity | Existing helper failure/diagnostic mechanism | Extended with the precise missing-`/1` reason; no wrapper exception layer. |
| Exercise real ownership | Existing formatter modules plus one minimal invalid fixture | Reuses production ownership; the fixture supplies only the otherwise missing protocol shape. |
| Preserve production fallback/exception behavior | Existing `astranaut_lib:format_error/4` | Correctly left untouched rather than reinvented in test support. |

No unnecessary abstraction was introduced. Centralizing protocol enforcement in
the existing helper is the smallest change because every current suite consumer
already enters through it. The new fixture is justified by a capability shape
that no valid formatter should otherwise expose.

## Representative call paths and jumps

1. Existing suite assertion → shared formatter checker → load/identity check →
   `/1`+`/2` capability check → `/2(Error, #{default => throw})` → shared
   non-empty deep-character-list validation.
2. Existing suite assertion → shared formatter checker → load/identity check →
   `/1`-only branch → `/1(Error)` → the same output validation → legacy result.
3. Protocol test → shared formatter checker → only-`/2` or no-`/1` identity →
   `missing_format_error_1` failure before formatting.

The module-load jump, mandatory-`/1` identity gate, arity branch, and common
output validator are required protocol steps. No noisy caller wrapper,
formatter proxy, callback chain, or second dispatch implementation was found.

## Parameters, data shapes, naming, and duplication

- Strict parameter order remains formatter-owned `Error` followed by the exact
  options map; no alternate context shape was introduced.
- Legacy input remains the same `Error`; its classification prevents an
  inaccurate strict-coverage claim.
- The accepted output is one explicit shape: a non-empty deep character list.
- `missing_format_error_1` names the actual invalid-identity condition for both
  only-`/2` and absent-`/1` cases.
- Capability selection and output validation remain centralized; no duplicated
  per-suite protocol logic or formatter text was introduced.

## Findings

No material correctness, compatibility, reuse, readability, naming,
data-shape, duplication, or scope finding remains.

## Scope and limitations

The product diff is confined to the two declared tracked test paths and the
single protocol fixture; no production source or staged path is present.
Workflow metadata listed in the runner caveat is not Task 2 product scope and
does not affect this result.

This review did not execute commands. Runtime, call-site, staging, and diff
hygiene conclusions rely on the completed runner-authored packet; semantic and
simplicity conclusions rely on source inspection of the Task 2 helper, tests,
fixture, and unchanged dispatcher boundary.
