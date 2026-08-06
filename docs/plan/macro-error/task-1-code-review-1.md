# Task 1 Code Review — Round 1

Verdict: `changes_required`

## Findings

### P1 — Frozen sibling-order contract disagrees with the real implementation evidence

The Task 1 contract requires the local sibling fixture to preserve diagnostics in
“source sibling order” as:

1. framework-owned `macro_exception`;
2. generated-local `sibling_return_error`;
3. framework-owned `invalid_macro_return`.

This is frozen in `docs/plan/macro-error/task-1.md:123-125` and repeated by the
implementation step at `:179-181`. The real test diff instead asserts the established
traversal order at `test/astranaut_macro_error_SUITE.erl:444-454`:

1. `invalid_macro_return`;
2. `macro_exception`;
3. generated-local `sibling_return_error`.

The coding worker and independent runner both report the macro-error suite passing,
so the implementation evidence establishes the latter order. The source fixture
places the three sibling calls inside one quoted tuple, and no production change in
this task changes traversal ordering. Reordering production diagnostics to satisfy
the contract would violate the task’s preservation invariant and expand scope.

Smallest valid correction: revise the Sol-owned Task 1 contract before accepting the
task so it freezes preservation of the established diagnostic order and names the
observed sequence `invalid_macro_return`, `macro_exception`, then
`sibling_return_error`. Do not change product code or weaken the test to accept
multiple orders. After the contract correction, rerun the coding self-test packet
and obtain a fresh independent runner packet against the final contract before
review resumes.

## Review evidence

- Production diff is limited to the existing `invoke_macro_function/1` catch branch:
  `src/astranaut_macro_expander.erl:589-610`. It binds only the catch-produced
  failure through `astranaut_traverse:update_pos(Pos, astranaut_macro, ...)` and
  leaves the outer descriptor formatter boundary unchanged.
- The two existing fixture changes remove only `macro_exception` proxy clauses;
  `bar` and `sibling_return_error` remain direct domain clauses
  (`test/astranaut_macro_SUITE_data/macro_with_error.erl:16-20` and
  `macro_sibling_errors_test.erl:9-14`).
- The two declared external fixtures are present and cover an external exception,
  returned error, and returned warning
  (`test/astranaut_macro_SUITE_data/macro_error_external_provider.erl:9-25`,
  `macro_error_external_test.erl:8-17`).
- The external assertions distinguish `astranaut_macro` for the exception from
  `macro_error_external_provider` for returned diagnostics
  (`test/astranaut_macro_error_SUITE.erl:466-488`).
- Coding evidence is complete mechanically: compile exit 0, macro-error CT 18
  passed, macro-local CT 41 passed, and diff check exit 0 after the contract
  whitespace correction.
- Independent runner evidence is complete mechanically: all listed commands exited
  0, both suites passed with 18 and 41 tests, no command timed out or was
  interrupted, and the targeted scope contains the four tracked Task 1 files plus
  the two declared untracked fixtures.
- No additional implementation, scope, proxy, or capability-reuse finding remains
  from the supplied patch and evidence. The contract mismatch above is the sole
  blocking finding.

## Routing decision

Do not route product rework yet. First apply the exact contract correction described
under P1, then route the unchanged six-path implementation to the coding worker to
rerun its complete Coding Self-Tests, followed by a fresh independent runner. Request
the next Sol review only after both evidence layers are complete.
