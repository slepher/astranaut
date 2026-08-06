# Task 6 Review 1

Verdict: changes_required

## Blocking finding — historical /1 fallback behavior is not preserved

The committed API migration changes three public formatter callers from an
explicit Astranaut fallback to the generic dispatch fallback, without adding
specific clauses that preserve the old cross-module behavior:

- Before 6308e25, src/astranaut_macro.erl:49-53 called
  astranaut_lib:format_error/4 with astranaut:format_error/2 as its fallback.
  At HEAD, src/astranaut_macro.erl:49-52 calls astranaut_lib:dispatch_error/3,
  whose unmatched path is only format_default_error/2.
- Before 6308e25, src/astranaut_compile_meta_transformer.erl:38-41 used the
  same astranaut:format_error/2 fallback. At HEAD, lines 38-40 use only
  dispatch_error/3, and format_error_1/1 at lines 42-45 covers only the two
  compile-meta-specific reasons.
- Before 6308e25, src/astranaut_quote.erl:994-997 used the same explicit
  Astranaut fallback. At HEAD, lines 994-996 use only dispatch_error/3,
  while its private formatter clauses cover quote-specific reasons.

This is observable for an Astranaut-owned generic reason such as
{invalid_option_value, bad} or any validate_key_failure tuple. The old
fallback reached src/astranaut.erl:601-614, which formats those reasons with
their established messages, under both ordinary and strict options. The
current src/astranaut_lib.erl:629-637 instead returns io_lib:write(Error)
under ordinary options and throws the original reason under
#{default => throw}. Thus the public /1 compatibility promise in plan.md:30-33
is not preserved, and strict /2 behavior also changes from a formatted generic
reason to a throw for these callers.

The current tests do not close this gap. test/astranaut_SUITE.erl:498-518
checks the generic behavior on astranaut itself; the macro, quote, and
compile-meta tests exercise their own formatter reasons. The complete
mechanical packets therefore establish execution health but do not establish
old /1 fallback equivalence for these three public modules.

Smallest authorized resolution would require a new implementation contract:
either preserve the module-specific fallback semantics in the shared API
design, or add explicit clauses in each affected formatter for the complete
Astranaut-owned generic reason set, with direct ordinary /1 and strict /2
assertions. No product rework is authorized by Task 6, so this review cannot
accept the initiative as complete.

## Other audit results

The remaining gates were inspected and revealed no additional material
finding:

- src/astranaut_lib.erl:15-24,612-656 correctly exports dispatch_error/3,
  performs top-level no-match fallback, preserves #{default => throw}, and
  re-raises nested function_clause failures after comparing the formatter
  frame.
- The migrated callers at src/astranaut.erl:592-599,
  src/astranaut_macro.erl:45-52, src/astranaut_do.erl:36-50,
  src/astranaut_compile_meta_transformer.erl:35-45,
  src/astranaut_quote.erl:991-1005,
  src/astranaut_rebinding.erl:34-49, and
  src/astranaut_struct_transformer.erl:37-52 use the three-argument API.
  src/astranaut_struct.erl:25-29 remains an intentional facade to
  astranaut_macro; the legacy astranaut_disable_tco formatter remains
  outside the structured-error migration.
- The macro fixtures retain exact local warning/error reasons, counts,
  positions, messages, sibling shapes, generated local identities, strict
  entry points, and private formatter isolation at
  test/astranaut_macro_error_SUITE.erl:40-109,131-174,372-457.
- The shared dispatcher, strict protocol, and nested-error assertions remain
  centralized in test/astranaut_SUITE.erl:520-578; the public export set is
  asserted at test/astranaut_design_SUITE.erl:107-113.
- The local-macro implementation keeps formatter roots and related forms
  separate from macro members and exports at
  src/astranaut_macro_local.erl:811-846,860-875. Generation keys and
  committed members remain member-only at :905-959; callable state is
  derived only from compiled local macros at :1104-1116; fingerprints and
  cache selection remain based on macro environments and local versions at
  :680-707,990-1019.
- The focused lifecycle assertions at
  test/astranaut_macro_local_SUITE.erl:732-830 exclude formatter entries
  from members, closure IDs/FAs, request forms, frozen IDs, and retained
  macro state while checking formatter-only state identity, generation,
  callable status, fingerprints, and private exports. The broader suite
  covers cache, retain, boundary, and generation behavior.
- The implementation reuses the shared dispatcher, default formatter,
  existing closure analyzer, fixture loader, diagnostic realization, and
  protocol assertion. No duplicate fallback detector, closure walker,
  lifecycle model, or test harness was introduced.
- The committed product boundary is the 16-path 6308e25 change. The current
  worktree contains only workflow metadata in its uncommitted diff
  (docs/plan/format-error-migration/plan.md and root status.md), with
  initiative artifacts and project-workflow-local/ outside the product
  commit; cached paths are empty and no product deletion is present.

## Mechanical evidence assessment

Both supplied packets are complete mechanical evidence: all fourteen commands
ran and exited 0, with compile success, local suite 42 cases, macro error
suite 15 cases, Astranaut suite 37 cases, rebinding suite 21 cases, full
Common Test 442 cases, clean committed/worktree diff checks, HEAD
6308e2584b1e89114a018831e253b5e196abbefc, no staged product paths, and no
crash artifacts. These results do not override the source-level fallback
regression identified above.

No reusable workflow-skill gap was identified. The blocker is a product API
compatibility/design issue in the committed migration, not a routing or
review-process deficiency. No improvement artifact or skill edit is created.

## Final routing

Task 6 cannot hand off initiative completion. The dispatcher must obtain a
new authorized contract addressing the historical Astranaut fallback behavior
before any product rework, new self-tests, independent verification, or
further Sol review.
