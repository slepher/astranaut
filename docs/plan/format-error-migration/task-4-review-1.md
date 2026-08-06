# Task 4 Review 1

Verdict: changes_required

## Findings

### Medium — The strict fixture makes the declared fallback unreachable and breaks `/1` compatibility for unknown reasons

Evidence:

- `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl:11-17` correctly routes `/1` through `/2` and passes `fun astranaut_macro:format_error/2` to `astranaut_lib:format_error/4`.
- However, `format_error_1/1` has a catch-all at `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl:21-22`. Therefore the formatter fun always returns or throws internally; the fourth-argument fallback is never selected by `astranaut_lib:format_error/4`.
- The catch-all also discards the caller's `Options` and forces `#{default => throw}`. Consequently `format_error(Unknown)` calls `/2` with `#{}` but still throws instead of preserving the normal global/default formatting behavior. This conflicts with the initiative's preserved `/1` fallback and the Task 4 invariant that `/1` is a compatibility wrapper using the library dispatcher and global fallback.
- `astranaut_lib:format_error/4` owns top-level no-match detection and forwards the original options to its fallback at `src/astranaut_lib.erl:613-628`; reproducing that decision inside `format_error_1/1` defeats the shared capability.

Smallest correction:

- Make the anonymous formatter passed at fixture lines 15-17 pattern-match only `strict_local_formatter_warning` and call private `format_error_1/1` from that matching clause.
- Remove the catch-all `format_error_1(Error)` clause. Keep `format_error_1/1` specific and keep `strict_local_formatter_message/0` as its second private helper.
- This preserves direct-call closure discovery, exercises the custom path under `#{default => throw}`, and allows the existing fourth-argument fallback to handle every unmatched reason with the original options.
- Add focused assertions that an unknown reason uses ordinary fallback for `/1` and throws that reason for `/2` with `#{default => throw}`. The existing custom-reason assertion must remain.

### Low — The `/2`-only case contains a tautological assertion

Evidence:

- `test/astranaut_macro_error_SUITE.erl:154-158` compares the result of `astranaut_macro:format_error/2` with the identical call. It cannot fail due to formatter selection or output behavior.
- Exact formatter identity is already established by the warning pattern at `test/astranaut_macro_error_SUITE.erl:153`, and the shared strict protocol/result check is already performed at line 159 by `astranaut_test_lib:assert_formatted_messages/1` (`test/astranaut_test_lib.erl:115-178`).

Smallest correction:

- Delete the self-comparison. Retain the exact `{8, astranaut_macro, invalid_macro_attribute}` match and shared formatted-message assertion; together they prove global formatter selection and strict formatting without duplicate protocol logic.

## Confirmed contract coverage

- Product scope is exactly the three authorized untracked fixtures and two authorized modified suites. `git diff --name-only` contains only the two suites; `git status --short` identifies the three fixtures separately; the cached diff is empty. Initiative documents, `project-workflow-local/`, and root `status.md` are workflow metadata outside the Task 4 product commit. No product source, dependency, generated, deletion, or staged path is present.
- The fixtures are structurally distinct: `/1` only at `macro_local_formatter_legacy_test.erl:7-12`, `/1` plus `/2` at `macro_local_formatter_strict_test.erl:7-25`, and `/2` only with a distinguishable local result at `macro_local_formatter_only_v2_test.erl:7-12`.
- The strict custom path invokes `astranaut_lib:format_error/4` and private `format_error_1/1`, which calls private `strict_local_formatter_message/0`; the suite invokes it with `#{default => throw}` and verifies both helpers are absent from exports at `astranaut_macro_error_SUITE.erl:129-140`.
- The `/2`-only diagnostic formatter is matched exactly as `astranaut_macro` at `astranaut_macro_error_SUITE.erl:153`; no local formatter is accepted.
- The local-suite case keeps exact member-only `members`, `closure_ids`, `closure_fas`, request-form keys, and frozen IDs, and explicitly excludes both formatter entries and both helpers at `astranaut_macro_local_SUITE.erl:732-830`.
- Test design reuses the existing fixture loader, compiler, realization, formatter-protocol assertion, generated-module prefix assertion, and existing local state harness. No parallel compiler, protocol detector, closure walker, or state machine was introduced.

## Mechanical evidence assessment

- The coding packet reports successful compile, focused cases, complete error suite (15 cases), complete local suite (42 cases), and clean scope checks.
- The independent packet reports compile success; complete error (15/15), local (42/42), Astranaut (37/37), rebinding (21/21), and full CT (442/442) success; empty cached diff; and clean `git diff --check`.
- The focused selector failure is not a product failure: `formatter_closure_is_private_and_identity_free` belongs to `astranaut_macro_local_SUITE.erl`, but the recorded command selected it from `astranaut_macro_error_SUITE.erl`, producing the reported `undef`. The complete owning suite passed 42/42, and the complete suites are the supplied mechanical acceptance evidence. No command was rerun during this review.

## Capability-reuse and design audit

| Responsibility | Existing capability and decision |
| --- | --- |
| Fixture loading/compilation/diagnostic realization | Reuses `astranaut_test_lib:test_module_forms/2`, `compile_test_forms/1`, and `realize_with_baseline/2`. |
| Formatter protocol and non-empty message validation | Reuses `astranaut_test_lib:assert_formatted_messages/1`; no duplicate detector was added. |
| Generated local-module identity | Reuses `assert_local_macro_module/2` in the existing error suite. |
| Formatter fallback | Must remain owned by `astranaut_lib:format_error/4`; the strict fixture's catch-all currently duplicates and changes this policy. |
| Compile-plan lifecycle state | Extends the existing `formatter_closure_is_private_and_identity_free/1` harness with direct map assertions; no second harness exists. |

## Local skill assessment

No reusable gap exists in `project-workflow-local/SKILL.md`. The current rules already require Sol-owned semantic/test-design review, two audit passes, completed runner evidence, actionable corrections, retrospective creation, and conditional skill changes. No improved-skill artifact or skill edit is warranted.
