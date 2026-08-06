# Task 1 Code Review 1 — `astranaut_rebinding` strict formatter migration

## Verdict

`changes_required`

## Scope and evidence

Reviewed the task contract, repository instructions, lessons, actual diff,
fixtures, callers/callees, shared dispatcher, test helper, adjacent formatter
migrations, and formatter history. Coding self-test and independent
verification both report `rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl`
exit 0 with 21/21 passing; `git diff --check` exits 0.

Expected Task 1 paths are the rebinding source, suite, and two fixtures. The
worktree also contains dispatcher-owned `status.md`, which is excluded from
the implementation commit.

## Responsibility map

| Responsibility | Owner |
|---|---|
| Compiler formatter identity | `astranaut_rebinding:format_error/1` |
| Strict formatter entry | `astranaut_rebinding:format_error/2` |
| Rebinding-owned reason coverage | `format_error_1/1` |
| Mismatch detection and nested exception preservation | `astranaut_lib:format_error/4` |
| Generic validator reasons | `astranaut:format_error/2` |
| Strict diagnostic checking | `astranaut_test_lib:assert_formatted_messages/1` |

## Capability-reuse audit

The patch correctly reuses `astranaut_lib:format_error/4`,
`astranaut:format_error/2`, `test_module_forms/2`, `compile_test_forms/1`, and
`assert_formatted_messages/1`. No production dispatcher, fallback, or
validation capability was reinvented. The `/1` to `/2` wrapper is required
public compatibility, and the dispatcher boundary is semantically required.

## Findings

### Medium — preserve public `/1` output

Before the patch, every non-character-list term used `io_lib:write/1`.
The new `format_error_1/1` returns `"invalid rebinding function: 42"`, changing
the compiler-facing output for `{invalid_rebinding_fun, 42}` without evidence.
The task contract explicitly requires preserving visible text.

Smallest correction: make
`format_error_1({invalid_rebinding_fun, Function} = Error)` return
`io_lib:write(Error)`, and add an exact assertion that legacy `/1` and strict
`/2` return the historical value.

### Low — remove test-only duplication or rename accessors

`warning_formatter/1` and `warning_reason/1` are used for both errors and
warnings and duplicate facts already established by tuple patterns. Remove
the redundant assertions/helpers, or rename them to neutral diagnostic names
if explicit accessor assertions are retained.

## Required rework and verification

1. Preserve and lock the historical invalid-function message.
2. Remove or neutrally rename the misleading test helpers.
3. Do not alter dispatcher routing, generic fallback, fixtures, or
   parse-transform behavior.
4. Rerun coding self-tests and independent verification before Review 2.

## Conclusion

The strict architecture, coverage boundary, generic delegation, unknown
strict throw, diagnostic ownership, and capability reuse are accepted. The
task remains rejected until the historical message is preserved and tested.

## Continuity

Reuse this review context for the correction review; the remaining change is
narrowly bounded.
