# Task 1 Code Review 1

## Status

Completed.

## Verdict

`changes_required`

## Findings

### Medium — `?RETURN_FAIL` compiler-adapter branch lacks its contracted semantic assertion

Evidence:

- `src/astranaut_return.erl:180-195` has separate `?RETURN_OK` and `?RETURN_FAIL` clauses, and Task 1 changes both to apply `compiler_diagnostics/1`.
- `test/astranaut_SUITE.erl:530-552` exercises the adapter only with `astranaut_return:ok/2`: one OK struct containing errors+warnings and one OK struct containing warnings. It never constructs `astranaut_return:fail(ErrorStruct)`.
- `docs/plan/transform-error/task-1.md` Ordered Step 1 explicitly requires `to_compiler/1` adapter assertions for failure with errors+warnings. The OpenSpec “失败返回包含 errors 和 warnings” scenario likewise begins from a failed return monad.

Impact:

The implementation is presently correct by inspection, but the independent packet's 443 passing tests cannot protect the distinct failure clause from omission or regression. Task 1's assertion-semantics completion criterion is therefore unmet.

Smallest valid correction:

In the existing `test_to_compiler_adapter/1`, reuse `ErrorStruct`, `WrappedErrors`, and `WrappedWarnings` and add:

```erlang
?assertEqual(
   {error, WrappedErrors, WrappedWarnings},
   astranaut_return:to_compiler(
     astranaut_return:fail(ErrorStruct))).
```

Keep the existing `ok/2` assertions because they separately cover the successful-monad warning path and the defensive OK-with-errors path. No product-source change is requested.

## Review Summary

No other material finding was identified. The inspected implementation otherwise matches the adapter design: internal `astranaut_error:realize/1` ownership remains unchanged; `to_compiler/1` wraps position/formatter/reason without changing grouping or order; `astranaut_lib:format_error/1,2` owns shared fallback; production formatters expose pure `/1` clauses; local formatter closure exports only `/1` plus private real helpers; only-v2 fixtures remain negative; and no Task 2 missing-formatter warning was introduced.

The runner-authored packet is complete for the independent mechanical gate: compile and all listed suites completed with exit 0, full CT reported all 443 tests passed, residual searches reported only legitimate negative-fixture markers, and `git diff --check` was empty. Those results remain valid evidence but do not replace the missing assertion.

## Scope

The Task 1 source/test paths inspected are within the declared owned set, including the quote-suite amendment. No staged changes or unauthorized product deletions were found. The separate pre-existing OpenSpec/workflow/skill changes remain outside coder scope and must not be staged with Task 1.

## Next Worker

`luna_coding_worker`: apply only the test correction above, run the full Task 1 Coding Self-Tests, then route to a fresh independent `luna_runner` packet before Code Review 2.
