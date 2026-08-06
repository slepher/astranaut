# Task 4 Review Improvement 1

## Why the initial design and contract did not prevent the findings

### Strict fallback ownership

The plan and Task 4 contract required `astranaut_lib:format_error/4`, a private `format_error_1/1`, a transitive private helper, the global fallback, and a custom strict call. They did not require the fixture to demonstrate both sides of the dispatcher boundary: a matching custom reason and an unmatched reason forwarded with the original options. That omission allowed `macro_local_formatter_strict_test.erl:21-22` to add a catch-all that made the supplied fallback unreachable while all custom-path assertions and complete suites still passed.

Smallest recurrence-prevention correction:

- In the strict fixture, keep the anonymous formatter for closure discovery but restrict its clause to `strict_local_formatter_warning`.
- Keep `format_error_1/1` specific to that reason and retain its call to `strict_local_formatter_message/0`; remove its catch-all.
- In the strict integration case, preserve the custom `/2` assertion under `#{default => throw}`, then add one unknown-reason assertion for ordinary `/1` fallback and one for `/2` throw behavior. This proves the fourth argument is live and the original options reach it.

This correction belongs only in the Task 4 fixture and suite. It requires no product-source, plan, dependency, or workflow-skill change.

### Tautological `/2`-only assertion

The contract required exact `astranaut_macro` identity and use of the shared formatted-message assertion, but it did not explicitly require every additional assertion to compare independent observations. The identical-call comparison at `astranaut_macro_error_SUITE.erl:154-158` therefore added structural noise without strengthening acceptance.

Smallest recurrence-prevention correction:

- Remove only the tautological comparison.
- Retain the exact formatter tuple match and `assert_formatted_messages/1`; these are the existing project capabilities that own formatter identity and strict result validation.

No contract expansion or new helper is needed.

## Required rework boundary

Authorized rework remains confined to:

- `test/astranaut_macro_SUITE_data/macro_local_formatter_strict_test.erl`
- `test/astranaut_macro_error_SUITE.erl`

The other three accepted Task 4 paths need no semantic change. After rework, the coding worker and independent runner must repeat their assigned validation layers; Sol must continue to consume their evidence without executing commands.

## Skill-gap decision

No reusable `project-workflow-local/SKILL.md` gap caused these findings. The workflow correctly delivered the real diff and independent evidence to semantic review; the defects are task-specific fixture and assertion design issues. Do not create `task-4-review-improved-skill-1.md` and do not modify the local skill.
