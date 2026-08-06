# Task 3 Code Review 2

Verdict: changes_required

## Actionable finding

The derive-once/thread invariant is still unmet. `local_formatter_info/1` is derived through `formatter_info_for_source/2` in three operation stages:

- `src/astranaut_macro_local.erl:81-85`, `handle_form/3`;
- `src/astranaut_macro_local.erl:207-213`, `finish_attribute_pass/3`;
- `src/astranaut_macro_local.erl:895-900`, `ensure_formatter_info/1`.

`compile_boundary/3` now consumes threaded information at `src/astranaut_macro_local.erl:925-938` and does not itself recompute it, but that fixes only the prior consumer-side duplication. The operation still has multiple formatter-info producers/derivation paths.

Smallest valid correction: establish one operation-owned formatter-info producer and thread its resulting value through every stage from the local form pass to `compile_boundary/3`. `handle_form/3`, `finish_attribute_pass/3`, and `ensure_formatter_info/1` must consume or carry the existing value rather than call `formatter_info_for_source/2` again. Preserve the current protocol, closure, selection, export, and member behavior. Do not solve this by adding another cache or by changing identity-sensitive context shapes without stopping for clarification.

## Audit pass 1 — contract and source structure

The packet confirms that the following requirements remain satisfied:

- Protocol precedence is at `src/astranaut_macro_local.erl:822-829`.
- Existing closure machinery is reused at `src/astranaut_macro_local.erl:831-839`.
- Selection is the required union at `src/astranaut_macro_local.erl:859-862`.
- Exports are the required union at `src/astranaut_macro_local.erl:863-864`.
- `Members` remains the value passed to `commit_compiled/3` at `src/astranaut_macro_local.erl:933-940`.
- `maybe_add_formatter/2` and `has_function/3` are removed.
- Task 3 product changes are limited to `src/astranaut_macro_local.erl` and `test/astranaut_macro_local_SUITE.erl`; there are no deletions and nothing staged.

The decisive failure is structural: the required single derivation is distributed across `handle_form/3`, `finish_attribute_pass/3`, and `ensure_formatter_info/1`. The fact that `compile_boundary/3` consumes threaded info does not satisfy “derive once and thread throughout” while earlier stages independently derive it.

The correction must make the producer/consumer ownership explicit. There must be one derivation event for a local-macro operation, with all later stages receiving the same value. If multiple callbacks can encounter formatter source forms, the existing accumulator/state must be extended so the first producer records the value and later callbacks carry it forward; later callbacks must not reconstruct it.

## Audit pass 2 — runtime evidence and regression safety

The runner reports:

- `rebar3 compile`: exit 0.
- Focused Common Test: 42/42 passed.
- Full Common Test: 439/439 passed.
- `git diff --check`: exit 0.

The packet also confirms protocol, closure, selection, export, member, and path-scope behavior. These results establish that the current implementation is functionally stable under the supplied suite, but they cannot discharge the explicit structural derive-once requirement. Equivalent recomputation is observationally silent in the existing tests.

After correction, verification must additionally identify every call to `formatter_info_for_source/2` and every derivation wrapper in the Task 3 diff, prove one producer per operation, and prove the unchanged value path into `compile_boundary/3`. A passing runtime suite without that source evidence is insufficient for Review 2 acceptance.

## Scope

In scope:

- `src/astranaut_macro_local.erl`: consolidate formatter-info derivation and thread the value through the form/attribute/compile path.
- `test/astranaut_macro_local_SUITE.erl`: add a focused structural or behavioral guard only if it can prove the invariant without exposing private implementation unnecessarily.
- Repeat coding self-tests and independent verification after the correction.

Out of scope:

- Protocol precedence or `astranaut_macro` fallback changes.
- Closure traversal changes, selection/export equations, or membership semantics.
- Changes to product paths outside the two Task 3 paths.
- `status.md`, workflow artifacts, staging, commits, and unrelated worktree changes.

## Review 2 acceptance

- One operation-owned producer derives formatter info.
- `handle_form/3`, `finish_attribute_pass/3`, and `ensure_formatter_info/1` no longer independently derive it.
- The same value is threaded into `compile_boundary/3`.
- Existing tests and independent verification pass.
- Source inspection explicitly inventories derivation call sites and reports no duplicate producer path.
- No material findings remain.
