# Task 3 Code Review 1

Verdict: changes_required

## Actionable finding

The implementation derives `local_formatter_info` twice instead of deriving it once and threading the result through the local-macro pipeline.

- `src/astranaut_macro_local.erl:175` derives `local_formatter_info/1` in `local_macro_definitions/5`.
- `src/astranaut_macro_local.erl:906-908` derives `local_formatter_info/1` again in `compile_boundary/3`.

Smallest valid correction: compute the formatter-info value once at the earliest shared boundary, pass that exact value through the existing local-macro context/call chain into `compile_boundary/3`, and remove the second derivation. Preserve the existing protocol value and all set-selection/export behavior. If the current function signatures cannot carry the value without changing an identity-sensitive context shape, stop and return the signature/context evidence for clarification rather than introducing a second cache or recomputing from forms.

## Contract comparison

The Task 3 contract requires:

- `formatter_protocol()` to be `none | legacy | strict`.
- Local `format_error/1` to determine strict identity precedence.
- Only the arity-2 path to fall back to `astranaut_macro`.
- `local_formatter_info` to be derived once and threaded through the pipeline.
- `Members` and `commit_compiled(Members, ...)` to remain formatter-free.
- Selection to be `Members ∪ MacroRelated ∪ FormatterRelated`.
- Exports to be `Members ∪ FormatterExports`.
- Reuse of the existing closure machinery.
- No contamination of closure IDs, frozen forms, retain roots, callable state, generation keys, or fingerprints.

The implementation satisfies every listed requirement except the derive-once/thread requirement. This is a contract violation even though the duplicate computation currently preserves the observed behavior.

## Audit pass 1 — source and contract

Source inspection evidence from the runner packet:

- Protocol precedence and fallback are implemented at `src/astranaut_macro_local.erl:789-819`.
- Existing closure machinery is reused at `src/astranaut_macro_local.erl:822-830`.
- Selection and exports follow the required unions at `src/astranaut_macro_local.erl:844-858`.
- `Members` remains the input to `commit_compiled/3` at `src/astranaut_macro_local.erl:917-925`.
- The only material mismatch is duplicate `local_formatter_info/1` derivation at lines 175 and 906-908.

Required correction is localized to data flow. Do not alter protocol semantics, closure traversal, member identity, or export equations while removing the recomputation.

## Audit pass 2 — tests and regression safety

The supplied packet reports:

- `rebar3 compile`: exit 0.
- Focused Common Test: 42/42 passed.
- Full Common Test: 439/439 passed.
- `git diff --check`: exit 0.
- Tests cover formatter closure/frozen IDs, retain roots, fingerprints, private helpers, and exports.

These results support functional correctness and the principal non-contamination invariants. They do not waive the explicit derive-once contract requirement. The packet also states that new tests do not directly assert generation-key or callable-state non-contamination; those dimensions are supported by source inspection and the full regression suite, but remain less direct than the other assertions.

## Scope

In scope:

- `src/astranaut_macro_local.erl`: eliminate the duplicate derivation and thread the already-derived value.
- Existing Task 3 tests only if a focused assertion is needed to prove the single derivation/data-flow contract.
- Re-running the assigned coding self-tests and independent verification after correction.

Out of scope:

- Changes to protocol behavior, fallback semantics, closure algorithms, `Members`, exports, caches, generation/fingerprint semantics, or public APIs beyond the minimal threading correction.
- Any file outside `src/astranaut_macro_local.erl` and `test/astranaut_macro_local_SUITE.erl`.
- Status updates, staging, commits, or workflow-skill edits.

## Acceptance for Review 2

- There is one derivation of `local_formatter_info/1` for the relevant local-macro operation.
- The same value reaches `compile_boundary/3`; no second derivation, hidden recomputation, or parallel formatter-info cache exists.
- Existing protocol, selection, export, closure, and identity-sensitive invariants remain intact.
- The coding worker and independent runner repeat the required compile and test evidence successfully.
- No material review findings remain.
