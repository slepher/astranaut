# Task 3 Code Review 1 Retrospective

## Finding addressed

Review 1 found that `local_formatter_info/1` is derived at both `src/astranaut_macro_local.erl:175` and `src/astranaut_macro_local.erl:906-908`, although the Task 3 contract required one derivation threaded through the pipeline.

## Why the initial task/design did not prevent it

The initial contract stated “derive a `local_formatter_info` value once” and repeated the requirement in the ordered steps and completion criteria. It did not, however, define the owner of that value at a specific boundary, enumerate the exact producer-to-consumer path, or require a structural review gate that counted derivation sites. That left the implementation free to make a locally convenient second derivation at `compile_boundary/3` while preserving all observable protocol and set behavior.

The contract also emphasized behavioral invariants—selection, exports, closure isolation, and identity-sensitive state—more concretely than the data-flow invariant. The runner therefore had strong evidence that behavior passed while the architectural “once/thread” requirement remained unverified.

## Contract and test gate for future tasks

The task contract should make the invariant operational:

1. Name one producer boundary and one consumer path: the producer computes `local_formatter_info` before local form loading; the value is passed through the existing context into `compile_boundary/3`.
2. State that `compile_boundary/3` is a consumer only and must not call `local_formatter_info/1`.
3. Add a source-review gate requiring exactly one call site/derivation in the Task 3 diff, with the runner to report the producer line and consumer parameter path.
4. Add a focused test or instrumentation seam only if it can prove the data-flow contract without changing product behavior; otherwise make the structural source check mandatory and separate from runtime tests.
5. Treat a passing behavioral suite as insufficient when a required architectural/data-flow invariant is not directly evidenced.

## Local skill assessment

No `project-workflow-local/SKILL.md` change is needed. The failure is a task-contract specificity and review-gate gap, not a general workflow-routing or artifact-lifecycle gap. The existing workflow can support this correction once the task contract names the producer/consumer path and requires a structural derivation-count check.

## Recommended wording for the next contract revision

“`local_formatter_info/1` has exactly one derivation in the operation. It is produced at `<producer boundary>`, passed unchanged through `<context/call path>`, and consumed by `compile_boundary/3`; `compile_boundary/3` must not derive or reconstruct formatter info. Independent verification must identify the single producer and the consumer parameter path from source inspection.”
