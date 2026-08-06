# Task 3 Code Review 2 Retrospective

## Finding addressed

Review 2 found that the first rework removed recomputation from `compile_boundary/3` but left three earlier derivation paths: `handle_form/3:81-85`, `finish_attribute_pass/3:207-213`, and `ensure_formatter_info/1:895-900`.

## Why the first rework still allowed multi-path derivation

Review 1 framed the correction around the observed duplicate at `local_macro_definitions/5:175` and `compile_boundary/3:906-908`. The rework correctly made `compile_boundary/3` a consumer, but it treated the pre-boundary value as if it already had a single producer. The source actually has a staged form-processing flow, and formatter information can be derived through helper calls in more than one stage.

The prior review did not require a complete derivation inventory. It named the known call sites but did not require tracing all wrappers and operation paths, including `formatter_info_for_source/2` and `ensure_formatter_info/1`. As a result, the rework satisfied a local line-level correction while failing the whole-pipeline invariant.

## Insufficient design/test gate

The insufficient gate was “`compile_boundary/3` consumes threaded info and does not recompute it.” That gate checked one consumer but not the producer cardinality across the operation.

Runtime tests were also insufficient for this requirement: equivalent recomputation yields the same protocol and set outputs, so the 42 focused and 439 full passing cases do not reveal it. The next gate must require both:

1. A source-structure inventory of every direct and wrapped call that derives formatter information, including `formatter_info_for_source/2`, `local_formatter_info/1`, and `ensure_formatter_info/1`.
2. A producer/consumer data-flow statement naming the one operation-owned producer and each consumer path through `handle_form/3`, `finish_attribute_pass/3`, and `compile_boundary/3`.
3. Independent verification that later stages carry the existing value and do not reconstruct it.
4. Runtime regression coverage for protocol, closure, export, member, retain, fingerprint, callable, and generation behavior after the data-flow change.

If a structural test is impractical, the source inventory is a mandatory independent-review check rather than an optional inference from runtime behavior.

## Local skill assessment

The initial classification was that no local-skill change was needed because
the immediate defect belonged to the task contract and review gate. The later
workflow-level decision is preserved separately in
`task-3-code-review-2-skill-change-spec.md`, rather than mixing a normative
skill amendment into this retrospective.

## Recommended contract amendment

“For each local-macro operation, formatter info has exactly one producer derivation. The producer is identified by function and line in the implementation contract. Every staged form-processing path carries that value unchanged; `handle_form/3`, `finish_attribute_pass/3`, `ensure_formatter_info/1`, and `compile_boundary/3` are consumers unless explicitly designated as the sole producer. Independent verification must inventory direct and wrapped derivation calls and reject any second producer path.”
