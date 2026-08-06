# Task 3 — Isolate Local Formatter Protocol and Dependency Closure

## Objective

Extend local-macro loading so a local formatter is modeled as an explicit protocol and compiled with exactly its own dependency closure, without making formatter functions macro members or changing the identities and caches that are derived from macro membership.

This task is complete only when local formatters support the required `none | legacy | strict` protocol model, formatter selection and export sets obey the invariants below, focused regression coverage is present, the coding worker's self-tests pass, and a separate `luna_runner` independently verifies the completed implementation.

## Decisive Evidence

Planning baseline: `9fe3c3d3fd4ce35eab59ef8576df9852a3006fb6`. The supplied runner packet reports no Task 3 source changes and no runtime validation.

- `src/astranaut_macro_local.erl:162-177`, `local_macro_definitions/5`: entry point that assembles local macro definitions.
- `src/astranaut_macro_local.erl:313-320`, `function_clauses_map/2`: existing function/form indexing available to protocol and closure analysis.
- `src/astranaut_macro_local.erl:324-383`: registration and closure snapshot construction; these structures currently represent macro membership and must not acquire formatter identities.
- `src/astranaut_macro_local.erl:785-800`, `load_local_macro_forms/6`: local form loading boundary where the selected compilation set is passed onward.
- `src/astranaut_macro_local.erl:824-878`, `compile_boundary/3`: compilation boundary whose membership input must remain formatter-free.
- `src/astranaut_macro_local.erl:1050-1056`, `maybe_add_formatter/2`, and `:1058-1064`, `has_function/3`: current formatter discovery/addition behavior to replace or narrow around the explicit protocol.
- `src/astranaut_macro_local.erl:1086-1104`: current function selection logic.
- `src/astranaut_macro_local.erl:1114-1119`: current export construction.
- `src/astranaut_macro_local.erl:1168-1229`: existing closure analysis, including reusable `closure/5` and `analyze_closure_function/4` behavior.
- `test/astranaut_macro_local_SUITE.erl`: 37 existing cases covering closure, boundary, cache, callable, retain, fingerprint, and generation behavior; Task 3 must add focused formatter-protocol and formatter-related-closure coverage without weakening these protections.
- Repository instructions in `AGENTS.md` require `lessons.md` to be consulted before changing monad bridging, traverse state, or scan-and-splice behavior. This task must stop if implementation reaches those areas without reconciling the recorded lessons.

## Required Model

Introduce one explicit internal model:

```erlang
formatter_protocol() :: none | legacy | strict.
```

Protocol determination is based on local formatter function availability:

- `strict`: local `format_error/1` exists. Arity 1 determines formatter identity even if arity 2 also exists.
- `legacy`: local `format_error/1` does not exist and local `format_error/2` exists.
- `none`: neither local formatter function exists.

Invocation compatibility is distinct from identity: only the arity-2 path may fall back to `astranaut_macro`; arity 1 is never synthesized from or identified by that fallback. Preserve the repository's existing external error-formatting behavior outside this local compilation change.

Derive a `local_formatter_info` value once from the indexed local forms and thread it through selection/export decisions instead of repeatedly probing or implicitly adding formatter functions. Its representation may be a small map, record, or tagged tuple consistent with nearby code, but it must carry enough information to distinguish:

- protocol (`none | legacy | strict`);
- formatter roots that actually exist locally;
- formatter-related closure IDs/forms;
- formatter exports required by the selected protocol.

Do not put fallback functions from `astranaut_macro` into local roots, closure IDs, forms, or exports.

## Invariants

Use these names as conceptual sets; implementation names may follow local conventions.

- `Members` contains only registered local macro members.
- `commit_compiled(Members, ...)` and every registration/closure snapshot derived from `Members` remain formatter-free.
- `MacroRelated` is the existing dependency closure required by `Members`.
- `FormatterRoots` contains only the locally defined formatter entry points selected by `formatter_protocol`.
- `FormatterRelated` is the dependency closure reachable from `FormatterRoots`, computed with the same form map and closure machinery used for macro functions.
- Selected compilation functions/forms are exactly the stable union `Members ∪ MacroRelated ∪ FormatterRelated` (deduplicated with the existing ordering semantics).
- `FormatterExports` contains only local formatter entry points required by the selected protocol.
- Compilation exports are exactly the stable union `Members ∪ FormatterExports`; formatter helper dependencies are compiled but not exported unless independently present in `Members`.
- `none` contributes no formatter roots, related closure, or exports.
- Formatter-only IDs never enter macro closure IDs, frozen forms, retain roots, callable state, generation keys, or fingerprints.
- Adding or changing only formatter/helper code must not redefine macro membership or callable registration. Any cache invalidation that occurs must be justified by the already-defined compiled-form/cache boundary, not by contaminating membership-derived identity.
- Reuse `forms_id_map/1`, `closure/5`, and `analyze_closure_function/4`; do not introduce a parallel call-graph walker.
- Preserve behavior for macros with no formatter and for all existing closure, boundary, cache, callable, retain, fingerprint, and generation scenarios.

## Ownership

### Owned paths

- `src/astranaut_macro_local.erl`
- `test/astranaut_macro_local_SUITE.erl`

### Non-owned paths

- All other product source and tests.
- `docs/plan/format-error-migration/plan.md` and `status.md`.
- Other task, review, and workflow artifacts except where the dispatcher separately assigns them.
- Build configuration, dependencies, generated output, staging, commits, and external state.

If implementation requires a non-owned path, changes public formatter semantics beyond the model above, or requires a new dependency, stop and return the exact need and evidence to the dispatcher.

## Ordered Implementation Steps

1. Read this task contract, the applicable `AGENTS.md` instructions, and
   `lessons.md`; inspect the current diff and the full surrounding functions
   identified in Decisive Evidence before editing. Do not use `plan.md`,
   `status.md`, or `project-workflow-local/SKILL.md` as coding inputs: Sol must
   already have promoted every implementation-relevant decision into this task
   contract, and the dispatcher owns workflow state and routing.
2. Add focused Common Test cases first in `test/astranaut_macro_local_SUITE.erl` for `none`, legacy arity 2, strict arity 1, both arities with arity 1 controlling identity, formatter helper closure, and a formatter helper that is not exported.
3. Add regression assertions proving formatter functions/helpers are absent from `Members`-derived registration and closure snapshots and do not alter retain roots, callable state, generation keys, or fingerprints. Prefer extending existing fixtures/assertion helpers over creating a second test harness.
4. In `src/astranaut_macro_local.erl`, derive `formatter_protocol` and `local_formatter_info` once from the local function/form index. Keep the decision pure and deterministic; encode arity-1 precedence explicitly.
5. Compute `FormatterRelated` from only the locally present roots selected by the protocol, reusing `forms_id_map/1`, `closure/5`, and `analyze_closure_function/4`. Preserve existing recursion, local-call, and ordering behavior from macro closure analysis.
6. Replace the implicit formatter addition in selection with `Members ∪ MacroRelated ∪ FormatterRelated`. Ensure formatter roots are included through `FormatterRelated`/its roots as defined by the existing closure API, without adding them to `Members`.
7. Build exports as `Members ∪ FormatterExports`. Keep formatter helper functions private unless they are independently macro members.
8. Thread only the minimal formatter information needed across `local_macro_definitions/5`, `load_local_macro_forms/6`, and `compile_boundary/3`. Do not widen stored registration/cache context shapes unless unavoidable; if unavoidable, stop for review because those shapes participate in identity-sensitive behavior.
9. Remove superseded formatter-discovery helpers such as `maybe_add_formatter/2` only after all callers are migrated. Keep `has_function/3` only if it still has a clear non-duplicative use.
10. Review the final diff against every invariant, then perform the Coding Self-Tests below. Do not update `status.md`, stage, or commit; return evidence to the dispatcher for independent verification and review.

## Expected Paths and Deletions

Expected modified paths:

- `src/astranaut_macro_local.erl`
- `test/astranaut_macro_local_SUITE.erl`

Expected deleted files: none.

Expected symbol-level deletion: `maybe_add_formatter/2` should be removed if fully replaced by `local_formatter_info`-driven selection. `has_function/3` may be removed only if no callers remain. No other deletion is authorized by this task.

## Coding Self-Tests

The `luna_coding_worker` owns implementation and self-test execution. It must report exact commands, exit status, and concise failure/success output. At minimum:

- Run the focused `astranaut_macro_local_SUITE` cases added for formatter protocol, formatter closure, and export isolation.
- Run the complete `astranaut_macro_local_SUITE` to protect all 37 pre-existing closure/boundary/cache/callable/retain/fingerprint/generation cases plus new cases.
- Run the repository's normal compile check needed by this Erlang change.
- Inspect the diff for accidental changes outside the owned paths and report any generated or crash artifacts.

The worker must not claim success if any command is interrupted, skipped, or inconclusive. Environment-specific execution must follow `AGENTS.md`; approval or sandbox failure is returned to the dispatcher rather than worked around.

## Independent Verification

After implementation self-tests complete, the dispatcher must assign a fresh, separate `luna_runner`. Sol does not execute these commands. The runner receives the Task 3 contract, implementation diff, and coding self-test packet and must independently:

1. Confirm the diff is limited to the two expected modified paths and no files are deleted.
2. Inspect the real source to verify the protocol precedence and fallback rule: `/1` determines strict identity; only `/2` uses the `astranaut_macro` fallback.
3. Inspect data flow to prove `Members` and `commit_compiled(Members, ...)` remain formatter-free; selected functions are `Members ∪ MacroRelated ∪ FormatterRelated`; exports are `Members ∪ FormatterExports`.
4. Confirm formatter closure reuses the existing form map and closure analyzer and does not introduce a parallel traversal.
5. Run the focused formatter cases, the complete `astranaut_macro_local_SUITE`, and the normal compile check independently, recording exact commands, exit statuses, and relevant output.
6. Report explicit evidence for non-contamination of closure IDs, frozen forms, retain roots, callable state, generation keys, and fingerprints. Any untested dimension is a verification gap, not an inferred pass.
7. Produce a bounded evidence packet for Sol review; do not edit source, tests, workflow status, staging, or commits.

## Stop Conditions

Stop and return `Clarification required` with exact code evidence if any of the following occurs:

- `/1` and `/2` precedence or fallback behavior in surrounding code conflicts with the required model.
- Formatter functions are already intentionally represented as macro members in a way that cannot be separated without changing public behavior.
- Correct formatter closure requires changing `forms_id_map/1`, `closure/5`, or `analyze_closure_function/4` semantics for all callers rather than reusing them.
- The implementation would modify registration snapshots, frozen forms, retain roots, callable state, generation keys, fingerprints, or persisted cache shapes.
- A required change falls outside the owned paths, adds a dependency, deletes a file, or expands into monad bridging, traverse-state, or scan-and-splice processing.
- Existing tests/documentation specify behavior inconsistent with `formatter_protocol()` or with the required selection/export equations.
- Self-test or independent-verification execution is blocked, interrupted, or produces ambiguous evidence.

## Commit Contract

The dispatcher owns staging and commit creation only after coding self-tests, independent verification, and Sol review pass.

Commit subject:

```text
Separate local formatter dependency closure
```

The commit must contain only the two expected modified paths unless a separately approved contract revision says otherwise.

## Completion Criteria

- `formatter_protocol()` has exactly `none | legacy | strict`, with local `/1` taking identity precedence and only `/2` using the `astranaut_macro` fallback.
- `local_formatter_info` is derived once and drives formatter roots, related closure, and exports.
- `Members` and `commit_compiled(Members, ...)` remain formatter-free.
- Selection and export equations hold exactly, with formatter helpers compiled privately.
- Existing closure machinery is reused.
- Regression coverage demonstrates protocol behavior, formatter helper closure, export isolation, and no contamination of identity-sensitive macro state.
- Coding self-tests pass with recorded evidence.
- Independent verification by a separate `luna_runner` passes with recorded evidence.
- Sol review reports no material findings.
- Only then may the dispatcher update `status.md` and create the task commit.
