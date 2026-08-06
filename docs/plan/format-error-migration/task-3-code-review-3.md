# Task 3 Code Review 3

Verdict: passed

## Findings

No material findings remain. The current Task 3 implementation resolves the producer-cardinality defect from Reviews 1 and 2, preserves the formatter protocol and macro-identity boundaries required by `task-3.md`, reuses the existing closure machinery, and is supported by the completed independent runner packet.

## Responsibility map

| Responsibility | Owner and evidence |
| --- | --- |
| Produce formatter information once | `register/5` calls `declaration_formatter_state/2` at `src/astranaut_macro_local.erl:341`. Its guard at `:801-802` returns a state that already owns `formatter_info`; only the uninitialized clause at `:803-806` calls `formatter_info_for_source/2`. Repository search finds no second producer call. |
| Determine protocol and local roots | `formatter_info_for_source/2`, `local_formatter_info/1`, and `formatter_protocol/1` at `src/astranaut_macro_local.erl:808-836` derive `none | legacy | strict`, give `/1` strict precedence, and include only locally present strict formatter entry points. |
| Compute formatter closure | `formatter_info_with_closure/2` at `src/astranaut_macro_local.erl:838-846` builds the existing form map and calls the existing `closure/5` and `closure_ids/2`; it stores formatter-related data separately from macro closure entries. |
| Select formatter identity | `local_macro_definitions/5` reads the stored value at `src/astranaut_macro_local.erl:174-181`; `formatter_options/3` at `:795-799` selects the generated local module only for `strict` and otherwise retains `astranaut_macro`. |
| Carry the value through declaration/finalization | `handle_form/3` reads it from registered state at `src/astranaut_macro_local.erl:88-96`. `finish_attribute_pass/3` reads the same state value at `:207-216`. Neither path derives or reconstructs it. |
| Carry the value through execution | `execute_plan/3` calls `thread_formatter_info/2` at `src/astranaut_macro_local.erl:881-903`, which copies the state-owned value into the workflow context. `compile_boundary/3` only reads that context at `:928-929`. |
| Assemble selected forms and exports | `load_local_macro_forms/7` at `src/astranaut_macro_local.erl:860-875` computes selected functions as `Members ∪ MacroRelated ∪ FormatterRelated` and exports as `Members ∪ FormatterExports`. |
| Preserve macro identity and commit state | `compile_boundary/3` passes the original `Members` to `load_local_macro_forms/7` and `commit_compiled/3` at `src/astranaut_macro_local.erl:936-943`. `generation_boundary_key/1` remains members-only at `:957-959`; formatter data does not enter boundary identity. |
| Verify behavior independently | The supplied runner packet reports compile success, focused suite success (42/42), full Common Test success (439/439), clean diff whitespace, no staged paths, and source confirmation of protocol, closure, export, and identity protections. |

## Call paths

The single production path is:

`handle_form/3 -> register_return/5 -> register/5 -> declaration_formatter_state/2 -> formatter_info_for_source/2 -> local_formatter_info/1 + formatter_info_with_closure/2`.

The guard in `declaration_formatter_state/2` makes later registration calls consumers of the existing operation-owned value rather than new derivations.

The declaration consumer path is:

`handle_form/3 -> maps:get(formatter_info, State1) -> prepare_declaration/3 + local_macro_definitions/5 -> formatter_options/3`.

The final compilation consumer path is:

`finish_attribute_pass/3 -> finalize/3 -> execute_plan/3 -> thread_formatter_info/2 -> execute_plan_1/3 -> compile_boundary/3 -> load_local_macro_forms/7`.

`compile_boundary/3 -> commit_compiled(Members, ...)` remains a separate members-only path.

## Audit pass 1 — patch correctness and contract fidelity

- The prior Review 1 defect is resolved: `compile_boundary/3` no longer derives formatter information and only reads the threaded value at `src/astranaut_macro_local.erl:928-929`.
- The prior Review 2 defect is resolved: `handle_form/3`, `finish_attribute_pass/3`, and execution no longer call derivation wrappers. The only call to `formatter_info_for_source/2` is the guarded producer at `src/astranaut_macro_local.erl:806`.
- `formatter_protocol/1` at `src/astranaut_macro_local.erl:830-836` implements exact `/1` precedence: `{true, _}` is `strict`, `/2` alone is `legacy`, and neither is `none`.
- `formatter_options/3` at `src/astranaut_macro_local.erl:795-799` chooses the local module only for strict identity. Legacy `/2`-only and no-local-formatter cases use `astranaut_macro`, preserving the fallback boundary described by the task.
- Strict formatter roots and exports contain only locally present `format_error/1` and optional `format_error/2` at `src/astranaut_macro_local.erl:811-827`. Fallback functions are not inserted into local roots, closure, forms, or exports.
- The selected-function and export equations are explicit at `src/astranaut_macro_local.erl:866-875`. Formatter helper dependencies are selected through closure but are absent from `ExportFunctions` unless independently members.
- Macro membership remains isolated: registration closure IDs and frozen forms are still produced from member closures in `do_register/6`; compilation commits the original `Members` at `src/astranaut_macro_local.erl:942-943`; generation keys remain members-only at `:957-959`.
- Superseded `maybe_add_formatter/2` and `has_function/3` were removed after their callers were migrated. No unauthorized product deletion or dependency change is present.

## Audit pass 2 — capability reuse, regression safety, and evidence

- The implementation reuses `function_clauses_map/2` (`src/astranaut_macro_local.erl:324-331`), `forms_id_map/1` (`:1216-1221`), `closure/5` (`:1252-1276`), `analyze_closure_function/4` (`:1278-1292`), `closure_ids/2`, `ordsets`, and existing form selection/compilation. No parallel AST walker, call graph, cache, or formatter-specific compiler path was introduced.
- The formatter additions are localized to a small protocol/info model and threading through existing state/context boundaries. The guarded state owner is the smallest correction that satisfies one derivation across staged registration and compilation.
- Focused tests at `test/astranaut_macro_local_SUITE.erl:697-731` cover `none`, strict `/1`, `/1` precedence when `/2` also exists, and `/2`-only fallback.
- `formatter_closure_is_private_and_identity_free/1` at `test/astranaut_macro_local_SUITE.erl:732-817` checks formatter roots, member-only closure/frozen IDs, unchanged macro-environment snapshots and fingerprints, member-only plans and compiled-form state, retain isolation, generation/callable equivalence, formatter exports, private helper non-exports, and successful transitive helper invocation.
- The authoritative runner packet reports: `rebar3 compile` exit 0; `rebar3 ct --suite=test/astranaut_macro_local_SUITE.erl` exit 0 with 42/42; `rebar3 ct` exit 0 with 439/439; `git diff --check` exit 0; `git status --short` exit 0; `git diff --name-only` exit 0; and `git diff --cached --name-only` exit 0 with no staged paths.
- The runner's source inspection independently reports exactly one guarded producer at `src/astranaut_macro_local.erl:801-806`, downstream threading/reads through `handle_form/3`, `finish_attribute_pass/3`, `execute_plan/3`, and `compile_boundary/3`, and coverage of protocol precedence/fallback, closure reuse, union equations, private helpers, and identity protections.

## Scope

Task 3 product changes are confined to:

- `src/astranaut_macro_local.erl`
- `test/astranaut_macro_local_SUITE.erl`

There are no product-file deletions and nothing is staged. The runner reports four modified paths and ten untracked workflow entries overall; `docs/plan/format-error-migration/plan.md`, `status.md`, and workflow artifacts are outside the Task 3 product implementation scope. The dispatcher must preserve that separation and stage only the explicitly accepted paths under the applicable workflow/commit contract.

## Limitations

- This review did not execute CT, tests, builds, lints, compilation, or verification. Runtime conclusions rely exclusively on the supplied completed runner packet.
- The review inspected the current source and test diff plus surrounding implementation, Task 3 contract, prior review/improvement artifacts, committed baseline, initiative plan, and current local workflow skill. It does not approve unrelated `plan.md`, `status.md`, or other workflow-entry content.
- `related_ids` and `related_forms` are retained in `local_formatter_info` as required model data but are not separate compilation-state inputs; their presence does not affect the members-only identity paths reviewed above.

## Local skill assessment

No local-skill gap is exposed by Review 3. The current `project-workflow-local/SKILL.md` already requires completed runner evidence, two audit passes, immutable review artifacts, conditional skill correction, and scope separation. No skill-change specification or skill modification is warranted.
