# Task 1 Contract — Freeze macro-error OpenSpec on the committed formatter adapter

## Objective

Goal: `macro-error`.

Complete the interrupted specification-only reconciliation by applying the frozen edit map below to exactly four macro-error OpenSpec files. The coding worker performs no design, reconciliation, requirement selection, or prose invention. Product implementation, tests, and README edits belong to later tasks.

## Decisive Evidence

Facts:

- The committed transform-error capability makes `astranaut_lib:format_error/1,2` the compiler adapter and shared fallback, while domain formatters expose only pure `format_error/1` clauses (`openspec/changes/transform-error/specs/transform-error-formatting/spec.md:26-83`).
- That capability removed `dispatch_error/3`, public `format_default_error/2`, domain `format_error/2`, options, and `default => throw`; adapter-scope `error:function_clause` falls back without stack inspection or rethrow (`openspec/changes/transform-error/specs/transform-error-formatting/spec.md:28-50,128-141`; `openspec/changes/transform-error/design.md:46-87`).
- Current production code implements that adapter as `astranaut_lib:format_error/1,2`, catches adapter-scope `error:function_clause`, and keeps default formatting private (`src/astranaut_lib.erl:612-632`).
- `astranaut_struct` actually exports and defines only the forwarding `format_error/1` facade (`src/astranaut_struct.erl:18-76`); there is no `astranaut_struct:format_error/2` facade to remove.
- The maintained future documentation targets are the `# Macro` sections in `README.md` and `README.zh.md`, not a standalone `macro_error.md`.
- The interrupted worker changed exactly the four owned OpenSpec files. Its current text already removes the stale protocol, preserves ownership-at-production, identifies the real `/1` facade, and names both README targets, except that `specs/macro-error-ownership/spec.md:82` still phrases facade removal as a deferred implementation note instead of the required normative final state.

Inference:

- The smallest coherent final patch is to preserve the current partial edits exactly and make one exact normative correction in the ownership spec. Rewriting any other accepted prose would add coding-worker discretion without changing the required capability.

Unresolved questions: none.

## Fixed Decisions and Invariants

- Macro diagnostic ownership is fixed at reason production and stored in the internal diagnostic. It is never inferred from reason shape or selected during final formatting.
- Macro registration, import, parsing, expansion, exception wrapping, recursion-limit, and return-validation reasons remain owned by `astranaut_macro`.
- Error and warning computations deliberately returned by a successfully invoked user macro remain owned by the descriptor's registry formatter.
- The catch branch that produces `macro_exception` explicitly selects `astranaut_macro`; its formatter override must not cover successful user-returned computations.
- Providers without `format_error/1` continue to use `astranaut_macro`; transform-error's existing missing-formatter warning behavior is unchanged and is not redesigned here.
- Domain formatters are pure direct `format_error/1` clauses. They do not call the shared adapter, contain a generic catch-all, proxy another formatter, or expose a domain `/2` API.
- The only `/2` retained in this specification is the current shared adapter `astranaut_lib:format_error/2`. No operative requirement for `dispatch_error/3`, public `format_default_error/2`, domain `format_error/2`, formatter options, throw mode, nested-stack inspection, or `function_clause` rethrow may remain.
- Adapter-scope unknown-reason and `error:function_clause` behavior is inherited unchanged from the committed transform-error capability; macro-error adds no fallback behavior.
- `astranaut_struct` has one actual forwarding facade, `format_error/1`, and the capability requires its later removal. `astranaut_struct_transformer:format_error/1` remains the owner of struct-transform reasons.
- `README.md` and `README.zh.md` are later documentation targets. They are not edited by this task. No `macro_error.md` or `macro-error.md` target exists.
- Reason terms, positions, files, error/warning classification, MFA, arguments, stack payload, sibling order/count, AST results, and failed-call recovery remain unchanged.

## Exact Final State and Edit Map

The current worktree versions of the four owned documents are the editing baseline. “Accept verbatim” means the coding worker must leave that current text byte-for-byte unchanged. The only authorized content edit beyond the existing partial patch is the exact replacement specified below.

### `openspec/changes/macro-error/proposal.md`

Accepted final state: accept the entire current worktree file verbatim.

- `## Why`: accept the dependency paragraph naming the committed transform-error capability, `astranaut_lib:format_error/1,2`, and pure domain `format_error/1` clauses.
- `## What Changes`: accept ownership at diagnostic recording, framework ownership, successful user-computation ownership, the actual `/1` struct facade, and inherited fallback mechanics.
- `## Capabilities`: accept unchanged.
- `## Impact`: accept the current adapter/fallback dependency, facade migration statement, and diagnostic-preservation statement.
- Required corrections: none.
- Must not remain or be reintroduced: `astranaut_macro:format_error/2` proxy guidance; operative `dispatch_error/3`; public `format_default_error/2`; domain `/2`; throw/rethrow semantics; `astranaut_struct:format_error/1,2`.

### `openspec/changes/macro-error/design.md`

Accepted final state: accept the entire current worktree file verbatim.

- `## Context`: accept the registry boundary, current exception-ownership defect, existing invalid-return boundary, and transform-error adapter dependency.
- `## Goals / Non-Goals`: accept all current bullets, including production-point ownership, successful-computation inheritance, pure `/1`, adapter reuse, and explicit non-redesign boundaries.
- `## Decisions`: accept all current subsections and their present text: catch-branch formatter override; registry formatter as user-domain protocol; no formatter fallback chain; removal of the actual `/1` struct facade; continued struct-transformer ownership.
- `## Risks / Trade-offs`: accept all current bullets verbatim.
- `## Migration Plan`: accept the six current ordered steps, including later updates to both README macro sections and later product verification.
- `## Open Questions`: remain `无。`.
- Required corrections: none.
- Must not remain or be reintroduced: the old strict-dispatch design; public/default formatter APIs; domain `/2`; options or throw mode as supported behavior; nested-stack distinction or rethrow; another formatter fallback chain; `/1,2` struct-facade wording; `macro_error.md`.

### `openspec/changes/macro-error/specs/macro-error-ownership/spec.md`

Accepted final state: accept every current worktree requirement and scenario verbatim except the one exact replacement below.

- Preserve unchanged: framework ownership and its external exception, local exception, and invalid-return scenarios.
- Preserve unchanged: successful user error/warning registry ownership and provider-without-formatter scenarios.
- Preserve unchanged: ownership-not-inferred-by-fallback requirement and its domain-owned reason, adapter fallback, adapter `function_clause`, and no-framework-proxy scenarios.
- Preserve unchanged: struct-transformer ownership and pure `/1` adapter boundary.
- Preserve unchanged: all diagnostic content, payload, ordering, classification, and recovery requirements and scenarios.
- Exact required correction in `#### Scenario: astranaut_struct 没有自身领域 reason`:

  Replace exactly:

  ```text
  - **THEN** 后续实现移除 `astranaut_struct` 现有的 `/1` facade
  ```

  with exactly:

  ```text
  - **THEN** 系统 MUST 移除 `astranaut_struct` 现有的 `format_error/1` facade
  ```

- Keep the following existing `AND` line unchanged: `- **AND** registry 为其 macro descriptor 选择 \`astranaut_macro\``.
- Must not remain or be reintroduced: “后续实现移除” as the requirement outcome; `astranaut_struct:format_error/1,2`; direct unknown-reason fallback inside a domain callback; generic catch-all; formatter proxy; old options/throw scenarios; nested helper-stack inspection or rethrow.

### `openspec/changes/macro-error/tasks.md`

Accepted final state: accept the entire current worktree file verbatim.

- Sections 1 and 2: accept the focused ownership assertions, production-point fix, removal of framework proxy clauses, and unchanged framework ownership audit.
- Section 3: accept removal of the actual `/1` struct facade, preservation of struct-transformer ownership, and later updates to both `README.md` and `README.zh.md` macro sections.
- Section 4: accept the later implementation verification checklist. Those commands describe later tasks and are not run in this specification-only task.
- Required corrections: none.
- Must not remain or be reintroduced: `astranaut_macro:format_error/2`; `astranaut_struct:format_error/1,2`; `macro_error.md`; any Task 1 instruction to edit product, tests, or README files.

## No-Decision Rule

The coding worker must apply only the single exact replacement above and preserve all other current text in the four files verbatim. It must not choose among alternate requirements, improve wording, translate terminology, reorder sections, add examples, normalize style, or invent migration prose. If the exact source line is absent, duplicated, or materially different, stop and report the observed line and diff; do not approximate the replacement.

## Ownership, Permissions, and Forbidden Paths

Owned paths, and the complete expected task change set:

- `openspec/changes/macro-error/proposal.md`
- `openspec/changes/macro-error/design.md`
- `openspec/changes/macro-error/specs/macro-error-ownership/spec.md`
- `openspec/changes/macro-error/tasks.md`

No new files and no deletions are authorized.

Forbidden writes include all product source, tests, OpenSpec files outside the four owned paths, `README.md`, `README.zh.md`, `docs/plan/macro-error/**`, `status.md`, skill files, dependencies, generated output, staging, and commits. The coding worker must not delegate or spawn children. Existing changes to `.codex/skills/local-workflow/SKILL.md` and untracked `docs/plan/macro-error/` artifacts are preserved as pre-existing workflow evidence, are not worker-owned, and are not part of this task's expected commit.

## Ordered Execution

1. Confirm the exact old scenario line exists once in the current ownership spec.
2. Apply the exact one-line replacement from the edit map.
3. Make no other content edit.
4. Run every Coding Self-Test exactly as listed below and return each command, exit status, and raw output.
5. Stop without staging or committing.

## Stop Conditions

Stop immediately and report exact evidence if:

- the exact replacement source line is absent, duplicated, or changed;
- any accepted current partial text would need another correction to satisfy a fixed decision;
- a requirement conflict or ambiguity requires choosing or inventing prose;
- the committed transform-error capability or current adapter differs from the evidence above;
- completing the edit requires any path outside the four owned files, any new file, or any deletion;
- any product, test, README, workflow, skill, staging, commit, or unrelated OpenSpec change would be required;
- strict validation cannot start, is interrupted, or fails for a reason that requires scope expansion;
- approval, dependency installation, delegation, or external state is required.

## Coding Self-Tests

Run by the assigned coding worker. This specification-only task runs no test, build, compile, lint, CT, xref, dialyzer, or acceptance command.

1. `openspec validate macro-error --strict`
2. `rg -n 'dispatch_error/3|format_default_error/2|default[[:space:]]*=>[[:space:]]*throw|astranaut_(macro|struct|struct_transformer):format_error/2|macro_error\.md|macro-error\.md|保留.*stacktrace.*重新抛出|检查.*stack.*重新抛出' openspec/changes/macro-error`
3. `rg -n 'astranaut_lib:format_error/1,2|format_error/1|astranaut_macro|astranaut_struct_transformer|README\.md|README\.zh\.md' openspec/changes/macro-error`
4. `git diff --check -- openspec/changes/macro-error/proposal.md openspec/changes/macro-error/design.md openspec/changes/macro-error/specs/macro-error-ownership/spec.md openspec/changes/macro-error/tasks.md`
5. `git diff --name-only -- openspec/changes/macro-error/proposal.md openspec/changes/macro-error/design.md openspec/changes/macro-error/specs/macro-error-ownership/spec.md openspec/changes/macro-error/tasks.md`
6. `git diff --stat -- openspec/changes/macro-error/proposal.md openspec/changes/macro-error/design.md openspec/changes/macro-error/specs/macro-error-ownership/spec.md openspec/changes/macro-error/tasks.md`
7. `git status --short`

Expected results: command 1 exits 0; command 2 exits 1 with no matches; commands 3-7 exit 0. Command 5 lists exactly all four owned files. Command 7 may also show only the pre-existing workflow-gate skill change and untracked initiative directory described above. Any other path or exit is gating.

## Independent Verification

Run later by a separate independent runner against the same worktree. The runner changes no file. No product test, build, compile, lint, CT, xref, dialyzer, or acceptance command is authorized for this task.

1. `git status --short`
2. `git diff --check -- openspec/changes/macro-error/proposal.md openspec/changes/macro-error/design.md openspec/changes/macro-error/specs/macro-error-ownership/spec.md openspec/changes/macro-error/tasks.md`
3. `git diff --name-only -- openspec/changes/macro-error/proposal.md openspec/changes/macro-error/design.md openspec/changes/macro-error/specs/macro-error-ownership/spec.md openspec/changes/macro-error/tasks.md`
4. `git diff --stat -- openspec/changes/macro-error/proposal.md openspec/changes/macro-error/design.md openspec/changes/macro-error/specs/macro-error-ownership/spec.md openspec/changes/macro-error/tasks.md`
5. `openspec validate macro-error --strict`
6. `rg -n 'dispatch_error/3|format_default_error/2|default[[:space:]]*=>[[:space:]]*throw|astranaut_(macro|struct|struct_transformer):format_error/2|macro_error\.md|macro-error\.md|保留.*stacktrace.*重新抛出|检查.*stack.*重新抛出' openspec/changes/macro-error`
7. `rg -n 'astranaut_lib:format_error/1,2|format_error/1|astranaut_macro|astranaut_struct_transformer|README\.md|README\.zh\.md' openspec/changes/macro-error`
8. `git status --short`

Expected results: commands 1-5 and 7-8 exit 0; command 6 exits 1 with no matches. Command 3 lists exactly all four owned files. Initial and final status are identical and contain no unexpected path.

## Expected Scope, Commit, and Completion

- Expected tracked task modifications: exactly the four owned macro-error OpenSpec files.
- Permitted untracked paths: only the pre-existing `docs/plan/macro-error/` workflow artifacts; the coding worker does not modify them.
- Authorized deletions: none.
- Expected product, test, README, dependency, generated, staged, and committed changes by the coding worker: none.
- Proposed dispatcher commit subject: `Align macro error specs with formatter adapter`.

Completion requires the exact edit map, all fixed decisions and invariants, strict validation exit 0, forbidden residual search exit 1 with no matches, positive terminology search exit 0, clean diff check, exact four-file task scope, completed independent runner evidence, and a passed Sol review. Only the dispatcher may stage and commit afterward.

## Continuity Recommendation After Pass

- Next Task: `task-2`.
- Next Sol: `reuse`.
- Reason: Task 2 directly implements the production-point ownership boundary frozen here; retained source and specification context reduces reinterpretation risk.
