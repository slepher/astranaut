# Project Workflow Status

## Initiative

- Artifact directory: `docs/plan/macro-error/`
- Repository: `/home/slepher/project/astranaut`
- Updated at: 2026-08-07
- Plan revision: revised by Sol after the user's explicit generic-formatter decision

## Repository snapshot

- HEAD: `55a98c3 55a98c3 2026-08-07 Preserve struct formatter with public default fallback`
- Worktree summary: Task 2 implementation and review are committed; OpenSpec edits remain uncommitted and outside Task 2; the previous initiative task/review files remain deleted
- Expected task paths: `docs/plan/macro-error/plan.md`, `status.md`, `task-1.md`, `task-2.md`; Task 2 contract excludes documentation
- Unexpected paths: seven OpenSpec files remain preserved and uncommitted; no unrelated implementation path is authorized

## Progress

- Current task: none
- Current phase: handoff
- Latest completed boundary: Task 2 Sol review passed and dispatcher committed `55a98c3`
- Exact next action: await explicit authorization for a separate README/documentation boundary; do not dispatch it as a coder task
- Blocker: none

## Active task evidence

- Task artifact: `docs/plan/macro-error/task-2.md`
- Changed paths: `.codex/skills/local-workflow/SKILL.md`; `docs/plan/macro-error/status.md`; `docs/plan/macro-error/task-2.md`; four source/test paths contain preserved Task 2 edits; six OpenSpec paths contain preserved out-of-scope edits
- Coding self-tests: compile 0; `astranaut_SUITE` 40 passed; `astranaut_struct_SUITE` 20 passed; `astranaut_macro_error_SUITE` 18 passed; xref 0; diff check 0
- Independent verification: compile 0; `astranaut_SUITE` 40 passed; `astranaut_struct_SUITE` 20 passed; `astranaut_macro_error_SUITE` 18 passed; xref 0; diff check 0
- Latest review: `docs/plan/macro-error/task-2-code-review-1.md`; prior Task 1 reviews and retrospective retained
- Review verdict: Task 2 round 1 `passed`

## Commit

- State: Task 2 committed
- Last completed task commits: Task 1 `bd38f1dae8a95bf8497f2abf241a7b01513cb5b4`; scope checkpoint `29f67ac`; Task 2 `55a98c3`

## Continuity

- Next task: none until a separate documentation boundary is explicitly authorized
- Next Sol: none
- Reason: Task 2 is complete; the remaining README work is documentation and must not be dispatched as a coder task under the updated skill
- Evidence focus: n/a
- Last known child: `019fd83a-a3ac-77c3-bcc9-27e21829692d` (Task 2 review round 1; passed)

## Completed tasks

- Task 1 — committed as `bd38f1d`

## Notes

- Goal: `macro-error`
- OpenSpec input directory: `openspec/changes/macro-error/`
- The previous initiative files were explicitly discarded; no old task or review artifact is authoritative for this fresh plan.
- No tests, builds, lints, staging, or commits were run during planning.
- The local skill now requires unresolved conflicts to be reported as blockers; it forbids creating a document-only task to choose or conceal the unresolved behavior.
- The local skill now permits direct Sol implementation only for explicitly frozen simple tasks; Sol still cannot execute tests, builds, lints, or acceptance commands, and independent `luna_runner` verification remains mandatory.
- The local skill now clarifies the staged path: Sol owns only simple frozen `src/` edits, Luna owns test-file edits and self-tests, and test-code style/abstraction quality is not a review gate.
- The local skill now explicitly keeps OpenSpec/README/workflow documents out of coder tasks; specification changes require a separate authorized boundary before dependent coding dispatch.
