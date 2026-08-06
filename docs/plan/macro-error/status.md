# Project Workflow Status

## Initiative

- Artifact directory: `docs/plan/macro-error/`
- Repository: `/home/slepher/project/astranaut`
- Updated at: 2026-08-07
- Plan revision: revised by Sol after the user's explicit generic-formatter decision

## Repository snapshot

- HEAD: `597b150 597b1506aca81a6c3ff47bc5dc271e1faade0573 2026-08-07 Align macro-error plan and specifications`
- Worktree summary: Task 2 source/test and OpenSpec edits are uncommitted; the previous initiative task/review files remain deleted
- Expected task paths: `docs/plan/macro-error/plan.md`, `status.md`, `task-1.md`, `task-2.md`; Task 2 contract revised to exclude documentation
- Unexpected paths: OpenSpec edits are preserved but out of scope for Task 2; no unrelated implementation path is authorized

## Progress

- Current task: task-2
- Current phase: task_planning
- Latest completed boundary: the old Task 2 worker was stopped; Sol revised `task-2.md` so the coder owns only four source/test paths
- Exact next action: commit the revised skill, task contract, and status checkpoint, then dispatch a fresh `luna_coding_worker` against the four-path contract
- Blocker: none

## Active task evidence

- Task artifact: `docs/plan/macro-error/task-2.md`
- Changed paths: `.codex/skills/local-workflow/SKILL.md`; `docs/plan/macro-error/status.md`; `docs/plan/macro-error/task-2.md`; four source/test paths contain preserved Task 2 edits; six OpenSpec paths contain preserved out-of-scope edits
- Coding self-tests: earlier worker evidence passed compile, the three suites, xref, and diff check; the run after the contract correction is pending
- Independent verification: not run for Task 2
- Latest review: `docs/plan/macro-error/task-1-code-review-2.md`; prior review and retrospective retained
- Review verdict: Task 1 round 2 `passed`; Task 2 pending

## Commit

- State: checkpoint changes pending commit; product implementation remains uncommitted until the revised Task 2 gates pass
- Last completed task commit: `bd38f1dae8a95bf8497f2abf241a7b01513cb5b4`

## Continuity

- Next task: task-2
- Next Sol: reuse
- Reason: Sol selected reuse because Task 2 directly continues the formatter-ownership boundary
- Evidence focus: public default API surface, struct fallback behavior, registry warning/count preservation, and strict four-path coder scope
- Last known child: `019fd82b-37a7-7d21-9986-32d534be06be` (old mixed-scope Task 2 worker; stopped after scope correction)

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
