# Project Workflow Status

## Initiative

- Artifact directory: `docs/plan/macro-error/`
- Repository: `/home/slepher/project/astranaut`
- Updated at: 2026-08-07
- Plan revision: revised by Sol after the user's explicit generic-formatter decision

## Repository snapshot

- HEAD: `b1499cf b1499cfbd9c86614cedafc5899ab5c815ce1e542 2026-08-06 Align macro error specs with formatter adapter`
- Worktree summary: new `plan.md` and `status.md`; previous initiative task/review files remain deleted; four `openspec/changes/macro-error/` files are modified
- Expected task paths: `docs/plan/macro-error/plan.md`, `status.md`, `task-1.md`; Task 1 product paths are frozen in the contract
- Unexpected paths: none beyond the explicitly requested initiative-file removal and the pre-existing four OpenSpec modifications

## Progress

- Current task: task-1
- Current phase: handoff
- Latest completed boundary: Task 1 committed as `bd38f1dae8a95bf8497f2abf241a7b01513cb5b4` (`Bind macro exceptions to astranaut_macro`); final status leaves only pre-existing plan/OpenSpec/skill changes and the authorized old-artifact deletion
- Exact next action: confirm whether the four pre-existing macro-error OpenSpec edits are authorized Task 2 scope, then execute Sol continuity `reuse` to write `task-2.md`
- Blocker: Task 2 requires explicit attribution of the existing OpenSpec edits before it may own or stage those paths

## Active task evidence

- Task artifact: `docs/plan/macro-error/task-1.md`
- Changed paths: `docs/plan/macro-error/plan.md`; `docs/plan/macro-error/status.md`; `.codex/skills/local-workflow/SKILL.md`; deleted prior initiative artifacts; modified four macro-error OpenSpec files
- Coding self-tests: PASS — compile exit 0; macro_error CT exit 0 (18 passed); macro_local CT exit 0 (41 passed); `git diff --check` exit 0; rerun had no timeout/interruption
- Independent verification: PASS — fresh runner exit 0 for all 7 commands; macro_error 18 passed; macro_local 41 passed; artifact `_build/test/cover/index.html`; exact scope/status packet returned
- Latest review: `docs/plan/macro-error/task-1-code-review-2.md`; prior review and retrospective retained
- Review verdict: round 2 `passed`

## Commit

- State: committed
- Expected subject: `Bind macro exceptions to astranaut_macro`
- Commit hash: `bd38f1dae8a95bf8497f2abf241a7b01513cb5b4`

## Continuity

- Next task: task-2
- Next Sol: reuse
- Reason: Sol selected reuse because Task 2 directly continues the formatter-ownership boundary
- Evidence focus: current formatter registry behavior, missing-formatter warning/count semantics, and explicit disposition of the four OpenSpec modifications
- Last known child: `019fd7fa-b008-7cd1-9d90-cd404221a4ae` (Task 1 review round 2; passed; continuity task-2 reuse)

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
