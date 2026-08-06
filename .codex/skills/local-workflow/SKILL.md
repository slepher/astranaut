---
name: local-workflow
description: Execute or resume multi-task repository work through a durable initiative directory containing plan.md and status.md, with Sol owning technical decisions, Luna implementing and verifying, and the dispatcher routing decisions and performing authorized Git operations. The user's explicit Goal selects the initiative; unrelated plans must not be read or resumed. Use when starting a project plan, continuing a partially completed task, recovering after interruption or a new session, processing tasks one at a time, or requiring independent review and per-task commits. Use $delegate for all worker-routing mechanics.
---

# Project Workflow

Run one project task at a time. Use `$delegate` for role selection, spawn
contracts, runtime identity checks, permissions, and result verification.

## Goal routing

The user's explicit `Goal` is authoritative for initiative selection. Treat
phrases such as `Goal = transform-error`, `goal: transform-error`, or an
explicit request to work on `transform-error` as the canonical initiative
name, preserving the spelling used by the repository when resolving paths.

Before reading any workflow plan or status:

1. Resolve the requested Goal only to its repository-scoped initiative
   directory, normally `docs/plan/<Goal>/`. Goal selects the workflow
   initiative; it does not select, imply, or require an OpenSpec path.
2. Read only the selected Goal's `plan.md`, `status.md`, and task artifacts.
   Read OpenSpec documents independently when the user explicitly names them,
   a task explicitly references them, or repository instructions require them.
   Do not assume `openspec/changes/<Goal>/` exists, is authoritative, or must
   match the initiative name. A plan with a similar subject, historical
   migration, stale root `status.md`, or another Goal is unrelated unless the
   user explicitly asks to use it.
3. If the matching initiative directory is absent, create a new initiative
   for the requested Goal. Do not infer that an existing unrelated initiative
   is a predecessor, continuation, or source of requirements.
4. If the matching initiative exists, reconcile it against the requested
   Goal and the explicitly selected specification/input documents. If its
   artifacts describe another Goal, treat them as invalid for this request
   and start a fresh matching initiative rather than repairing or reusing it.

The Goal is also the top-level objective in the new `plan.md` and `status.md`.
Every delegated contract must repeat the exact Goal, identify specification
inputs separately from the initiative path, and explicitly state out-of-scope
documents. When a user names a Goal, never let an old workflow artifact
silently override the user's current specification.

## Separate decisions from dispatch

Sol owns every substantive workflow and technical decision: plan contents,
task selection and contracts, review verdicts, coder-facing corrections,
root-cause classification, local-skill changes, acceptance after review, and
Sol continuity. Sol's written decision is authoritative unless the user
overrides it or execution would violate an instruction, permission, safety
boundary, or the actual repository state.

The dispatcher does not reinterpret, adjudicate, summarize, or improve Sol's
technical decisions. It routes complete evidence to Sol, forwards Sol-authored
contracts and decisions verbatim to the selected worker, maintains `status.md`,
performs identity and scope checks, requests required approvals, and executes
authorized staging and commits. When a Sol decision cannot be executed safely
or exactly, the dispatcher returns the blocker to Sol or the user instead of
inventing a replacement decision.

## Persist the initiative

Choose one project-relative artifact directory, normally
`docs/plan/<Goal>/`. The dispatcher owns the directory choice, numbering, and
`status.md`. Sol owns and directly writes every other workflow document inside
that directory. Persist:

- `plan.md`: goal, constraints, dependencies, migration order, and ordered
  `task-M` contracts;
- `status.md`: current repository snapshot, active task and phase, completed
  boundaries, evidence, commit state, blocker, and exact next action;
- `task-M.md`: implementation plan for exactly task M;
- `task-M-code-review-N.md`: immutable Sol decision for review round N, written
  for the next coding worker when the verdict is `changes_required`;
- `task-M-code-review-N-retrospective.md`: immutable Sol-facing and human-audit
  explanation of why the original design, contract, evidence gate, or workflow
  rule failed to prevent a `changes_required` finding;
- `task-M-code-review-N-skill-change-spec.md`: optional normative specification
  written before changing this local skill and used as the sole basis for that
  skill change.

Create `plan.md` and `status.md` together when the initiative starts. The
initiative directory is invalid for execution if either is missing. Use the
bundled [status template](assets/status-template.md) as the minimum status
shape; add project-specific fields only when they improve recovery.

Sol is read-only for product source, tests, staging, commits, and `status.md`.
Sol has explicit write ownership for `plan.md`, `task-M.md`,
`task-M-code-review-N.md`, and
`task-M-code-review-N-retrospective.md` inside the initiative directory. When
needed, Sol also writes `task-M-code-review-N-skill-change-spec.md` and then
directly updates
`.codex/skills/local-workflow/SKILL.md`. Except for `status.md`, Sol is the
authoritative writer for workflow documents and local skill corrections.
The dispatcher checks only authority, declared scope, artifact presence, and
whether execution matches Sol's explicit routing decision; it does not validate
or overrule document content. Sol must verify that each skill diff conforms to
its skill-change specification. Sol must not reread a document it has just
written merely to transcribe, reconfirm, or restate its contents; its completed
write and returned artifact path are sufficient. M is the task number. N starts
at 1 for each task and increases monotonically. Never overwrite or renumber a
task, review, or improvement record.

Existing `task-M-review-N.md`, `task-M-review-improve-N.md`, and
`task-M-review-improved-skill-N.md` files are legacy immutable artifacts. Read
them when resuming their task, but never rename or rewrite them. Use the new
names for every newly created review round.

Every Sol planning/review contract must explicitly grant write scope for the
initiative's `plan.md`, current `task-M.md`, review artifacts, and, when
applicable, `.codex/skills/local-workflow/SKILL.md`. The contract must explicitly
forbid `status.md` edits, product-source edits, test edits, staging, commits,
delegation, and child spawning. A Sol review contract must also forbid running
test, build, or acceptance commands. Those commands belong to the coding
worker's self-test layer and the independent `luna_runner` verification layer.
A dispatcher must never put CT, test, build, lint, or Independent Verification
commands in a Sol contract. Route every Independent Verification command to a
separate `luna_runner`. If a Sol contract contains such commands, Sol must
reject that portion before tool execution and report the contract error.

## Maintain resumable status

Treat `status.md` as the workflow checkpoint and Git as the repository truth.
Update status after every durable boundary:

- overall plan created or revised;
- task selected and `task-M.md` accepted;
- coding started, changed paths updated, and self-tests completed;
- independent verification completed;
- each code-review decision and retrospective written;
- each skill-change specification written and its conforming local-skill
  correction applied;
- rework started or completed;
- task passed, commit attempted, and commit completed;
- continuity decision executed;
- blocker discovered or initiative completed.

Record one current phase from:

```text
planning | task_planning | coding | coding_self_test |
independent_verification | review | rework | ready_to_commit |
committing | handoff | blocked | complete
```

Keep the exact next action singular and executable. Record commands with exit
status rather than vague statements such as “tests passed.” Record live child
IDs only as hints; they are not durable and must be rechecked before reuse.

## Resume an interrupted task

On every new session or continuation request:

1. Locate the initiative directory and read `plan.md` and `status.md` before
   planning or spawning workers.
2. Read the active `task-M.md`, all existing `task-M-code-review-*` artifacts,
   and any legacy `task-M-review-*` artifacts for that task.
3. Inspect current HEAD, `git status --short`, the real diff, and expected task
   paths. Git and immutable artifacts override stale status prose.
4. Reconcile discrepancies in `status.md`; stop for clarification when current
   changes cannot be safely attributed to the active task.
5. Check whether a recorded child still exists before follow-up. If not, start
   the appropriate fresh worker with a self-contained contract.
6. Continue from the first incomplete phase. Do not regenerate accepted plans,
   discard partial edits, or repeat completed gates unless later changes
   invalidated their evidence.

Resume by phase:

- `coding` or `coding_self_test`: give the coding worker `task-M.md`, current
  diff, completed steps, failures, and remaining checks; preserve partial work.
- `independent_verification`: start a separate runner and execute the pending
  verification against the current diff.
- `review`: request the next missing review round using existing evidence.
- `rework`: forward Sol's latest coder-facing review decision and current diff
  to coding, then repeat both test layers.
- `ready_to_commit` or `committing`: revalidate the exact change set and finish
  the dispatcher-owned commit; do not start the next task first.
- `handoff`: execute the recorded `reuse`/`fresh`/`none` decision; for `fresh`,
  collect the required current repository packet first.
- `blocked`: verify whether the recorded blocker changed; otherwise request the
  missing authority or input.
- `complete`: verify final repository status and report completion without
  restarting the plan.

## Create the overall plan

Have `luna_runner` collect only mechanical repository evidence when needed:
HEAD, worktree/status, file names, existing command output, logs, and test
counts. It must not judge source design, assertion meaning, architecture,
scope legitimacy, or unresolved technical questions. Give the raw packet and
file paths to Sol; Sol performs the code and design reading.

Have Sol write and accept `plan.md` with independently verifiable tasks. Each plan task
states its objective, owned area, behavior boundary, prerequisites,
verification, and completion criteria. The dispatcher initializes and maintains
`status.md` and records Sol's first eligible next action there without
substantive reinterpretation.

## Plan one task

Sol selects one eligible task M from `plan.md` and writes only `task-M.md` for
the selected task; do not transfer or rewrite that task contract in the
dispatcher.
Do not recursively create a second task hierarchy. Ordered implementation
steps and checklists are allowed; nested task identifiers are not. If task M is
too broad, Sol revises `plan.md` instead.

Require `task-M.md` to include:

- decisive evidence, approach, owned files or modules, and invariants;
- ordered implementation steps and stop conditions;
- `Coding Self-Tests` run by the coding worker;
- `Independent Verification` run later by a separate runner;
- expected tracked paths, permitted untracked paths, authorized deletions;
- proposed commit subject and completion criteria.

## Implement and self-test

Send `task-M.md` to `luna_coding_worker`. It owns both implementation and every
Coding Self-Test. It must run those commands directly after implementation and
after each rework. It may not omit or delegate them because independent testing
will happen later. A failed self-test stays with coding responsibility until
fixed or genuinely blocked.

Treat `task-M.md` as the coding worker's complete implementation contract. Give
the worker applicable repository instructions, required domain references, the
current diff, and decisive source paths, but do not require it to read
`plan.md`, `status.md`, this workflow skill, retrospectives, or skill-change
specifications. Sol must promote every coder-relevant decision into `task-M.md`
or the current `task-M-code-review-N.md` before the dispatcher routes it.

## Verify independently

Only after coding self-tests pass, spawn a separate `luna_runner` with
`fork_turns = "none"`. Give it `task-M.md`, the current diff, and the Independent
Verification commands. It must execute the checks itself against the current
worktree, rerun task acceptance tests at minimum, and must not rely on the
coding worker's summary. Its job is mechanical only: execute commands and
return raw command results, exit statuses, test counts, timeout/interruption
state, and status/diff name outputs. It must not inspect or audit source code,
test assertion semantics, architecture, or conceptual scope. It reports
mechanical failures without editing code.

All command and test execution for Independent Verification belongs exclusively
to a separate `luna_runner`. Sol must never run CT, tests, builds, lints, or
verification commands during planning or review, even when a review prompt
mistakenly requests them. Sol owns all source, test-assertion, design,
architecture, and semantic scope audits, but consumes the runner's raw
mechanical packet. An accidental Sol-run command is non-gating evidence and
never substitutes for the required runner result.

On failure, route exact evidence through the dispatcher to Sol. Sol decides the
coder-facing correction and returns the exact rework instruction for the
dispatcher to forward. Rerun Coding Self-Tests, then start independent
verification again. Keep both evidence sets; neither substitutes for the other.

## Review until passed

For review round N, give Sol `plan.md`, `task-M.md`, the real diff, prior
`task-M-code-review-*`, legacy `task-M-review-*`, coding self-test evidence,
independent verification evidence, and decisive source paths. Sol writes
`task-M-code-review-N.md` with exactly one verdict:

- `changes_required`: actionable findings ordered by severity, exact evidence,
  and smallest valid corrections;
- `passed`: no material finding remains, both test layers satisfy the contract,
  and the diff matches the declared commit scope.

Sol only consumes the coding worker's report and completed independent
`luna_runner` evidence, inspects the implementation, performs the patch
correctness and capability-reuse audit passes, and writes
`task-M-code-review-N.md` plus the required
`task-M-code-review-N-retrospective.md` when changes are required. Sol does not
execute verification commands.

Do not start or finalize a Sol review until the dispatcher has supplied the
completed independent `luna_runner` evidence. If that evidence is missing,
interrupted, or still running, Sol reports that it is waiting and writes no
immutable review artifact or verdict. Sol may inspect source, diffs, history,
existing logs, and non-mutating Git scope evidence while waiting, but it may
not execute the missing acceptance commands itself. If a review contract
conflicts with this boundary, Sol follows this rule and reports the contract
error to the dispatcher.

The dispatcher must forward the runner-authored evidence packet to Sol without
replacing it with a dispatcher-written summary. The packet must identify the
runner, each command, completion or interruption state, exact exit status and
test counts, raw status/diff outputs, and generated artifacts. It must contain
no runner-authored code-audit conclusion. Dispatcher claims that checks passed,
even when numerically detailed, do not satisfy this gate.
If the runner-authored packet is absent, Sol remains waiting and writes no
review artifact or verdict.

For `changes_required`, do not send rework immediately. Sol must first complete
the retrospective and make the routing decision:

1. Write `task-M-code-review-N-retrospective.md` beside the triggering review.
2. Explain which initial design, task contract, invariant, evidence gate, or
   skill rule failed to prevent each finding and cite exact evidence.
3. Decide the smallest recurrence-prevention correction and whether it belongs
   in implementation, `plan.md`, `task-M.md`, verification, or this local skill.
4. Put coder-visible findings and corrections only in
   `task-M-code-review-N.md`. The retrospective is for future Sol reasoning and
   human audit; do not send it to Luna workers or runners.
5. Decide whether a reusable local-skill gap exists. If no, write no skill
   artifact and do not modify the skill.
6. If yes, first write `task-M-code-review-N-skill-change-spec.md` with the exact
   rule, target section, scope, invariants, and acceptance criteria. Then update
   `.codex/skills/local-workflow/SKILL.md` solely from that specification and verify
   specification-to-diff conformance.
7. Return a Sol-authored routing decision containing the coder review path,
   any revised task/plan paths, the retrospective path, any skill-change
   specification and conforming skill diff, and the next required worker.

After Sol completes those decisions and writes, the dispatcher performs only
identity, permission, and declared-scope checks, forwards the coder-facing
review unchanged, repeats both test layers as Sol directed, increments N, and
requests another review. Continue until Sol returns `passed` unless the user
explicitly overrides the gate.

## Select Sol continuity

On `passed`, require Sol to select the next eligible task K and return:

```text
Next Task: task-K | none
Next Sol: reuse | fresh | none
Reason: concise justification
Evidence Focus: required only for fresh
```

Choose `reuse` when decisive code, constraints, assumptions, or migration
context are materially shared. Choose `fresh` when the task is weakly related,
changes owned area or domain, or retained context would add noise. Choose
`none` when the plan is complete or no task is eligible.

The dispatcher executes Sol's choice because a child cannot spawn itself. It
must not substitute a different technical or continuity decision. Stop and
return to Sol or the user only for runtime impossibility, safety, missing
authority, explicit user instruction, or repository facts that invalidate the
decision; record the reason.

Before a fresh Sol spawn, have `luna_runner` perform only a mechanical current
repository scan using the Evidence Focus: HEAD, worktree, file names, and
available logs/command output. Then spawn fresh Sol with `fork_turns = "none"`,
`plan.md`, task K, and that raw packet. Sol must read the applicable
instructions, design documents, prerequisites, source, tests, and unresolved
questions itself. Do not substitute the old Sol summary for the refresh.

## Commit every passed task

When a retrospective occurred, Sol's accepted workflow change set includes its
`task-M-code-review-N-retrospective.md` artifact. If and only if Sol decided the
skill requires a reusable correction, it also includes
`task-M-code-review-N-skill-change-spec.md` and the conforming
`.codex/skills/local-workflow/SKILL.md` correction. Keep those paths distinct from
product implementation paths, verify both scopes explicitly, and never stage
an unrelated skill or source change.

After review passes and before following the continuity decision, the
dispatcher commits task M itself:

1. Compare `git status --short` with declared tracked, untracked, and deletion
   scope; stop on mismatch.
2. Confirm all deletions were explicitly authorized.
3. Stage explicit task paths only; never use a broad add.
4. Run `git diff --cached --check`.
5. Verify `git diff --cached --name-only` exactly matches the accepted change
   set.
6. Commit with the accepted subject and collect the real exit status.
7. Record hash, subject, and final `git status --short` in durable status.

Coding, runner, and Sol workers do not commit. If the commit fails, stop and do
not reuse or spawn Sol for task K. After a successful commit, execute
`reuse`/`fresh`/`none` and continue.

Keep every skill update scoped to this repository-local skill; do not modify the
shared/global project-workflow skill. The code review and retrospective are
mandatory for every `changes_required` verdict. The skill-change specification
and local-skill modification are conditional on Sol's explicit reusable
skill-gap decision.
