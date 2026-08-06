# Task 3 Code Review 2 Skill Change Specification

## Trigger

The second code review showed that the first retrospective corrected the known
consumer-side duplication but did not force a whole-operation producer
inventory. This exposed a reusable prevention gap in the review contract.

## Historical rule specified by this round

For every `changes_required` verdict, Sol must write a retrospective explaining
why the design, task contract, evidence gate, or workflow rule did not prevent
the finding before routing rework. Sol must then decide whether the diagnosis
exposes a reusable local-skill gap.

When a reusable skill gap exists, Sol must specify the concrete prevention rule
before editing the repository-local skill. When it does not exist, the
retrospective remains sufficient and no skill change is permitted.

## Scope and acceptance

- Keep the change limited to `project-workflow-local/SKILL.md`.
- Keep coder-facing corrections in the code-review document.
- Keep historical diagnosis in the retrospective.
- Make this specification the basis for the skill edit.
- Require the resulting skill diff to conform to this specification.

This migrated specification records the historical decision behind the local
workflow correction. The current `project-workflow-local/SKILL.md` remains the
authority for present behavior and may contain later superseding rules.
