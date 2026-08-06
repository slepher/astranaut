# Task 6 Code Review 3 Skill Change Specification

## Rule

The repository-local workflow skill is located at
`.codex/skills/project-workflow-local/SKILL.md`. Every reference in that file
to the skill file itself must use that exact repository-relative path. The
obsolete self-reference `project-workflow-local/SKILL.md` must not remain in
the file.

## Target sections

Update the four existing self-references only:

- two references in `Persist the initiative`;
- one reference in `Review until passed`, within the
  `changes_required` procedure;
- one reference in `Commit every passed task`.

## Modification scope

- Modify only `.codex/skills/project-workflow-local/SKILL.md` under this
  specification.
- Replace each exact `project-workflow-local/SKILL.md` self-reference with
  `.codex/skills/project-workflow-local/SKILL.md`.
- Do not alter any other wording, formatting, rule, workflow behavior, or
  artifact name.
- Do not modify the shared/global
  `/home/slepher/.codex/skills/project-workflow/SKILL.md`.

## Invariants

- Sol, dispatcher, coding-worker, and runner responsibilities remain
  unchanged.
- Write ownership, review gates, verification boundaries, artifact lifecycle,
  continuity selection, and commit procedure remain unchanged.
- The edit is a repository-path correction only and introduces no new workflow
  requirement.

## Acceptance criteria

- All four self-references in the repository-local skill use
  `.codex/skills/project-workflow-local/SKILL.md`.
- No `project-workflow-local/SKILL.md` self-reference lacking the `.codex/skills/`
  prefix remains in the repository-local skill.
- Apart from those four exact replacements, the repository-local skill text is
  unchanged.
- The shared/global project-workflow skill is unchanged.
