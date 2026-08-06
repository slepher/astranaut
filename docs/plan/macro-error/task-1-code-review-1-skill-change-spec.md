# Local Workflow Skill Change Specification

## Trigger and Scope

Task 1 exposed a reusable workflow gap: a coding contract can describe inputs,
constraints, and a broad reconciliation objective while still leaving the
coding worker to choose the substantive protocol or specification end state.
This correction applies to every coding dispatch, including product-code and
specification-only tasks. It changes no Sol, dispatcher, coding-worker, or
independent-verification authority boundary.

## Target

Add one concise pre-dispatch gate to
`.codex/skills/local-workflow/SKILL.md` immediately after `## Plan one task`
and before `## Implement and self-test`.

## Normative Rule

Before routing `task-M.md` to a coding worker, Sol must freeze every
substantive technical and specification decision in the coding contract. The
contract must state the accepted end state, exact owned paths, invariants,
forbidden alternatives, Coding Self-Tests, and stop conditions. Reconciliation,
interpretation, and selection among conflicting requirements remain Sol work,
not coding-worker work.

If implementation reveals an ambiguity or conflict not decided by the
contract, the coding worker must stop and report the exact evidence instead of
choosing a resolution. Dispatcher preflight must reject any coding contract
that retains an unresolved design choice and return it to Sol for a written
decision artifact before dispatch.

For a specification rewrite, Sol must provide an exact final-state/edit map for
each owned document or section. An open-ended objective such as “reconcile” is
insufficient unless the contract also fixes the resulting requirements,
terminology, retained behavior, removed behavior, and cross-document outcome.

## Invariants

- Sol retains exclusive ownership of substantive technical/specification
  decisions and coder-facing contracts.
- The dispatcher performs a completeness preflight but does not interpret or
  improve Sol's decisions.
- Coding workers implement frozen decisions and do not adjudicate requirements.
- Existing self-test and independent-verification ownership remains unchanged.
- The correction does not require coding workers to read workflow-only
  artifacts beyond their dispatched coding contract.

## Acceptance Criteria

- The skill contains a clear gate before coding-worker routing.
- All required contract fields and stop/escalation behavior are explicit.
- Dispatcher rejection returns unresolved choices to Sol for a decision
  artifact without granting the dispatcher technical authority.
- Specification rewrites require an exact final-state/edit map and cannot be
  dispatched as open-ended reconciliation.
- No existing Sol ownership or independent-verification rule is weakened.
