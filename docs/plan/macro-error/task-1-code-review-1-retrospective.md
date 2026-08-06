# Task 1 Code Review — Round 1 Retrospective

## Trigger

Review found one `changes_required` issue: the Sol-owned Task 1 contract froze a
source-order sequence that contradicts the real traversal order asserted by the
implementation and confirmed by both completed test packets.

## Why the initial contract failed

The contract correctly froze formatter ownership, payload preservation, proxy removal,
fixture scope, and the catch-local traversal approach. It failed at
`docs/plan/macro-error/task-1.md:123-125` by converting “preserve sibling order” into
an unsupported “source sibling order” requirement. The existing test before Task 1
used `lists:any/2` for the three diagnostics and therefore did not provide decisive
order evidence. Sol inferred source order instead of freezing the established order
from the actual traversal behavior.

The coding worker then made the smallest product-safe choice: it preserved the
existing diagnostic order and asserted the observed sequence at
`test/astranaut_macro_error_SUITE.erl:444-454`. Both coding and independent evidence
passed, which confirms this is a contract error rather than an implementation or
test failure.

## Recurrence correction

The smallest correction is local to `task-1.md`: replace the source-order statement
with an explicit preservation rule naming the observed sequence
`invalid_macro_return`, `macro_exception`, `sibling_return_error`. Keep the existing
test and production patch unchanged. Rerun both evidence layers afterward because
the accepted contract must be the one used for the final review gate.

No local workflow skill change is warranted. The skill already requires decisive
evidence, frozen test semantics, and review against the real diff; this was a
task-specific Sol inference made without sufficient order evidence, not a missing
general workflow rule.

## Required routing sequence

1. Sol corrects the Task 1 contract as specified above.
2. The coding worker reruns all four Coding Self-Tests with no product rework unless
   a command exposes a new failure.
3. A separate `luna_runner` repeats the complete Independent Verification packet.
4. Sol performs review round 2 only after that runner packet is complete.
