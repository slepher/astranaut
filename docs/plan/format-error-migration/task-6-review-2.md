# Task 6 Review 2

Verdict: passed

## Review boundary and authoritative correction

This is the immutable second semantic review of the no-op Task 6 acceptance
at HEAD 6308e2584b1e89114a018831e253b5e196abbefc. Review 1 is not rewritten.
Its finding that macro, compile-meta, and quote must retain a historical
caller-specific astranaut:format_error/2 fallback is withdrawn as an audit
misjudgment. The authoritative migration decision is that every top-level
formatter no-match handled by dispatch_error/3 uses
astranaut_lib:format_default_error/2. The product must not preserve that old
caller-specific fallback chain.

The contradictory Sol-owned wording was reconciled in plan.md and task-6.md:
the contract now names the shared format_default_error/2 behavior as the
format_error/1 default, explicitly excludes caller-specific fallback
preservation, and audits shared /1 default behavior rather than historical
caller routing. No accepted review history, status file, product source, or
product test was rewritten.

## Mechanical evidence assessment

The supplied Coding acceptance packet and Independent runner packet are
mechanical evidence only. Both report all fourteen requested commands
completed with exit 0 and no timeout, interruption, failure, or skip:
compile; local 42; macro error 15; astranaut 37; rebinding 21; full Common
Test 442; both diff checks; HEAD identity; commit subject; committed path
listing; raw working-tree scope; and empty cached scope. They report no
erl_crash.dump. I did not rerun any command.

## API and fallback audit

src/astranaut_lib.erl:15-24 exports dispatch_error/3 and
format_default_error/2. The implementation at lines 612-627 accepts a
one-argument formatter, returns a matching result, and catches only
error:function_clause. formatter_no_match/2 and formatter_frame_matches/3 at
lines 639-656 identify a top-level no-match by the formatter function's
module, name, and arity. A matching top-level no-match calls
format_default_error(Error, Options) at line 623.

The shared fallback at src/astranaut_lib.erl:629-637 is decisive:
deep character lists are returned unchanged, other ordinary reasons use
io_lib:write/1, and #{default => throw} throws the original Error. If a
function_clause came from a nested formatter helper rather than the
formatter entry function, the frame does not match and lines 624-625
re-raise the original error and stacktrace. This is the required
top-level/no-match versus nested-failure distinction.

The public behavior is covered independently in
test/astranaut_SUITE.erl:498-518 and the shared dispatcher assertions at
lines 520-578. test/astranaut_design_SUITE.erl:100-113 checks the exact
public export set, including both shared functions. No product source or
Task 5 fixture uses format_error/4, an injected fallback argument, or a
private catch-all formatter clause.

## Caller and formatter audit

The relevant callers at these exact entry points all use the shared
dispatcher:

- src/astranaut.erl:592-603;
- src/astranaut_macro.erl:45-56;
- src/astranaut_compile_meta_transformer.erl:35-45;
- src/astranaut_quote.erl:991-1006;
- src/astranaut_do.erl:36-50;
- src/astranaut_rebinding.erl:34-49; and
- src/astranaut_struct_transformer.erl:37-53.

src/astranaut_struct.erl:20-29 is an intentional public facade delegating
to astranaut_macro. The known macro-exception and validation clauses in
astranaut_do, astranaut_rebinding, macro_with_error.erl, and
macro_sibling_errors_test.erl deliberately delegate a known reason to a
strict formatter. They are specific protocol clauses, not generic
caller-specific fallback ownership and not private catch-alls.

The four Task 5 fixture paths have the required exact clauses:
macro_with_warnings.erl:23-36 covers noop and noop_function;
macro_with_error.erl:18-27 covers bar and delegates the known
macro_exception; macro_sibling_errors_test.erl:11-13 covers
sibling_return_error and delegates the known macro_exception; and the
strict local fixture retains its specific warning clause. Their /1 wrappers
pass ordinary options through /2, and their private format_error_1/1
functions have no catch-all.

## Diagnostics and protocol assertions

The macro error suite asserts exact warning positions, formatter ownership,
reasons, order, and counts at test/astranaut_macro_error_SUITE.erl:40-109.
The strict local case at lines 131-160 asserts the generated local identity,
both exported formatter arities, private helper isolation, custom strict
formatting, shared ordinary fallback for an unknown reason, and strict
throw for that same unknown reason. The /2-only case at lines 162-174
requires astranaut_macro as the diagnostic formatter and does not accept a
local formatter. Sibling diagnostics at lines 372-429 assert three sibling
results, macro exception payload shape, sibling return shape, invalid return
shape, local identity, and non-exported format_error_1/1.

The corrected /1 invariant is shared default behavior, not preservation of
the historical caller-specific Astranaut fallback. Consequently the
ordinary unknown-reason assertions in test/astranaut_SUITE.erl:498-501 and
test/astranaut_rebinding_SUITE.erl:301-304 validate deep-character-list and
io_lib:write behavior, while strict unknown-reason assertions at
astranaut_SUITE.erl:503-506 and rebinding_SUITE.erl:297-300 validate
throwing. Known local and global formatter clauses retain their exact
messages and strict behavior.

## Generation, lifecycle, and local-state audit

The existing local macro implementation remains the owner of lifecycle
semantics. In src/astranaut_macro_local.erl:174-224 it records formatter
options and finalizes retained state; lines 449-475 plan only member
functions; lines 550-616 commit compiled members and enforce generation,
retained, frozen, and nonclosure state; lines 680-707 maintain cache and
environment fingerprints; and lines 795-875 select formatter protocol,
roots, transitive formatter-related forms, and generated exports. Boundary
keys and committed Members remain separate from formatter-related forms at
lines 905-959, while request fingerprints/cache state are maintained at
lines 990-1019 and callable selection at lines 1104-1116.

The focused lifecycle assertions in
test/astranaut_macro_local_SUITE.erl:62-222, 589-695, and 697-830 cover
generation and retain behavior, frozen IDs, callable compiled state,
boundary keys, cache/fingerprint and environment snapshots, protocol
selection, formatter roots, closure IDs/FAs/forms, generated exports, and
private helper absence. In particular, lines 732-830 prove that formatter
entries and helpers are absent from Members, request closure IDs/FAs,
request forms, and frozen IDs; that formatter-only forms do not alter
generation or boundary state; and that the generated local formatter keeps
the required public /1 and /2 identity without exporting private helpers.
The source and tests reuse the existing lifecycle and closure machinery;
the migration adds no parallel closure walker, protocol detector,
fingerprint, cache, or state harness.

## Scope and reuse decision

The committed product boundary is exactly the sixteen paths recorded by
6308e25: seven source modules, three macro fixture modules, and six
modified suites listed by git show for that commit. There is no product
source/test deletion, dependency, generated, or staged path attributable to
Task 6. The current uncommitted diff is workflow metadata only
(plan.md and root status.md, with initiative artifacts and
project-workflow-local/ in the raw worktree); it is outside the committed
product boundary and remains dispatcher-owned. The cached diff is empty.

The implementation reuses astranaut_lib:dispatch_error/3 and
format_default_error/2, existing formatter protocol assertions, existing
fixture compilation/realization helpers, and the existing local lifecycle
state model. No redundant fallback or test machinery was introduced. No
reusable gap in project-workflow-local/SKILL.md caused or remains from this
review, so no improved-skill artifact or skill edit is warranted.

## Continuity

Next Task: none

Next Sol: none

Reason: initiative complete.
