# Task 5 Review 1

Verdict: passed

## Scope and commit boundary

The accepted Task 5 product boundary is the four paths already committed by
6308e25 (Switch formatter callers to dispatch_error):

- test/astranaut_macro_SUITE_data/macro_with_warnings.erl
- test/astranaut_macro_SUITE_data/macro_with_error.erl
- test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl
- test/astranaut_macro_error_SUITE.erl

The commit also contains the shared caller migration and the Task 4 strict
fixture adaptation; those are prerequisite context, not additional Task 5
reference paths. The current working-tree changes are workflow metadata
(docs/plan/format-error-migration/plan.md, root status.md, initiative
artifacts, and project-workflow-local/). There is no current Task 5 product
diff, staged product path, deletion, or duplicate commit. The old task-5.md
remains the immutable superseded contract, as required by
task-5-replan.md:22-25,283-296.

## API and fallback audit

src/astranaut_lib.erl:612-656 exports dispatch_error/3 and
format_default_error/2, and no longer exports or implements the removed
four-argument dispatcher. dispatch_error(Error, Options, FormatterFun)
invokes the one-argument formatter. A top-level error:function_clause
whose first frame identifies that formatter fun calls
format_default_error(Error, Options); ordinary options preserve a deep
character list or use io_lib:write/1, while exactly
#{default => throw} throws the original reason. A nested function_clause
whose frame is not the dispatcher fun is re-raised with its original stack.
The matching logic at :639-656 compares the actual formatter module, name,
and arity, so fallback is not triggered by an internal formatter failure.

All relevant committed source callers use dispatch_error/3, including
astranaut:format_error/2 at src/astranaut.erl:592-599 and the global macro
formatter at src/astranaut_macro.erl:45-52. The shared behavioral assertions
at test/astranaut_SUITE.erl:529-578 independently cover a matching clause,
ordinary fallback, strict throwing, and nested function_clause propagation.
The design export assertion at test/astranaut_design_SUITE.erl:107-113 also
requires the committed dispatch_error/3 API.

## Fixture and diagnostic audit

The three reference fixtures have the required exact formatter clauses and no
injected fallback or private catch-all:

- macro_with_warnings.erl:21-36 exports /1 and /2; /1 delegates with {}
  and /2 dispatches only noop and noop_function through private
  format_error_1/1. Their results remain "oops, noop" and
  io_lib:write(noop_function).
- macro_with_error.erl:16-27 exports both arities; the local clause covers
  only bar, and the separate exact {macro_exception, _, _, _} clause
  delegates to astranaut_macro:format_error/2 with strict options. This is a
  known global reason clause, not a fallback callback or catch-all.
- macro_sibling_errors_test.erl:9-13 exports both arities; the local clause
  covers only sibling_return_error, with the same exact known
  macro_exception delegation. invalid_macro_return remains owned by the
  global astranaut_macro formatter.

The exact warning sequence, positions, formatter identities, reasons, and
count remain asserted at test/astranaut_macro_error_SUITE.erl:40-70.
The error sequence, positions, macro reasons, max-depth payload, and exception
payload shape remain asserted at :72-109. The sibling test retains three
diagnostics and its accepted direct/nested sibling representations at
:372-414; sibling_local_formatter/1 at :436-457 verifies the generated local
formatter identity. The new direct strict calls cover noop, noop_function,
bar, and sibling_return_error, while the shared
assert_formatted_messages/1 call at test/astranaut_test_lib.erl:118-178
continues to be the authority for strict non-empty character-list results.

Each generated local formatter assertion requires exported /1 and /2 and
rejects private format_error_1/1. The existing local-macro implementation
at src/astranaut_macro_local.erl:811-846,860-875 keeps formatter entries in
the generated module's temporary related forms/exports without making them
macro members or closure roots. Task 4's established closure and lifecycle
assertions remain the relevant companion proof for that boundary.

## Reuse and design audit

The migration reuses the single shared astranaut_lib:dispatch_error/3 and
format_default_error/2 capability, delegates known macro diagnostics to the
existing astranaut_macro formatter, and retains the existing fixture loader,
compiler/realization path, generated-module identity checks, and shared
formatter-protocol assertion. No second fallback detector, protocol checker,
closure walker, diagnostic harness, or state model was introduced. The
anonymous formatter clauses are the smallest local adapter needed to expose
specific private formatter clauses while leaving fallback ownership and
nested-error semantics in the shared dispatcher.

## Mechanical evidence assessment

The supplied Coding acceptance packet reports the committed HEAD, successful
compile, focused formatter cases, relevant complete suites, clean diff check,
and no staged paths or deletions. The supplied independent runner packet is
complete and reports no timeout, interruption, skip, or failure, including
full Common Test success (442 cases). These packets are accepted as mechanical
evidence only; the semantic conclusion above is based on the committed source,
assertions, history, and scope inspection.

No reusable gap was found in project-workflow-local/SKILL.md. The observed
issues in the superseded /4 contract were task-specific API/design mistakes
already corrected by task-5-replan.md; the workflow already assigns raw
mechanical execution to the runner and semantic review to Sol. No improvement
artifact or skill edit is warranted.

## Continuity

- Next Task: task-6
- Next Sol: reuse
- Reason: Task 6 is the final regression and initiative acceptance stage for
  this same formatter migration. The committed dispatch_error/3 contract,
  diagnostic invariants, scope boundary, and runner-versus-Sol evidence rules
  remain directly relevant and should carry forward.
