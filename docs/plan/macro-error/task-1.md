# Task 1 Contract — Macro exception formatter ownership

## Goal and objective

Goal: `macro-error`.

Implement macro exception formatter ownership at the invocation boundary and add
focused local/external regression coverage. The accepted end state is:

- the catch branch that constructs `macro_exception` records it with
  `astranaut_macro` at production;
- a successfully invoked macro's returned error/warning computation continues to
  receive the descriptor/provider or generated-local formatter;
- `invalid_macro_return` and every other framework reason remain owned by
  `astranaut_macro`;
- exception class/reason/stack payload/MFA/arguments, position, file,
  error/warning classification, diagnostic order/count, AST results, and failed-call
  sibling recovery are unchanged; and
- only fixture clauses whose sole purpose is forwarding `macro_exception` are
  removed. Real user-domain formatter clauses remain.

The later `astranaut_struct` universal fallback and `astranaut_lib` API change are
explicitly out of scope and are not reopened by this task.

## Decisive evidence and approach

Current source establishes the narrow implementation boundary:

- `expand_macro_with/3` currently applies the descriptor formatter around
  `invoke_macro_function/1` (`src/astranaut_macro_expander.erl:561-580`).
- `invoke_macro_function/1` catches `Class:Exception:Stacktrace`, builds
  `macro_exception`, and returns `astranaut_traverse:fail(Error)` without a
  formatter (`src/astranaut_macro_expander.erl:589-608`). Its `Macro` map includes
  `pos`, so the catch branch can call
  `astranaut_traverse:update_pos(Pos, astranaut_macro, fail(Error))` directly.
- `astranaut_traverse:update_pos/3` binds a formatter only to pending diagnostics
  (`src/astranaut_traverse.erl:394-417`), so this catch-local binding does not
  overwrite successful user-returned computations handled by the outer descriptor
  boundary.
- `process_macro_return/3` remains outside this catch and already produces
  `invalid_macro_return` under the framework traversal boundary
  (`src/astranaut_macro_expander.erl:610-648`).
- Existing local coverage uses `macro_with_error` for an exception and a deliberate
  returned error (`test/astranaut_macro_SUITE_data/macro_with_error.erl:28-47`), and
  `macro_sibling_errors_test` covers exception, deliberate returned error, invalid
  return, and sibling recovery (`:16-32`). Both currently contain exception-only
  proxy clauses (`macro_with_error.erl:20-21`; `macro_sibling_errors_test.erl:13-14`).
- Existing external formatter coverage (`macro_uniform_a`) has deliberate returned
  errors but no external exception case, and it is not an owned Task 1 fixture
  (`test/astranaut_macro_SUITE_data/macro_uniform_a.erl:9-13,53-57`). Therefore the
  two new external fixtures listed below are required to prove both external paths
  without editing an unrelated fixture.
- The selected macro-error inputs require framework ownership at production,
  successful user-computation ownership, preserved exception payload/recovery, and
  removal of formatter proxies (`openspec/changes/macro-error/design.md:35-67`;
  `openspec/changes/macro-error/specs/macro-error-ownership/spec.md:1-96,118-131`).
  Their struct-facade conflict is later-task scope and is not touched here.
- `lessons.md:7-27,69-96,132-145` requires correct traverse/return separation,
  monadic state threading, and preservation of scoped traversal state. The fix must
  use the existing traversal API and must not manually construct formatted
  diagnostics.

Approach: add `Pos` to the existing `invoke_macro_function/1` pattern, wrap only its
catch-produced failure with `update_pos(Pos, astranaut_macro, ...)`, then tighten the
existing local assertions and add one external provider/consumer pair. Do not move
the outer formatter boundary or route by reason shape.

## Exact owned paths and modules

The coding worker owns exactly these implementation/test paths:

- `src/astranaut_macro_expander.erl` — catch-local formatter binding only.
- `test/astranaut_macro_error_SUITE.erl` — local/external test registration,
  assertions, and helpers for this behavior only.
- `test/astranaut_macro_SUITE_data/macro_with_error.erl` — remove its
  `macro_exception` proxy; preserve `format_error(bar)` and existing macro bodies.
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl` — remove its
  `macro_exception` proxy; preserve `format_error(sibling_return_error)` and all
  sibling macro bodies.
- `test/astranaut_macro_SUITE_data/macro_error_external_provider.erl` — **required
  new fixture**. It exports `format_error/1`, exports external macros
  `raise/0`, `return_error/0`, and `return_warning/0`, raises
  `external_macro_exception` from `raise/0`, returns
  `{error, external_return_error}` from `return_error/0`, and returns a quoted AST
  with `external_return_warning` from `return_warning/0`. Its direct formatter
  clauses cover only `external_return_error` and `external_return_warning`.
- `test/astranaut_macro_SUITE_data/macro_error_external_test.erl` — **required new
  fixture**. It imports `macro_error_external_provider`, invokes `raise/0`,
  `return_error/0`, and `return_warning/0` as ordered sibling calls in `run/0`, and
  has the existing baseline marker so diagnostic positions can be checked.

No other path is owned. In particular, do not edit any OpenSpec file, README,
`src/astranaut_struct.erl`, `src/astranaut_lib.erl`, registry module, local-generation
module, `status.md`, skill file, dependency, generated source, or workflow artifact.

## Frozen behavior and invariants

1. The only production change is equivalent to:

   ```erlang
   invoke_macro_function(
     #{pos := Pos, module := Module, function := Function,
       arguments := Arguments} = Macro) ->
       ...
       catch
           Class:Exception:Stacktrace ->
               ...
               Error = macro_exception(...),
               astranaut_traverse:update_pos(
                 Pos, astranaut_macro, astranaut_traverse:fail(Error))
       end.
   ```

   Preserve the existing stack trimming, `macro_exception/5` payload construction,
   and `recover_macro_call/2` path exactly.

2. The outer `update_pos(Pos, Formatter, invoke_macro_function(Macro))` remains in
   place. Successful returned computations are never preformatted by the catch fix.

3. Local exception diagnostics are raw tuples whose formatter is `astranaut_macro`
   and whose reason remains `{macro_exception, MFA, Arguments, {Class, Reason,
   Stack}}`; local deliberate returned `bar` remains owned by the generated local
   formatter. The local sibling fixture must preserve the established traversal
   order exactly: framework-owned `invalid_macro_return`, framework-owned
   `macro_exception`, and generated-local `sibling_return_error`. The production
   patch must not reorder diagnostics, and the test must assert this exact sequence.

4. External fixture assertions must prove one raw `macro_exception` with
   formatter `astranaut_macro` and MFA for
   `macro_error_external_provider:raise/0`, while the returned error and warning
   retain formatter `macro_error_external_provider` and their exact user reasons.
   Exception and returned diagnostics must remain in source order, with the
   deliberate warning still collected after the error path.

5. Assertions must check the preserved positions against the fixture call sites,
   exact error/warning classification, exact reason terms, MFA/function/arity,
   arguments, and `{Class, Reason, Stack}` payload shape. Stack values may vary only
   according to the existing runtime stack representation; the implementation must
   not trim or remap them differently.

6. Existing invalid-return ownership, final adapter-produced messages, returned AST
   forms, failed-call recovery, sibling count/order, and file association remain
   unchanged. Use raw internal diagnostics for identity assertions and the existing
   `astranaut_test_lib:assert_formatted_messages/1` path for message safety.

## Forbidden alternatives

- Do not apply `astranaut_macro` to the whole invocation computation or move the
  descriptor formatter boundary.
- Do not format by inspecting `Reason`, retry another formatter, add a proxy,
  fallback chain, generic formatter clause, `/2` formatter, or registry special case.
- Do not change `macro_exception` reason shape, stack trimming, MFA construction,
  arguments, recovery, or `astranaut_macro:format_error/1`.
- Do not alter `invalid_macro_return` or any other framework reason path.
- Do not modify `macro_uniform_a`; the dedicated external fixture is required.
- Do not remove real domain clauses (`bar`, `sibling_return_error`) from fixtures.
- Do not touch the later struct fallback/API decision or any of its paths.
- Do not edit OpenSpec, README, status, skill, staging, commits, or generated output.

## Ordered implementation steps

1. Confirm the current diff has no pre-existing change in the six owned paths and
   leave all existing unrelated workflow/OpenSpec changes untouched.
2. In `src/astranaut_macro_expander.erl`, add `pos := Pos` to the existing
   `invoke_macro_function/1` map pattern and replace only the catch branch's final
   `astranaut_traverse:fail(Error)` with the explicit catch-local
   `astranaut_traverse:update_pos(Pos, astranaut_macro,
   astranaut_traverse:fail(Error))`. Preserve all surrounding logic verbatim.
3. In `macro_with_error.erl`, remove only the two-line
   `format_error({macro_exception, ...})` proxy clause. Keep the export, `bar` clause,
   and all macro declarations/bodies.
4. In `macro_sibling_errors_test.erl`, remove only the two-line
   `format_error({macro_exception, ...})` proxy clause. Keep the export,
   `sibling_return_error` clause, and all four local macro definitions.
5. Add the exact external provider and consumer fixtures specified above. Register
   the provider in `init_per_suite/1` and add one `all/0` case in
   `astranaut_macro_error_SUITE.erl`.
6. Tighten `test_macro_with_error` to require `astranaut_macro` for the local
   exception and the generated local formatter for `bar`, including payload/MFA and
   positions. Tighten `test_macro_sibling_errors` to require the three raw diagnostics
   in the established traversal order `invalid_macro_return`, `macro_exception`,
   `sibling_return_error`, with framework ownership for the first two and
   generated-local ownership for the deliberate returned error; assert this exact
   sequence and retain recovery/count assertions. The production patch must not
   reorder diagnostics.
7. Add the external test case to assert exception/provider ownership, exact reasons,
   payload shape, positions, classification, order/count, returned-warning
   collection, and formatted messages. Do not weaken assertions to accept a user
   formatter for `macro_exception` or a `local_macro_diagnostic` proxy wrapper.
8. Run the Coding Self-Tests below and report every raw result. Do not stage or
   commit.

## Stop conditions

Stop and report exact evidence without choosing a design if:

- `Macro` does not contain the required `pos` at the catch boundary, or the
  catch-local `update_pos` would overwrite a successful returned computation;
- the current `macro_exception/5` payload or recovery path requires modification;
- any expected local/external raw diagnostic differs in formatter, reason, position,
  classification, order/count, payload, or recovery;
- the exact fixture API cannot be compiled using the existing macro fixture pattern;
- a test requires changing a path outside the six owned paths;
- any pre-existing change overlaps an owned path and cannot be attributed safely;
- a proxy, fallback chain, reason dispatch, registry change, struct/API change, or
  OpenSpec/README change appears necessary; or
- Coding Self-Tests are missing, interrupted, stale, or failing. The worker reports
  the command and evidence and makes no design choice.

## Coding Self-Tests — `luna_coding_worker` only

Run after implementation and after every rework:

1. `rebar3 compile`
2. `rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl`
3. `rebar3 ct --suite=test/astranaut_macro_local_SUITE.erl`
4. `git diff --check`

On Windows Codex sandbox, follow `AGENTS.md`: use direct escalated `rebar3 compile`
and direct escalated suite commands rather than the sandbox wrapper. These commands
belong to the coding worker; Sol does not run them.

## Independent Verification — separate `luna_runner` only

Only after Coding Self-Tests pass, a separate runner with no child delegation repeats:

1. `rebar3 compile`
2. `rebar3 ct --suite=test/astranaut_macro_error_SUITE.erl`
3. `rebar3 ct --suite=test/astranaut_macro_local_SUITE.erl`
4. `git diff --check`
5. `git status --short`
6. `git diff --name-only`

The runner returns raw command text, completion/interruption state, exit status, test
counts, generated artifacts, and status/diff-name output. It performs no source or
assertion audit and edits no file. Sol reviews the evidence packet only after it is
complete.

## Expected scope, deletions, and commit

Expected tracked implementation modifications are exactly:

- `src/astranaut_macro_expander.erl`
- `test/astranaut_macro_error_SUITE.erl`
- `test/astranaut_macro_SUITE_data/macro_with_error.erl`
- `test/astranaut_macro_SUITE_data/macro_sibling_errors_test.erl`

Expected new untracked implementation fixtures, which the dispatcher may later add
to the task commit after review, are exactly:

- `test/astranaut_macro_SUITE_data/macro_error_external_provider.erl`
- `test/astranaut_macro_SUITE_data/macro_error_external_test.erl`

No deletion is authorized. Existing deleted workflow artifacts and modified
OpenSpec/plan/status/skill paths are pre-existing and remain outside this task. No
other untracked path is permitted in the accepted task scope; ignored build output
is not stageable task content.

Proposed dispatcher commit subject: `Bind macro exceptions to astranaut_macro`.

## Completion criteria

Task 1 is complete only when:

- the production catch binds only catch-produced `macro_exception` to
  `astranaut_macro` at its position;
- local and external returned error/warning computations retain their generated-local
  or provider formatter;
- invalid return and other framework reasons remain `astranaut_macro`;
- exact local/external payload, position, file, class, MFA, arguments, stack shape,
  classification, sibling order/count, AST recovery, and failed-call recovery
  assertions pass;
- both exception-only fixture proxies are removed while real domain clauses remain;
- both Coding Self-Tests and independent runner evidence pass;
- Sol review reports `passed`; and
- the dispatcher confirms exact scope, stages only these six implementation paths,
  performs commit-scope checks, and commits with the proposed subject.

## Workflow authority and next routing

The coding worker owns implementation and Coding Self-Tests. The separate
`luna_runner` owns Independent Verification. Sol remains read-only for source/tests,
does not run commands, and writes no status, review, or implementation file during
this task. The dispatcher owns attribution, status, staging, and commit operations.

After a passed review and dispatcher commit: `Next Task: task-2`; `Next Sol: reuse`;
reason: Task 2 directly consumes this task's formatter-at-production boundary and
the same macro diagnostic evidence.
