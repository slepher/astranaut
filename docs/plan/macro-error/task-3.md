# Task 3 — Document macro diagnostic ownership and formatter boundaries

## Boundary

This is a separate, explicitly user-authorized README documentation boundary after
Tasks 1 and 2. It records already accepted behavior; it makes no product, API, or
specification decisions and is not a coder-worker task.

## Owned path

Exactly:

- `README.md`

Expected implementation diff: one modified `README.md` and no additions,
deletions, or modifications to any other path. `README.zh.md`, OpenSpec, source,
tests, `plan.md`, `status.md`, skill files, and review artifacts are out of scope.

## Exact edit map

Preserve the README's existing Markdown and prose style. Add only these two sections;
do not rewrite surrounding documentation.

1. Under `# astranaut_lib`, immediately after the existing public API code block and
   before the `reload_forms/2` paragraph, add a formatter API subsection with exactly
   this content:

   ```markdown
   ### formatter api

   ```erlang
     astranaut_lib:format_error({Module, Reason}) -> Message.
     astranaut_lib:format_error(Reason, FormatterFun) -> Message.
     astranaut_lib:format_default_error(Reason) -> Message.
   ```

   `format_error/1,2` are the compiler adapter and shared formatter boundary. When a
   formatter has no matching clause, `format_error/2` uses
   `format_default_error/1`: deep character lists are returned unchanged, and every
   other term is formatted with `io_lib:write/1`.
   ```

2. Under `# Macro`, immediately after the existing `*Warnings*` table, add this
   diagnostic ownership section with exactly this content:

   ```markdown
   *Diagnostic ownership and formatting*

   Expected domain failures are returned as Astranaut error or warning computations.
   The registry-selected formatter for that macro owns and formats those returned
   domain diagnostics; a macro formatter defines only its own domain reasons.

   An unexpected `error`, `throw`, or `exit` raised during macro invocation is caught
   at the invocation boundary and recorded as `macro_exception`, owned by
   `astranaut_macro`. This is fault containment, not the domain-failure protocol.
   The diagnostic preserves the exception payload, including its class, reason,
   stacktrace, MFA, arguments, and position.

   Framework reasons remain owned by `astranaut_macro`. Struct-transform reasons remain
   owned by `astranaut_struct_transformer`.

   `astranaut_struct:format_error/1` remains exported as a compatibility formatter
   with one universal fallback clause:

   ```erlang
   format_error(Msg) ->
       astranaut_lib:format_default_error(Msg).
   ```

   Because that export is present, compiling a struct provider emits no
   `{missing_macro_formatter, astranaut_struct}` warning.
   ```

The wording must not describe exceptions as expected domain failures, make
`astranaut_struct` the owner of framework diagnostics, describe the struct formatter
as removed, imply a formatter fallback chain, or introduce a new API or behavior.

## Acceptance boundary

- The two exact sections above are present in `README.md` at the specified anchors.
- Existing README content outside those insertions is preserved.
- The documented ownership, returned-computation protocol, exception payload
  preservation, default-format semantics, adapter boundary, struct export, and
  no-warning consequence match the accepted Task 1/2 behavior.
- The expected path set is exactly `{README.md}`.

No product, test, specification, or additional documentation change is authorized by
this task. No dispatch or coding-worker contract is defined here.

## Next action

Apply the exact edit map to `README.md`, then present the README-only diff for the
separate documentation-boundary review.
