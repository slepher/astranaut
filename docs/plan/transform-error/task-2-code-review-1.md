# Task 2 Code Review 1

## Status

Completed.

## Verdict

`passed`

## Findings

No material findings remain.

## Review Result

The current Task 2 implementation satisfies the warning migration contract without reopening the formatter protocol committed by Task 1 (`ae32f6c Adapt compiler diagnostics through astranaut_lib`).

- External providers are checked only for `format_error/1`. The registry records already-warned provider modules in a compile-local `ordsets` value (`src/astranaut_macro_registry.erl:44`, `:62`, `:93-118`, `:407-418`). A first successful registration of a missing formatter emits one return-monad warning and retains the registered macros; repeated imports and later use/calls do not repeat it.
- Local formatter detection uses the existing cached `formatter_info` as the once-per-source-compilation gate (`src/astranaut_macro_local.erl:82-96`, `:109-114`). The warning reason names `SourceModule`, while the pending diagnostic remains on the `astranaut_macro` traversal formatter. Multiple local declarations therefore retain source-module identity and emit once.
- Both warning paths are sequenced as monadic actions. The external action follows successful registry merges, and the local action precedes declaration preparation, so warnings are neither discarded by comma sequencing nor prevented from accompanying a later failure. Existing traversal boundaries remain responsible for source position attachment.
- The public reason has one pure formatter clause (`src/astranaut_macro.erl:78`), with no compatibility dispatch, options-bearing formatter path, throw path, or Task 1 protocol rollback.
- The only-v2 fixture deliberately exports `format_error/2` and no `/1` (`test/astranaut_macro_SUITE_data/macro_only_v2_formatter_provider.erl:7-10`). Exact assertions treat it as missing for both local and external coverage (`test/astranaut_macro_error_SUITE.erl:170`, `:179-214`).
- External coverage repeats provider import and macro calls (`test/astranaut_macro_SUITE_data/macro_missing_formatter_external_test.erl:10-18`); local coverage declares two local macro groups (`test/astranaut_macro_SUITE_data/macro_missing_formatter_local_test.erl:11-12`). The assertions check identity, position, formatter, reason, warning count, and successful expansion. Updated pass and uniform expectations preserve pre-existing diagnostics rather than masking them.

## Simplicity and Reuse

The implementation reuses the existing `astranaut_return` warning action, traversal return bridge, formatter/position boundary, cached local formatter metadata, and OTP `ordsets`. The two small warning helpers correspond to distinct registry and local lifecycles. No process-global state, alternate warning channel, compatibility adapter, or new abstraction is introduced.

## Scope

The product/test patch is confined to the three declared source modules, three declared suites that required expectation changes, and the four declared new fixtures. The other two declared suites required no edits. There are no product/test deletions. Current OpenSpec, status, and local-workflow changes remain excluded from Task 2 product ownership; Task 1 committed files and protocol are unchanged.

## Evidence

The independent runner reported every command completed without interruption and without modifying, staging, or committing files:

- Initial and final status/stat/check commands exited 0; diff check was empty. The reported working-tree stat was 436 insertions and 201 deletions.
- Compile exited 0.
- Declared suites passed with the reported counts: macro scan 12, macro error 17, macro local 41, macro pass 37, and macro uniform 19. Regression suites also passed: astranaut 40, design 21, quote 73, rebinding 21, struct 19, and disable TCO 4.
- Full Common Test exited 0 with all 445 tests passed.
- Xref exited 0 with no warnings.
- Strict OpenSpec validation exited 0 and reported the change valid.
- The forbidden-residual search exited 1 with no matches. The `missing_macro_formatter` audit exited 0 with matches limited to the three implementation modules and specified suites.
- Dialyzer exited 1 only for `src/astranaut_syntax_schema.erl:699:14`, where `is_list(Node1::erl_anno:anno())` breaks argument opaqueness. That module has an empty Task 2 diff, and the warning is the previously approved baseline exception. Task 2 neither adds nor changes a Dialyzer warning. The retained artifact is `_build/default/29.0.4.dialyzer_warnings`; coverage is `_build/test/cover/index.html`.

The mechanical packet and direct source/diff review agree; no evidence contradicts the contract.

## Continuity

- Next Task: none
- Next Sol: none
- Reason: Task 2 completes the remaining formatter-protocol warning migration. Goal `transform-error` is complete after the dispatcher commits Task 2 with the declared boundary.
