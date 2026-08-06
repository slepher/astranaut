# Task 2: tighten the test-helper formatter protocol

## Decisive Evidence

- Current HEAD: `72992b31b0273c17061aef22526663e96403ea65`.
- Task 1 implementation is committed and Review 2 passed.
- `test/astranaut_test_lib.erl:115-133` currently checks only `/2` and silently
  accepts a formatter without `/2`.
- Existing `/1`-only fixture: `test/astranaut_SUITE_data/sample_transformer_1.erl`.
- Existing strict formatter: `src/astranaut.erl:592-616`.
- Shared dispatcher and nested `function_clause` semantics:
  `src/astranaut_lib.erl:613-628`, with regression tests in
  `test/astranaut_SUITE.erl:538-565`.

## Objective

Tighten `astranaut_test_lib:assert_formatted_messages/1` so it distinguishes:

1. strict formatter: both `/1` and `/2`; call `/2(Error, #{default => throw})`;
2. legacy formatter: only `/1`; call `/1(Error)` without strict options;
3. invalid formatter: `/1` absent, whether `/2` exists or not; fail explicitly.

Every valid result must be a non-empty deep character list. Do not change
`astranaut_lib:format_error/4` or production formatter behavior.

## Invariants and approach

- `/1` remains the compiler formatter identity anchor.
- `/2` is invoked only when both exports exist.
- Legacy `/1` is checked and identified as legacy, never reported as strict.
- Only `/2`, neither entry point, and unloadable formatter identifiers fail;
  only-`/2` must not be invoked merely because it can return text.
- Existing diagnostic tuple shape and public helper export remain unchanged.
- Formatter exceptions fail with formatter, error, class, reason, and stack.
- Use the existing fixture loading and assertion conventions; add no public
  protocol API.

## Owned paths

Allowed changes:

- `test/astranaut_test_lib.erl`
- `test/astranaut_SUITE.erl`
- `test/astranaut_SUITE_data/sample_transformer_only_v2.erl`

Do not modify `src/`, `astranaut_rebinding` files, existing
`sample_transformer_1.erl`, other suites/fixtures, workflow artifacts, or
generated `_build` output. No deletions are authorized.

## Ordered implementation steps

1. Classify the formatter after confirming the identifier is loadable:
   `strict` for `/1`+`/2`, `legacy` for `/1` only, and `invalid` when `/1` is
   absent or the module cannot be loaded.
2. For `strict`, invoke exactly
   `Formatter:format_error(Error, #{default => throw})` and require
   `io_lib:deep_char_list(Message)` plus a non-empty flattened result.
3. For `legacy`, invoke exactly `Formatter:format_error(Error)` and apply the
   same non-empty character-list check without claiming strict coverage.
4. For `invalid`, fail with a clear protocol marker such as
   `{invalid_formatter_protocol, Formatter, missing_format_error_1}`.
5. Add helper protocol tests for `astranaut` (strict),
   `sample_transformer_1` (legacy), `sample_transformer_only_v2` (invalid),
   and a formatter with neither entry point (invalid).
6. Add the test-only only-`/2` fixture, export only `format_error/2`, return
   non-empty text, and load it through existing suite setup.
7. Preserve existing dispatcher tests and all current call sites of
   `assert_formatted_messages/1`.

Stop if the implementation would require a production change, a public API
change, a dispatcher semantic change, or an out-of-scope path.

## Coding Self-Tests

Run each with a real Common Test timeout of at least 120 seconds:

```bash
rebar3 ct --suite=test/astranaut_SUITE.erl
rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl
rebar3 ct
git diff --check
```

Every command must exit 0. The helper protocol cases, existing rebinding
strict tests, nested dispatcher tests, and full suite must pass.

## Independent Verification

A separate read-only verifier must run:

```bash
rebar3 ct --suite=test/astranaut_SUITE.erl
rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl
rebar3 ct
git diff --check
git status --short
git diff --name-only
git diff --cached --name-only
```

It must inspect the complete helper diff, all protocol tests, the only-`/2`
fixture, every current helper call site, exact strict and legacy invocation,
invalid protocol rejection, unchanged dispatcher implementation, and absence
of staged or unauthorized paths.

## Expected paths and commit

Expected modified tracked paths:

```text
test/astranaut_test_lib.erl
test/astranaut_SUITE.erl
```

Expected permitted untracked path:

```text
test/astranaut_SUITE_data/sample_transformer_only_v2.erl
```

Authorized deletions: none.

Proposed commit subject:

```text
Tighten formatter protocol checks
```

## Completion criteria

- `/1`+`/2` formatters are strictly checked through `/2`.
- `/1`-only formatters are checked through `/1` and marked legacy.
- only-`/2` and no-`/1` formatters fail explicitly.
- focused suites and full Common Test pass.
- `git diff --check` passes.
- final implementation scope is exactly the two tracked paths and one fixture.
- no production formatter or `astranaut_lib:format_error/4` changed.
