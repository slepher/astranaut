# Changelog

[中文](changelog.zh.md)

## 0.13

### Quote context

- **Configurable quote context**: quote variables are now encoded as
  `Name@astranaut_quote@Context` instead of `Name@Module`. The context defaults
  to the current source module and can be set explicitly with the `context`
  option, or disabled with `no_context` to keep original variable names.
- Added a shared quote-variable codec (`encode_quote_variable/2,3`,
  `decode_quote_variable/1`) used by both quote and the clause macro expander,
  with `@` and `%` escaping. The expander no longer compares variable suffixes
  with the macro execution module and now renames named fun binders too.
- `context` and `no_context` are mutually exclusive; passing both returns
  `{conflicting_quote_context_options, Context, no_context}`. `context` must be
  a non-empty atom (the empty atom `''` and any non-atom are rejected with
  `{invalid_quote_context, _}`), `no_context` must be a boolean, and the low
  level `quoted/1,2` reports invalid options instead of raising a badmatch.

### Diagnostics and formatter protocol

- Added `astranaut_lib:format_error/1,2` and `format_default_error/1` as the
  shared compiler-diagnostic adapter. Astranaut formatters now use one strict
  dispatch path: an unmatched formatter clause falls back to a deep character
  list or `io_lib:write/1`, while other formatter failures remain visible.
- Macro diagnostics now have explicit ownership. A macro provider formats its
  returned domain errors and warnings, while framework failures remain owned
  by `astranaut_macro` and struct-transform failures by
  `astranaut_struct_transformer`. Providers without `format_error/1` emit a
  `missing_macro_formatter` warning.
- Unexpected `error`, `throw`, and `exit` exceptions from macro execution are
  contained as `macro_exception` diagnostics and preserve the class, reason,
  stacktrace, MFA, arguments, and source position.

### Macro isolation and OTP maintenance

- Made local-macro support an optional, lazily registered capability. Modules
  without a `-local_macro` declaration no longer load or initialize
  `astranaut_macro_local`, and ordinary imported or exported macros continue to
  work when the local provider is omitted from a build.
- Added GitHub Actions coverage for Erlang/OTP 21 through 29 and aligned the
  local Docker CI matrix with the same supported releases.
- Fixed OTP 27 `syntax_tools` crashes by passing raw abstract-format nodes
  through `astranaut_syntax:revert/1` unchanged, including record fields and
  multi-template comprehensions.

### Compatibility

- **Breaking change**: quote variable encoding changed. Clean rebuild all
  modules that define or use Astranaut macros after upgrading. Mixing old macro
  beams with the new expander is not supported.
- Raised the minimum supported Erlang/OTP release from 19 to 21 and removed
  the pre-21 stacktrace, syntax-schema, reference-data, and CI compatibility
  paths.

## 0.12

### Syntax schema

- Added a generated syntax schema (`astranaut_syntax_schema` backed by the
  `syntax.term` reference data) describing OTP abstract-form node layouts, and
  moved syntax validation, normalization, and uniplate/quote traversal-rule
  derivation onto it. A versioned audit (`scripts/check_syntax_schema.escript`
  with `syn.md`) keeps the reference data aligned with each OTP release.
- Regenerated the syntax schema directly from OTP abstract forms
  (`scripts/generate_syntax_schema.escript`) and strengthened the
  syntax-adapter symmetry checks.
- Simplified the `astranaut_syntax` API surface: removed the form-ordering and
  attribute-subtree compatibility proxies and the explicit pattern/guard/
  expression node markers, and fixed legacy OTP validation.
- Documented subtree traversal and the Luna test execution policy.

## 0.11

### Macro system

- **Unified external and local macro expansion**: attribute macros and
  function-body macros now share one recursive expander backed by dedicated
  registry, source scanner, and local-macro lifecycle components. Macro
  expansion in guard contexts is also fixed.
- **Declaration-scoped local macros**: `-local_macro` freezes its declared
  functions and statically discovered helper closure, together with the macro
  environment and attributes visible at the declaration. Later declarations
  no longer affect an already frozen closure.
- **Explicit local-macro closure control**: added `-local_macro_retain`,
  and `closure_roots` for retaining closure members, including helpers that
  cannot be found statically.
- **Unambiguous macro calls**: removed the `internal_function` option. A direct
  call matching the macro environment is always a macro call; ordinary
  function invocation uses a distinct helper or standard Erlang indirection.
- **Source-ordered macro processing**: the attribute pass now performs
  scan-and-splice in source order and continues scanning generated forms at
  their insertion point. The function-body pass then uses the complete final
  macro environment. Generated `-import_macro`, `-use_macro`,
  `-macro_options`, and `-local_macro` declarations affect only subsequent
  forms.
- **Safer expansion and clearer diagnostics**: added recursive expansion with
  `max_depth`, macro-name and alias conflict detection, duplicate local-macro
  declaration checks, incompatible closure-environment checks, AST-role
  validation for macro results, and isolation of sibling expansion errors.
- **Faster compilation for large macro modules**: attribute environments and
  macro-call analysis are reused instead of repeatedly rescanning forms.
  Benchmarks now cover deep recursion and macro modules closer to production
  size.
- Give each parse-transform invocation a uniquely named local macro module,
  removing fixed-name lifecycle locking and allowing concurrent compilation of
  the same source module without sharing generated macro code.

### Traversal, quoting, and AST

- **AST role validation and normalization**: `astranaut_syntax` adds
  `validate_node/2,3`, `normalize/2,3`, `child_specs/3`, and `node_roles/1`
  for validating or normalizing syntax trees from their parent slot, OTP
  version, and record definitions.
- **Validation-aware traversal results**: traverse validators detect invalid
  AST transformations and child nodes. `fail_on_error/1` and
  `catch_on_error/2` stop later steps after traversal errors, replacing the
  old `listen_has_error` flow.
- **Revised Uniplate API**: removed `astranaut_uniplate:map/4`, `reduce/5`,
  `mapfold/5`, static uniplate, and `keep` semantics. Use
  `astranaut:map/3`, `reduce/4`, and `mapfold/4`; new helpers include
  `search/3`, `map_with_state/4`, and `smap_with_state/4`.
- **Separate list traversal from module-form processing**:
  `astranaut:map_m/3` preserves ordinary list order, while the new
  `map_m_forms/3` handles generated-form insertion, function/spec merging, and
  canonical reordering for module forms.
- **Monad API cleanup**: the `maybe` monad/type is renamed to `monad_maybe` to
  avoid the OTP 25 `maybe` keyword. Writer/listener APIs consistently use the
  `writer_updated` and `listen_updated` names.
- **Stricter quote validation**: quote bindings now reject invalid strings,
  atom names, and values with consistent errors; `quote_code` validates code
  and option placement; AST-valued `pos` options are normalized correctly.
- **Correct traversal contracts and diagnostics**: fixed `mapfold` and related
  public type contracts, made explicit root validators take precedence, and
  preserved the correct node position and formatter when validation reports
  diagnostics.
- **More accurate `disable_tco` transformation**: tail calls inside `case`,
  `if`, `receive`, `try`, blocks, boolean operators, and `maybe` expressions
  are handled recursively. Direct recursion, named-fun recursion, and mutually
  recursive local functions retain tail-call optimization.

### Structs and compile-time tooling

- **Refactored the Struct system** into the `astranaut_struct` macro API,
  `astranaut_struct_record` data layer, and
  `astranaut_struct_transformer` parse transform. It now uses the new macro
  pipeline and adds `from_other_record/4`.
- **Added `astranaut_compile_meta_transformer`** to collect post-transform
  forms, compiler errors, and warnings for compile-time metaprogramming and
  diagnostics.
- **Consolidated the shared `astranaut_lib` API**: option-validation types are
  public, module locking and safe binary reload helpers are centralized, and
  exports used only by obsolete converters/wrappers are removed.
- **Repaired public type contracts** across traversal, forms, quote, macro,
  return, rebinding, and struct modules, including exporting the public
  `astranaut:walk_return/2` type.

### OTP compatibility and development

- Added AST support for `map_generator`, `strict_generator`,
  `strict_binary_generator`, `strict_map_generator`, `maybe_expr`, and
  `maybe_match_expr`, primarily for rebinding, with Erlang/OTP 19–29
  compatibility.
- Replaced `maps:merge_with/3` with APIs available since OTP 19 and accepted
  the extra wrapping used by OTP 19–23 compile-attribute analysis, preserving
  valid parse-transform source positions.
- Declared `syntax_tools` as an OTP application dependency, removed production
  use of `eunit_lib`, and moved EUnit-dependent test helpers out of the
  production compilation order.
- Removed obsolete compatibility headers and fixed the local container CI
  scripts so suite/case selection and test failure statuses propagate
  correctly.
- Split the macro Common Test coverage into focused core, uniform-expansion,
  pass, diagnostic, local-state, and scan-and-splice suites.
- Added a Chinese README, OTP 19–29 abstract-form references and fetch scripts,
  local container CI, coverage tooling, and macro compilation benchmarks.
- Added support for OTP 29 native record syntax and fixed syntax projection
  and OTP 29 validation.
- Replaced the checked-in local Docker CI scripts with project configuration
  for the host-side `rebar3_docker_ci` plugin.
