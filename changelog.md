# Changelog

[中文](changelog.zh.md)

## 0.11.1

### Development tooling

- Replaced the checked-in local Docker CI scripts with project configuration
  for the host-side `rebar3_docker_ci` plugin.

## 0.11.0

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
