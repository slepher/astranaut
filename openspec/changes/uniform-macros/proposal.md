# Uniform Macros

## Summary

Change macro expansion from import-order staged passes to a unified macro environment, while preserving the existing `outer` then `inner` expansion semantics inside a single expansion pass.

## Motivation

External macros are currently expanded one imported macro module at a time. If a later imported macro expands into a call to an earlier imported macro, the generated call is left unexpanded because the earlier module pass has already completed.

This is incorrect behavior. Macro expansion should not depend on `-import_macro` order when all involved macros are already known.

## Scope

This change covers:

- Building a uniform external macro map from all imported macro modules.
- Running external attribute macro expansion against the uniform external macro map before local macro discovery.
- Expanding local macro source snapshots with external macros only.
- Building a final uniform macro map that combines external and local macros.
- Running final form expansion with the final uniform macro map.
- Skipping local macro functions and related functions during final expansion.

This change does not cover:

- Removing or changing `{order, outer}`.
- Changing the existing `outer` then `inner` traversal behavior within one macro environment.
- Allowing local macro definitions to expand other local macros.
- Dynamically rediscovering new external macro modules introduced after local macro loading.

## Expected Behavior

Given:

```erlang
-import_macro(macro_a).
-import_macro(macro_b).
```

If `macro_b` expands into `macro_a:foo(...)`, the generated `macro_a:foo(...)` call must be expanded by the same uniform macro environment.

Existing behavior where `order = outer` runs during the pre traversal phase and `order = inner` runs during the post traversal phase must be preserved.

