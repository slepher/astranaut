# Tasks

## Specification

- [x] Define validator data forms for role validators and slot validators.
- [x] Document the distinction between `validate_local/2` and `validate_recursive/2`.
- [x] Document `validate => false | true | input | output | both` traversal behavior and default-off semantics.
- [x] Document macro error attribution for validation failures.
- [x] Document validation environment separation from traversal attrs.
- [x] Document guard validation with filtered record-definition forms.
- [x] Document structural identity checks for map fields and binary fields.
- [x] Document `subtrees(Child)` current-node shape validation.

## Implementation

- [x] Extend `child_spec` to include `validator`.
- [x] Propagate `validator`, `parent_type`, and `parent_slot` through child attrs.
- [x] Keep validator propagation active regardless of the selected `validate` mode.
- [x] Add `astranaut_syntax:validate_local/2`.
- [x] Add `astranaut_syntax:validate_recursive/2`.
- [x] Keep `astranaut_syntax:validate/2,3` as compatibility recursive role-validation entry points.
- [x] Implement slot validators for match, clause, function, generator, binary generator, comprehensions, attribute, type, record, map, and binary node families.
- [x] Add `validate => false | true | input | output | both` to traversal options with default `false`.
- [x] Normalize `validate => true` to `validate => output`.
- [x] Validate input locally during the pre stage only when input validation is enabled.
- [x] Distinguish direct `node_changed` from `children_changed` inside traversal validation logic.
- [x] Trigger automatic validation only for direct walker-return changes.
- [x] Avoid automatic validation of ancestor nodes rebuilt only because children changed.
- [x] Update macro expansion to use propagated validators for macro return validation.
- [x] Fuse macro return input validation with quoted-variable and zero-position updates.
- [x] Add `validate_opts` with `record_defs` and `fail => raise | collect`.
- [x] Route collected validation failures through monadic fail/catch without tagged node return values.
- [x] Preserve macro-specific validation errors by rewriting collected traversal errors.
- [x] Pass filtered record-definition forms through validation opts for guard checks.
- [x] Keep record-definition forms out of traversal attrs.
- [x] Use `erl_lint:is_guard_test/2` for guard validation.
- [x] Validate current-node abstract-format shape with `subtrees/1`.
- [x] Wrap macro-triggered validation failures as `{invalid_macro_return, Detail}`.

## Tests

- [x] Validator metadata is propagated when `validate` is omitted.
- [x] Validator metadata is propagated when `validate => false`.
- [x] `validate_local/2` checks current node and direct children only.
- [x] `validate_recursive/2` checks the full returned node subtree.
- [x] Parent slot validation distinguishes `generator` pattern and body slots.
- [x] Parent slot validation distinguishes `binary_generator` pattern and body slots.
- [x] Direct walker node replacement is validated when `validate => true`.
- [x] Child-only changes do not trigger automatic ancestor validation.
- [x] `pre`, `post`, `all`, `subtree`, and `none` validation behavior is covered.
- [x] Macro invalid return from a wrong slot reports `{invalid_macro_return, Detail}`.
- [x] Nested macro invalid return preserves origin/current macro detail.
- [x] Guard validation sees record definitions through validation opts.
- [x] Structural identity errors reject binary fields in ordinary element slots.
- [x] Type/tag-correct but malformed structural nodes fail as `invalid_node`.
- [x] Existing Common Test suites continue to pass.
