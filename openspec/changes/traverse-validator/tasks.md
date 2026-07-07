# Tasks

## Specification

- [x] Define validator data forms for role validators and slot validators.
- [x] Document the distinction between `validate_local/2` and `validate_recursive/2`.
- [x] Document `validate => boolean` traversal behavior and default-off semantics.
- [x] Document macro error attribution for validation failures.

## Implementation

- [x] Extend `child_spec` to include `validator`.
- [x] Propagate `validator`, `parent_type`, and `parent_slot` through child attrs.
- [x] Keep validator propagation active regardless of `validate => boolean`.
- [x] Add `astranaut_syntax:validate_local/2`.
- [x] Add `astranaut_syntax:validate_recursive/2`.
- [x] Keep `astranaut_syntax:validate/2,3` as compatibility recursive role-validation entry points.
- [x] Implement slot validators for match, clause, function, generator, binary generator, comprehensions, attribute, type, record, map, and binary node families.
- [x] Add `validate => boolean` to traversal options with default `false`.
- [x] Distinguish direct `node_changed` from `children_changed` inside traversal validation logic.
- [x] Trigger automatic validation only for direct walker-return changes.
- [x] Avoid automatic validation of ancestor nodes rebuilt only because children changed.
- [x] Update macro expansion to use propagated validators for macro return validation.
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
- [x] Existing Common Test suites continue to pass.
