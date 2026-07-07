# Tasks

## Specification

- [ ] Define validator data forms for role validators and slot validators.
- [ ] Document the distinction between `validate_local/2` and `validate_recursive/2`.
- [ ] Document `validate => boolean` traversal behavior and default-off semantics.
- [ ] Document macro error attribution for validation failures.

## Implementation

- [ ] Extend `child_spec` to include `validator`.
- [ ] Propagate `validator`, `parent_type`, and `parent_slot` through child attrs.
- [ ] Keep validator propagation active regardless of `validate => boolean`.
- [ ] Add `astranaut_syntax:validate_local/2`.
- [ ] Add `astranaut_syntax:validate_recursive/2`.
- [ ] Keep `astranaut_syntax:validate/2,3` as compatibility recursive role-validation entry points.
- [ ] Implement slot validators for match, clause, function, generator, binary generator, comprehensions, attribute, type, record, map, and binary node families.
- [ ] Add `validate => boolean` to traversal options with default `false`.
- [ ] Distinguish direct `node_changed` from `children_changed` inside traversal validation logic.
- [ ] Trigger automatic validation only for direct walker-return changes.
- [ ] Avoid automatic validation of ancestor nodes rebuilt only because children changed.
- [ ] Update macro expansion to use propagated validators for macro return validation.
- [ ] Wrap macro-triggered validation failures as `{invalid_macro_return, Detail}`.

## Tests

- [ ] Validator metadata is propagated when `validate` is omitted.
- [ ] Validator metadata is propagated when `validate => false`.
- [ ] `validate_local/2` checks current node and direct children only.
- [ ] `validate_recursive/2` checks the full returned node subtree.
- [ ] Parent slot validation distinguishes `generator` pattern and body slots.
- [ ] Parent slot validation distinguishes `binary_generator` pattern and body slots.
- [ ] Direct walker node replacement is validated when `validate => true`.
- [ ] Child-only changes do not trigger automatic ancestor validation.
- [ ] `pre`, `post`, `all`, `subtree`, and `none` validation behavior is covered.
- [ ] Macro invalid return from a wrong slot reports `{invalid_macro_return, Detail}`.
- [ ] Nested macro invalid return preserves origin/current macro detail.
- [ ] Existing Common Test suites continue to pass.
