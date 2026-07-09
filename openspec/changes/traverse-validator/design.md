# Traverse Validator Design

## Current Behavior

`astranaut_syntax` has a recursive role-based validator built around:

```text
subtrees/1
child_specs/3
node_roles/1
validate_nodes/5
```

Traversal currently has an `updated` signal, but it does not distinguish between:

- a walker directly replacing the current node
- a child changing and causing the current node to be rebuilt

This distinction matters for validation. Only direct walker changes should trigger automatic changed-node validation.

## Validator Propagation

Validator metadata must be propagated independently of automatic validation.

Each child spec should carry both broad traversal role information and a slot-specific validator:

```erlang
#{slot => Slot,
  role => Role,
  validator => Validator,
  nodes => Nodes,
  subtrees => Subtrees,
  annotate => Annotate,
  attr => Attr#{node => Role,
                validator => Validator,
                parent_type => ParentType,
                parent_slot => Slot}}
```

`node => Role` remains useful for traversal behavior. `validator => Validator` is the structural contract for the current child position.

Traversal attrs must stay local to the current AST position. They should carry node identity and slot metadata only, such as `node`, `validator`, `parent_type`, `parent_slot`, and structure-specific role hints like `map_field_role` or `binary_field_role`.

Module-wide validation context must not be propagated as attrs. In particular, record definition forms used by guard validation belong to the validation environment, not to node attrs.

## Validator Shape

Validators should be data, not anonymous functions, so errors are inspectable and testable.

Initial validator forms:

```erlang
{role, expression}
{role, pattern}
{role, guard}
{role, form}
{role, type}
{role, clause}
{role, name}
{slot, ParentType, Slot, Role}
```

The slot form allows parent-specific rules while carrying the broad role used for current-node checks:

```erlang
{slot, binary_generator, pattern, pattern}
{slot, binary_generator, body, expression}
{slot, generator, pattern, pattern}
{slot, generator, body, expression}
{slot, match_expr, left, pattern}
{slot, match_expr, right, expression}
{slot, clause, guards, guard}
{slot, clause, body, expression}
```

The validator dispatcher may map a slot validator to a role plus direct child slot rules, but callers should not need to know that mapping.

Some child positions require structural identity checks in addition to broad role checks. For example:

```erlang
{slot, map_expr, fields, map_field}
{slot, binary, elements, binary_field}
{slot, map_field_assoc, map_field_assoc_key, expression}
{slot, map_field_assoc, map_field_assoc_value, expression}
{slot, map_field_exact, map_field_exact_key, expression}
{slot, map_field_exact, map_field_exact_value, Role}
```

This prevents structural nodes from being used in ordinary expression slots. For example, a `bin_element` is a valid binary field node but must not be accepted as an ordinary list or tuple element. Likewise, a `map_field_exact` node is valid as a map field but not as the value expression inside another map field.

## Validation APIs

Use separate functions for separate validation scopes:

```erlang
astranaut_syntax:validate_local(NodeOrNodes, Validator) -> ok | {error, map()}.
astranaut_syntax:validate_local(NodeOrNodes, Validator, Opts) -> ok | {error, map()}.
astranaut_syntax:validate_recursive(NodeOrNodes, Validator) -> ok | {error, map()}.
astranaut_syntax:validate_recursive(NodeOrNodes, Validator, Opts) -> ok | {error, map()}.
```

Existing compatibility entry points may remain:

```erlang
astranaut_syntax:validate(NodeOrNodes, ExpectedRole).
astranaut_syntax:validate(NodeOrNodes, ExpectedRole, Opts).
```

Those compatibility functions should behave like recursive validation with a role validator.

`Opts` is the validation environment. It may include:

```erlang
#{attr => Attr,
  forms => RecordForms}
```

`RecordForms` must already be filtered to only record definitions:

```erlang
[{attribute, Anno, record, {Name, Fields}}]
```

The syntax validator should pass those forms to `erl_lint:is_guard_test/2` for guard checks. This is valid because `erl_lint:is_guard_test/2` consumes its `Forms` argument by extracting record attributes only; it does not require a complete module form list.

Attrs and validation environment have different responsibilities:

- `Attr` is local traversal metadata used to derive child validators.
- `forms` is external validation context used by guard validation.
- `forms` must not be inserted into traversal attrs or propagated by `subtrees_pge/3`.

## Local Validation

`validate_local/2` is current-node validation.

It must:

1. Check that the current node or node list satisfies the provided validator.
2. Read the current node subtrees through `node_info/1`.
3. Apply role and slot-specific structural constraints for the supplied validator.
4. Stop there.

It must not validate child nodes recursively.

This is useful for parent insertion checks and for callers that want to control recursion themselves.

Every current-node check must also call `subtrees(Node)` through the node information path. This verifies that the current AST node itself has a legal abstract-format shape before role or slot checks succeed. The call is a shape check for the current node only; it is not a request to recursively validate children.

Some syntax nodes need slot-specific checks that cannot be derived from `subtrees/1` alone. For example, `erl_syntax:subtrees/1` normalizes try/catch handler clauses so `catch P -> ...` and `catch throw:P:_ -> ...` have the same subtree view. Handler legality across OTP versions must therefore be checked from the current handler clause's raw/reverted structure while validating the `{slot, try_expr, handlers, clause}` role.

Malformed nodes with a valid-looking tag, such as an incomplete `map_field_exact` or `bin_element`, should fail as `invalid_node`.

## Recursive Validation

`validate_recursive/2` repeats local validation through the entire returned node subtree:

```text
validate_local(Node, Validator)
for each child spec derived from Node:
  validate_recursive(Child, ChildValidator)
```

This is appropriate when a caller generates a whole new AST node and wants to ensure the generated subtree is structurally valid.

The scope is still the returned node subtree only. It does not extend to the containing function, form, or module unless that whole container is the returned node.

## Traversal Validation Switch

Traversal gets one option:

```erlang
#{validate => true | false}
```

Default is `false`.

When disabled:

- Validator metadata is still propagated.
- No automatic validation is performed.
- Callers may perform validation manually.

When enabled:

- Validation runs only for nodes directly changed by the walker return.
- Validation uses the current step's propagated validator.
- Automatic validation should use recursive validation for the returned changed node.
- Parent insertion should use local validation against the propagated parent slot validator.

## Change Detection

Traversal should distinguish direct node changes from child changes.

Existing mechanisms already expose the two points:

```erlang
updated_node_apply/3
```

detects whether the walker directly returned a different current node. This is `node_changed`.

```erlang
listen_updated(map_m_subtreess(...))
```

detects whether any child changed. This is `children_changed`.

Automatic validation should be attached to `node_changed`, not to ancestor rebuilds caused by `children_changed`.

The external writer `updated` boolean can remain unchanged for compatibility; internally the validator logic must not treat a rebuilt ancestor as a directly changed node.

## Traverse Modes

`pre`:

- Validate only if the pre walker directly changes the current node.
- The returned node may still be traversed afterward.

`post`:

- Children may already have changed and rebuilt the current node.
- Validate only if the post walker directly changes that current node.

`all`:

- Treat `pre` and `post` as separate steps.
- The existing `step` attr can identify the active step.

`subtree`:

- Validate only direct returns from the subtree walker.
- Do not validate every ancestor rebuilt from child changes.

`none`:

- Validate only the direct current node return, if validation is enabled.

## Macro Integration

Macro expansion should not rely on generic traversal error formatting.

Macro return validation should use the propagated validator and then wrap failures through the existing macro error path:

```erlang
{invalid_macro_return, macro_return_detail(Macro, Opts, Detail)}
```

This applies to both:

- the returned node being structurally invalid
- the returned node not satisfying the parent slot validator

Nested macro expansion should preserve the existing origin/current macro detail behavior.

Macro validation must pass record definitions through the validation environment, not traversal attrs. The macro transformer should filter module forms to record definitions once, keep that list in macro depth options, and call:

```erlang
astranaut_syntax:validate_recursive(Return, Validator,
                                    #{attr => Attr,
                                      forms => RecordForms})
```

This allows guard macros involving record construction or `is_record/2` checks to be validated by `erl_lint:is_guard_test/2` with the same record knowledge as the containing module.

## Error Details

Validation errors should include enough context for macro and traversal callers:

```erlang
#{reason => invalid_node | invalid_role | invalid_slot,
  validator => Validator,
  expected_role => Role,
  actual_type => Type,
  parent_type => ParentType,
  parent_slot => Slot,
  slot => Slot,
  path => Path}
```

Not every field is required for every error, but slot validator failures should include the validator and parent slot context.
