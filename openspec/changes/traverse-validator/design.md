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

## Validation APIs

Use separate functions for separate validation scopes:

```erlang
astranaut_syntax:validate_local(NodeOrNodes, Validator) -> ok | {error, map()}.
astranaut_syntax:validate_recursive(NodeOrNodes, Validator) -> ok | {error, map()}.
```

Existing compatibility entry points may remain:

```erlang
astranaut_syntax:validate(NodeOrNodes, ExpectedRole).
astranaut_syntax:validate(NodeOrNodes, ExpectedRole, Opts).
```

Those compatibility functions should behave like recursive validation with a role validator.

## Local Validation

`validate_local/2` is a one-step descend validation.

It must:

1. Check that the current node or node list satisfies the provided validator.
2. Read the current node subtrees.
3. Derive direct child specs for the current node.
4. Check that each direct child satisfies its own child validator.
5. Stop there.

It must not validate grandchildren.

This is useful for parent insertion checks and for callers that want to control recursion themselves.

## Recursive Validation

`validate_recursive/2` repeats local validation through the entire returned node subtree:

```text
validate_local(Node, Validator)
for each direct child:
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
