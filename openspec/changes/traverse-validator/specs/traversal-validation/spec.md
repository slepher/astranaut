# Traversal Validation Spec Delta

## ADDED Requirements

### Requirement: Validator Metadata Is Propagated

Traversal SHALL propagate slot-specific validator metadata to child nodes.

Validator metadata SHALL be propagated even when automatic traversal validation is disabled.

#### Scenario: Validator metadata exists without automatic validation

- **GIVEN** traversal runs without `validate => true`
- **WHEN** traversal descends into a child node
- **THEN** the child attr contains validator metadata for its parent slot

#### Scenario: Validator metadata includes parent slot context

- **GIVEN** traversal descends from a parent node into one of its child slots
- **WHEN** the child walker receives attr
- **THEN** the attr identifies the parent type, parent slot, and slot validator

### Requirement: Automatic Validation Is Opt In

Traversal SHALL NOT automatically validate transformed nodes unless `validate => true` is set.

#### Scenario: Validation is disabled by default

- **GIVEN** traversal options do not include `validate`
- **WHEN** a walker returns a changed node
- **THEN** traversal does not automatically validate the changed node

#### Scenario: Validation can be enabled

- **GIVEN** traversal options include `validate => true`
- **WHEN** a walker returns a changed node
- **THEN** traversal validates that changed node according to the active validator

#### Scenario: Boolean true means output validation

- **GIVEN** traversal options include `validate => true`
- **WHEN** traversal normalizes its options
- **THEN** validation behaves as `validate => output`

### Requirement: Input And Output Validation Are Selectable

Traversal SHALL accept `validate => false | true | input | output | both`.

When validation includes `input`, traversal SHALL locally validate each node during the pre stage before applying the pre walker, and SHALL NOT repeat input validation during the post stage.

When validation includes `output`, traversal SHALL preserve changed-output validation: pre-walker output validation SHALL be local and post-walker output validation SHALL be recursive.

#### Scenario: Input validation shares traversal recursion

- **GIVEN** traversal runs with `validate => input`
- **WHEN** traversal visits an input tree
- **THEN** each visited node is locally validated during its pre stage
- **AND** no separate recursive validation pass is performed

#### Scenario: Both enables input and output validation

- **GIVEN** traversal runs with `validate => both`
- **WHEN** traversal visits and directly changes nodes
- **THEN** input validation and the phase-appropriate output validation both run

### Requirement: Validation Options Carry Environment And Failure Policy

Traversal SHALL accept `validate_opts => #{record_defs => RecordDefinitions, fail => raise | collect}`.

The `fail` policy SHALL default to `raise`.

With `fail => collect`, a validation failure SHALL enter the traversal failure channel and be recovered by `catch_on_error`; the unchanged current node SHALL be returned, and traversal SHALL not descend into that invalid node. Validation state SHALL NOT be represented by a tagged node return value.

#### Scenario: Collect preserves the input node

- **GIVEN** traversal runs with input validation and `fail => collect`
- **WHEN** an input node fails local validation
- **THEN** traversal records `{invalid_transform_normalization, Detail}`
- **AND** returns the unchanged input node
- **AND** does not attempt to traverse its invalid subtree

#### Scenario: Record definitions reach guard validation

- **GIVEN** `validate_opts` contains `record_defs`
- **WHEN** a guard node is validated
- **THEN** those record definitions are available to guard validation

### Requirement: Validation Runs Only For Direct Node Changes

Traversal validation SHALL run only for nodes directly changed by a walker return.

Traversal validation SHALL NOT run for an ancestor merely because a child changed and the ancestor was rebuilt.

#### Scenario: Direct node replacement is validated

- **GIVEN** traversal runs with `validate => true`
- **WHEN** the walker returns a different current node
- **THEN** traversal validates the returned node

#### Scenario: Child-only changes do not cascade validation

- **GIVEN** traversal runs with `validate => true`
- **AND** a child node changes
- **WHEN** the parent is rebuilt from changed child subtrees
- **THEN** traversal does not automatically validate the rebuilt parent as a direct change

### Requirement: Current-Node Validation Checks Current Node

`validate_node/2` SHALL validate the current node against the supplied validator.

`validate_node/2` SHALL read current-node shape through `node_info/subtrees` and SHALL apply role and slot-specific structural constraints for the supplied validator.

`validate_node/2` SHALL NOT recursively validate child nodes.

#### Scenario: Local validation checks slot-specific current-node constraints

- **GIVEN** a try handler clause with OTP-version-specific catch syntax
- **WHEN** `validate_node/2` is called
- **THEN** the handler clause is checked against the supplied handler slot validator

#### Scenario: Local validation does not recurse into children

- **GIVEN** a current node has child nodes
- **WHEN** `validate_node/2` is called on the parent
- **THEN** child nodes are not validated by that call

### Requirement: Normalization Repeats Current-Node Validation

`normalize/2` SHALL validate the current node, recursively normalize all child
nodes with their derived validators, rebuild the node, and return the normalized
abstract-format tree.

#### Scenario: Recursive validation checks a generated subtree

- **GIVEN** a caller generates a new AST node containing nested child nodes
- **WHEN** `normalize/2` is called on the generated node
- **THEN** the complete generated node subtree is structurally validated

### Requirement: Slot Validators Are More Specific Than Roles

Parent insertion validation SHALL use slot-specific validators rather than broad roles only.

#### Scenario: Binary generator pattern and body are distinct

- **GIVEN** a `binary_generator` node
- **WHEN** validator metadata is propagated to its children
- **THEN** the pattern slot and body slot receive distinct validators

#### Scenario: Generator pattern and body are distinct

- **GIVEN** a `generator` node
- **WHEN** validator metadata is propagated to its children
- **THEN** the pattern slot and body slot receive distinct validators

### Requirement: Macro Validation Preserves Macro Error Attribution

Macro-triggered validation failures SHALL be reported as `{invalid_macro_return, Detail}`.

Macro-triggered validation failures SHALL include the macro call detail already used for invalid macro returns.

#### Scenario: Macro return fails slot validation

- **GIVEN** a macro expands in a parent slot with a specific validator
- **AND** the macro returns a node that does not satisfy that validator
- **WHEN** macro expansion validates the return
- **THEN** compilation reports `{invalid_macro_return, Detail}`

#### Scenario: Nested macro return preserves origin and current macro

- **GIVEN** a macro expands into another macro call
- **AND** the generated macro returns an invalid node
- **WHEN** macro expansion validates the generated return
- **THEN** the error detail identifies both the origin macro and the current macro

### Requirement: Macro Failures Do Not Stop Sibling Analysis

Every monadic macro-call failure SHALL temporarily recover the original macro
call so traversal can continue analysing sibling nodes. The diagnostic SHALL be
retained, and the outer traversal MAY delete the failed node after sibling
analysis completes.

#### Scenario: Generated sibling macros fail differently

- **GIVEN** a macro expands into sibling macro calls
- **AND** those siblings respectively throw, return an explicit error, and return an invalid AST
- **WHEN** the generated return tree is traversed
- **THEN** all three diagnostics are collected
- **AND** no sibling failure prevents analysis of the remaining siblings
