# Unique Local Macro Module Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Give every parse-transform invocation its own generated local-macro module so concurrent compilation needs no shared-name coordination.

**Architecture:** Allocate the unique module at the parse-transform boundary and thread it explicitly through scanner, registry, and local-macro state. Keep one name across cumulative generations, while deleting fixed-name locking, ownership, collision, and cleanup behavior.

**Tech Stack:** Erlang/OTP, rebar3, Common Test, EUnit assertions

---

### Task 1: Specify unique allocation and concurrent compilation

**Files:**
- Modify: `test/astranaut_macro_local_SUITE.erl`
- Modify: `test/astranaut_macro_pass_SUITE.erl`

**Step 1: Write the failing tests**

Add a local-state test asserting two states initialized for the same source
module hold different generated module names. Replace lifecycle serialization
tests with a test that starts two workers compiling identical module forms and
asserts that both return transformed forms.

**Step 2: Run tests to verify they fail**

Run: `rebar3 ct --suite test/astranaut_macro_local_SUITE.erl,test/astranaut_macro_pass_SUITE.erl`

Expected: FAIL because state initialization does not accept or expose a unique
module and concurrent parse transforms still share the fixed name.

### Task 2: Thread the invocation-local module name

**Files:**
- Modify: `src/astranaut_macro.erl`
- Modify: `src/astranaut_macro_scan.erl`
- Modify: `src/astranaut_macro_registry.erl`
- Modify: `src/astranaut_macro_local.erl`

**Step 1: Add allocation and state**

Add a unique-name allocator using `erlang:unique_integer([positive])`. Allocate
once in `parse_transform/2`, pass the result into scanner initialization, and
store it in both registry and local-macro state.

**Step 2: Replace fixed-name derivation**

Use the stored name when constructing local macro descriptors, choosing a local
formatter, and rewriting generated module forms.

**Step 3: Run focused tests**

Run: `rebar3 ct --suite test/astranaut_macro_local_SUITE.erl,test/astranaut_macro_pass_SUITE.erl`

Expected: PASS.

### Task 3: Delete obsolete lifecycle coordination

**Files:**
- Modify: `src/astranaut_macro.erl`
- Modify: `src/astranaut_macro_local.erl`
- Modify: `test/astranaut_macro_local_SUITE.erl`
- Modify: `test/astranaut_macro_pass_SUITE.erl`

**Step 1: Remove obsolete code**

Delete locking, owner attributes, availability checks, lifecycle cleanup,
diagnostic freezing, and fixed-name error mapping. Keep the ordinary sequential
reload path for cumulative generations inside one invocation.

**Step 2: Run focused tests**

Run: `rebar3 ct --suite test/astranaut_macro_local_SUITE.erl,test/astranaut_macro_pass_SUITE.erl`

Expected: PASS with no fixed-name lifecycle cases remaining.

### Task 4: Update user documentation and verify

**Files:**
- Modify: `README.md`
- Modify: `README.zh.md`
- Modify: `openspec/changes/local-macro/design.md`

**Step 1: Update documentation**

Describe invocation-unique generated modules and remove obsolete conflict and
in-use diagnostics and lifecycle guarantees.

**Step 2: Run full verification**

Run: `rebar3 compile`

Expected: exit 0.

Run: `rebar3 ct`

Expected: all suites pass with zero failures.

**Step 3: Review the diff**

Run: `git diff --check && git status --short`

Expected: no whitespace errors; only planned source, test, and documentation
files are changed.
