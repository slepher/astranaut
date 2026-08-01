# Unique Local Macro Module Design

## Goal

Replace cross-compilation coordination around the fixed
`<source>__local_macro` name with one unique local-macro module name per parse
transform invocation.

## Design

`astranaut_macro:parse_transform/2` allocates a module atom by appending a
VM-unique integer to the source module name. The name remains stable for every
cumulative local-macro generation produced by that invocation.

The name is explicit compilation context. `astranaut_macro_scan` passes it to
the registry and local-macro state at initialization. The registry uses it for
local macro call descriptors and a local `format_error/1` formatter. The
generation compiler uses the same stored name when rewriting the selected
forms' module attribute.

Compilation VMs are treated as disposable. Generated module atoms and loaded
modules therefore do not need lifecycle cleanup before the VM exits.

## Removed Coordination

Unique names eliminate cross-compilation sharing, so remove:

- module-wide generation locks;
- generated-module ownership attributes and name-conflict checks;
- parse-transform cleanup and diagnostic freezing;
- `local_macro_module_name_conflict` and `local_macro_module_in_use` errors.

Within one parse transform, cumulative generations remain sequential. Existing
reload support is sufficient for replacing that invocation's generated module.

## Testing

- Verify two allocations for the same source module are distinct and retain a
  recognizable source-module prefix.
- Compile the same source-module forms concurrently and require both results to
  succeed.
- Keep coverage that later cumulative generations replace earlier ones within
  one parse transform.
- Remove tests for fixed-name collision, global serialization, cleanup, and
  refusal to replace shared old code because those behaviors no longer exist.

## Documentation

Remove public diagnostics that describe fixed generated-module conflicts and
old-code contention. Describe generated local macro modules as invocation-local
implementation details.
