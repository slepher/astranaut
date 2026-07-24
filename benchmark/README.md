# Macro compile benchmark

`macro_2000.erl` is a generated, approximately 2,000-line compile-time macro
workload. It has 40 standard functions of approximately 20 lines and 20 deep
functions of approximately 50 lines. The deep functions contain sixteen nested
maps, and the benchmark verifies that every function has AST depth greater
than 10.

- 42 functions with one external macro call
- 7 functions with two nested external macro calls
- 6 functions whose macro result contains another macro call
- 5 functions with one local macro call
- 1 local macro definition

Every function mixes its macro call with ordinary arithmetic, list processing,
branching, and map construction.

Regenerate the workload:

```shell
escript benchmark/generate_macro_2000.escript
```

Compile Astranaut and run the benchmark (15 measured iterations by default):

```shell
rebar3 compile
escript benchmark/macro_compile_bench.escript
```

Pass a positive iteration count to change the sample size:

```shell
escript benchmark/macro_compile_bench.escript 30
```

The runner compiles and loads the external macro provider before timing. It
reports both the isolated `astranaut_macro` parse-transform time and the full
`compile:file/2` time. Each metric has three unreported warm-up runs.

## Desensitized Erlando do workload

`real_benchmark.erl` is a compile-only extraction of the 13 Erlando-style
`do([error_m || ...])` expressions in the historical `order_handler.erl`
workload. Business-specific records, constants, module names, metadata, and
literal values have been replaced with a neutral benchmark domain. A generated
neutral workload restores the source to approximately 5,000 lines and keeps
the ordinary-function scale close to the original module.

Its complete local include dependency is `include/realbench_min.hrl`. The
header contains only the macro registration, nine constants, and ten record
definitions referenced by the extracted workload. `do_macro.erl` is the
minimal provider adapted from `../erlando/src/do_macro.erl`; the monad
implementation itself remains an external runtime concern and is not copied.

Regenerate the neutral workload deterministically:

```shell
escript benchmark/generate_real_benchmark.escript
```
