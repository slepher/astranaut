# Vars collection benchmark — pre-optimization, 2026-11-05

Command: `rebar3 compile` then `escript benchmark/vars_collect_bench.escript 15`.
These historical measurements used the pre-optimization implementation and runner.
The runner now tests initialized gates against an eager reference; see
[post-optimization results](vars-collect-optimized-results.md).
See [methodology](README.md#variable-collection-overhead) and
[runner](vars_collect_bench.escript). All requested-result equivalence assertions passed.

For `collect => []`, median microseconds:

| Shape | Current | Skip exported | Skip binding counts | Skip both |
| --- | ---: | ---: | ---: | ---: |
| Expression body | 20,372 | 20,448 | 19,221 | 19,552 |
| Function clause | 20,676 | 20,973 | 20,130 | 19,964 |
| Branch join | 25,804 | 25,338 | 24,564 | 24,485 |

Exported construction is a small portion of total work: skipping it saves about
0.041% of reductions for the body, 0.049% for the branch join, and effectively
none for the clause with a restored empty environment. Its wall-time differences
are too small and inconsistent to justify claiming a reliable speedup.

Skipping unrequested binding-site count updates reduced median time by 2.64–5.65%
in these fixtures, with reductions down about 0.34–0.48%. This is a more promising
optimization, but not a guarantee for real rebinding workloads. The counterfactual
checks collect at each use; it does not precompute a feature flag at initialization.
The full rebinding naming callback is not included. No production optimization
was applied as part of this benchmark.

Each variant that still requests a metric retains its computation. Differences
in those control rows indicate measurement noise/extra dispatch overhead, so do
not interpret every negative percentage as an optimization effect.

## Raw output

```text
OTP 29 aarch64-apple-darwin25.6.0; schedulers=1 samples=15 warmups=3; medians, fresh process/sample

body (200 names, 2000 reads)
variant collect median_us median_reductions time_delta_pct
current [] 20372 5205198 0.00
current [exported] 20234 5205214 0.00
current [bindings] 20579 5207338 0.00
current all 31481 6076595 0.00
skip_exported [] 20448 5203042 0.37
skip_exported [exported] 20502 5205270 1.32
skip_exported [bindings] 20197 5205205 -1.86
skip_exported all 30501 6073739 -3.11
skip_counts [] 19221 5183236 -5.65
skip_counts [exported] 19496 5184429 -3.65
skip_counts [bindings] 21039 5213805 2.24
skip_counts all 30591 6084163 -2.83
skip_both [] 19552 5180968 -4.03
skip_both [exported] 19753 5184433 -2.38
skip_both [bindings] 20328 5211594 -1.22
skip_both all 30435 6083494 -3.32

clause (200 names, 2000 reads)
variant collect median_us median_reductions time_delta_pct
current [] 20676 5276561 0.00
current [exported] 20843 5276576 0.00
current [bindings] 21016 5278705 0.00
current all 31712 6149340 0.00
skip_exported [] 20973 5276562 1.44
skip_exported [exported] 20924 5276568 0.39
skip_exported [bindings] 20861 5278749 -0.74
skip_exported all 31033 6149336 -2.14
skip_counts [] 20130 5258778 -2.64
skip_counts [exported] 20092 5260307 -3.60
skip_counts [bindings] 21036 5283522 0.10
skip_counts all 31728 6155886 0.05
skip_both [] 19964 5258783 -3.44
skip_both [exported] 20118 5260331 -3.48
skip_both [bindings] 21059 5283486 0.20
skip_both all 31451 6155858 -0.82

branch_join (200 names, 2000 reads)
variant collect median_us median_reductions time_delta_pct
current [] 25804 6704090 0.00
current [exported] 25765 6704102 0.00
current [bindings] 25800 6708342 0.00
current all 35246 7678311 0.00
skip_exported [] 25338 6700833 -1.81
skip_exported [exported] 25888 6704099 0.48
skip_exported [bindings] 25574 6705085 -0.88
skip_exported all 34828 7677138 -1.19
skip_counts [] 24564 6671841 -4.81
skip_counts [exported] 25040 6673769 -2.81
skip_counts [bindings] 25730 6715757 -0.27
skip_counts all 34844 7683256 -1.14
skip_both [] 24485 6668537 -5.11
skip_both [exported] 24916 6673752 -3.30
skip_both [bindings] 25688 6712488 -0.43
skip_both all 34538 7682139 -2.01
```
