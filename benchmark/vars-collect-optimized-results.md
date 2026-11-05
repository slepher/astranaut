# Vars collect optimization verification — 2026-11-05

Command: `escript benchmark/vars_collect_bench.escript 15`, after compiling the
optimized default build. No regression tests or other builds ran concurrently.
15 samples, 3 warmups, one scheduler, alternating case order, fresh process per
sample. Fixture generation, module compilation and equivalence checks are not timed.

`current` is production source with initialization-time collect flags. `eager`
forces both flags on in the same engine, recreating the formerly unconditional
work. It shares cached keys and gating code, so this is an eager-work reference,
not an exact historical executable. No real rebinding naming callback is timed.

## collect => []

| Shape | Eager µs | Current µs | Time change | Eager reductions | Current reductions | Reductions change |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| body | 21,272 | 19,735 | -7.23% | 5,201,838 | 5,179,484 | -0.43% |
| clause | 19,880 | 19,644 | -1.19% | 5,276,992 | 5,252,791 | -0.46% |
| branch_join | 27,882 | 26,024 | -6.66% | 6,707,509 | 6,664,939 | -0.63% |

The strongest signal is avoiding binding-site count updates. Skipping exported
alone remains negligible, especially for clauses with an empty final environment.
Requested-metric control rows also show wall-time variation (up to several percent),
so the smaller changes are not evidence of a reliable speedup in isolation.
Results describe these synthetic fixtures, not the full rebinding pass.

All four variants returned identical ASTs and requested analysis for each fixture
and collect setting. Regression checks: 232 EUnit and 286 Common Test cases passed;
format check passed; Dialyzer retained the 6 known warnings with no new warnings.

Production now caches collect_keys, count_bindings and collect_exported once per
traversal. A use event bypasses binding-table count updates when not requested;
result construction bypasses exported enumeration and sorting when not requested.
Name-aggregated counts remain independent. Binding identity, lexical environments,
name resolution and branch joins remain active in every mode.

## Raw output

```text
OTP 29 aarch64-apple-darwin25.6.0; schedulers=1 samples=15 warmups=3; medians, fresh process/sample

body (200 names, 2000 reads)
variant collect median_us median_reductions time_delta_pct
eager [] 21272 5201838 0.00
eager [exported] 21310 5201918 0.00
eager [bindings] 21441 5203994 0.00
eager all 32297 6096943 0.00
skip_exported [] 21292 5199642 0.09
skip_exported [exported] 21198 5201952 -0.53
skip_exported [bindings] 21350 5201702 -0.42
skip_exported all 31419 6095449 -2.72
skip_counts [] 19708 5181769 -7.35
skip_counts [exported] 19594 5181774 -8.05
skip_counts [bindings] 21335 5203999 -0.49
skip_counts all 31508 6094749 -2.44
current [] 19735 5179484 -7.23
current [exported] 19530 5181775 -8.35
current [bindings] 21193 5201805 -1.16
current all 31485 6094758 -2.51

clause (200 names, 2000 reads)
variant collect median_us median_reductions time_delta_pct
eager [] 19880 5276992 0.00
eager [exported] 20040 5277015 0.00
eager [bindings] 19963 5279119 0.00
eager all 29261 6173052 0.00
skip_exported [] 19816 5277034 -0.32
skip_exported [exported] 20287 5276999 1.23
skip_exported [bindings] 19906 5279127 -0.29
skip_exported all 29563 6172465 1.03
skip_counts [] 19376 5252781 -2.54
skip_counts [exported] 19484 5252772 -2.77
skip_counts [bindings] 19871 5279115 -0.46
skip_counts all 29298 6172008 0.13
current [] 19644 5252791 -1.19
current [exported] 19289 5252808 -3.75
current [bindings] 19850 5279137 -0.57
current all 29393 6172159 0.45

branch_join (200 names, 2000 reads)
variant collect median_us median_reductions time_delta_pct
eager [] 27882 6707509 0.00
eager [exported] 27768 6707508 0.00
eager [bindings] 27580 6711776 0.00
eager all 38006 7680373 0.00
skip_exported [] 27940 6704228 0.21
skip_exported [exported] 27962 6707530 0.70
skip_exported [bindings] 27916 6708474 1.22
skip_exported all 38330 7679696 0.85
skip_counts [] 25910 6668432 -7.07
skip_counts [exported] 25918 6668464 -6.66
skip_counts [bindings] 27816 6711764 0.86
skip_counts all 37866 7679020 -0.37
current [] 26024 6664939 -6.66
current [exported] 25819 6668446 -7.02
current [bindings] 27367 6708470 -0.77
current all 36282 7676813 -4.54
```
