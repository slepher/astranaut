# 队列快照性能评估

日期：2026-10-28。基线：`c556e53a40448b16c449c84271d61ffc70e7faad`，加本次 Registry 清理；scanner 生产源码未修改。

结论：保留现有快照维护。当前实现共享队列尾部，不存在每个 form 复制完整队列的二次方成本。绕过快照减少少量 reductions，但本轮没有足够证据证明值得增加 capability 切换逻辑；这不表示所有工作负载都没有优化空间。

## 方法

从仓库根目录执行：

```shell
rebar3 compile
escript benchmark/queue_snapshot_bench.escript 51
```

环境：OTP 26，`x86_64-apple-darwin23.0.0`，单 scheduler。每组 3 次预热、51 次正式采样，报告中位数；每次使用新进程，交替 AB/BA 顺序。最终两轮独立运行期间没有同时运行编译或测试任务。

- 微基准：100 / 1,000 / 10,000 个普通属性 form，使用透传 handler，比较 `queue_state => true / false`。包含完整 scan-and-splice 循环和 traversal 开销，不是单次 map update 的纳秒测试。
- 实际工作负载：约 5,000 行、13 个 Erlando do 表达式的 `real_benchmark.erl`，仅使用外部 provider。比较完整 `astranaut_macro:parse_transform/2`，不包含 EPP、provider 编译、scanner 编译、代码加载或完整 Erlang 编译器耗时。
- 对照 scanner 由当前源码临时编译到内存，只将 `run/5` 的 `queue_state => true` 改为 `false`。运行后恢复原 BEAM，临时源码删除，仓库 scanner 不变。
- 两组输出逐次比较完全相等。绕过版本仅用于这份无 local declaration 的工作负载，不能用于本地宏或动态生成 local declaration 的生产路径。
- reductions 通过每个子进程自身的 `process_info/2` 前后差值测量。未测量峰值堆内存、分配字节数或完整编译速度，不能由这些结果推导对应收益。

## 结果

第一轮独立采样：

| 工作负载 | 保留快照 µs | 绕过快照 µs | 耗时下降 | 保留快照 reductions | 绕过快照 reductions |
| --- | ---: | ---: | ---: | ---: | ---: |
| 100 forms | 615 | 636 | -3.41% | 49,307 | 48,024 |
| 1,000 forms | 6,118 | 6,481 | -5.93% | 480,168 | 474,357 |
| 10,000 forms | 84,537 | 85,781 | -1.47% | 4,711,894 | 4,665,537 |
| 完整宏转换 | 114,638 | 109,967 | 4.07% | 9,565,775 | 9,556,681 |

第二轮独立采样：

| 工作负载 | 保留快照 µs | 绕过快照 µs | 耗时下降 | 保留快照 reductions | 绕过快照 reductions |
| --- | ---: | ---: | ---: | ---: | ---: |
| 100 forms | 611 | 604 | 1.15% | 49,307 | 48,024 |
| 1,000 forms | 5,914 | 6,515 | -10.16% | 480,168 | 474,469 |
| 10,000 forms | 90,426 | 97,162 | -7.45% | 4,726,954 | 4,683,985 |
| 完整宏转换 | 120,150 | 115,978 | 3.47% | 9,566,737 | 9,557,218 |

完整宏转换在两轮中有 3.47%–4.07% 的小幅改善（约 4.2–4.7 ms），应承认这一收益；纯扫描微基准却多数变慢。结果支持“收益依赖工作负载”，不能概括为关闭快照必然加速。当前只评估一个实际工作负载，没有统计置信区间，因此保留机制的决定是收益与切换复杂度之间的工程取舍，不是证明该优化永远无效。

负数表示绕过更慢。完整转换的 reductions 只下降约 0.10%，时间差不能直接归因于同等比例的 CPU 工作减少。小量分配变化可能改变 GC 行为，墙钟计时也受运行环境影响。

## 实现判断

`map_forms_splice_note_queue/2` 每步保存三元组 `{Form, Buffer, Rest}`，Buffer 和 Rest 共享现有不可变列表尾部，只增加固定规模的状态更新及 monad 操作。`source_view/1` 才执行 reverse / untag / 拼接，而且只在 provider 需要 context 时调用。

若后续数据证明 disabled 扫描成为热点，再评估按需维护。实现不能简单按初始 capability 关闭快照：首个 `local_macro`（包括外部宏生成的声明）在初始化 provider 时就需要当前准确的源码视图。必须同时验证当前 form、尚未耗尽的 generated buffer、原始 rest、首次声明和 splice-back 的顺序。
