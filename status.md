# Astranaut `format_error` 改造状态

更新时间：2026-08-06（Asia/Shanghai）
工作目录：`/home/slepher/project/astranaut`
当前提交：`1ab6770 Use shared format error dispatcher`

## 目标与已确认约束

- 保持原有 `format_error/1` 行为：未匹配错误仍然兜底格式化，不抛异常。
- 新增 `format_error/2`；传入 `#{default => throw}` 时，未匹配错误必须抛出，以检测 formatter 覆盖遗漏。
- 具体匹配子句改名为 `format_error_1/1`。
- 公共 dispatcher 与默认 fallback 位于 `astranaut_lib`。
- 默认 fallback 名称为 `format_default_error/2`。
- dispatcher 接受可注入的 `FallbackFun/2`，且只把 formatter 自身无匹配的 `function_clause` 当作未覆盖；formatter 内部发生的 `function_clause` 必须保留原堆栈重新抛出。
- `astranaut_test_lib` 仅在发现 formatter 导出 `format_error/2` 时用 `/2` 严格检查；只有 `/1` 时跳过检查。对实际 error 的格式化应确认不抛异常。
- `local_macro` 编译阶段应检测 `format_error` 闭包覆盖情况，不能把这些 formatter 函数一同塞入编译闭包。
- 每个小任务由 Luna 实现，主代理使用 `audit-implementation-simplicity` 两阶段审核；审核不通过则退回 Luna 修改，通过测试后逐步提交。

## 已提交内容

1. `870f574 Add strict format error fallback control`
   - 建立 `format_error/2` 严格模式，同时保持 `/1` 兜底语义。
2. `deb4d00 Check strict formatter coverage in tests`
   - 测试工具仅对存在的 `/2` formatter 执行严格覆盖检查。
3. `44f5a23 Add strict macro error formatting`
   - `astranaut_macro` 迁移至 `/1`、`/2`、`format_error_1/1`。
4. `f37ba34 Add strict do error formatting`
   - `astranaut_do` 完成相同迁移。
5. `f46b1e5 Add strict compile error formatting`
   - compile-meta 与 compile-options 路径完成迁移。
6. `33db3b2 Add strict quote error formatting`
   - `astranaut_quote` 完成迁移；审核发现伪覆盖后已返工为显式匹配子句。
7. `29a01d3 Centralize format error dispatch`
   - 新增 `astranaut_lib:format_error/4` 与 `astranaut_lib:format_default_error/2`。
   - dispatcher 支持传入 formatter 与 `FallbackFun/2`，并区分顶层无匹配和 formatter 内部异常。
8. `1ab6770 Use shared format error dispatcher`
   - macro、do、compile-meta、quote 改用共享 dispatcher，删除重复的 try/catch dispatcher。

上述各阶段均经过对应审核与定向 Common Test；最近一次已提交迁移的定向结果包括 macro 12/12、design 21/21、quote 73/73。

## 当前未提交内容

工作树仅有：

- `src/astranaut_struct.erl`
- `src/astranaut_struct_transformer.erl`
- `test/astranaut_struct_SUITE.erl`

内容：

- `astranaut_struct` 新增 `/2` 并委托 `astranaut_macro:format_error/2`。
- `astranaut_struct_transformer` 使用共享 dispatcher；五个具体子句已改为 `format_error_1/1`。
- 增加已知错误严格格式化、`/1` 兜底、严格未知错误抛出以及 struct `/2` 委托测试。

状态：

- Luna `gpt-5.6-luna`（xhigh）报告 struct Common Test 19/19 通过，`git diff --check` 通过。
- 主代理已按 `audit-implementation-simplicity` 完成两阶段审核，结论通过。
- 主代理复跑 struct CT 时因用户新消息在约 5.3 秒处中断；这是中断，不是测试失败。
- 尚未提交。

## 后续步骤

1. 主代理重新运行 struct 定向 CT；通过后提交上述 3 个文件。
2. 单独盘点并处理仍只有 `format_error/1` 的生产 formatter；当前明确可见 `astranaut_disable_tco` 与 `astranaut_rebinding`，需先确认它们是否会产生需要覆盖检测的结构化错误。
3. 单独检查并实现 `local_macro` 编译闭包的 formatter 覆盖检测，确保检测闭包而不是把 formatter 函数并入编译闭包。
4. 对每一步继续执行：Luna 小任务 → 主代理审核 → 审核意见返工 → 定向测试 → 独立提交。
5. 最后运行覆盖相关定向测试及完整 Common Test，并检查 `git diff --check`、工作树与提交历史。

## 代理状态

- `/root/migrate_shared_dispatcher`：已完成。
- `/root/struct_shared_formatter`：已完成。
- 当前没有仍在执行的 Luna 子任务。

## Token 路由备注

当前会话根代理是 Sol，无法在会话中交换父子身份。若要由 Luna 作为 dispatcher、Sol 仅作为架构或审核 worker，需要新建以 Luna 为根代理的会话，并以 `fork_turns = "none"` 给 Sol 传递精简任务。预计可减少约 50%–90% 的 Sol token，但多代理总 token 不一定下降。
