# Tasks

## Specification

- [x] 明确 whitelist control 的 `disabled`、`collect`、`verify(Expected)` 三种模式。
- [x] 明确仅 local frozen function 及其 replacement AST 启用，普通 function forms 禁用。
- [x] 明确白名单冲突和 expanded-form 冲突是两条独立不变量。
- [x] 明确 whitelist 观察接入统一 macro 发现—执行点，`process_macro_return` 不负责展开，也不增加扫描 pass。

## Data contracts

- [x] 为 `LocalMacroWhitelistControl` 和带 whitelist 结果的 function expansion 定义命名类型。
- [x] 在 ExpansionRecord 中增加 `canonical_whitelist` 和 per-input whitelist/result。
- [x] 保证 input fingerprint 只包含展开前可知输入，并排除白名单外 local generations。
- [x] final retained 使用 canonical whitelist 过滤 FinalEnv，名单外 local descriptors 不进入有效环境或 fingerprint。
- [x] 删除普通 function 接口中的隐式 whitelist 推断和双重返回形状容错。

## Expansion integration

- [x] 为通用 function 展开入口增加 whitelist control 参数，所有调用点显式传值。
- [x] local declaration 首次展开传 `collect`，已有 canonical 的后续处理传 `verify`。
- [x] 普通 Step 2 function、普通 retained function 和 attribute invocation 传 `disabled`。
- [x] 将 local macro match 观察接入原始 function与 replacement 共用的发现—执行路径。
- [x] replacement AST 递归处理继承相同 control、accumulator、depth 和错误上下文。
- [x] replacement 首次匹配未 callable local macro 时请求 `need_callable`，编译后从 frozen form 重试。
- [x] 保留既有 post-return 递归展开路径；不为 whitelist 增加额外 `transform_exprs` traversal。
- [x] 保持用户宏 State 通过 `scoped_state` 隔离，whitelist accumulator 由框架拥有。

## Conflict handling

- [x] `collect` 成功后原子提交 canonical whitelist 与 canonical result。
- [x] `verify` 观察到 unexpected FA 时立即报告 `conflicting_local_macro_whitelist` 并跳过该 macro 调用。
- [x] 完整 function expansion 后检查 missing FA。
- [x] whitelist 相同后继续执行 `conflicting_local_macro_closure_environment` AST 比较。
- [x] whitelist/AST/宏执行任一失败时不提交部分 cache、canonical form 或 generation。

## Simplification

- [x] 删除 final order/self/declaration-member 排除路径。
- [x] 删除共享 helper owner whitelist 的运行时猜测或无条件并集路径。
- [x] 不新增独立 whitelist scanner、whole-form rescan、AST diff 或 callback map。
- [x] 检查参数顺序保持 `subject, runtime context, control, state` 的函数族一致性。

## Tests

- [x] 普通 function 调用 local macro 时保持 `disabled`，不创建或校验 whitelist。
- [x] local frozen function 原始 AST 中的 local 调用进入 canonical whitelist。
- [x] external/local macro replacement 生成的新 local 调用在统一发现—执行点进入 whitelist。
- [x] 多层 replacement 递归继承同一 whitelist control，且不增加 whitelist 专用遍历。
- [x] unexpected FA 在完整 function 结束前立即失败。
- [x] missing FA 只在完整 function expansion 后失败。
- [x] whitelist 不同但 AST 相同仍报告 whitelist conflict。
- [x] whitelist 相同但 AST 不同仍报告 closure-environment conflict。
- [x] retained local-macro head 和 frozen helper 使用 `verify`。
- [x] final retained 的名单外同声明/后声明 local 调用保持普通调用。
- [x] external replacement 首次生成 pending local 调用时按需编译并成功重试。
- [x] 普通 retained/function forms 不启用 whitelist。
- [x] whitelist 冲突不提交部分 declaration、ExpansionRecord 或 generation。
- [x] 完整 Common Test suite 通过。
