# Tasks

## Specification

- [ ] 明确 whitelist control 的 `disabled`、`collect`、`verify(Expected)` 三种模式。
- [ ] 明确仅 local frozen function 及其 replacement AST 启用，普通 function forms 禁用。
- [ ] 明确白名单冲突和 expanded-form 冲突是两条独立不变量。
- [ ] 明确宏返回 AST 复用 `process_macro_return` 已有 traversal，不增加扫描 pass。

## Data contracts

- [ ] 为 `LocalMacroWhitelistControl` 和带 whitelist 结果的 function expansion 定义命名类型。
- [ ] 在 ExpansionRecord 中增加 `canonical_whitelist` 和 per-input whitelist/result。
- [ ] 保证 input fingerprint 只包含展开前可知输入，并排除白名单外 local generations。
- [ ] 删除普通 function 接口中的隐式 whitelist 推断和双重返回形状容错。

## Expansion integration

- [ ] 为通用 function 展开入口增加 whitelist control 参数，所有调用点显式传值。
- [ ] local declaration 首次展开传 `collect`，已有 canonical 的后续处理传 `verify`。
- [ ] 普通 Step 2 function、普通 retained function 和 attribute invocation 传 `disabled`。
- [ ] 将 local macro match 观察接入原始 function traversal 和 `process_macro_return` 的既有 return-tree traversal。
- [ ] replacement AST 递归处理继承相同 control、accumulator、depth 和错误上下文。
- [ ] 移除 post-return 对完整 `Node1` 的第二次 `transform_exprs` traversal。
- [ ] 保持用户宏 State 通过 `scoped_state` 隔离，whitelist accumulator 由框架拥有。

## Conflict handling

- [ ] `collect` 成功后原子提交 canonical whitelist 与 canonical result。
- [ ] `verify` 观察到 unexpected FA 时立即报告 `conflicting_local_macro_whitelist`。
- [ ] 完整 function expansion 后检查 missing FA。
- [ ] whitelist 相同后继续执行 `conflicting_local_macro_closure_environment` AST 比较。
- [ ] whitelist/AST/宏执行任一失败时不提交部分 cache、canonical form 或 generation。

## Simplification

- [ ] 删除 final order/self/declaration-member 排除路径。
- [ ] 删除共享 helper owner whitelist 的运行时猜测或无条件并集路径。
- [ ] 不新增独立 whitelist scanner、whole-form rescan、AST diff 或 callback map。
- [ ] 检查参数顺序保持 `subject, runtime context, control, state` 的函数族一致性。

## Tests

- [ ] 普通 function 调用 local macro 时保持 `disabled`，不创建或校验 whitelist。
- [ ] local frozen function 原始 AST 中的 local 调用进入 canonical whitelist。
- [ ] external/local macro replacement 生成的新 local 调用在同一 traversal 中进入 whitelist。
- [ ] 多层 replacement 递归继承同一 whitelist control，且每个 generated AST 只遍历一次。
- [ ] unexpected FA 在完整 function 结束前立即失败。
- [ ] missing FA 只在完整 function expansion 后失败。
- [ ] whitelist 不同但 AST 相同仍报告 whitelist conflict。
- [ ] whitelist 相同但 AST 不同仍报告 closure-environment conflict。
- [ ] retained local-macro head 和 frozen helper 使用 `verify`。
- [ ] 普通 retained/function forms 不启用 whitelist。
- [ ] whitelist 冲突不提交部分 declaration、ExpansionRecord 或 generation。
- [ ] 完整 Common Test suite 通过。

