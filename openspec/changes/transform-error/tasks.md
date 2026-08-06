## 1. Compiler adapter 与共享格式化 API

- [ ] 1.1 为 `astranaut_return:to_compiler/1` 添加 error/warning adapter 测试，验证 `{Pos, DomainFormatter, Reason}` 转为 `{Pos, astranaut_lib, {DomainFormatter, Reason}}` 且分组、顺序和位置不变。
- [ ] 1.2 为 `astranaut_lib:format_error/1,2` 添加 compiler wrapper、match、no-match、内部 helper `function_clause`、character-list fallback 和其他异常传播测试。
- [ ] 1.3 实现 `format_error({Module, Reason})`、`format_error(Reason, FormatterFun)` 和 private 默认格式化，并在 `to_compiler/1` 统一包装 diagnostics。
- [ ] 1.4 删除 `dispatch_error/3`、公开 `format_default_error/2`、stack-frame 区分逻辑及对应 API surface 断言。

## 2. Parse-transformer formatter 迁移

- [ ] 2.1 将 `astranaut`、`astranaut_macro` 和 `astranaut_quote` 恢复为直接的纯 `format_error/1` 领域 clauses，删除 callback 内 shared dispatch。
- [ ] 2.2 将 `astranaut_do`、`astranaut_rebinding` 和 `astranaut_struct_transformer` 恢复为纯 callback，并让共享 reason 在产生处绑定正确 formatter或由精确 clause 处理。
- [ ] 2.3 将 compile-meta、compile-opts、disable-tco 等其余 formatter 恢复为纯 callback。
- [ ] 2.4 检查所有生产领域 formatter，确认不导出 `/2`、不调用 shared dispatcher、不存在通用 `format_error_1/1`、options 或 generic catch-all。

## 3. Macro formatter 协议

- [ ] 3.1 将 external/local macro 测试 provider 的 formatter 改为纯 `/1` clauses，并删除 callback 内 shared dispatch、`/2`、throw options 和通用 `_1` 跳转层。
- [ ] 3.2 收敛 local macro formatter protocol/closure detection，使生成模块只复制和导出 `/1` 及其真实 helper 依赖。
- [ ] 3.3 更新 formatter module 调用点和测试 helper，统一使用 `astranaut_lib:format_error(Error, fun Module:format_error/1)`。

## 4. 缺失用户 macro formatter warning

- [ ] 4.1 在 external macro registry 检测缺失 `format_error/1`，以 `astranaut_macro` 产生一次 `{missing_macro_formatter, Module}` warning 并继续注册。
- [ ] 4.2 在 local macro formatter protocol 检测处产生同一 warning，使用 source module identity 并继续选择 `astranaut_macro`。
- [ ] 4.3 添加 external/local、仅 `/2` 以及同一 provider 多次使用的 warning 去重测试。
- [ ] 4.4 为 `missing_macro_formatter` 增加 `astranaut_macro` 领域格式化条款。

## 5. 回归与验证

- [ ] 5.1 更新 formatter contract 测试：直接 `/1` 精确消息证明领域映射，`astranaut_error:realize/1` 证明内部 ownership，`to_compiler/1` 证明 wrapper，shared adapter 证明 fallback。
- [ ] 5.2 运行 compile、astranaut、macro error、macro local、quote、rebinding、struct 和 design 专项 Common Test。
- [ ] 5.3 运行完整 Common Test，确认 compiler diagnostics、位置、reason 和 parse-transform 行为无回归。
- [ ] 5.4 运行 `rebar3 xref`、`rebar3 dialyzer`、`openspec validate transform-error --strict` 和 `git diff --check`。
