## 1. 错误所有权回归测试

- [ ] 1.1 更新 macro error suite，使 local macro 抛出的 `macro_exception` 明确断言 formatter 为 `astranaut_macro`，同时保留用户主动返回领域错误的生成 local formatter 断言。
- [ ] 1.2 增加或扩展 external macro 覆盖，证明拥有自定义 formatter 的 provider 抛出异常时使用 `astranaut_macro`，主动返回 error/warning 时仍使用 provider module。
- [ ] 1.3 保留并强化 sibling 诊断断言，验证异常、用户领域错误与 `invalid_macro_return` 三种所有权以及原有位置、reason、payload、累计顺序和恢复行为。

## 2. Macro 框架路由实现

- [ ] 2.1 在 `astranaut_macro_expander` 的异常包装产生点为 `macro_exception` 固定 `astranaut_macro` formatter，且不扩大覆盖到成功返回的用户 computation。
- [ ] 2.2 删除 macro 测试 provider 中仅用于代理 `macro_exception` 的框架条款，保留用户领域 reason 的具体 `format_error/1` clauses。
- [ ] 2.3 核对其他 macro 框架 reason 的产生路径，确保注册、解析、递归限制和返回值校验继续固定使用 `astranaut_macro`，且没有新增 formatter fallback 链。

## 3. Formatter 身份收敛

- [ ] 3.1 删除 `astranaut_struct` 的无领域 `/1` facade，使 registry 自然为其 macro descriptor 选择 `astranaut_macro`。
- [ ] 3.2 更新 struct formatter contract 测试，分别验证 `astranaut_struct` macro 使用框架 formatter、`astranaut_struct_transformer` 继续拥有 struct-specific reason。
- [ ] 3.3 更新 `README.md` 和 `README.zh.md` 的 macro sections，明确框架 reason 在产生点固定 formatter、用户成功返回的领域诊断才使用 registry formatter，并记录 `astranaut_struct` facade 的移除。

## 4. 验证

- [ ] 4.1 运行编译以及 macro error、macro local、struct 专项 Common Test，确认 formatter identity 与消息协议通过。
- [ ] 4.2 运行完整 Common Test，确认宏展开、兄弟错误恢复和其他 transformer 诊断无回归。
- [ ] 4.3 运行 `openspec validate macro-error --strict` 与 `git diff --check`，确认规格和改动格式有效。
