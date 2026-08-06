## 1. 错误所有权回归测试

- [ ] 1.1 更新 macro error suite，使 local macro 抛出的 `macro_exception` 明确断言 formatter 为 `astranaut_macro`，同时保留用户主动返回领域错误的生成 local formatter 断言。
- [ ] 1.2 增加或扩展 external macro 覆盖，证明拥有自定义 formatter 的 provider 抛出异常时使用 `astranaut_macro`，主动返回 error/warning 时仍使用 provider module。
- [ ] 1.3 保留并强化 sibling 诊断断言，验证异常、用户领域错误与 `invalid_macro_return` 三种所有权以及原有位置、reason、payload、累计顺序和恢复行为。
- [ ] 1.4 将异常用例命名和说明改为 unexpected fault/fault containment，确保领域错误示例只通过返回的 error/warning computation 表达。

## 2. Macro 框架路由实现

- [ ] 2.1 在 `astranaut_macro_expander:invoke_macro_function/1` 的 catch 分支通过 `astranaut_traverse:update_pos(Pos, astranaut_macro, ...)` 为 `macro_exception` 固定位置和 formatter，且不扩大覆盖到成功返回的用户 computation。
- [ ] 2.2 删除 macro 测试 provider 中仅用于代理 `macro_exception` 的框架条款，保留用户领域 reason 的具体 `format_error/1` clauses。
- [ ] 2.3 核对其他 macro 框架 reason 的产生路径，确保注册、解析、递归限制和返回值校验继续固定使用 `astranaut_macro`，且没有新增 formatter fallback 链。
- [ ] 2.4 保留 `astranaut_macro:format_error/1` 的 `macro_exception` 框架 clause；不得在用户 formatter 或 registry 中增加异常 reason dispatch。可将消息改为明确的 unexpected exception 措辞。

## 3. Struct formatter 与 shared default API

- [ ] 3.1 在 `astranaut_lib` 公开 `format_default_error/1`，保持 deep character list 原样返回、其他 term 使用 `io_lib:write/1`，并让 `format_error/2` 调用该 helper。
- [ ] 3.2 保留 `astranaut_struct:format_error/1` export，替换全部 proxy/reason-specific clauses 为唯一 universal fallback clause；registry 走 present/no-warning 路径。
- [ ] 3.3 更新 `astranaut_SUITE` 与 `astranaut_struct_SUITE`，覆盖 public/shared fallback、struct callable fallback、struct no-warning、framework formatter identity、struct-transformer formatter identity 及既有 diagnostic count/order/recovery invariants；同步 transform-error 与 macro-error 的六个 OpenSpec 文件，使 public helper、preserved struct export、no-warning consequence 和 ownership boundary 与实现及测试一致。
- [ ] 3.4 后续更新 `README.md` 和 `README.zh.md` 的 macro sections，记录框架 reason 在产生点固定 formatter、用户成功返回的领域诊断才使用 registry formatter、异常捕获仅用于故障隔离，以及保留的 struct formatter export 与 default helper；不得描述为移除 facade。

## 4. 验证

- [ ] 4.1 运行编译以及 macro error、macro local、struct 专项 Common Test，确认 formatter identity 与消息协议通过。
- [ ] 4.2 运行完整 Common Test，确认宏展开、兄弟错误恢复和其他 transformer 诊断无回归。
- [ ] 4.3 运行 `openspec validate macro-error --strict` 与 `git diff --check`，确认规格和改动格式有效。
