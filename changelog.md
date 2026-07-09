# Changelog

## 0.11.0

- **统一宏展开系统**：外部宏与本地宏使用统一管道展开，新增 `-import_macro` 跨模块导入，支持宏重名冲突检测、递归宏展开、宏返回值语法验证
- **Struct 系统重构**：拆分为 `astranaut_struct`（宏 API）、`astranaut_struct_record`、`astranaut_struct_transformer`（parse_transform），新增 `from_other_record/4`，迁移至新宏流程
- **AST 语法校验**：`astranaut_syntax` 新增 `validate_node/2,3`、`normalize/2,3`，引入 child_specs 角色校验体系
- **遍历校验系统**：新增 traverse validator，检测无效 AST 变换及子节点
- **Uniplate API 调整**：`map/4`、`reduce/5`、`mapfold/5` 从 `astranaut_uniplate` 移至 `astranaut` 模块，移除 static uniplate 和 keep 概念，新增 `astranaut:search/3`
- **新增 OTP 类型支持**：`map_generator`、`strict_generator`、`strict_binary_generator`、`strict_map_generator`、`maybe_expr`、`maybe_match_expr` 支持（主要在 rebinding 中）
- **Monad 重命名**：`maybe` monad 更名为 `monad_maybe`（避免 OTP-25 `maybe` 关键字冲突）
- **错误处理增强**：`fail_on_error/1` / `catch_on_error/2` 替代 `listen_has_error`，有错误时遍历即失败不再继续
- **新增 `astranaut_compile_meta_transformer`**：编译期元编程变换器
- 新增 `astranaut:map_with_state/4`、`astranaut:smap_with_state/4`
- 兼容 Erlang/OTP 19 ~ 29
