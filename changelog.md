# Changelog

## 0.11.0

- **统一宏展开系统**：外部宏与本地宏使用统一管道展开，新增 `-import_macro` 跨模块导入，支持宏重名冲突检测、递归宏展开、宏返回值语法验证
- **宏扫描职责收敛**：`map_forms_splice/3` 从通用 `astranaut` traversal 迁移到专用 `astranaut_macro_scan`，由宏扫描器统一管理 Rest、attribute Buffer 与生成 function/spec 的局部合并
- **Forms traversal 语义显式化**：`astranaut:map_m/3` 改为保留列表顺序；新增 `map_m_forms/3`，仅在明确需要时统一执行更新 form 的插入、重排及 `__original__` 合并；`prepare_exports` 不再隐式重排；function expander 以一次保序 Forms pass 展开全部目标，并为 frozen/local/ordinary function 分别应用其任务环境
- **Struct 系统重构**：拆分为 `astranaut_struct`（宏 API）、`astranaut_struct_record`、`astranaut_struct_transformer`（parse_transform），新增 `from_other_record/4`，迁移至新宏流程
- **AST 语法校验**：`astranaut_syntax` 新增 `validate_node/2,3`、`normalize/2,3`，引入 child_specs 角色校验体系
- **遍历校验系统**：新增 traverse validator，检测无效 AST 变换及子节点
- **Uniplate API 调整**：`map/4`、`reduce/5`、`mapfold/5` 从 `astranaut_uniplate` 移至 `astranaut` 模块，移除 static uniplate 和 keep 概念，新增 `astranaut:search/3`
- **新增 OTP 类型支持**：`map_generator`、`strict_generator`、`strict_binary_generator`、`strict_map_generator`、`maybe_expr`、`maybe_match_expr` 支持（主要在 rebinding 中）
- **Monad 重命名**：`maybe` monad 更名为 `monad_maybe`（避免 OTP-25 `maybe` 关键字冲突）
- **错误处理增强**：`fail_on_error/1` / `catch_on_error/2` 替代 `listen_has_error`，有错误时遍历即失败不再继续
- **`astranaut_lib` 共享工具层整理**：移除无关 converter/wrapper 导出，公开选项校验类型，并集中提供模块锁、二进制安全重载和 `reload_forms/2`
- **新增 `astranaut_compile_meta_transformer`**：编译期元编程变换器
- 新增 `astranaut:map_with_state/4`、`astranaut:smap_with_state/4`
- 兼容 Erlang/OTP 19 ~ 29
- **CI 与脚本**：新增本地 CI 容器方案（`ci_scripts`）、覆盖率报告脚本（`cover_report.escript`）、抽象语法文档抓取脚本（`fetch_absforms`）
