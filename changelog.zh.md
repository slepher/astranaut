# 更新日志

[English](changelog.md)

## 0.13.0

### 兼容性

- 最低支持版本从 Erlang/OTP 19 提升到 21，并删除 pre-21 的异常栈、语法
  schema、参考数据和 CI 兼容分支。

## 0.11.2

### 宏系统

- 每次 parse transform 调用使用名字唯一的本地宏模块，删除固定模块名所需的生命周期
  锁，使同一源码模块可以并发编译而不共享生成的宏代码。

## 0.11.1

### 开发工具

- 删除仓库内的本地 Docker CI 脚本，改用宿主机 `rebar3_docker_ci` 插件及
  项目配置。

## 0.11.0

### 宏系统

- **统一外部宏与本地宏的展开流程**：attribute 宏与 function-body 宏共用递归
  expander，并由独立的 registry、源码扫描器和本地宏生命周期组件管理；同时修复
  guard 上下文中的宏展开。
- **本地宏采用声明作用域语义**：`-local_macro` 会冻结声明函数及静态发现的
  helper 闭包，并使用声明位置可见的宏环境和 attributes；后续声明不再反向影响
  已经冻结的闭包。
- **新增本地宏闭包控制**：支持 `-local_macro_retain` 和 `closure_roots`，可显式
  保留闭包或加入无法静态发现的 helper。
- **明确宏调用语义**：删除 `internal_function` option；匹配宏环境的直接调用始终
  是宏调用，普通函数调用通过独立 helper 或 Erlang 间接调用完成。
- **明确宏的源码顺序**：attribute pass 按源码顺序执行 scan-and-splice，宏生成的
  forms 会在当前位置继续扫描；随后 function-body pass 使用完整的最终宏环境。
  生成的 `-import_macro`、`-use_macro`、`-macro_options` 和 `-local_macro`
  只影响其后的 forms。
- **增强宏安全性与诊断**：支持递归宏展开及 `max_depth` 限制；检测宏名或 alias
  冲突、重复本地宏声明和不兼容的闭包环境；按 expression、pattern、guard、
  form、type 等 AST role 校验宏返回值，并隔离同级宏展开错误。
- **优化大型宏模块的编译性能**：预解析 attribute 环境并复用宏调用分析，减少
  重复扫描；新增深层递归和接近真实代码规模的宏编译 benchmark。

### Traversal、Quote 与 AST

- **新增 AST role 校验与规范化**：`astranaut_syntax` 新增
  `validate_node/2,3`、`normalize/2,3`、`child_specs/3` 和
  `node_roles/1`，可根据父节点 slot、OTP 版本和 record 定义校验或规范化
  syntax tree。
- **遍历结果默认校验**：traverse validator 会检测无效的 AST 变换及子节点；
  遍历遇到错误后通过 `fail_on_error/1`、`catch_on_error/2` 停止后续步骤，
  不再使用旧的 `listen_has_error` 流程。
- **调整 Uniplate API**：移除 `astranaut_uniplate:map/4`、`reduce/5`、
  `mapfold/5`、static uniplate 和 `keep` 语义；使用
  `astranaut:map/3`、`reduce/4`、`mapfold/4`，并新增 `search/3`、
  `map_with_state/4` 和 `smap_with_state/4`。
- **区分普通列表遍历与 module forms 处理**：`astranaut:map_m/3` 保留输入
  列表顺序；新增 `map_m_forms/3`，仅在处理 module forms 时执行生成 form 的
  插入、function/spec 合并和规范重排。
- **清理 Monad API**：`maybe` monad/type 更名为 `monad_maybe`，避免与
  OTP 25 的 `maybe` 关键字冲突；writer/listener API 统一为
  `writer_updated`、`listen_updated` 命名。
- **加强 Quote 输入校验**：quote binding 会统一拒绝无效的 string、atom 名称及
  其他类型值；`quote_code` 会校验代码和 option 的位置；AST 形式的 `pos`
  option 也会被正确规范化。
- **修正 traversal 契约与诊断格式化**：修复 `mapfold` 等公开类型契约，让显式
  root validator 具有优先级，并确保 validation 诊断保留正确的节点位置和
  formatter。
- **完善 `disable_tco` 变换**：递归处理 `case`、`if`、`receive`、`try`、
  block、布尔运算和 `maybe` 表达式中的尾位置；直接递归、named fun 递归和
  相互递归的本地函数都会保留尾调用优化。

### Struct 与编译期工具

- **重构 Struct 系统**：拆分为宏 API `astranaut_struct`、record 数据处理模块
  `astranaut_struct_record` 和 parse transform
  `astranaut_struct_transformer`，迁移至新的宏流程，并新增
  `from_other_record/4`。
- **新增 `astranaut_compile_meta_transformer`**：收集 parse transform 后的
  forms、编译错误和 warning，供编译期元编程与诊断使用。
- **整理 `astranaut_lib` 共享 API**：公开选项校验相关类型，集中提供模块锁、
  二进制安全重载和 `reload_forms/2`，并移除仅供旧 converter/wrapper 使用的
  导出。
- **修复公开类型契约**：统一修正 traversal、forms、quote、macro、return、
  rebinding 和 struct 模块的公开类型，并导出公共的
  `astranaut:walk_return/2` 类型。

### OTP 兼容性与开发工具

- 新增 `map_generator`、`strict_generator`、`strict_binary_generator`、
  `strict_map_generator`、`maybe_expr` 和 `maybe_match_expr` 等 AST 类型支持，
  主要用于 rebinding；兼容 Erlang/OTP 19～29。
- 使用 OTP 19 已支持的 `maps` API 替换 `maps:merge_with/3`，并兼容
  OTP 19～23 对 compile attribute 分析结果的额外包装，避免 parse transform 的
  有效行号被报告为 `0`。
- 将 `syntax_tools` 声明为正式 OTP 应用依赖；移除生产代码对 `eunit_lib` 的
  调用，并把使用 EUnit 的测试辅助模块移出生产编译顺序。
- 删除已经失效的兼容头文件，并修复本地容器 CI 的 suite/case 选择和测试失败
  状态传播。
- 将宏相关 Common Test 拆分为基础行为、统一展开、pass、诊断、本地状态和
  scan-and-splice 等职责清晰的 SUITE。
- 新增中文 README、OTP 19～29 abstract forms 参考文档及抓取脚本、本地容器
  CI、覆盖率报告脚本，以及宏编译 benchmark。
