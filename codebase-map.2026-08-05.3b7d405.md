# Astranaut Codebase Map

> 生成时间: 2026-08-05 · 版本: 0.13.0 · 语言: Erlang/OTP (≥ 21)
> 仓库: https://github.com/slepher/astranaut
>
> **Revision:** `8f48873` (`8f48873` · "refactor: isolate local macro capability")
> 2026-08-05 · 相对上一版 (2717f7d) 的增量：local-macro 重构为可选懒注册 capability
>
> 本文档主体以已提交内容（revision 8f48873）为准；未提交的变更列于文末「未提交增补」。

## 项目概览

Astranaut 是 Erlang 的元编程库，围绕 **parse transform** 提供四组能力：

1. **Traverse** — 通用 AST 遍历/变换框架（map、reduce、mapfold、monadic map_m）
2. **Quote/Unquote** — 类 Lisp 的准引用语法，编译期把代码片段转成 AST
3. **Macro** — 卫生宏系统（局部宏、导出宏、属性宏、闭包冻结、作用域展开）
4. **Rebinding / Struct** — 变量重绑定（Elixir 式 pin）、record → Elixir 式 struct 转换

另有支撑性子系统：`do` 记法、traverse/return/error monad、syntax 校验与规范化、uniplate 遍历内核、struct 转换器、TCO 禁用、编译元数据注入。

## 构建与测试

| 项目 | 说明 |
| --- | --- |
| 构建 | `rebar3 compile`（无第三方依赖，`rebar.config` 中 `erl_first_files` 强制编译顺序） |
| 测试 | `rebar3 ct`（Common Test，约 19 个 SUITE、600+ 用例）；`provider_hooks` 自动跑 cover |
| 静态检查 | `rebar3 xref`（undefined calls / deprecated / locals_not_used） |
| CI | `.cirrus.yml` + `.travis.yml`（docker_ci 覆盖 OTP 21/23/25/28/29） |
| 代码覆盖率 | `cover.spec` 启用；`scripts/cover_report.escript` 生成报告 |

Windows sandbox 环境（Codex 应用）下用 `scripts/rebar3_sandbox.ps1`，但 compile/ct 必须直接跑 rebar3（见 AGENTS.md）。

## 目录结构

```
astranaut/
├── src/                  # 核心源码（27 个模块，见下方模块地图）
├── include/              # 11 个 .hrl 头文件（parse transform 开关 + 内部结构名）
├── test/                 # Common Test SUITE + *_data 测试夹具模块
├── absforms/             # 各 OTP 版本（21-29）的 absform XML/md，供 syntax schema 生成
├── scripts/              # escript 工具与 sandbox 脚本
├── docs/plans/           # 设计/实现计划（unique local macro module）
├── openspec/changes/     # OpenSpec 变更提案（local-macro / macro-passes / traverse-validator / uniform-macros）
├── benchmark/            # 宏展开与真实编译基准（escript 生成宏代码）
├── README.md / README.zh.md / changelog.md(.zh) / syn.md  # 文档
├── rebar.config          # 构建配置（erl_first_files、xref、docker_ci、profiles）
└── AGENTS.md / lessons.md  # agent 工作说明与踩坑记录
```

## 源码模块地图

### 入口与 API 层

| 模块 | 职责 |
| --- | --- |
| `astranaut.erl` | 公开 API：`map/3, map_m/3, map_m_forms/3, smap/3, reduce/4, mapfold/4, search/3` 等；`format_error/1`；错误格式统一入口 |

### 遍历与 monad 层

| 模块 | 职责 |
| --- | --- |
| `astranaut_traverse.erl` | **traverse monad**：reader 属性 + 遍历状态 + 更新追踪 + 结构化 error/warning；`map_m` 的后端；提供 `lift_m/2, scoped_state/2, scoped_state_run/2, eval/1` 等 |
| `astranaut_return.erl` | **return monad**（结果 monad）：`ok/fail/warning_ok/error_ok` 等；`to_compiler/1, from_compiler/1` 桥接编译器返回格式 |
| `astranaut_error.erl` | 错误/警告存储：pending、formatted、per-file 三种形态；`update_pos/3, update_file/2, merge/2, realize/1` |
| `astranaut_monad.erl` | 底层 monad 组合子：identity/maybe/either/reader/state/writer，组装 traverse 实现 |
| `astranaut_uniplate.erl` | 内部 uniplate/context 实现；`with_subtrees/2,3` 允许 walker 重排子树访问顺序（如 match 先访右子树） |

### Syntax 层

| 模块 | 职责 |
| --- | --- |
| `astranaut_syntax.erl` | OTP 兼容语法树助手：`type/1, subtrees/1, update_tree/2, revert/1, get_pos/1`；节点**校验** `validate_node/3` 与**规范化** `normalize/3`（校验器为不透明 token，见 README Validator 一节） |
| `astranaut_syntax_schema.erl` | 由 `src/syntax.term` 生成（见 `scripts/generate_syntax_schema.escript`）：`node_roles/1, child_specs/3` 直接分派，跨 OTP 版本 AST 差异适配 |

### Quote 层

| 模块 | 职责 |
| --- | --- |
| `astranaut_quote.erl` | `quote/1,2` parse transform：`unquote/unquote_splicing`、`_X@V` 绑定、`_A@Atom` 值绑定、pattern 中的 quote、`context`/`no_context`/`code_pos`/`debug` 选项、quote 变量编解码（`Name@astranaut_quote@Context`）与卫生性计数 |

### Macro 层（5 模块职责分离）

| 模块 | 职责 |
| --- | --- |
| `astranaut_macro.erl` | parse transform 入口与 pass 编排；分配每次编译唯一本地宏模块名（见 docs/plans）；新增 `macro_capability_unavailable` 错误 |
| `astranaut_macro_scan.erl` | **源码顺序扫描与 splice**：scan queue、passed/remaining 视图、属性扫描时的 traverse state；`map_forms_splice/3`；**按需注册 capability**——遇到第一个 `-local_macro` form（含属性宏生成的）才注册 `astranaut_macro_local` provider，否则 capability 保持 `disabled` |
| `astranaut_macro_registry.erl` | **声明与环境**：宏描述符构造、指令校验、checked registry 更新、AttributeEnv 环境解析（`-import_macro/-use_macro/-export_macro/-local_macro/-macro_options`） |
| `astranaut_macro_expander.erl` | **匹配与递归展开**：共享的属性/函数宏匹配、递归函数体展开、expansion-local traverse state、observation 协议 |
| `astranaut_macro_local.erl` | **局部宏生命周期**：声明快照、闭包冻结/发现（`closure_roots`）、callable 生成、编译计划与执行、retain 处理；接口为 `finish_attribute_pass/5`（8f48873 重构后） |

> 数据流：`macro.hrl` → `astranaut_macro:parse_transform/2` → scan(源码序属性 pass, 懒注册 local capability) → registry(环境) → expander(递归展开) → local(局部宏冻结/生成)，所有路径共用同一 expander。无 `-local_macro` 声明的模块从不加载/初始化 local provider（构建中移除 `astranaut_macro_local.erl` 不影响 import/export macro 展开）。

### 其它 parse transform

| 模块 | 职责 |
| --- | --- |
| `astranaut_do.erl` | `do([ Monad || ... ])` 记法（`do.hrl` 引入） |
| `astranaut_rebinding.erl` | `-rebinding_all/-rebinding_fun` 变量重绑定（pattern 变量重命名、pin 语法 `+Var`） |
| `astranaut_struct_transformer.erl` | `-astranaut_struct` record → struct map 转换（`'__struct__'`、enforce_keys、non_auto_fill） |
| `astranaut_struct.erl` / `astranaut_struct_record.erl` | struct 运行期 API（from_record/to_record/from_map/update）与 record 元数据 |
| `astranaut_compile_meta_transformer.erl` | 编译元数据注入（`compile_meta.hrl`） |
| `astranaut_compile_opts.erl` | 编译选项收集（`compile_opts.hrl`） |
| `astranaut_disable_tco.erl` | 禁用 TCO 的 parse transform（`disable_tco.hrl`） |

### 工具层

| 模块 | 职责 |
| --- | --- |
| `astranaut_lib.erl` | 共享工具：`abstract_form/1, gen_function/2, gen_exports/2, compile_forms/2, load_forms/reload_forms`（模块锁 + soft_purge）、`analyze_forms_*/2, with_attribute/5, validate/2, option_map/1`、AST 字符串化 |
| `astranaut_forms.erl` | form 排序与插入：`reorder_updated_forms/1, sort_forms/1, insert_forms/2`、`__original__` 生成函数合并 |
| `astranaut_macros.erl` | 内部宏定义集（被内部模块 `-use_macro` 使用） |

## include/ 头文件

| 头文件 | 用途 |
| --- | --- |
| `quote.hrl` / `macro.hrl` / `rebinding.hrl` / `struct.hrl` / `do.hrl` / `disable_tco.hrl` | 各 parse transform 的启用开关（`-compile({parse_transform, ...})`） |
| `astranaut.hrl` | 汇总引入 quote + macro |
| `compile_meta.hrl` / `compile_opts.hrl` | 编译期元数据/选项注入 |
| `astranaut_struct_name.hrl` | 内部 monad/返回结构键名（`?TRAVERSE_M, ?RETURN_OK, ?WALK_RETURN, ?STRUCT_KEY`），仅项目内部使用 |
| `otp_vsn.hrl` | OTP 版本宏（`?ASTRANAUT_OTP_VSN_GE/1`） |

## 测试地图（test/）

| SUITE | 覆盖 | 用例数 |
| --- | --- | --- |
| `astranaut_SUITE.erl` | traverse API 集成（map/reduce/mapfold/search） | 41 |
| `astranaut_syntax_SUITE.erl` | syntax 校验/规范化/OTP 版本适配 | 121 |
| `astranaut_macro_local_SUITE.erl` | 局部宏状态机/闭包/编译生命周期 | 69 |
| `astranaut_quote_SUITE.erl` | quote/unquote/绑定/卫生性 | 75 |
| `astranaut_macro_pass_SUITE.erl` | macro pass 集成（属性顺序、作用域、错误） | 43 |
| `astranaut_struct_SUITE.erl` | struct 转换（含失败夹具 *_fail.erl） | 38 |
| `astranaut_design_SUITE.erl` | 设计约束回归 | 37 |
| `astranaut_uniplate_SUITE.erl` | uniplate 遍历内核 | 36 |
| `astranaut_macro_SUITE.erl` | 宏基础功能 | 35 |
| `astranaut_rebinding_SUITE.erl` | 重绑定规则 | 29 |
| `astranaut_macro_uniform_SUITE.erl` | 属性/函数宏统一展开 | 24 |
| `astranaut_traverse_SUITE.erl` | traverse monad | 22 |
| `disable_tco_SUITE.erl` | TCO 禁用 | 22 |
| `astranaut_macro_scan_SUITE.erl` | scan-and-splice 循环（含无 local capability 用例） | 17 |
| `astranaut_macro_error_SUITE.erl` | 宏错误路径 | 17 |
| `astranaut_error_SUITE.erl` | 错误存储/合并/格式化 | 9 |
| `astranaut_forms_SUITE.erl` | form 排序/合并 | 6 |

辅助：`astranaut_test_lib.erl`（编译测试夹具的公共工具）；各 `*_data/` 目录存放被编译的夹具模块（`macro_pass_*_test.erl` 为宏 pass 错误/警告场景夹具）。

## 脚本与工具（scripts/）

| 脚本 | 用途 |
| --- | --- |
| `generate_syntax_schema.escript` | 从 `src/syntax.term` + `absforms/` 生成 `astranaut_syntax_schema.erl` |
| `check_syntax_schema.escript` | 校验 schema 与真实 OTP absform 一致 |
| `fetch_absforms.sh/.ps1` | 拉取各 OTP 版本 absform 样本 |
| `cover_report.escript` | 覆盖率报告 |
| `rebar3_sandbox.ps1` + `codex_inetrc` | Codex 沙箱专用包装（见 AGENTS.md） |

## 文档与设计记录

- `docs/plans/` — unique-local-macro-module 的设计与分步实现计划（2026-08-01，已实现）
- `openspec/changes/` — OpenSpec 变更提案：`macro-passes`（源码序 pass 重构）、`local-macro`、`uniform-macros`（属性/函数宏统一）、`traverse-validator`（节点校验器）
- `lessons.md` — 开发踩坑记录（monad 桥接、scoped_state、scan-splice 等），改 macro 层前必读
- `README.md`（1276 行，中文版 `README.zh.md` 727 行）— 完整 API 文档
- `changelog.md` / `changelog.zh.md` — 变更日志

## 未提交增补（以 revision 8f48873 为准的增量）

> 以下内容存在于工作区但尚未提交，不在上文的已提交快照内。提交后应合并进正文并更新 Revision。

- **`luna.md` / `luna_web.md`（?? 未跟踪）**：根目录新增文档，未纳入上述文档章节。（`luna_max.md`/`luna_med.md` 已不存在于工作区）
- **`codebase-map.md`（?? 未跟踪）**：本文档本身。

## 关键架构要点

1. **双 monad 桥接**：`traverse_return/1`（非 `lift_m/2`）用于把 return monad 结果桥进 traverse monad；需要隔离 State 时用 `scoped_state/2`（见 lessons.md）
2. **源码序即语义**：macro 属性 pass 从左到右扫描、生成 form 插回队列当前位置；`import/use/macro_options` 只影响其后的 form
3. **卫生性**：quote 变量编码 `Name@astranaut_quote@Context@Counter`，每次展开唯一命名空间；`no_context` 可故意碰撞
4. **validator 不透明**：`Attr.validator` 是位置契约 token，walk 回调校验替换节点时必须原样传递
5. **本地宏唯一模块**：每次 parse transform 分配 VM 唯一模块名编译局部宏，并发编译无需共享名协调（跨模块锁定已移除）
6. **local-macro 为可选 capability**（8f48873）：scanner 懒注册 provider，无 `-local_macro` 的模块不加载 local 代码；capability 状态存于 `state.capability`（`disabled | #{provider, state}`）
7. **schema 生成**：`astranaut_syntax_schema` 由脚本从 `syntax.term` 生成，`absforms/` 提供跨 OTP 版本（21-29）校验样本
