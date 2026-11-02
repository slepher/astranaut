# Astranaut 全仓地图

> 调研日期：2026-11-02
> 基准版本：`ba34758` / `ba34758b062017787c7f583c98343de07bd2ae76`
> 提交主题：`Fix macro closure analysis and displaced import cleanup`
> 发布版本：`0.13.11` · Erlang/OTP 21+ · MIT
> 调研边界：正文只描述上述已提交快照；工作区差异单列于文末「未提交增补」。

这是一份面向维护者和后续 agent 的导航文档：先说明系统为什么存在，再给出模块职责、调用方向、关键状态与不变量、测试落点、生成物边界和已知文档漂移。本轮以旧基准 `78683bf` 到 `ba34758` 的已提交差异增量更新。

## 一页结论

Astranaut 是一个无第三方依赖的 Erlang 元编程库。它以 Erlang abstract format 为共同数据面，在其上提供：

1. AST 遍历、变换、状态累积和结构化诊断；
2. Quote/Unquote 与变量卫生性；
3. 源码序、递归、支持局部闭包的编译期宏；
4. Rebinding、Struct、do 记法、禁用 TCO、编译元数据等 parse transform；
5. 跨 OTP 21–29 的 AST schema、校验和规范化适配。

系统最重要的边界是：

- `astranaut` 是公开遍历门面，`astranaut_uniplate` 是递归内核，`astranaut_traverse` 是带 reader/state/writer/diagnostic 的执行上下文。
- `astranaut_return` 与 `astranaut_traverse` 是两种不同结果形状；桥接必须走 `astranaut:traverse_return/1`，局部 State 隔离优先走 `scoped_state/2`。
- 宏不是“一次全树替换”，而是“源码序 attribute scan-and-splice + final function pass”。声明、生成 form 和环境更新的相对位置属于语义。
- `astranaut_macro_local` 是扫描器遇到首个 `-local_macro` 后才注册的可选 capability；普通 external macro 路径不初始化它。
- `astranaut_syntax_schema.erl` 是生成文件，事实源是 `src/syntax.term` 与 `absforms/`，不能手改生成结果。

## 仓库快照

| 指标 | 已提交快照 |
| --- | ---: |
| tracked files | 291 |
| `src/*.erl` | 26 个，15,077 行 |
| `include/*.hrl` | 11 个 |
| EUnit | 9 个模块，157 个不同测试入口 |
| Common Test | 10 个 suite，`all/0` 声明 282 个用例 |
| OpenSpec 文件 | 30 个，6 组 change |
| README | 英文 1,320 行；中文 728 行 |

统计由 `git ls-tree`、`git show HEAD:<path>` 、EUnit `_test/0` / `_test_/0` 入口与 suite `all/0` 得出（同名 OTP 条件编译分支只计一次），不包含工作区未提交文件。

## 系统边界与执行时机

### 编译期

八个头文件可启用 parse transform：

| 头文件 | transform | 作用 |
| --- | --- | --- |
| `quote.hrl` | `astranaut_quote` | 把 quote 语法编译成 AST 构造代码 |
| `macro.hrl` | `astranaut_macro` | 扫描声明并展开属性宏/函数宏 |
| `rebinding.hrl` | `astranaut_rebinding` | 重写变量绑定与 pin 语义 |
| `struct.hrl` | `astranaut_struct_transformer` | record 语法转 struct map，并引入 struct macros |
| `do.hrl` | `astranaut_do` | 展开 monadic `do([...])` |
| `disable_tco.hrl` | `astranaut_disable_tco` | 对选定尾调用插入不可尾调包装 |
| `compile_meta.hrl` | `astranaut_compile_meta_transformer` | 收集变换后 forms、errors、warnings |
| `compile_opts.hrl` | `astranaut_compile_opts` | 生成编译选项查询函数 |

`astranaut.hrl` 只是 `quote.hrl` 与 `macro.hrl` 的聚合入口；`otp_vsn.hrl` 和 `astranaut_struct_name.hrl` 是内部兼容/结构键定义。

### 运行期

遍历 API、AST helper、struct 转换 helper 和 monad 模块可在普通运行期调用。宏函数本身在编译期接收 AST 或 attribute term；生成后的业务函数不依赖宏展开器继续运行。局部宏会被编译进每次 parse-transform invocation 唯一命名的临时模块，生命周期只服务当前编译流程。

## 总体调用层级

```text
用户模块 / 其他 parse transform
├─ astranaut                         公开遍历门面
│  ├─ astranaut_uniplate             子树投影、递归顺序、重建
│  ├─ astranaut_traverse             reader + state + updated + diagnostics
│  │  ├─ astranaut_monad             底层 monad 组合子
│  │  └─ astranaut_error             诊断累积与定位
│  ├─ astranaut_return               对外结果与编译器结果桥接
│  ├─ astranaut_syntax               AST 适配、validator、normalize
│  │  └─ astranaut_syntax_schema     生成的 OTP schema 分派
│  └─ astranaut_forms                module forms 排序与 splice 合并
├─ astranaut_quote                   quote/unquote 与卫生变量 codec
├─ astranaut_macro                   宏两阶段 orchestrator
│  ├─ astranaut_macro_scan           源码序 attribute queue
│  ├─ astranaut_macro_registry       声明、选项、时点环境
│  ├─ astranaut_macro_expander       统一匹配、递归展开、观察协议
│  └─ astranaut_macro_local          首次绑定、闭包与展开调度
│     └─ astranaut_macro_local_compile 唯一临时模块、BEAM 与 generation
└─ 其他 transforms
   ├─ astranaut_rebinding            自定义子树访问顺序 + 变量状态
   ├─ astranaut_struct_transformer   record 定义/表达式改写
   ├─ astranaut_do / disable_tco
   └─ compile_meta_transformer / compile_opts
```

依赖方向总体由上到下；macro 子系统复用通用 traverse/return/syntax/forms/lib，但通用层不反向依赖 local macro 策略。

## 模块地图

### 遍历门面与内核

| 模块 | 公开/内部职责 | 关键入口或约束 |
| --- | --- | --- |
| `astranaut.erl` | 公开门面 | `map/reduce/mapfold/search` 返回 `astranaut_return`；`s*` 版本直接返回值；`map_m/3` 保持普通列表顺序；`map_m_forms/3` 才做 module-form 收尾 |
| `astranaut_uniplate.erl` | 内部递归内核 | `map_m/5`；`with_subtrees/2,3` 可改变访问顺序并提供逆向重建，不泄漏为通用 AST 语义 |
| `astranaut_traverse.erl` | traverse monad | Reader 是 traversal attr，State 是 walker state，Writer 追踪 changed，另携 errors/warnings/file/formatter；`scoped_state/2` 隔离嵌套状态 |
| `astranaut_return.erl` | 结果 monad | `ok/fail/warning_ok/error_ok`；`from_compiler/1`、`to_compiler/1` 将内部 diagnostics 包装为 `astranaut_lib` compiler adapter |
| `astranaut_error.erl` | 诊断存储 | pending、formatted、per-file 三种阶段；`update_pos`/`update_file` 只补足尚未格式化的上下文 |
| `astranaut_monad.erl` | 组合子基础 | identity/maybe/either/reader/state/writer 及组合 monad；不是业务入口 |
| `astranaut_forms.erl` | forms 结构收尾 | 排序、生成 form 插入、冲突函数改名为 `__original__` 语义并合并 spec |

遍历数据流：

```text
map/reduce/mapfold
→ 规范化 options / 推断 root role
→ uniplate 投影当前节点 child groups
→ walker 返回普通值、walk_return、return monad 或 traverse monad
→ traverse 统一位置、formatter、state、updated 与诊断
→ syntax validator（按 validate 模式）
→ return monad / compiler tuple
```

`validate` 默认为关闭，但 validator 元数据仍沿 Attr 传播。`true` 等价于 output；还可选 `input | output | both`。walker 直接替换节点时才触发 output validation；仅因 child 改动而重建的 ancestor 不重复校验。

### Syntax 与跨 OTP 适配

| 模块/数据 | 职责 |
| --- | --- |
| `astranaut_syntax.erl` | 对 `erl_syntax` 的兼容边界；特殊处理 type/spec/callback、record field、typed record field；公开当前节点校验和递归 normalize |
| `astranaut_syntax_schema.erl` | 根据 node type/OTP/role/slot 直接分派节点可用性、child layout 和 slot contract；生成文件 |
| `src/syntax.term` | schema 的项目内事实源 |
| `absforms/absform-21..29` | 各 OTP abstract format 参考输入；21–26 为 XML，27–29 为 Markdown |
| `scripts/generate_syntax_schema.escript` | 从事实源生成 schema 模块 |
| `scripts/check_syntax_schema.escript` | 对照当前 OTP 检查 schema |

validator 是 `{role, Role}` 或 `{slot, ParentType, Slot, Role}` 的位置契约，对普通 walker 应视为不透明 token。`validate_node` 只检查当前位置；`normalize` 会递归规范化子树。

### Quote

`astranaut_quote.erl` 同时承担 parse transform、低层 `quoted/1,2` API、quote value/binding 编解码和诊断格式化：

- `quote` 支持 expression、pattern、type/spec/callback 等 abstract forms；
- `unquote` 注入单节点，`unquote_splicing` 注入节点列表；
- `_X@V` 注入 AST，`_A@Atom` 等 value binding 构造对应字面节点；
- 默认把变量编码为 `Name@astranaut_quote@Context`；函数宏展开追加 counter，属性宏不追加；
- `context` 必须是非空 atom，`no_context` 可故意关闭卫生命名，两者互斥；
- codec 对 `@`、`%` 转义，由 quote 与 macro expander 共享。

### Macro 子系统

| 模块 | 独占职责 | 不负责什么 |
| --- | --- | --- |
| `astranaut_macro.erl` | parse transform 入口、默认选项、attribute pass 与 function pass 编排、最终 compiler 结果 | 不保存扫描队列或 local closure 细节 |
| `astranaut_macro_scan.erl` | `Queue/Output/PassedForms/Registry/Capability`；源码序 scan-and-splice；生成 forms 回插当前位置 | 不解释宏声明语义或递归展开 |
| `astranaut_macro_registry.erl` | external/export/use/import/options 校验；macro descriptor；冲突/override；AttributeEnv；时点/最终 MacroEnvironment；按 source compilation 去重缺失 formatter warning | 不执行宏，不含 local 专属分支 |
| `astranaut_macro_expander.erl` | attribute/function 统一匹配与调用；inner/outer 递归；depth；AST role 校验；call analysis | 不拥有 source queue 或 local generation |
| `astranaut_macro_local.erl` | local declaration 校验、closure 冻结、首次环境绑定、canonical expansion、依赖调度、retain、finalize | 不改变通用 external expansion 规则 |
| `astranaut_macro_local_compile.erl` | 唯一临时模块名、编译输入组装、BEAM 编译加载、callable version 与 generation 提交、被移除函数的编译属性清理 | 不拥有声明快照和展开调度 |
| `astranaut_macros.erl` | 项目内部导出的 `literal/1` macro provider | 不是框架编排模块 |

宏主流程：

```text
parse_transform(Forms, CompileOpts)
→ registry 初始化 external 环境
→ attribute scan（从左到右）
   ├─ import/use/macro_options：更新其后可见环境
   ├─ 普通 attribute：记录到 AttributeEnv
   ├─ attribute macro：展开并把结果 splice 回当前 queue
   └─ 首个 local_macro：注册 local capability、分配唯一临时模块名，冻结声明时上下文与闭包（不立即编译）
→ local capability 完成 attribute pass / 生成 callable / 计算 skip 集合
→ registry 构造 final macro environment
→ expander 对最终 function tasks 做一次源码序批量展开
→ forms 排序、__original__/spec 合并、诊断转 compiler 格式
```

核心状态/不变量：

- `MacroEnvironment = #{macro_map, macro_options, function_call_analysis?}`；attribute 调用、declaration snapshot 与 final function context 形状一致，只是快照时点不同。
- local closure 只沿直接本地 call 静态发现；`fun helper/1`、动态 dispatch、`apply/3` 必须用 `closure_roots` 补充。
- 同一 `-local_macro([FAs])` 的成员共享声明快照。已匹配宏的调用不作为普通本地函数闭包边；显式 `closure_roots` 补充静态闭包。
- frozen form 永久绑定首次包含它的声明环境，在实际需要编译时只展开一次；共享 helper、retain/export 函数复用 canonical form，不再做多环境重放或 whitelist 比较。
- `need_callable` 按需触发最小累计编译边界；无宏调用的独立声明共享编译，带宏调用的闭包先确保前序本地宏就绪。编译失败不提交 canonical/generation。
- local capability 状态是 `disabled | #{provider, state}`。无 local declaration 的项目即使不编译 `astranaut_macro_local.erl`，external macro 主路径仍成立。
- 源码序控制环境何时可见；宏选项 `order = inner | outer` 只控制单次嵌套调用的递归顺序，两者不可混淆。

### 其他 parse transform 与运行期支持

| 模块 | 职责 |
| --- | --- |
| `astranaut_rebinding.erl` | 按 function/match/comprehension 等作用域重命名重复绑定变量；`+Var` 表示 pin；借 `with_subtrees` 让 match 等节点按求值需要访问 child |
| `astranaut_struct_transformer.erl` | 读取 `-astranaut_struct` 与 record 定义，重写 record construction/access/update/type，生成 struct metadata |
| `astranaut_struct_record.erl` | record/struct 字段、默认值、类型、warning、enforce_keys、auto_fill 的数据层 |
| `astranaut_struct.erl` | 导出 `from_record/to_record/from_map/update/from_other_record` macros 及实际运行期转换 helper |
| `astranaut_do.erl` | 把 `do([Monad || generators/expressions])` 转成 bind/return 链，并保持错误位置 |
| `astranaut_disable_tco.erl` | 分析 case/if/receive/try/block/boolean/maybe 等尾位置；只阻断目标调用，保留直接/互递归与 named-fun 的 TCO |
| `astranaut_compile_meta_transformer.erl` | 从 transform 后 forms 和 compiler diagnostics 生成元数据函数；插入前移除自身 transform 属性 |
| `astranaut_compile_opts.erl` | 生成导出的 `compile_opts/0` |
| `astranaut_lib.erl` | AST 构造/位置、forms 分析、选项验证、compile/load/reload、模块锁与 soft purge、安全字符串化；compiler `format_error/1` adapter、shared formatter fallback |

## 目录与事实源

| 路径 | 内容 | 修改注意 |
| --- | --- | --- |
| `src/` | 26 个生产模块、schema 输入 | `astranaut_syntax_schema.erl` 是生成物 |
| `include/` | 用户启用入口与内部宏 | `struct.hrl` 会串联 macro transform |
| `test/` | EUnit 单元测试、共享 helper 和少量编译夹具 | `*_tests.erl` 由 EUnit 自动发现 |
| `common_tests/` | 10 个 CT suites 及各自 `*_data` 集成夹具 | suite 生命周期、动态编译和跨模块行为 |
| `test_data/` | EUnit AST 夹具 | 不计入测试入口数量 |
| `absforms/` | OTP 21–29 参考 AST | 与 schema 脚本共同维护 |
| `openspec/changes/` | local-macro、macro-passes、traverse-validator、uniform-macros 的提案/设计/spec/tasks | tasks 均记录历史演进，不等于新的 runtime 层 |
| `docs/plans/` | unique local macro module 的设计与实施计划 | 当前实现已完成，属于设计记录 |
| `benchmark/` | 约 2,000 行 macro workload 与约 5,000 行脱敏 Erlando do workload | 生成器负责重建大文件；runner 测 transform 与 full compile |
| `scripts/` | schema 生成/检查、absform 获取、cover、Windows sandbox wrapper | schema 变更应走脚本 |
| `lessons.md` | monad bridge、State 串联、diagnostic boundary、scan/splice 等高风险经验 | 修改 traverse/macro 前必读 |
| `README*.md` | 用户 API 与示例 | 英文版明显更完整；见下方漂移 |

## 构建、依赖与 CI

- `rebar.config` 的 `{deps, []}` 表明无第三方 Hex/Git 依赖；OTP applications 为 `kernel, stdlib, compiler, syntax_tools`。
- `erl_first_files` 固定核心 monad → traversal → syntax → quote → macro 的编译顺序；optional local provider 已移出该列表。
- `rebar3 compile` 构建，`rebar3 eunit` 与 `rebar3 ct` 分别运行两类测试，CT post hook 执行 cover；`scripts/cover_report.escript` 合并 EUnit 和 CT 覆盖数据。
- GitHub Actions 的 CI 与 release workflow 使用 OTP 21–29，运行 EUnit 和 CT；`docker_ci` 同样配置 OTP 21–29 并启用 EUnit。当前快照不含旧 Cirrus/Travis 配置。
- `scripts/rebar3_sandbox.ps1` 保留 Windows wrapper；项目操作约束以当前 `AGENTS.md` 为准。

## 测试地图

EUnit 按不同测试入口名计数，CT 按 `all/0` 声明计数；不把 fixture 算作测试。

| 类型 | 模块 / Suite | 用例入口 |
| --- | --- | ---: |
| CT | `astranaut_SUITE` | 40 |
| CT | `astranaut_design_SUITE` | 21 |
| CT | `astranaut_macro_SUITE` | 28 |
| CT | `astranaut_macro_error_SUITE` | 18 |
| CT | `astranaut_macro_pass_SUITE` | 38 |
| CT | `astranaut_macro_uniform_SUITE` | 19 |
| CT | `astranaut_quote_SUITE` | 73 |
| CT | `astranaut_rebinding_SUITE` | 21 |
| CT | `astranaut_struct_SUITE` | 20 |
| CT | `disable_tco_SUITE` | 4 |
| EUnit | `astranaut_error_tests` | 6 |
| EUnit | `astranaut_forms_tests` | 2 |
| EUnit | `astranaut_macro_expander_tests` | 2 |
| EUnit | `astranaut_macro_local_tests` | 25 |
| EUnit | `astranaut_macro_registry_tests` | 3 |
| EUnit | `astranaut_macro_scan_tests` | 12 |
| EUnit | `astranaut_syntax_tests` | 72 |
| EUnit | `astranaut_traverse_tests` | 14 |
| EUnit | `astranaut_uniplate_tests` | 21 |

测试导航：通用遍历改动看 `astranaut_SUITE` 与 error/traverse/uniplate EUnit；schema 改动看 syntax EUnit 和 schema check；宏改动按 scan/registry/expander/local EUnit 定位，再跑 pass/uniform/error CT；公共契约看 `astranaut_design_SUITE`。

## 设计文档与实现状态

六组 OpenSpec change 的实现主题如下：

| Change | 已落地主题 |
| --- | --- |
| `uniform-macros` | external/local 统一匹配环境、递归 expansion、冲突与 depth |
| `traverse-validator` | opaque validator、slot contract、opt-in validation、macro 错误归因 |
| `macro-passes` | 两阶段源码序扫描、统一 RuntimeContext、ExpansionValidator、canonical compiler boundary |
| `local-macro` | 首次绑定、按需展开、canonical 复用、最小累计编译、compiler 模块隔离 |
| `transform-error` | compiler diagnostic adapter、纯 `/1` formatter、缺失 macro formatter warning 与 source-compilation 去重 |
| `macro-error` | 宏错误归属权、local/external formatter 与已格式化诊断保留 |

`openspec/changes/macro-passes/Hierarchy_final.md` 是宏架构最完整的设计层级；`lessons.md` 是实现操作层面的反例与约束。两者与源码冲突时，以当前源码和回归测试为事实，以文档记录设计意图并标记差异。

## 已知风险与文档漂移

以下是本轮把已提交 README 与实现对照后发现的维护风险，不代表实现缺陷：

1. README 开头的 traversal 签名仍写成 `astranaut_traverse:map/reduce/...`，实际公开函数在 `astranaut`；同段旧类型名也与当前导出类型有偏差。
2. README 的 Rebinding 与 Struct 用法仍引用 `erlando/include/...`，仓库实际头文件属于 `astranaut/include/...`。
3. 英文宏示例仍使用 `unquote_atom(Name)`，需要更新为当前 value binding。
4. 英文 README 1,320 行而中文 README 728 行，validator、local capability、quote context 等近期内容并非完全对齐。
5. README 的功能深度仍不完全对齐；不要把中文文档的省略当作功能缺失。
6. `astranaut_syntax_schema.erl` 很大但不可直接优化；任何结构修改必须回到 `syntax.term`、生成脚本和 absform 审计。
7. macro local 的正确性由首次绑定快照、canonical result、compilation boundary 与 generation 提交共同保证；绕过 provider callback 直接改 scanner/registry 很容易破坏事务边界。

## 本轮已提交增量

从 `78683bf` 到 `ba34758`：单元测试迁到 `test/*_tests.erl`，集成测试迁到 `common_tests/`；新增 registry/expander 测试；本地宏 BEAM 编译拆到 `astranaut_macro_local_compile`；多环境重放改为首次绑定与 canonical 复用；宏闭包分析区分宏调用和普通本地调用，清理被替换导入的使用状态；README/OpenSpec 同步这些语义。以上均依据已提交差异，不包含本轮工作区修复。

## 修改入口速查

| 目标 | 首要文件 | 联动检查 |
| --- | --- | --- |
| 新增/修改 traversal option | `astranaut.erl` | `astranaut_traverse`、README、`astranaut_SUITE` |
| 改 child 投影/节点角色 | `syntax.term`、生成脚本 | 生成 schema、syntax/uniplate/quote/rebinding suites |
| 改 attribute 扫描顺序 | `astranaut_macro_scan.erl` | registry/local callbacks、scan/pass suites、`lessons.md` |
| 改宏匹配或递归 | `astranaut_macro_expander.erl` | uniform/pass/error/local suites、quote codec |
| 改 local closure/编译 | `astranaut_macro_local.erl` | local/pass suites、OpenSpec hierarchy/invariants |
| 改 macro 指令/选项 | `astranaut_macro_registry.erl` | README option scope、macro/error suites |
| 改生成 forms 排序 | `astranaut_forms.erl` | `map_m_forms`、macro scan/pass、forms suite |
| 改 compiler diagnostics | `astranaut_error/return/traverse` | design/error/traverse suites |

## 未提交增补

> 正文基于 `ba34758`；以下是本次工作区变更，不纳入正文统计。

- `README.md`、`README.zh.md`：修正遍历入口和结果类型、头文件应用名；英文宏示例改用 `_A@Name`。
- `src/astranaut_error.erl`、`astranaut_lib.erl`、`astranaut_macro.erl`、`astranaut_macro_registry.erl`、`astranaut_return.erl`、`astranaut_uniplate.erl`：前轮缺陷修复；调用方 `common_tests/astranaut_SUITE.erl`、其 `sample_transformer_1.erl` 夹具和 `test/astranaut_test_lib.erl` 同步显式结构化返回选项。
- `src/astranaut_do.erl`：保留合法的 `{error, expected_list_comprehension}` 返回形式，新增 `do_return/0` 联合类型并补错误格式化。
- `test/astranaut_review_regression_tests.erl`（未跟踪）：10 个 P0 回归测试。
- `test/astranaut_review_p1_tests.erl`（未跟踪）：13 个 P1 测试，覆盖文件解析、AST 格式/位置、forms 合并/诊断顺序、Monad 短路/Writer listen、TCO 与 rebinding 作用域。
- `docs/plans/00-codebase-and-test-review.md` 及 `01`–`05` 计划（未跟踪）：原审查及后续计划；本次只补充审查实施状态，未实施功能演进。
- `.vscode/`（未跟踪）：用户已有配置，本次未修改。
- 空目录 `src/traverse/` 已删除；Git 不追踪空目录。

本地图按 `AGENTS.md` 要求单独提交；上述源码、测试和 README 改动留在工作区。
