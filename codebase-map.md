# Astranaut 全仓地图

> 调研日期：2026-08-05
> 基准版本：`3b7d405` / `3b7d405074063f3d7c9c53457287abcfb7773e7a`
> 提交主题：`update codebase map for local macro capability refactor`
> 发布版本：`0.13.0` · Erlang/OTP 21+ · MIT
> 调研边界：正文只描述上述已提交快照；工作区差异单列于文末「未提交增补」。

这是一份面向维护者和后续 agent 的导航文档：先说明系统为什么存在，再给出模块职责、调用方向、关键状态与不变量、测试落点、生成物边界和已知文档漂移。历史版原样保存在 [`codebase-map.2026-08-05.3b7d405.md`](codebase-map.2026-08-05.3b7d405.md)。

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
| tracked files | 219 |
| `src/*.erl` | 25 个，13,355 行 |
| `include/*.hrl` | 11 个 |
| Common Test suite | 17 个 |
| suite `all/0` 声明用例 | 422 个 |
| 测试 Erlang 文件 | 116 个（17 suite + 99 helper/fixture） |
| 测试 Erlang 代码 | 12,404 行 |
| OpenSpec 文件 | 19 个，4 组 change |
| README | 英文 1,276 行；中文 727 行 |

统计由 `git ls-tree`、`git show HEAD:<path>` 和 suite `all/0` 得出，不包含工作区未提交文件。

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
│  └─ astranaut_macro_local          可选 local capability 生命周期
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
| `astranaut_return.erl` | 结果 monad | `ok/fail/warning_ok/error_ok`；`from_compiler/1`、`to_compiler/1` 连接 Erlang compiler 返回格式 |
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
| `scripts/check_syntax_schema.escript` | 对照当前 OTP 检查 schema；`syn.md` 记录审计背景 |

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
| `astranaut_macro_registry.erl` | external/export/use/import/options 校验；macro descriptor；冲突/override；AttributeEnv；时点/最终 MacroEnvironment | 不执行宏，不含 local 专属分支 |
| `astranaut_macro_expander.erl` | attribute/function 统一匹配与调用；inner/outer 递归；depth；AST role 校验；call analysis；observation protocol | 不拥有 source queue 或 local generation |
| `astranaut_macro_local.erl` | local declaration 校验、closure 冻结、canonical expansion、依赖调度、临时模块编译、retain、finalize | 不改变通用 external expansion 规则 |
| `astranaut_macros.erl` | 项目内部导出的 `literal/1` macro provider | 不是框架编排模块 |

宏主流程：

```text
parse_transform(Forms, CompileOpts)
→ 分配 invocation-unique local module 名
→ registry 初始化 external 环境
→ attribute scan（从左到右）
   ├─ import/use/macro_options：更新其后可见环境
   ├─ 普通 attribute：记录到 AttributeEnv
   ├─ attribute macro：展开并把结果 splice 回当前 queue
   └─ 首个 local_macro：注册 local capability，冻结声明时上下文与闭包
→ local capability 完成 attribute pass / 生成 callable / 计算 skip 集合
→ registry 构造 final macro environment
→ expander 对最终 function tasks 做一次源码序批量展开
→ forms 排序、__original__/spec 合并、诊断转 compiler 格式
```

核心状态/不变量：

- `MacroEnvironment = #{macro_map, macro_options, function_call_analysis?}`；attribute 调用、declaration snapshot 与 final function context 形状一致，只是快照时点不同。
- local closure 只沿直接本地 call 静态发现；`fun helper/1`、动态 dispatch、`apply/3` 必须用 `closure_roots` 补充。
- 同一 `-local_macro([FAs])` 的成员共享声明快照，但成员间调用仍是普通 Erlang call；扫描后不保留 group identity。
- frozen form 首次完整展开建立 canonical result 与 observed local whitelist；不同环境重放必须得到相同 canonical AST 和 whitelist，否则显式报冲突。
- `NeedCallable` 触发按 declaration 顺序的最小累计 compilation boundary；compiler 只消费 canonical forms，不在编译边界再次做 request-specific expansion。
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
| `astranaut_lib.erl` | AST 构造/位置、forms 分析、选项验证、compile/load/reload、模块锁与 soft purge、安全字符串化等共享工具 |

## 目录与事实源

| 路径 | 内容 | 修改注意 |
| --- | --- | --- |
| `src/` | 25 个生产模块、schema 输入 | `astranaut_syntax_schema.erl` 是生成物 |
| `include/` | 用户启用入口与内部宏 | `struct.hrl` 会串联 macro transform |
| `test/` | CT suites、编译夹具、测试 helper | 大量 `*_data` 是被测试动态编译的输入，不是独立 suite |
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
- `rebar3 compile` 构建，`rebar3 ct` 跑 Common Test，post hook 自动执行 cover；xref 检查 undefined、deprecated 和 unused locals。
- Cirrus CI 覆盖 OTP 21、23–29；Travis 配置覆盖 21–28（包括 22）；`docker_ci` 插件配置抽样 21/23/25/28/29 并运行 xref、不跑 dialyzer。三处矩阵不同，不应概括为同一个列表。
- Windows Codex app sandbox 下轻量诊断可走 `scripts/rebar3_sandbox.ps1`；compile/ct 的 junction helper 需要直接在 sandbox 外运行。完整规则见 `AGENTS.md`。

## 测试地图

下表是各 suite 的 `all/0` 精确声明数，总计 422；不把 `_data` 编译夹具误算成测试用例。

| SUITE | 用例 | 主要覆盖 |
| --- | ---: | --- |
| `astranaut_syntax_SUITE` | 72 | node role/slot、validate/normalize、OTP AST 兼容、schema 对称性 |
| `astranaut_quote_SUITE` | 73 | quote/unquote、pattern/type、binding、位置、codec/context、诊断 |
| `astranaut_macro_pass_SUITE` | 37 | 源码序 pass、generated forms、closure、attribute buffer、final context |
| `astranaut_macro_local_SUITE` | 36 | closure 冻结、canonical cache、dependency boundary、retain、capability callbacks |
| `astranaut_SUITE` | 30 | 公开遍历 API、walk return、forms 顺序、validator 集成 |
| `astranaut_macro_SUITE` | 27 | 宏基础、递归、guard、quote hygiene |
| `astranaut_design_SUITE` | 21 | 公开契约、模块加载/锁、compile meta/opts、monad 边界 |
| `astranaut_uniplate_SUITE` | 20 | map/reduce/mapfold、context、非法节点/重建失败 |
| `astranaut_macro_uniform_SUITE` | 19 | external/local 统一环境、override、递归深度、validator |
| `astranaut_struct_SUITE` | 19 | struct API、transform、enforce/unknown/missing fields |
| `astranaut_rebinding_SUITE` | 18 | comprehension、control flow、map/record、pin/作用域 |
| `astranaut_traverse_SUITE` | 14 | traverse monad、位置/文件、State、scoped state |
| `astranaut_macro_scan_SUITE` | 12 | queue/splice/state、generated merge、无 local capability 路径 |
| `astranaut_macro_error_SUITE` | 11 | warnings/errors、错误格式、局部声明与 sibling errors |
| `astranaut_error_SUITE` | 6 | error state 累积与读取 |
| `disable_tco_SUITE` | 5 | TCO 变换及嵌套控制流 |
| `astranaut_forms_SUITE` | 2 | form 排序与 `__original__` 合并 |

测试导航：

- 改通用 traversal：先看 `astranaut_SUITE`、`astranaut_traverse_SUITE`、`astranaut_uniplate_SUITE`。
- 改 AST schema/validator：先看 `astranaut_syntax_SUITE`，再跑 schema check。
- 改宏：按职责选择 scan/registry/expander/local suite，并补 pass/uniform/error 集成覆盖。
- 改 parse transform 的 compiler 返回/类型契约：同时看 `astranaut_design_SUITE`。

## 设计文档与实现状态

四组 OpenSpec change 的 tasks 当前均为已勾选状态：

| Change | 已落地主题 |
| --- | --- |
| `uniform-macros` | external/local 统一匹配环境、递归 expansion、冲突与 depth |
| `traverse-validator` | opaque validator、slot contract、opt-in validation、macro 错误归因 |
| `macro-passes` | 两阶段源码序扫描、统一 RuntimeContext、ExpansionValidator、canonical compiler boundary |
| `local-macro` | declaration snapshot、closure/retain、cache/whitelist、optional capability isolation 与热路径优化 |

`openspec/changes/macro-passes/Hierarchy_final.md` 是宏架构最完整的设计层级；`lessons.md` 是实现操作层面的反例与约束。两者与源码冲突时，以当前源码和回归测试为事实，以文档记录设计意图并标记差异。

## 已知风险与文档漂移

以下是本轮把已提交 README 与实现对照后发现的维护风险，不代表实现缺陷：

1. README 开头的 traversal 签名仍写成 `astranaut_traverse:map/reduce/...`，实际公开函数在 `astranaut`；同段旧类型名也与当前导出类型有偏差。
2. README 的 Rebinding 与 Struct 用法仍引用 `erlando/include/...`，仓库实际头文件属于 `astranaut/include/...`。
3. README Macro 概览中 `astranaut_macro_local` 的职责句重复了一次。
4. 英文 README 1,276 行而中文 README 727 行，validator、local capability、quote context 等近期内容并非完全对齐。
5. `.cirrus.yml`、`.travis.yml` 与 `docker_ci` 的 OTP 矩阵各不相同；改变“支持版本”时需要同时判断产品承诺与各 CI 层目的。
6. `astranaut_syntax_schema.erl` 很大但不可直接优化；任何结构修改必须回到 `syntax.term`、生成脚本和 absform 审计。
7. macro local 的正确性由声明时快照、canonical result、whitelist、compilation boundary 四组状态共同保证；绕过 provider callback 直接改 scanner/registry 很容易破坏事务边界。

## 旧版与本轮调研对比

| 维度 | 历史版 | 本轮改进 |
| --- | --- | --- |
| revision | 头部写 `8f48873` 且未给 full hash；实际文件位于 `3b7d405` | 同时记录 short/full hash、主题，并明确它是调研基准 |
| 源码规模 | 写成 27 个模块 | 从 committed tree 核验为 25 个 `.erl`、13,355 行 |
| 测试规模 | “约 19 个、600+”，各 suite 数偏高 | 核验为 17 suite、`all/0` 422 个，并逐 suite 列数 |
| 架构表达 | 模块职责表为主 | 增加编译期/运行期边界、依赖层级、遍历与宏数据流、状态不变量 |
| 事实源 | 提到 schema 与设计文档 | 明确生成物、源数据、审计脚本、OpenSpec 已落地状态 |
| 构建/CI | 把 CI 概括成一个版本列表 | 分开 Cirrus、Travis、docker_ci 三套矩阵 |
| 可维护性 | 缺少“改哪里、测哪里” | 增加测试导航、风险区和文档漂移 |
| 工作区隔离 | 已采用“正文/增补”结构 | 延续该结构，并确保正文统计全部来自 `HEAD` |

历史版最有价值的内容——宏模块职责、源码序语义、local capability、validator 不透明性和 lessons 中的 monad 约束——均被保留；错误统计、模糊边界和缺失导航则由本轮独立核验修正。

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

> 以下内容存在于调研时的工作区，但不属于基准 revision `3b7d405`，因此没有进入上文事实统计。

- `AGENTS.md`（`M`）：新增 Codebase Map Maintenance 规则，要求正文基于 committed snapshot、工作区变更单列、地图独立提交。本次调研遵循该规则。
- `luna.md`、`luna_web.md`（`??`）：未跟踪调研/外部检索文档；不属于项目已提交文档集。
