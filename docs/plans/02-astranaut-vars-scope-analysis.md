# Plan 02：`astranaut_vars` 变量分析与作用域引擎设计及迁移计划

## 1. 核心架构原则与模块边界

1. **`astranaut_vars` 只负责通用 Erlang 词法作用域机制，绝不混入业务规则**：
   - **从 `astranaut_vars` 中彻底剥离 `astranaut_rebinding` 的业务逻辑**：移除 `rebind => boolean()`、`clause_pinned => boolean()`、`+Var`（`{op, _, '+', {var, _, _}}`）pin 操作符以及默认 `_1` 重命名后缀等业务概念。
   - **从 `astranaut_vars` 中彻底剥离 `macro` / `quote` 的伪模式**：移除绕过作用域状态机的 `mode => names`。
   - `astranaut_vars` 的默认行为严格遵循 **标准 Erlang 词法作用域规则**（例如在 `match_left` / `clause_match` 中首次出现的变量为 `bind`，已绑定变量再次匹配或同模式重复 `{A, A}` 默认为 `use`；在 `function_clause` / `comprehension_generate` 中默认为遮蔽 `bind`）。
2. **`astranaut_vars:map_m` / `mapfold_m` 是基础原语，其余分析/改写接口均为其上层实现**：
   - 与 `astranaut:map_m` 作为 `astranaut` 的基础原语（衍生出 `map`/`reduce`/`mapfold`）一致，`astranaut_vars:mapfold_m` 与 `astranaut_vars:map_m` 是 `astranaut_vars` 的底层基础接口。
   - `rewrite/2`、`analyze/1,2` 作为 `mapfold_m` 的上层封装；分类查询统一使用 analyze 的 collect 参数。消除警告的策略由 `astranaut_lib:silence_unused_vars/1` 实现。
3. **跨模块迁移原则：按需使用 `map_m`，非单子场景使用上层纯函数接口**：
   - **`astranaut_rebinding` 必须使用 `astranaut_vars:map_m`**：
     `astranaut_rebinding` 是编译期转换 pass，需要在遍历子句时支持 `astranaut_traverse` 的错误/警告收集与文件/行号定位，并通过向 `astranaut_vars:map_m` 传入业务回调来实现 `+Var` pin 识别、`clause_pinned` 判定以及重绑定变量分配（`{bind, NewVar}` / `{use, Var}`）。
   - **`astranaut_disable_tco` 与 `astranaut_lib` 不使用 `map_m`**：
     仅需调用上层纯函数接口 `all_vars/1`、`fresh_var_asts/4`、`alloc_var_asts/3`；lib 的警告抑制使用 `analyze/2` 与 `rewrite/2`。
   - **`astranaut_macro_expander` 与 `astranaut_quote` 不使用 `astranaut_vars:map_m`**：
     回退此前将 `process_macro_return` 强塞进 `astranaut_vars:map_m(..., #{mode => names})` 以及在 `astranaut_quote` 中增加 `prepare_template/2` 预遍历的改动。

---

## 2. 状态机内核设计（三个独立维度与统一作用域帧）

### 2.1 三个独立状态维度

`#scope_state{}` 维护以下三个正交维度：

1. **`allocated`（`ordsets:ordset(final_var())`）**：当前命名空间已占用的全部最终变量名，仅用于生成新变量名时避让冲突。
2. **`renames`（`#{orig_var() => final_var()}`）**：当前求值点生效的 `[重命名前 => 重命名后]` 映射表。
   - 分支退出时恢复进入分支前已有变量的 `renames`，不将这些变量的内部重绑定泄漏到分支外；共同导出的新变量则建立显式映射。
   - 新变量的分支最终名不一致时不预留名称，也不在组退出时写入虚假的唯一映射；真正读取该导出变量时协调其到达绑定点，保留早期绑定和闭包捕获的身份。未读取的绑定允许独立改名，例如消除未使用变量警告。
   - 实际汇合优先复用调用方已生成、且不与其他绑定点冲突的最终名；没有安全候选时才使用通用 fresh_var 命名。读取节点的回调 Scope 提供即将使用的 current_var 和对应 active_map，实际动作执行读取后才更新内部映射和占用集。
   - 默认 `argument_scope => isolated` 下的 `argument` 作用域在退出参数组时合并新增的 `renames`。
3. **`env`（`#{orig_var() => ordsets:ordset(bind_id())}`）**：到达定值（Reaching Definitions）词法环境。
   - 每次 `bind` 事件分配单调递增的 `BindId :: pos_integer()`，登记 `#bind_entry{id = BindId, orig = OrigVar, final = FinalVar, count = 0}`。
   - 每次 `use` 事件：若 `OrigVar` 在 `env` 中，将其对应的所有到达 `BindId` 的引用计数 `+ 1`；若不在 `env` 中，将 `OrigVar` 记入 `free`。
   - 分支汇合（`#group_frame{}`）时，对所有分支的 `env` 按 `orig_var()` 取键交集，并对同名变量的 `BindId` 集合取并集；退出 `scope_group` 时以 `maps:merge(Exports, PreGroupEnv)` 合并（仅导出进入分支前未绑定的新变量，不覆盖进入分支前已有的外层绑定）。

### 2.2 统一作用域帧栈

`#scope_frame{kind, allocated, renames, env, pattern, pattern_vars}` 统一管理进入作用域或模式时的快照（`bindings`、`next_id` 和统计事件全局累积，不随作用域回滚；指标在最终名确定后计算）：

| 作用域类型 (`kind`) | 对应语法结构 | 退出时状态恢复与汇合规则 |
| :--- | :--- | :--- |
| `shadowed` | `fun`、`named_fun`、`lc`/`mc`、生成器源表达式 | 恢复 `allocated`、`renames`、`env` |
| `shadowed_leaky` | `bc`、`maybe` 主体、`andalso`/`orelse` 右侧、`catch`、`try` 各段 | 恢复 `renames`、`env`，保留 `allocated` |
| `nonfun_clause` | `case` / `if` / `receive`（含 `after` 超时体） / `maybe else` 子句 | 将 `allocated` 与分支 `env` 汇总至 `#group_frame{}`，恢复进入子句前的 `allocated`、`renames`、`env` |
| `argument` | 默认 `argument_scope => isolated` 下的调用/元组/列表/Map/Record/中缀表达式参数 | 参数间相互隔离 `renames`/`env`；退出时将新增 `renames` 与 `env` 汇入 `#group_frame{}`，保留 `allocated` |
| `function_clause` / `clause_match` / `match_left` / `comprehension_generate` | 各类 Pattern 作用域 | 进入时清空 `pattern_vars`（用于检测 `{A, A}` 同模式重复出现），退出时恢复外层 `pattern` 与 `pattern_vars` |

`receive` 的消息子句与超时体组成分支组；按运行顺序先遍历 timeout，再遍历分支组。两侧作为隔离的参数组成员从相同入口环境出发，名称占用跨两侧累积，导出在组退出时合并。timeout 新绑定可在 receive 之后使用，但不能供消息体或超时体读取。若原先未绑定的变量在所有分支中也被绑定，后执行的分支最终绑定优先；进入 receive 前已有的变量仍遵循分支重绑定不导出的规则。

隔离的兄弟参数若绑定相同原名且最终名也相同，合并其全部 BindId，并将共享变量的隐式匹配计入引用，防止未使用变量改写移除匹配约束。若调用方已为这些绑定生成不同最终名，则保持独立、由后一个参数的绑定向后生效；不能无条件把重绑定后的不同名字合为同一个变量。

`try` 的主体/成功子句与异常子句先汇总名称占用，再恢复入口绑定与重命名环境，随后处理 after。这样 after 能避让前面区段生成的名字，又不会读取到前面区段的新绑定。

---

## 3. 基础原语 `map_m` / `mapfold_m` 与回调协议设计

### 3.1 作用域视图 `scope_view()`（纯标准 Erlang 语义，不含业务字段）

```erlang
-type scope_view() :: #{
    %% 当前节点所处的语法角色
    role := pattern | expression | guard | binary_size,
    %% 当前所处的模式类别（非 pattern 时为 none）
    pattern_kind := function_clause | clause_match | match_left | comprehension_generate | none,
    %% 按标准 Erlang 规则推导的默认变量事件（非变量节点为 node）
    event := bind | use | node,
    %% 当前变量在当前词法作用域是否已绑定（仅变量节点提供）
    is_bound => boolean(),
    %% 当前变量是否在同一 Pattern 内已出现过（如 {A, A} 的第二个 A）
    same_pattern := boolean(),
    %% 当前变量在 active_map 中对应的当前名称（bind 时默认为 OrigVar，use 时为已映射的 FinalVar）
    current_var => final_var(),
    %% 若发生 bind 将分配的绑定点 ID
    binding_id := pos_integer(),
    %% 当前正在展开的外层变量集合（防止同名自引用无限递归）
    expanding := ordsets:ordset(orig_var()),
    %% 当前命名空间已占用的全部变量名
    allocated := ordsets:ordset(final_var()),
    %% 当前生效的 [OrigVar => FinalVar] 重命名映射
    active_map := #{orig_var() => final_var()}
}.
```

### 3.2 回调动作 `var_action()` 与单子返回支持

回调 `WalkFun(Node, Scope)`（或 `transform` 选项）不仅可以拦截 `{var, Pos, Orig}`，还可以拦截任意父节点（如 `astranaut_rebinding` 中的 `{op, Pos, '+', {var, _, _}}`）：

```erlang
-type var_action() ::
    %% 保持标准 Erlang 默认行为（变量按默认 event 记录，非变量节点正常递归子树）
    keep
    %% 显式作为变量绑定处理，将原变量绑定/重命名为 FinalVar（允许在 match_left/clause_match 中强制重绑定已绑定变量）
    | {bind, final_var()}
    | {bind, {var, erl_anno:location(), orig_var()}, final_var()}
    %% 显式作为变量读取引用处理（允许将模式中的 +Var 或 clause_pinned 变量重写为读取引用 {var, Pos, CurrentVar}）
    | {use, {var, erl_anno:location(), orig_var()}}
    | {use, {var, erl_anno:location(), orig_var()}, final_var()}
    %% 保持默认 bind/use 事件分类，仅将目标变量名改为 FinalVar
    | {rename, final_var()}
    %% 将当前节点展开为任意 AST，且不加 skip，立即挂上作用域钩子继续向下参与同一次 vars 遍历
    | {expand, astranaut:tree()}
    | final_var()
    | astranaut:tree().
```

在 `map_m` / `mapfold_m` 中，回调返回值支持完整的 `astranaut` 单子返回协议：
- 纯动作：`var_action()`
- 带诊断或外层状态更新的 `walk_return`：`{error, Reason}`、`{warning, Action, Warning}`、`astranaut:walk_return(#{return => Action, warnings => [...], errors => [...], state => NewCallerState})`
- `astranaut_traverse` 单子：`astranaut_traverse:struct(S, var_action())`

### 3.3 基础接口与上层接口的派生关系

```erlang
%% 1. 基础单子原语（底层唯一遍历实现）
-spec mapfold_m(astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(S, {astranaut:trees(), analysis_result()}).
-spec mapfold_m(scope_walk(S), astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(S, {astranaut:trees(), analysis_result()}).

-spec map_m(astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(S, astranaut:trees()).
-spec map_m(scope_walk(S), astranaut:trees(), scope_opts()) ->
    astranaut_traverse:struct(S, astranaut:trees()).

%% 2. 上层同步接口（通过 astranaut_traverse:eval 运行 mapfold_m 派生）
-spec rewrite(astranaut:trees(), scope_opts()) -> {astranaut:trees(), analysis_result()}.
-spec analyze(astranaut:trees()) -> analysis_result().
-spec analyze(astranaut:trees(), scope_opts() | [var_metric()]) -> analysis_result().

%% 3. 不解释作用域的变量名扫描
all_vars(Nodes) -> ordsets:ordset(var_name()).

%% 4. 新变量生成与占用集分配
fresh_var(Base, Scope) -> var_name().
fresh_var(Base, Scope, Opts) -> var_name().
fresh_var_asts(Base, Count, Pos, Scope) -> [var_tree()].
alloc_var_asts(Bases, Pos, Scope) -> {[var_tree()], ordsets:ordset(var_name())}.

```

### 3.4 导出范围与绑定点分析

vars 仅导出 12 个函数接口：`analyze/1,2`、`rewrite/2`、`map_m/2,3`、`mapfold_m/2,3`、`all_vars/1`、`fresh_var/2,3`、`fresh_var_asts/4`、`alloc_var_asts/3`。

- 删除无生产调用方的 `rename_var_node/2`，不保留兼容别名。
- 删除 `bound_vars/used_vars/free_vars/exported_vars` 便利封装，改用 `maps:get(Key, astranaut_vars:analyze(Nodes, [Key]))`。
- 删除 `fresh_var_ast`、公开的 `fresh_vars`、`alloc_vars` 和多余的批量 AST 重载。批量命名仅作为私有实现。
- `analyze(Nodes, [bindings])` 返回 `#{bindings => #{BindId => #{orig => Orig, final => Final, count => Count}}}`。这是通用分析事实，不暴露内部 record，也不决定哪些名字应改写；默认 collect=all 包含该指标。
- bindings 按绑定点统计，不能用按最终名字聚合的 counts 替代：同名的外层与遮蔽绑定可能分别被引用或未被引用。

### 3.5 标准参数作用域与 rebinding 的 strict 边界

- vars 不接收 rebinding 的 strict 选项。默认 `argument_scope => isolated`，兄弟参数间不传播绑定，参数组结束后再汇合；例如 `{A = 1, B = A}` 中第二个参数的 A 为自由变量，与 Erlang 编译器判定一致。
- 通用机制允许调用方显式指定 `argument_scope => sequential`，按遍历顺序传播绑定。这是非标准方言的显式覆盖，不能作为标准分析的默认值。
- rebinding 保留已有用户选项与默认行为：strict=true 映射为 isolated，strict=false（默认）映射为 sequential。选择由业务模块负责，词法引擎只执行明确指定的作用域机制。

---

## 4. 各模块迁移设计

### 4.1 `src/astranaut_rebinding.erl`（使用 `astranaut_vars:map_m`，业务规则留在本模块）

`astranaut_rebinding` 保留所有业务语义（`+Var` 识别、`clause_pinned` 选项、重绑定变量 `_1` 命名策略及错误诊断格式化），通过 `astranaut_vars:map_m/3` 执行遍历：

具体实现见 `src/astranaut_rebinding.erl` 的 `walk_function_clause/2` 与
`walk_clause_node/3`；参数作用域选择遵循 §3.5。

### 4.2 `src/astranaut_disable_tco.erl` 与 `src/astranaut_lib.erl`（不使用 `map_m`，使用纯函数）

- `astranaut_disable_tco.erl`：
  - 使用 `astranaut_vars:all_vars(Function)` 收集函数内所有变量；
  - 在 `add_try_catch/2` 中使用 `{[ClassVar, ExceptionVar, StackTraceVar], NVariables} = astranaut_vars:alloc_var_asts(['Class', 'Exception', 'StackTrace'], Pos, Variables)`。
- `astranaut_lib.erl`：
  - 在 `gen_function/2` 的 `named_fun` 分支中使用 `Args = astranaut_vars:fresh_var_asts('AstranautArg', Arity, Pos, Fun)`。

警告抑制接口迁入 `astranaut_lib:silence_unused_vars/1`：先通过 vars:analyze 收集 bindings/all，跳过已有下划线前缀的名字，为零引用绑定生成无冲突的下划线名字，再通过 vars:rewrite 按 Binding ID 改写。无需改写时直接返回输入；vars 不包含这些命名与警告抑制策略。

### 4.3 `src/astranaut_macro_expander.erl` 与 `src/astranaut_quote.erl`（不使用 `astranaut_vars:map_m`）

- 完全回退 `src/astranaut_quote.erl` 中新增的 `prepare_template/2` 与 `template_subtrees/2` 预遍历改动，删除 `test/astranaut_vars_quote_tests.erl`。
- 完全恢复 `src/astranaut_macro_expander.erl` 中 `process_macro_return/3` 原有的 `astranaut:map_m`（`traverse => post`）单遍后置遍历与 `update_macro_return_node/3`。

---

## 5. 实现约定与验证边界

- 回调在 pre 阶段运行。显式 WalkFun 支持 `(Node, Scope)` 与 `(Node, CallerState, Scope)`；未提供时使用二参数 `transform`。入口统一为三参数形式，Event、CurrentVar 从 Scope 读取，不再提供四参数变量回调。
- `keep` 保持默认处理，返回原变量名等同 `keep`，其他裸 atom 是 `{rename, FinalVar}` 的简写；显式 `{rename, FinalVar}` 始终执行指定重命名；裸 AST 表示展开，与 `{expand, Ast}` 一样重新参与遍历，不根据变量 AST 的位置推断 rename。返回原节点本身保持默认处理。
- `transform` 在变量自展开过程中跳过已在 expanding 中的变量回调；显式 WalkFun 仍可观察该节点，重复展开由动作处理层终止。
- 唯一作用域遍历驱动为 mapfold_state_m。同步接口通过 eval/simplify 提取结果；需要接收诊断时使用单子接口。词法作用域退出不回滚调用方状态。
- 仅可能参与分支汇合的绑定及其引用临时保留身份和原始位置，使用 OTP 支持的 erl_anno text 字段保存内部信息，不添加自定义 annotation 标签。作用域遍历结束后做一次不调用用户回调的名称还原，移除内部标记并保留原始位置、file、generated 和原有 text；没有此类绑定时跳过还原扫描。
- current_name 优先使用显式 renames；尚未协调的多分支到达绑定只在实际读取时选择安全最终名，不再因名称不唯一而回退原名。
- 分支新绑定尚未读取且最终名仍不一致时，结果 rename_map 不声称存在唯一映射；exported/bindings 反映实际输出名称。collect 的选择不影响 AST。
- rebinding 外层逐 form 调用 astranaut:traverse_form，保留 file/eof 与诊断位置边界；内层调用 vars:map_m。
- 验证命令：`rebar3 fmt -w`、`rebar3 eunit`、`rebar3 ct`、`rebar3 dialyzer`、`rebar3 fmt --check`。

### 按需统计的执行约定

- context 初始化时展开 collect 为 collect_keys，并计算 count_bindings 与 collect_exported 两个布尔开关。
- 仅 collect 包含 bindings（或为 all）时累加每个 Binding ID 的引用次数；counts 是按最终名字聚合的独立指标，请求 counts 不需要启用绑定点计数。
- all/bound/used/free/counts/rename_history 在需要时记录变量事件及绑定点引用，结束后按最终绑定名计算，避免分支协调名称后留下旧统计。collect=[] 不保存统计事件。
- rename_history 头插累积，返回时反转，避免同一原变量反复尾部追加。
- 仅请求 exported 时，结果阶段才枚举最终环境、查绑定表并排序去重；未请求时跳过这些步骤。词法 env、Binding ID、绑定名称及分支汇合始终保留，因为变量分类和名称解析依赖它们。
- collect 的任意子集必须与完整分析结果投影一致，AST 改写不受 collect 影响；回归覆盖 orig/final/pair、分支汇合与遮蔽绑定。
- 性能验证使用 benchmark/vars_collect_bench.escript。eager 对照在同一实现中强制启用两项计算，current 使用初始化开关；对照不是历史版本的完整性能复现。

修复前的按需统计优化历史记录：232 个 EUnit、286 个 Common Test 通过；格式检查通过，Dialyzer 无新增警告。collect=[] 的三种基准中位耗时相对 eager 对照下降 1.19%–7.23%，reductions 均下降；这不是完整 rebinding 或本次作用域修复后的性能结论。详见 [性能验证记录](../../benchmark/vars-collect-optimized-results.md)。
