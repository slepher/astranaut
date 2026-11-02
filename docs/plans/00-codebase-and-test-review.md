# Astranaut 项目全面代码审查与测试覆盖评估报告

> **2026-11-02 实施核对**：下文保留原审查快照，最新实施状态和更正见文末。原统计不准确：`ba34758` 实际含 26 个源码模块、11 个头文件、9 个 EUnit 模块（157 个不同入口）及 10 个 CT suite（282 个用例）。其提交主题是 `Fix macro closure analysis and displaced import cleanup`。

- **审查基准版本**：`ba34758` (`Move unit-style suites from common_tests to eunit test`)
- **审查范围**：
  - 主 Agent：全量非测试源码（`src/*.erl` 共 26 个模块、`include/*.hrl` 共 6 个头文件、`scripts/*.escript` 共 3 个脚本、`rebar.config`、`.github/workflows/*`、`README.md`、`README.zh.md`、`codebase-map.md`、`lessons.md`）
  - 子 Agent（`flash`）：全量测试代码（`test/*_tests.erl` 9 个 EUnit 模块共 127 个用例、`common_tests/*_SUITE.erl` 10 个 CT 套件共 281 个用例，合计 408 个用例）
  - 主 Agent 交叉评估：源码实现分支、代码缺陷与 408 个测试用例的覆盖度对比分析

---

## 第一部分：项目源码与架构全面审查（非测试部分）

### 1. 架构与模块分层评价

项目共包含 **26 个源码模块**（`src/*.erl`）、**6 个公共头文件**（`include/*.hrl`）和 **3 个构建/审计脚本**（`scripts/*.escript`），整体分为三层：

1. **核心 AST 遍历、语法 Schema 与 Monad 基础设施层**
   - `src/astranaut.erl`、`src/astranaut_traverse.erl`、`src/astranaut_uniplate.erl`、`src/astranaut_monad.erl`、`src/astranaut_return.erl`、`src/astranaut_error.erl`、`src/astranaut_forms.erl`。
   - `src/astranaut_syntax.erl` 搭配生成的 `src/astranaut_syntax_schema.erl`（由 `scripts/generate_syntax_schema.escript` 从 `priv/syntax_schema.terms` 生成，并由 `scripts/check_syntax_schema.escript` 对齐 OTP 21–29 `erl_lint` 语义）：将角色校验（`form` / `expression` / `pattern` / `guard` / `type` / `clause`）和 OTP 版本门控编译为确定性模式匹配函数，兼顾性能与可维护性。
2. **元编程核心引擎层（Quote & Macro & Lib）**
   - `src/astranaut_quote.erl`：编译期 `quote`/`unquote`/`unquote_splicing`、类型/模式/表达式多态 quote 以及基于 `@astranaut_quote@` 的卫生变量（hygienic variable）编解码。
   - 宏子系统（6 模块）：
     - `src/astranaut_macro.erl`：两阶段（attribute pass + function-body pass）编排；
     - `src/astranaut_macro_scan.erl`：单向 scan-and-splice 引擎与 `__original__` 合并；
     - `src/astranaut_macro_registry.erl`：宏声明、导入与源码序宏环境管理；
     - `src/astranaut_macro_expander.erl`：统一宏匹配、参数打包与递归展开器；
     - `src/astranaut_macro_local.erl`：本地宏静态闭包冻结、最小累积编译边界（DAG）与代数生命周期（按需动态注册解耦）；
     - `src/astranaut_macro_local_compile.erl`：每次 parse transform 分配唯一模块名的本地宏编译器。
   - `src/astranaut_lib.erl`：AST 构造、位置替换、参数校验器、动态编译与模块锁热更工具箱。
3. **上层 Parse Transforms 与工具模块**
   - `src/astranaut_rebinding.erl`（类 Elixir 变量重绑定与 `^Var` pin 匹配）
   - `src/astranaut_struct_transformer.erl` / `src/astranaut_struct.erl` / `src/astranaut_struct_record.erl`（基于 Map 的 Struct 转换）
   - `src/astranaut_do.erl`（Monad `do` 语法糖）
   - `src/astranaut_disable_tco.erl`（禁用尾递归优化保留异常调用栈）
   - `src/astranaut_compile_meta_transformer.erl`（多 Parse Transform 串联与错误隔离）
   - `src/astranaut_compile_opts.erl`（注入 `compile_opts/0`）
   - `src/astranaut_macros.erl`（内置 `literal/1` 宏）

---

### 2. 发现的代码缺陷与潜在 Bug（按严重程度排序）

#### 🔴 严重缺陷（调用必触发死循环或异常崩溃）

##### Bug 1：`astranaut_error:new_error/1` 无限递归死循环
- **位置**：`src/astranaut_error.erl:353-354`
- **现状代码**：
  ```erlang
  new_error(Error) ->
      new_error([Error]).
  ```
- **根因分析**：原本应调用 `new_errors([Error])`（对比第 359–360 行的 `new_warning(Warning) -> new_warnings([Warning]).`），误写为自递归调用 `new_error([Error])`。任何调用 `astranaut_error:new_error/1` 的路径都会无限构造嵌套列表直至进程 OOM。
- **修复方案**：改为 `new_errors([Error])`。

##### Bug 2：`astranaut_error:update_file/3` 在 `file = undefined` 时误调 `is_empty/1` 导致必抛异常
- **位置**：`src/astranaut_error.erl:439-445`
- **现状代码**：
  ```erlang
  update_file(undefined, eof, #?ERROR_STATE{formatted_errors = Errors, formatted_warnings = Warnings} = Struct) ->
      case is_empty(Errors) and is_empty(Warnings) of
          true ->
              Struct;
          false ->
              erlang:error({match_eof_without_file, Struct})
      end;
  ```
- **根因分析**：`Errors` 和 `Warnings` 的类型是 `endo(...)`（`#?ENDO{}`），而 `is_empty/1`（`src/astranaut_error.erl:204-214`）期望的参数是 `#?ERROR_STATE{}`，其兜底子句 `is_empty(_ErrorState) -> false` 导致 `is_empty(Errors)` **永远返回 `false`**。因此，即使没有任何错误和警告，在未设置 `file` 的状态上调用 `eof/1` 也会无条件抛出 `{match_eof_without_file, Struct}`。
- **修复方案**：将 `is_empty(Errors) and is_empty(Warnings)` 改为 `endo_is_empty(Errors) andalso endo_is_empty(Warnings)`。

##### Bug 3：`astranaut_lib:gen_function/2` 处理 `named_fun` 存在两处 AST 结构与属性键不匹配
- **位置**：`src/astranaut_lib.erl:557-569`
- **现状代码**：
  ```erlang
  gen_function(Name, {named_fun, Pos, {var, _, FunName1}, Clauses}) ->
      Clauses1 =
          astranaut:smap(
              fun
                  ({var, FunNamePos, FunName2}, #{type := expression}) when FunName2 =:= FunName1 ->
                      {atom, FunNamePos, Name};
                  (Node, _Attr) ->
                      Node
              end,
              Clauses,
              #{traverse => post}
          ),
      {function, Pos, Name, clauses_arity(Clauses1), Clauses1};
  ```
- **根因分析**：
  1. Erlang Abstract Format 中 `named_fun` 的节点结构是 `{named_fun, Anno, NameAtom, Clauses}`（第三元是 `atom()`，如 `src/astranaut_quote.erl:809` 与 `src/astranaut_disable_tco.erl:67` 所示），而非 `{var, _, FunName1}`。传入合法 `named_fun` AST 会直接落入第 570 行报 `{invalid_function_ast, NamedFun}`。
  2. 内部 `astranaut:smap` 回调匹配 `#{type := expression}`，但 Astranaut 的 `Attr` 中表示节点角色的键是 `node` 而非 `type`，且未传 `role => clause` 时 `#{traverse => post}` 下 `Attr` 不含 `node => expression`，导致函数体内的自调用不会被替换为 `{atom, FunNamePos, Name}`。
- **修复方案**：匹配 `{named_fun, Pos, FunName1, Clauses} when is_atom(FunName1)`，并在遍历 `Clauses` 时传入 `#{traverse => post, role => clause}`、匹配 `#{node := expression}`（同时注意内部同名的局部变量绑定遮蔽或直接替换调用目标 `{call, CallPos, {var, FunNamePos, FunName1}, Args}`）。

---

#### 🟠 中等缺陷（边界条件错误、契约违背或调试代码残留）

##### Bug 4：`astranaut_uniplate:apply_modifies/2` Guard 变量笔误
- **位置**：`src/astranaut_uniplate.erl:749-754`
- **现状代码**：
  ```erlang
  apply_modifies([Modify | T], S) when is_function(Modify) ->
      apply_modifies(T, Modify(S));
  apply_modifies([S1 | T], S) when not is_function(S) ->
      apply_modifies(T, S1);
  apply_modifies([], S) ->
      S.
  ```
- **根因分析**：第二个子句的 guard 写成了 `when not is_function(S)`，检查的是旧状态 `S` 而不是新状态项 `S1`。当遍历的 `State` 本身是一个函数，且通过 `astranaut_uniplate:with/3` 传入非函数的新状态值 `S1` 时，会因 `is_function(S)` 为 `true` 而触发 `function_clause` 崩溃。
- **修复方案**：改为 `when not is_function(S1)`。

##### Bug 5：`astranaut_lib:with_attribute/5` 忽略了 `simplify_return` 选项契约
- **位置**：`src/astranaut_lib.erl:613-653`
- **根因分析**：`@doc` 和 `-spec` 明确声明 `with_attribute/5` 默认返回精简后的 `State`（仅当 `simplify_return => false` 时返回 `astranaut_return:struct(State)`），但实现直接返回了 `astranaut:reduce(Fun, Init, Forms, Opts)` 的结果（始终为 `astranaut_return:struct(State)`），未像 `forms_with_attribute/4`（`src/astranaut_lib.erl:698-711`）那样根据 `maps:get(simplify_return, Opts, true)` 执行 `astranaut_return:simplify/1`。
- **修复方案**：对齐 `forms_with_attribute/4` 的 `simplify_return` 处理逻辑（同时检查内部调用方如 `astranaut_rebinding`、`astranaut_disable_tco` 是否传了 `simplify_return => false` 或依赖 `astranaut_return:struct`）。

##### Bug 6：`astranaut_do:do/2` 返回值类型违背 `-spec` 且 `format_error/1` 缺失子句
- **位置**：`src/astranaut_do.erl:23-45`
- **根因分析**：
  1. `do/2` 的 `-spec` 声明返回 `astranaut_return:struct(astranaut:trees())`，但兜底子句 `do(_Pos, _Other) -> {error, expected_list_comprehension}` 返回了裸 `{error, ...}` 元组（应为 `astranaut_return:error_fail(expected_list_comprehension)`）。
  2. `format_error/1` 只处理了 `empty_do_list_comprehension` 和 `last_expression_could_not_be_generator`（且消息文本有拼写错误 `"count not be a generator"`），漏掉了 `expected_list_comprehension`。
- **修复方案**：`do/2` 返回 `astranaut_return:error_fail(expected_list_comprehension)`，并在 `format_error/1` 中补充 `expected_list_comprehension` 子句。

##### Bug 7：`astranaut_macro:invalid_macro_return_mfa/1` 缺少兜底子句
- **位置**：`src/astranaut_macro.erl:158-161`
- **根因分析**：仅匹配 `#{macro := #{mfa := MFA}}` 和 `#{current_macro := #{mfa := MFA}}`。若 `invalid_macro_return` 传入的 `Attr` 不含这两个键，会触发 `function_clause` 异常并导致整条 `format_error` 退化为 `io_lib:write/1`。
- **修复方案**：增加兜底子句 `invalid_macro_return_mfa(_) -> {unknown, unknown, 0}.`。

##### Bug 8：`astranaut_macro_registry:log_macro_load/2` 残留调试日志与硬编码 `"erlando"` 过滤
- **位置**：`src/astranaut_macro_registry.erl:556-566`
- **根因分析**：当 `-import_macro(Module)` 加载失败时，无条件向 `standard_error` 输出 `[astranaut_macro_debug] load_failed ...` 并用 `string:find(Path, "erlando") =/= nomatch` 过滤路径。
- **修复方案**：移除 `log_macro_load/2` 及其在 `ensure_macro_module_loaded/2` 中的调用。

---

#### 🟡 轻微问题（类型签名与文档/目录漂移）

##### 问题 9：`astranaut_return:map_m/2` 的 `-spec` 类型签名错误
- **位置**：`src/astranaut_return.erl:100`
- **问题**：`-spec map_m(fun((A) -> struct(B)), [struct(A)]) -> struct(B).` 应为 `-spec map_m(fun((A) -> struct(B)), [A]) -> struct([B]).`。

##### 问题 10：文档与仓库状态漂移
- **`codebase-map.md`**：Revision 停留在 `78683bf`（落后 `HEAD` `ba34758` 共 7 个提交），正文中仍写 25 个模块（未反映拆分出的 `src/astranaut_macro_local_compile.erl`），且“未提交增补”中的 EUnit/CT 拆分已正式提交。
- **`README.md` 与 `README.zh.md`**：
  - 顶部仍将遍历入口写为 `astranaut_traverse:map/...`、`reduce/...`、`map_with_state/...`、`mapfold/...`（实际位于 `astranaut` 模块）；
  - Rebinding 与 Struct 章节头文件仍写为 `-include_lib("erlando/include/rebinding.hrl").` 和 `-include_lib("erlando/include/struct.hrl").`；
  - `README.md` 第 1012、1039 行宏展开示例残留未展开的 `unquote_atom(hello)`。
- **空目录残留**：`src/traverse/` 作为空目录残留在工作区中。

---

## 第二部分：测试用例全量盘点与简要描述（共 408 个用例）

### 1. 测试架构概览与辅助模块

- **EUnit (`test/*_tests.erl`)**：9 个测试模块，共 **127 个单元测试用例**，聚焦核心数据结构、AST Schema 校验、Traversal Monad、Local Macro 状态机及 Scan-and-Splice 逻辑。
- **Common Test (`common_tests/*_SUITE.erl`)**：10 个测试套件，共 **281 个集成测试用例**，聚焦端到端 Parse Transform 流水线、多模块宏展开、动态编译热更与跨 OTP 版本兼容性。
- **测试辅助模块 (`test/astranaut_test_lib.erl`)**：提供 `-baseline(Mark)` 相对行号诊断断言（`realize_with_baseline/2`）、Fixture 编译加载（`compile_test_module/2`、`load_data_modules/2`）及 `format_error/1` 协议校验。

### 2. EUnit 单元测试模块详单（`test/*_tests.erl`，127 个用例）

#### 2.1 `test/astranaut_error_tests.erl`（6 个用例）
- `test_state_1_test`：`append_error/2` 追加单个错误，断言 `errors/1` 与空 `warnings/1`。
- `test_state_2_test`：连续追加错误，断言 FIFO 顺序保持。
- `test_state_3_test`：交错调用 `append_warnings/2` 与 `append_error/2`，断言错误与警告独立追踪。
- `test_state_4_test`：`update_pos/3` 位置绑定，断言 `formatted_errors/1` 与 `formatted_warnings/1` 生成 `{Pos, Formatter, Reason}`。
- `test_state_5_test`：`update_file/2` 与 `eof/1` 单文件边界更新，断言 `realize/1` 按 `?FILE` 聚合诊断。
- `test_state_6_test`：跨 `?FILE` 与 `File2` 多文件交错切换，断言 `realize/1` 按文件正确分组。

#### 2.2 `test/astranaut_forms_tests.erl`（2 个用例）
- `test_sort_forms_test`：乱序 `[Function, module_form, eof_form]` 经 `sort_forms/1` 后按 module → function → eof 排列。
- `test_insert_forms_original_test`：`insert_forms/2` 插入含 `__original__` 调用的同名函数，断言原函数重命名为 `foo_1` 且包装函数调用 `foo_1`。

#### 2.3 `test/astranaut_macro_expander_tests.erl`（2 个用例）
- `provider_isolation_test_`：启动独立子 Erlang 节点并排除 `astranaut_macro_local.beam`，断言外部宏正常展开，而本地宏报错 `{macro_capability_unavailable, astranaut_macro_local}`。
- `shared_invocation_test`：测试属性宏与表达式调用宏在 `group_args => true` 及精确 Arity 匹配下的参数打包与分发。

#### 2.4 `test/astranaut_macro_local_tests.erl`（23 个用例）
- `prepare_target_only_requests_context_when_compilation_is_needed_test`：验证仅在实际触发字节码编译时才惰性请求编译上下文。
- `module_name_is_unique_per_allocation_test`：验证每次分配生成唯一的 `<Original>__local_macro__<ID>` 模块名。
- `register_freezes_static_closure_test`：注册本地宏时冻结静态调用闭包（函数与 `-spec`）。
- `duplicate_declaration_fails_atomically_test`：重复声明 `-local_macro` 原子失败并返回 `{duplicate_local_macro_declaration, {F, A}}`。
- `retain_controls_final_skip_ids_test`：验证 `-local_macro_retain` 控制最终保留与剔除（`skip_ids`）的函数集合。
- `source_view_only_contains_materialised_forms_test`：验证 `source_view/2` 仅暴露已物化的 Forms。
- `frozen_splice_reports_generated_form_position_test` / `frozen_splice_is_rejected_test`：验证篡改已锁定闭包 Form 被拒绝并精确定位到生成 Form 的源码位置。
- `local_compiler_preserves_loaded_code_on_compile_failure_test`：增量重编译失败时保留上一代已加载的 BEAM 代码。
- `closure_roots_and_self_recursion_test`：测试自递归本地宏与显式 `closure_roots` 依赖（含缺失报错）。
- `minimal_cumulative_compile_boundaries_test` / `independent_macros_share_one_boundary_test` / `attribute_between_independent_macros_shares_one_boundary_test` / `shared_declaration_stays_in_one_boundary_test` / `independent_declarations_compile_once_test` / `prefix_member_dependencies_are_prepared_test`：验证本地宏依赖 DAG 的最小累积编译边界与单次合并编译。
- `non_frozen_retain_root_has_no_effect_test`：retain 普通非闭包函数不影响 skip 集合。
- `formatter_protocol_none_test` / `formatter_protocol_v1_only_test` / `formatter_protocol_v1_is_present_test`：本地宏 `format_error/1` 协议检测与回退。
- `first_binding_and_lazy_registration_test`：注册保持代数 0，首次 `need_callable/3` 才触发代数 1 编译。
- `macro_call_does_not_retain_runtime_closure_test`：调用本地宏不会将其运行时闭包残留到目标模块。
- `external_replacement_executes_once_test` / `shared_helper_retains_first_canonical_result_test` / `failed_expansion_does_not_commit_canonical_or_generation_test`：确保 canonical 展开只执行一次且失败不污染状态。

#### 2.5 `test/astranaut_macro_registry_tests.erl`（3 个用例）
- `import_use_history_test`：测试 `import_macro` 与 `use_macro` 幂等性、alias、debug 标记及无 `force_override` 时的冲突报错。
- `cross_module_force_override_test`：跨模块动态编译 Provider 并通过 `force_override` 切换同名宏绑定。
- `local_override_then_use_test`：验证本地宏声明与后续同名 `use_macro` 的冲突检测。

#### 2.6 `test/astranaut_macro_scan_tests.erl`（11 个用例）
- `run_without_local_capability_test`：无本地宏能力时的扫描运行。
- `lift_m_bridge_test`：验证 `astranaut_traverse` 与 `astranaut_return` 之间的 Monad 桥接。
- `basic_test` / `splice_test` / `splice_empty_test` / `expand_test` / `expand_with_state_test`：测试单向扫描中 `{splice, [...]}` 插入、`{splice, []}` 删除及跨 splice 状态累积。
- `preserve_generated_function_position_test` / `generated_function_original_merge_test` / `generated_merge_preserves_original_spec_test` / `generated_merge_prefers_generated_spec_test` / `generated_merge_keeps_generated_spec_without_original_test`：测试生成函数位置保持、`__original__` 重命名合并及 `-spec` 保留/覆盖规则。

#### 2.7 `test/astranaut_syntax_tests.erl`（45 个用例）
- **角色校验（20 个用例）**：测试 `expression`、`form`、`pattern`、`guard`（含结合 Record Forms 的 Guard 校验）、`type`（含字面量单例与 `-type`/`-opaque`/`-spec`/`-callback`）、`clause`、`name`、`attribute_body` 及列表推导式的接受与拒绝路径。
- **Schema 分发与槽位校验器（9 个用例）**：穷举校验 `astranaut_syntax_schema` 生成的所有 `child_specs` 槽位校验器、浅校验 `validate_node` 与深递归 `normalize`。
- **OTP 版本门控（11 个用例）**：测试 OTP 21 异常栈捕获、OTP 23 Map Pattern Key 表达式与 Binary Size 表达式、OTP 25 `maybe ... end`、OTP 26 Map 推导式、OTP 28 `<:-` 严格生成器、OTP 29 多模板推导式与原生 Record。
- **遍历期校验控制与 Adapter（13 个用例）**：测试 `validate => true | false | input | output | both`、`fail => collect | raise`、只读遍历跳过校验开销，以及 Attribute / Record Field Adapter 往返还原。

#### 2.8 `test/astranaut_traverse_tests.erl`（14 个用例）
- 测试 `return/1`、`bind`、`update_pos/2`、`with_all_error/2`、`state/1`、`put/1`、`update_file/1`、`with_formatter/2`、`eof/0`、`fail_on_error/1`、`scoped_state/2`、`scoped_state_run/2` 以及 `map_m` 的根 `attr` 和显式 `role` 覆盖。

#### 2.9 `test/astranaut_uniplate_tests.erl`（21 个用例）
- 测试 Writer(`or`)、`smap`、`sreduce`、`mapfold`、`up_attr`、节点返回列表一对多展开、`traverse => all`、`with_subtrees/3` 自定义子树顺序、`astranaut_uniplate:with/3` 低层上下文操作，以及 9 种非法变换异常分支（`invalid_pre_transform`、`invalid_post_transform_with_context`、`invalid_transform_maketree` 等）。

---

### 3. Common Test 集成测试套件详单（`common_tests/*_SUITE.erl`，281 个用例）

| 测试套件 | 用例数 | 覆盖重点简述 |
| :--- | :---: | :--- |
| `astranaut_SUITE.erl` | 40 | 核心遍历 API（`map`/`reduce`/`map_with_state`/`mapfold`）、`with_attribute`/`forms_with_attribute`、`astranaut_lib:validate/2` 选项校验器全分支契约、`format_error` 协议与异常回退、`to_compiler` 适配，以及 Record/Map/If/Case/Try AST 节点遍历封装验证。 |
| `astranaut_design_SUITE.erl` | 21 | 架构契约回归测试：`walk_return` 全形态契约、`astranaut` 导出类型检查、`astranaut_lib` 33 个导出函数签名与文件定位契约、`reload_forms/2` 动态加载与 `module_in_use` 并发保护、`astranaut_do` 转换与报错、`astranaut_monad` 变换器栈、`astranaut_return`/`astranaut_error` 边界、`astranaut_struct` 负向契约、`astranaut_compile_opts` 与 `astranaut_compile_meta_transformer` 全套契约。 |
| `astranaut_macro_SUITE.erl` | 28 | 宏展开端到端测试：函数宏、属性宏、`quote`/`unquote`/`unquote_splicing`、模式匹配宏、`order`（inner/outer）、OTP 26+ `code_server` 加载延迟竞态处理、`__original__` 合并、嵌套/递归宏、OTP 29 原生 Record 宏、`astranaut_macros:literal/1`、Guard 宏，以及 9 个 Quote 卫生上下文（hygienic context / `no_context` / `named_fun` / counter）测试。 |
| `astranaut_macro_error_SUITE.erl` | 18 | 宏错误诊断与恢复：编译警告/错误收集、本地与外部宏 Formatter 协议（v1/v2/缺失警告）、选项作用域非法属性拒绝、重复或部分失败的本地宏声明诊断、预设错误码格式化、同子句兄弟节点多错误收集、外部宏错误归属权保留。 |
| `astranaut_macro_pass_SUITE.erl` | 38 | 宏单向扫描与生命周期：无回扫保证、外部/本地属性宏生成链、`closure_roots` 静态闭包与缺失报错、冻结快照篡改拦截、声明位点宏环境隔离、属性缓冲与深度上限（cross/self/total depth），以及并发编译本地宏不阻塞源模块锁测试。 |
| `astranaut_macro_uniform_SUITE.erl` | 19 | 统一宏解析与递归展开：跨模块导入顺序、内外序嵌套宏、宏生成宏链、直接调用 vs 宏调用、本地与外部宏相互覆盖与 `force_override`、槽位角色校验失败诊断及最大展开深度限制。 |
| `astranaut_quote_SUITE.erl` | 73 | Quote 子系统全量测试：字面量/表达式/函数/Named Fun/Guard/Pattern/Type/Spec/Callback/Opaque/Record/原生 Record 引用、位置替换、`unquote` 与 `unquote_splicing`（含 Map/Record 匹配与列表拼接）、动态绑定（`_@`、`_L@`、`_A@`、`_I@`、`_F@`、`_S@`、`_V@`）、各类编译期 Warning 诊断，以及卫生变量编解码器（codec）与 `context`/`no_context` 全排列测试。 |
| `astranaut_rebinding_SUITE.erl` | 20 | 变量重绑定测试：对比转换后函数与手写 SSA 函数的 AST 及运行结果等价性（覆盖 LC/BC/MC 推导式、Function、Case、If、Maybe、Record、Map、操作符、列表/元组）、`^Var` 固定变量匹配及非法选项/函数警告错误。 |
| `astranaut_struct_SUITE.erl` | 20 | Struct 转换测试：Struct 构造、更新、模式匹配、Record 索引访问、`from_record`/`to_record`/`from_map`/`update` 宏、自动注入 `-import_macro(astranaut_struct)`，以及 `enforce_keys` 缺失、未知字段、非 Record 等编译期/运行期错误。 |
| `disable_tco_SUITE.erl` | 4 | 禁用 TCO 测试：验证尾调用（普通调用、高阶函数、`case`/`if`/`begin`/`receive`/`andalso`/`try` 嵌套尾位置）被改写为 `try ... catch` 并在异常栈中保留完整调用帧，同时保持直接自递归不改写。 |

---

## 第三部分：测试覆盖全面性交叉评估与缺口修复清单

### 1. 因未覆盖而漏检实际 Bug 的测试盲区（P0 必补）

1. **`astranaut_error` 的单项构造器与无文件 `eof` 边界**
   - **未覆盖路径**：
     - `astranaut_error:new_error/1`（触发 Bug 1 无限递归）、`new_errors/1`、`new_warning/1`、`new_warnings/1`；
     - 在 `file = undefined` 的空错误状态 `astranaut_error:new()` 上直接调用 `astranaut_error:eof/1`（触发 Bug 2 误调 `is_empty/1` 抛 `{match_eof_without_file, ...}`）；
     - 存在未绑定位置的 pending errors 时调用 `update_pos(eof, ...)` 触发 `{match_eof_without_pos, ...}`。
2. **`astranaut_lib:gen_function/2` 的 `named_fun` 分支**
   - **未覆盖路径**：向 `astranaut_lib:gen_function/2` 传入 `{named_fun, Anno, FunName, Clauses}`（触发 Bug 3）。
3. **`astranaut_lib:with_attribute/5` 的 `simplify_return` 选项契约**
   - **未覆盖路径**：显式测试 `with_attribute/5` 在默认（`simplify_return => true`）与 `simplify_return => false` 下的返回值形态差异（触发 Bug 5）。
4. **`astranaut_uniplate:with/3` 在函数类型 `State` 下的字面量状态替换**
   - **未覆盖路径**：当遍历状态 `S` 本身是 `fun()` 时，在 `astranaut_uniplate:with(Subtrees, [NewStateVal], Node)` 中传入非函数新状态值（触发 Bug 4）。
5. **`astranaut_do:format_error/1` 对 `expected_list_comprehension` 的格式化**
   - **未覆盖路径**：对 `expected_list_comprehension` 调用 `astranaut_do:format_error/1`（触发 Bug 6）。

### 2. 各模块其他未覆盖分支与边界场景（P1 建议补充）

| 模块 | 未覆盖或覆盖薄弱的分支场景 |
| :--- | :--- |
| `src/astranaut_lib.erl` | 1. `original_forms/2` 与 `parse_file/2`：含 UTF-8 编码注释文件（`epp:read_encoding/1` 分支）、`-D` 宏定义（`{d, M}` 与 `{d, M, V}`）、`deterministic` 编译选项剥离文件属性、以及源文件不存在时的 `{open_file_failed, File, Error}` 分支。<br>2. `ast_safe_to_string/2`：传入非法 AST 触发 `erl_pp:form` 崩溃时回退为 `io_lib:format("~p", [Form])` 的容错分支。<br>3. `replace_deep_pos/2`：对 `erl_scan:anno()` 复杂 proplist/tuple 位置元数据的深层替换。 |
| `src/astranaut_forms.erl` | 1. `merge_functions/2`：3 个及以上同名函数连续链式调用 `__original__`（生成 `foo_1`、`foo_2`）以及目标模块中已存在同名 `foo_1` 时的冲突避让逻辑（`rename_function/4`）。<br>2. `sort_forms/1` 中顶层 `{warning, _}`、`{error, _}` Form 与普通函数/属性的相对顺序保持。 |
| `src/astranaut_disable_tco.erl` | 1. `named_fun` 内部自递归豁免与外部尾调用包裹（`src/astranaut_disable_tco.erl:67-76`）。<br>2. `orelse` 右操作数尾位置包裹。<br>3. 尾位置调用 `erlang:error/1,2`、`erlang:exit/1`、`erlang:throw/1`、`erlang:raise/3`、`record_info/2` 时的黑名单豁免分支。<br>4. `-disable_tco([debug])` 属性的调试打印分支。 |
| `src/astranaut_rebinding.erl` | 1. `try ... of ... catch ... after ... end` 表达式（`src/astranaut_rebinding.erl:351-398`）完整作用域传播与异常模式重绑定专项测试。<br>2. `receive ... after ... end` 多子句与超时分支变量重绑定合并专项测试。<br>3. `named_fun` 函数名自身作为绑定变量在函数体内的重绑定场景。 |
| `src/astranaut_return.erl` & `src/astranaut_monad.erl` | 1. `astranaut_return:map_m/2` 与 `foldl_m/3` 在中途遇到 `fail` 时的短路行为与错误累积。<br>2. `astranaut_monad:sequence_m/2` 与 `writer` Monad 的 `listen`/`pass`/`censor` 组合子专项测试。 |

---

## 第四部分：后续高价值特性演进计划索引

针对本次审查识别出的高价值功能扩展点，已按优先级顺序在 `docs/plans/` 下制定了 5 份可执行的详细实施计划：

1. **[Plan 01 (`01-struct-type-ast-rewrite.md`)](./01-struct-type-ast-rewrite.md)**：**P0** — `astranaut_struct` 的 Type/Spec AST 同步改写与类型生成实现计划
2. **[Plan 02 (`02-astranaut-vars-scope-analysis.md`)](./02-astranaut-vars-scope-analysis.md)**：**P1** — `astranaut_vars` 通用变量与作用域分析模块实现计划
3. **[Plan 03 (`03-astranaut-decorator.md`)](./03-astranaut-decorator.md)**：**P1** — `astranaut_decorator` 声明式函数装饰器实现计划
4. **[Plan 04 (`04-astranaut-pipe-and-lens.md`)](./04-astranaut-pipe-and-lens.md)**：**P2** — `astranaut_pipe` 与 `astranaut_lens` 编译期宏模块实现计划
5. **[Plan 05 (`05-astranaut-diff.md`)](./05-astranaut-diff.md)**：**P2** — `astranaut_diff` 编译期 Form 级 AST Diff 诊断工具实现计划


## 2026-11-02 实施状态与报告更正

### 已完成的修复和文档维护

- P0 回归测试共 10 个，位于 `test/astranaut_review_regression_tests.erl`。
- Bug 2：内部 endo 集合检查确实用错函数，已修正；但公开 `eof/1` 不走报告所述分支，“无文件必抛异常”不成立。保留现有公开行为并测试空状态与待归属诊断。
- Bug 5：进一步核对确认这是旧 spec/doc 漂移；`with_attribute/5` 与现有调用方都约定返回 result struct。撤回默认 simplify 的行为变更和调用方适配，仅修正 spec/doc，测试默认及遗留选项下结果形态和警告保留。
- Bug 6：`{error, expected_list_comprehension}` 是合法的 traversal callback 返回值，`astranaut:traverse_return/1` 会适配它。恢复原实现，直接使用并导出 `astranaut` 已有的 `map_walk_return/0` 共有类型，不新增 `do` 专用类型；补齐 formatter。回归测试直接验证 tuple 及其消费路径。
- 问题 10：两份 README 的遍历模块名、结果类型和 `include_lib` 应用名已修正；英文示例以 `_A@Name` 替换不存在的 `unquote_atom(Name)`。原文所称 `unquote_atom(hello)` 并不存在，展开后的 `hello() -> hello.` 本来正确。
- `codebase-map.md` 正文按已提交 `ba34758` 重核统计和架构，将测试拆分、本地宏 compiler 拆分、首次绑定/canonical 复用纳入正文；本轮工作区变更只记在未提交增补。
- 删除工作区空目录 `src/traverse/`。未实施 Plan 01–05。

### P1 覆盖清单

新增 `test/astranaut_review_p1_tests.erl`，共 13 个测试：

| 报告建议 | 当前实现与测试 |
| --- | --- |
| `original_forms/2`、`parse_file/2` | UTF-8 注释和非 ASCII 原子；`{d, M}`、`{d, M, V}`；重读源码；deterministic 使用 basename（不是移除 file 属性）；不存在的文件返回 compiler/epp `enoent`（不是 `open_file_failed`） |
| AST 安全格式化 | 实际 API 是 `ast_safe_to_string/1`，断言非法 AST 回退仍含错误标识与原始节点 |
| 深层位置替换 | 实际 API 是 `replace_pos/2` 与 `replace_pos_zero/2`；覆盖带 file/generated 的 annotation、tuple 位置和零位置 |
| forms 合并与顺序 | 三层 `__original__` 链及预占 `foo_1`；编译运行验证调用链与预占函数不受损；warning/error 与属性、函数相对顺序 |
| disable_tco | named fun 自递归保留、外部尾调用包装；`orelse` 右侧包装与运行行为；error/exit/throw/raise/record_info 调用包装 |
| rebinding | 完整 try/of/catch/after 的局部重绑定、异常模式及 after 执行；receive 两个消息分支与超时分支的重绑定合并；named fun 名称在函数体内重绑定 |
| return monad | map/fold 中途失败停止后续 callback，并保留之前的错误和警告顺序 |
| monad / writer | sequence 成功、失败短路和空输入；Writer 日志顺序与 listen 保留结果/日志 |

原 P1 清单中的 TCO 黑名单豁免、`-disable_tco([debug])` 打印分支及 Writer `pass/censor` 在当前源码中不存在。TCO 当前包装所列内建调用，已有 CT 亦明确要求包装 `erlang:error/1`；本次测试当前契约，没有按过期描述引入新特性。

验证环境为本机 OTP 29。全量 EUnit 180 个测试、Common Test 282 个用例通过；执行 `rebar3 fmt -w` 与格式检查。没有运行完整 OTP 21–29 矩阵。
