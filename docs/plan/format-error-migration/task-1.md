# Task 1：迁移 `astranaut_rebinding` strict formatter

## 目标

让 `astranaut_rebinding` 同时支持编译器兼容的 `format_error/1` 和用于覆盖检测的 `format_error/2`，同时保持现有 parse-transform 行为、诊断 reason、位置和 formatter 归属不变。

## 范围

允许修改：

- `src/astranaut_rebinding.erl`
- `test/astranaut_rebinding_SUITE.erl`
- 为真实诊断新增的 fixture：
  - `test/astranaut_rebinding_SUITE_data/rebinding_invalid_fun_test.erl`
  - `test/astranaut_rebinding_SUITE_data/rebinding_invalid_option_test.erl`

不得修改：`astranaut_disable_tco`、`astranaut_macro_local`、测试 helper 或其他 formatter 模块。

## 已核对的行为来源

- `add_fun_options/3` 在 `src/astranaut_rebinding.erl:59-62` 通过 `astranaut_lib:validate/2` 处理函数选项。
- `add_fun_options/3` 的兜底分支在 `src/astranaut_rebinding.erl:72-77` 产生 `{invalid_rebinding_fun, Other}` warning。
- `load_attributes/1` 在 `src/astranaut_rebinding.erl:42-52` 为属性校验和 `rebinding_fun` 处理设置 `astranaut_rebinding` formatter。
- `astranaut_lib:validate_attribute_option/4` 在 `src/astranaut_lib.erl:659-675` 使用传入 parse-transform 作为 formatter，并更新诊断位置与文件。
- `astranaut_lib:format_error/4` 在 `src/astranaut_lib.erl:613-628` 只有 formatter 顶层 clause 不匹配时才调用 fallback；formatter 内部的 `function_clause` 必须继续抛出。
- `astranaut:format_error/2` 是通用 validator reason 的现有 formatter/fallback。

因此，rebinding formatter 自己只负责 `{invalid_rebinding_fun, Function}`；`validate_key_failure`、`invalid_option_value` 等通用 reason 必须委托给 `astranaut:format_error/2`，不能使用裸 `io_lib:write/1` 把 strict 未覆盖错误吞掉。

## 实施步骤

### 1. 更新导出和 formatter API

在 `src/astranaut_rebinding.erl:14` 将：

```erlang
-export([parse_transform/2, format_error/1]).
```

改为：

```erlang
-export([parse_transform/2, format_error/1, format_error/2]).
```

将当前 `format_error/1`（约 `src/astranaut_rebinding.erl:34-38`）改为三层结构：

```erlang
format_error(Error) ->
    format_error(Error, #{}).

format_error(Error, Options) ->
    astranaut_lib:format_error(
      Error, Options,
      fun format_error_1/1,
      fun astranaut:format_error/2).

format_error_1({invalid_rebinding_fun, Function}) ->
    io_lib:format("invalid rebinding function: ~p", [Function]).
```

约束：

- `/1` 必须继续对文本返回文本、对普通 term 返回可打印字符列表。
- `/2` 不增加 catch-all 子句。
- `format_error_1/1` 只覆盖 rebinding 自有 reason。
- 未知 reason 在 `#{default => throw}` 下必须抛出原 term；默认选项下仍由 `astranaut:format_error/2` 提供原有通用格式化。
- 若项目现有可见消息要求严格兼容，实施时先用现有测试确认 `{invalid_rebinding_fun, Function}` 的历史文本，再决定使用明确文本还是 `io_lib:write/1`；不得无证据改变用户可见消息。

### 2. 增加直接 formatter 契约测试

在 `test/astranaut_rebinding_SUITE.erl` 的 `all/0` 增加 formatter contract case，建议函数名为 `test_format_error_contract/1`。

测试必须包含：

```erlang
Error = {invalid_rebinding_fun, 42},
Message = astranaut_rebinding:format_error(Error, #{default => throw}),
?assert(io_lib:deep_char_list(Message)),
?assertNotEqual([], lists:flatten(Message)),
```

以及通用 fallback 的等价性断言：

```erlang
Generic =
    {validate_key_failure,
     {invalid_value, boolean},
     strict, invalid},
?assertEqual(
   astranaut:format_error(Generic, #{default => throw}),
   astranaut_rebinding:format_error(Generic, #{default => throw})),
```

第二段的具体 validator payload 必须以一次实际测试/现有实现为准；若 `{invalid_value, boolean}` 不是实际 reason，则只调整测试输入，不改变 fallback 设计。

最后锁定 strict 未覆盖语义：

```erlang
Unknown = {unknown_rebinding_error, value},
?assertException(
   throw, Unknown,
   astranaut_rebinding:format_error(Unknown, #{default => throw})),
```

保留或补充 `/1` 默认语义测试：已格式化字符串原样返回，普通 term 经 `io_lib` 得到非空字符列表。

### 3. 增加真实 invalid rebinding warning fixture

新增 `test/astranaut_rebinding_SUITE_data/rebinding_invalid_fun_test.erl`，内容应表达一个真实 parse-transform 输入：

```erlang
-module(rebinding_invalid_fun_test).
-compile({parse_transform, astranaut_rebinding}).
-rebinding_fun(42).
-export([run/0]).

run() -> ok.
```

在 suite 中使用现有 `astranaut_test_lib:test_module_forms/2` 和 `compile_test_forms/1` 流程，提取真实 warnings，精确断言包含：

```erlang
{_Line, astranaut_rebinding,
 {invalid_rebinding_fun, 42}}
```

随后调用 `astranaut_test_lib:assert_formatted_messages/1`。该断言必须证明：真实诊断仍归属 `astranaut_rebinding`，且 `/2` 能严格格式化它。

### 4. 增加通用 validator fallback fixture

新增 `test/astranaut_rebinding_SUITE_data/rebinding_invalid_option_test.erl`，使用非法 `rebinding_all` 或等价属性值触发 `astranaut_lib:validate/2`：

```erlang
-module(rebinding_invalid_option_test).
-compile({parse_transform, astranaut_rebinding}).
-rebinding_all([{strict, invalid}]).
-export([run/0]).

run() -> ok.
```

测试中先以实际运行结果确认 reason 的完整 tuple，再精确断言其 formatter 为 `astranaut_rebinding`，并调用 `assert_formatted_messages/1`。该 case 的目的不是新增 rebinding reason，而是证明通用 validator reason 从 `astranaut_rebinding:format_error/2` 正确委托到 `astranaut:format_error/2`。

## 验证命令

在修改完成后，Luna 运行：

```bash
rebar3 ct --suite=test/astranaut_rebinding_SUITE.erl
git diff --check
```

若需要缩小失败范围，可先运行 suite 中新增的 formatter cases，再运行整套 rebinding suite；完整 suite 通过才算任务完成。

## 完成标准

- `astranaut_rebinding` 导出 `/1`、`/2`，且 `/1` 保持兼容包装语义。
- `{invalid_rebinding_fun, Function}` 在 strict `/2` 下返回非空字符列表。
- 通用 validator reason 与 `astranaut:format_error/2` 的 strict 结果一致。
- strict 未知 reason 抛出原 term，而不是被格式化为 term 文本。
- 真实 invalid rebinding warning 仍是 `astranaut_rebinding` formatter，并通过严格检查。
- 现有 rebinding AST/运行行为测试全部通过。
- `git diff --check` 通过。

## 提交与回滚边界

测试通过后，Luna 不提交；由 dispatcher 复核实际 diff 后决定是否单独提交本任务。若失败，只回滚本任务列出的 rebinding 模块、suite 和两个 fixture，不触及 local-macro 生命周期改造。

## 未决项

- 非法 `rebinding_all` 的 validator 内部 payload 需要由首次定向测试确认，计划不预先伪造第二字段。
- `{invalid_rebinding_fun, Function}` 的最终消息文本需以项目现有兼容性要求为准；reason 覆盖边界不可改变。
