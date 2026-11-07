# astranaut_vars 审核与校验记录

## 1. 基线与范围

- 校验日期：2026-11-07。
- 校验提交：`f2933650b83bbdae9c28b920610af738415d5e3e`。
- 提交说明：`Fix regression test compatibility with older OTP versions`。
- 输入：用户提供的 `astranaut_vars` 设计、实现、过度设计及设计偏离审核报告。
- 对照设计：[02-astranaut-vars-scope-analysis.md](02-astranaut-vars-scope-analysis.md)。
- 主要实现：[astranaut_vars.erl](../../src/astranaut_vars.erl)、[astranaut_rebinding.erl](../../src/astranaut_rebinding.erl)、[astranaut_lib.erl](../../src/astranaut_lib.erl)、[astranaut_disable_tco.erl](../../src/astranaut_disable_tco.erl)。
- 工作范围：核实各项结论，提供确认与反例；未修改实现、测试或设计文档。

以上为原始审核基线。后续用户授权实施修复，第一轮修复见 §9，追加边界审核后的最新修复与 OTP 21–29 验证见 §10；前文保留各阶段的事实及反例，不表示当前工作区仍有相同故障。

原报告来自另一份 checkout，其中本地 `master`、`origin/master` 的关系不作为本次事实依据。本次直接检查上述提交的代码与文档，校验开始和结束时仓库工作区均干净。

## 2. 结论摘要

确认以下问题：

1. `try` 的 `of` 子句与 `after` 区段，以及 `receive` 的消息分支与 timeout 表达式之间，存在生成变量名冲突；后者不读取导出变量时也可能编译成功并在消息命中后运行崩溃。
2. 分支内重绑定次数不同，分支后读取可能静默返回旧值。
3. 分支导出的实际变量名与 `active_map`、`rename_map` 不一致；原报告给出的匿名函数占用示例需要替换。
4. timeout 表达式的绑定被错误地带入超时体分析。这是校验发现的相邻问题，不是原报告所说的“错误导出到 receive 外部”。
5. 收集 `rename_history` 时，对同一原变量反复尾部追加具有二次复杂度。

不能直接采用以下结论或修复建议：

- timeout 绑定在 `receive` 外可见是合法 Erlang 行为，不能为阻止导出而简单包裹 `with_shadowed_leaky(Timeout)`。
- 让所有 `nonfun_clause` 的 `allocated` 单调累积，会破坏普通互斥分支的共同变量导出。
- 展开协议、双回调入口、调用方 State 隔离、统计开关及 benchmark 已写入当前设计，不能直接归为“计划外实现”。

## 3. 原报告逐项判定

下表编号沿用原报告的章节与条目编号。

| 原报告条目 | 判定 | 校验依据与修正 |
| --- | --- | --- |
| 一-1：`try` / `receive` 暂存分配集合导致重名 | 确认 | 实际 parse transform 后编译报 `unsafe_var`，见 §4.1、§4.2。不能据此将所有互斥分支的分配改为单调累积。 |
| 一-2：timeout 绑定错误泄漏到 `receive` 外部 | 不成立 | OTP 21、25、28、29 均接受 timeout 绑定在 `receive` 之后使用。真正相邻问题是超时体错误地看见该绑定，见 §4.3。 |
| 一-3.1：分支不同重绑定次数导致错误读取 | 确认 | 原报告示例经真实转换、编译和执行，分支 1 返回 1，见 §4.4。 |
| 一-3.2：`active_map` / `rename_map` 失真 | 问题确认，示例需修正 | 匿名函数退出会恢复 `allocated`，原示例没有产生 `A_1`；使用保留占用名的 `catch` 可复现，见 §4.5。 |
| 一-4：`rename_history` 的尾部追加为 O(K²) | 确认，有适用条件 | 仅请求该指标时发生；`collect => []` 不执行这项追加，见 §4.6。 |
| 二-1：展开机制是 quote 回退后的多余设计 | 仓库内缺少生产消费者的观察成立；删除结论不足 | §3.2、§5 明确保留展开、裸 AST、自展开防重入和两入口差异。属于可重新评估的公开接口设计，不是未约定的残留。 |
| 二-2：统计矩阵及开关属于过度设计 | 调用面较窄属实；整体结论需设计决定 | 生产调用主要使用名称工具及 rebinding 的 `collect => []`；`silence_unused_vars/1` 使用 `[bindings, all]`。指标和按需统计均有明确设计与测试。仓库内无直接消费者不能证明导出 API 无外部用途。 |
| 二-3：双入口及动作协议冗余 | 接口复杂的观察成立；不能直接判为 Bug | §3.2、§5 明确规定各形态与入口差异。若精简，应先决定接口契约并评估调用方兼容性。 |
| 二-4：State 拆装只为三参数回调服务 | 不准确 | 二参数回调同样可返回操作调用方 State 的 traverse monad，现有 `monadic_transform_test/0` 覆盖此用法。每节点包装确有开销，但未在本次测量其性能影响。 |
| 三-1：benchmark 及两篇结果报告属于计划外 | 不成立 | 当前设计 §5 明确指定脚本和优化结果报告。脚本确实依赖源码文本匹配，但有唯一匹配断言，不会静默生成未修改的对照。 |
| 三-2：参数组额外 `env` 合并未充分写入设计 | 部分成立 | 具体覆盖规则在 §2.1 的描述不完整；但参数组结束后汇合绑定和重命名是 §2.2、§3.5 的明确要求，不能直接删掉合并。 |
| 三-3：`current_name/2` 的反查回退未充分写入设计 | 确认 | 实现依赖 `env -> bindings` 恢复最终名，且名称不唯一时回退原名已造成实际错误。 |
| 三-4：`init_renames` 用空绑定集合初始化 `env` | 实现事实确认；未确认其本身是 Bug | 该表示让初始重命名对应的名字被视为已绑定，同时没有本次遍历分配的 BindId。需要补充初始化契约，而不是仅因集合为空认定语义错误。 |
| 三-5：map generator 角色特判位于 vars | 实现事实确认；下沉是架构建议 | 普通 map pattern 的键是读取，map generator 的键是绑定；现有特判承担这个区别。可以考虑归入语法层，但必须保留上下文差异。 |
| 三-6：`scope_view()` 类型必填/可选标记与文档不同 | 确认，属于契约描述不一致 | `pattern_kind`、`binding_id`、`same_pattern` 的类型标记有差异；当前 `scope_view/2` 实际始终构造这些键，未发现由此触发的运行错误。 |

## 4. 语义与性能问题的校验证据

### 4.1 try 的 of 与 after 分配重名

输入函数，经 `astranaut_rebinding:parse_transform/2` 转换：

```erlang
f(_) ->
    X = 0,
    try ok of
        ok -> X = 1
    after
        X = 2
    end,
    X.
```

生成的两个区段均使用 `X_1`：

```erlang
try ok of ok -> X_1 = 1 after X_1 = 2 end
```

`compile:forms/2` 返回错误，核心诊断为：

```erlang
{unsafe_var, 'X_1', {'try', 1}}
```

`nonfun_clause` 退出将占用名暂存至 group，但 `try` 后续区段进入时尚未取回这些名字。修复需要区分互斥子句与跨区段名称占用，不能把两种关系统一处理。

### 4.2 receive 消息分支与 timeout 分配冲突

输入：

```erlang
f(_) ->
    X = 0,
    receive
        a -> X = 1
    after begin X = 2, 0 end ->
        ok
    end,
    X.
```

实际转换后，消息分支与 timeout 都使用 `X_1`，末尾也读取 `X_1`。编译诊断：

```erlang
{unsafe_var, 'X_1', {'receive', 1}}
```

这确认转换存在问题；同时涉及 timeout 绑定及其汇合规则，不宜仅按“子句退出丢失 allocated”修补。

后续补充校验还确认：删除末尾的 `X` 读取、将 timeout 改为 100 后，转换结果可编译成功，但 `self() ! a` 后执行该函数会抛出 `{badmatch, 1}`。详情及执行条件见 §8.1。

### 4.3 receive timeout：外部导出合法，超时体可见性有误

标准 Erlang 正例：

```erlang
f() ->
    receive
        a -> ok
    after (T = 0) ->
        ok
    end,
    T.
```

OTP 21、25、28、29 的原生 `compile:forms/2` 均返回 `{ok, Module, Binary, []}`。
当前 `astranaut_vars:analyze/1` 将 `T` 列为 `exported`，该结论不能作为 Bug。

标准 Erlang 反例：

```erlang
f() ->
    receive
        a -> T
    after (T = 0) ->
        T
    end.
```

上述四个 OTP 版本均返回两个 `unbound_var` 诊断，分别对应消息体与超时体中的读取。因此原报告“T 仅在 after Body 内可见”的说法也不成立。

分析器的最小反例：

```erlang
receive a -> ok after (T = 0) -> T end.
```

当前分析器返回 `free=[]`。原因是 timeout 直接修改当前环境，随后超时体从这个环境开始遍历，错误地把 `T` 视为已绑定。

正确修复必须同时满足：timeout 绑定保留在 `receive` 之后；消息体和超时体不能因此获得该绑定。

### 4.4 分支重绑定次数不一致，读取旧值

输入：

```erlang
f(Tag) ->
    case Tag of
        1 -> B = 1, B = B + 1;
        2 -> B = 10
    end,
    B.
```

实际转换：

```erlang
f(Tag) ->
    case Tag of
        1 -> B = 1, B_1 = B + 1;
        2 -> B = 10
    end,
    B.
```

编译成功，但产生 `unused_var B_1` 警告。加载生成的 BEAM 后执行：

| 调用 | 实际结果 | 按分支中新变量的最新绑定预期 |
| --- | --- | --- |
| `f(1)` | `1` | `2` |
| `f(2)` | `10` | `10` |

分支汇合保留多个最终名，而 `current_name/2` 在不能得到唯一名字时回退原名。该回退掩盖了命名不一致，生成可编译但错误的代码。

修复前必须明确分支新变量的汇合规则；不能通过任取一个最终名来解决。

### 4.5 active_map 与实际改写名称不一致

原报告示例：

```erlang
fun(A) -> ok end,
case Tag of 1 -> A = 1; 2 -> A = 2 end,
A.
```

实际不会生成 `A_1`，因为 `shadowed` 作用域退出会恢复 `allocated`。

可复现的替代示例：

```erlang
catch (A = 0),
case Tag of 1 -> A = 1; 2 -> A = 2 end,
A.
```

使用与 rebinding 相同的模式绑定命名策略调用 `rewrite/2`，生成：

```erlang
catch (A = 0),
case Tag of 1 -> A_1 = 1; 2 -> A_1 = 2 end,
A_1.
```

但末尾读取节点的回调 Scope 为 `current_var='A_1'`、`active_map=#{}`，最终结果也有 `rename_map=#{}`。
这是名称解析依赖 `env` 反查，而公开映射只来自 `renames` 的实际不一致。

### 4.6 rename_history 的尾部追加

`record_var/4` 收集绑定历史时使用：

```erlang
maps:get(Orig, V, []) ++ [Final]
```

对同一原变量产生 K 个绑定点，列表拷贝累计为 O(K²)。可考虑头插并在返回结果时反转，保持现有历史顺序。

此结论来自实现复杂度分析，本次没有运行性能基准。该追加位于按键统计更新中，未请求 `rename_history` 时不会执行，不能据此声称生产 rebinding 的 `collect=[]` 路径有该开销。

## 5. 对原修复建议的反例

### 5.1 不能对所有 nonfun_clause 单调累积 allocated

在临时副本中，仅将 `nonfun_clause` 退出时传给 `add_branch/3` 的状态从恢复旧 `allocated` 改为保留当前 `allocated`，其余逻辑不变。

输入：

```erlang
case Tag of 1 -> B = 1; 2 -> B = 10 end,
B.
```

当前实现生成两个分支均绑定 `B`，编译成功。采用该建议后变为：

```erlang
case Tag of 1 -> B = 1; 2 -> B_1 = 10 end,
B.
```

编译报 `{unsafe_var, 'B', {'case', 1}}`。互斥分支的同一个逻辑导出变量仍需协调名称；跨区段避免冲突与分支汇合需要分别解决。

### 5.2 不能为阻止外部导出而直接隔离 Timeout

§4.3 已证明 timeout 新绑定在 `receive` 之后可用。简单包裹 `with_shadowed_leaky(Timeout)` 会恢复进入前的环境，丢掉这个合法导出，不能作为完整修复。

### 5.3 不能以仓库内无消费者直接删除公开接口

展开动作、统计指标及两种回调入口确实扩大了实现面，但当前设计明确包含这些能力。
后续若收敛，需要先决定公开 API 的保留范围，再同步实现、测试与文档；不能把测试中的契约覆盖当作“没有真实用途”的证明。

## 6. 校验方法与证据边界

### 6.1 实际执行

1. 执行 `rebar3 compile`，使用当前提交构建本机 OTP 29 的库。
2. 使用 `/tmp` 中的独立 escript，通过真实 `astranaut_rebinding:parse_transform/2` 转换最小函数。
3. 使用 `compile:forms(Forms, [return_errors, return_warnings])` 编译转换结果；对于分支旧值案例，加载生成的 BEAM 并分别执行 `f(1)`、`f(2)`。
4. 通过 `rewrite/2` 的回调记录 `current_var`、`active_map`、`allocated`，并检查返回的 `rename_map`。
5. 用 `erlang:21`、`erlang:25`、`erlang:28`、`erlang:29` 本地 Docker 镜像验证 §4.3 的原生 Erlang 作用域行为。容器断网，只读挂载探针，不加载项目插件。
6. 在 `/tmp` 编译仅改动 `nonfun_clause` 分配恢复规则的对照模块，验证 §5.1 的建议会破坏原有合法案例。
7. 检查结束时执行 `git status --short`，确认仓库未修改。
8. 收到后续审核补充后，在本机 OTP 29 验证 §8 的消息命中运行错误、提前闭合 try group 的反例，以及不增加分支结构的最终名协调示例；仅更新本文。

### 6.2 复现入口

在仓库根目录执行 `rebar3 compile` 后，可在已加入 `_build/default/lib/astranaut/ebin` 的 Erlang shell 中运行以下步骤；替换 `Source` 即可复现 §4.1、§4.2、§4.4：

```erlang
Source = "f(Tag) -> case Tag of 1 -> B = 1, B = B + 1; 2 -> B = 10 end, B.",
{ok, Tokens, _} = erl_scan:string(Source),
{ok, Form} = erl_parse:parse_form(Tokens),
Forms = [
    {attribute, 1, module, vars_review_probe},
    {attribute, 1, export, [{f, 1}]},
    Form
],
Transformed = astranaut_rebinding:parse_transform(Forms, []),
io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(Transformed))]),
{ok, Module, Binary, Warnings} =
    compile:forms(Transformed, [return_errors, return_warnings]),
{module, Module} = code:load_binary(Module, "probe", Binary),
{Module:f(1), Module:f(2), Warnings}.
```

预期观察到 `{1, 10, Warnings}`，其中包含未使用 `B_1` 的警告。对于 §4.1、§4.2，编译会返回 `{error, Errors, Warnings}`，不应匹配上述成功返回值。

分析器反例可直接运行：

```erlang
{ok, Tokens, _} = erl_scan:string("receive a -> ok after (T = 0) -> T end."),
{ok, Nodes} = erl_parse:parse_exprs(Tokens),
maps:get(free, astranaut_vars:analyze(Nodes)).
```

当前返回 `[]`，未识别超时体中自由读取的 `T`。

### 6.3 未做的校验

- 未修改正式实现、回归测试或设计文档；本文不是修复完成记录。
- 未重跑完整 EUnit、Common Test 或 Dialyzer。
- 转换器缺陷及建议反例主要在本机 OTP 29 验证；只有 §4.3 的原生编译器规则做了四版本对照，§8 的补充探针未做多版本验证。
- 未做本次性能测量；未重新运行已有 benchmark。
- 未调查仓库外部对导出 API 的使用。
- 临时探针不作为长期测试入库；本文保留输入、观察结果及复现入口，后续修复应将有效案例转为正式回归测试。

## 7. 后续修复顺序与验收边界

1. 优先解决分支旧值读取和 `try` / `receive` 名称冲突，避免静默错误及合法方言代码无法编译。
2. 同时确定分支新绑定的名称汇合规则，使 `current_var`、`active_map`、`rename_map` 一致；不得依靠名称不唯一时静默回退原名。
3. 按原生编译器规则拆分 timeout 的外部导出与子体可见性。
4. 独立处理 `rename_history` 的追加复杂度，保留输出顺序和指标投影契约。
5. 补正文档中的参数组覆盖、初始化绑定和 Scope 类型约定；公开 API 精简另作设计决定。

修复至少应保留普通 `case` 共同导出、已有外层绑定不被分支重绑定覆盖、调用方 State 不随词法作用域回滚等既有契约，并新增上述真实失败案例的回归测试。

## 8. 后续审核补充与再次校验

本节记录用户对本文的二次审核，以及对其中新增事实和修复建议的独立验证。源码依据为本机 OTP 29.0.4 的 `stdlib-8.0.3/src/erl_lint.erl` 和对应 `compiler` 应用中的 `src/v3_core.erl`。行号仅对应本机该版本，不作为其他 OTP 版本的定位依据。

### 8.1 receive 重名的运行期错误：确认，但有触发条件

补充输入：

```erlang
f() ->
    X = 0,
    receive
        a -> X = 1
    after begin X = 2, 100 end ->
        ok
    end.
```

实际转换的核心部分：

```erlang
receive
    a -> X_1 = 1
after begin X_1 = 2, 100 end ->
    ok
end
```

`compile:forms/2` 成功，警告中包含未使用变量及无法匹配的相关诊断。加载生成的 BEAM 后执行：

```erlang
self() ! a,
vars_receive_probe:f().
```

捕获到 `error:{badmatch, 1}`。因此 §4.2 不仅是编译失败风险，也存在编译成功后的运行期错误。

执行顺序与 `v3_core:expr/2` 的 receive 处理一致：先处理 timeout 表达式，将其预备表达式放到 receive 之前。因此 `X_1` 已取值 2，消息命中后的 `X_1 = 1` 不能成功。

“必现”应限定为该示例消息 `a` 命中对应分支时；如果超时而未命中该分支，不会发生这次匹配错误。

### 8.2 receive 的分组设计：方向合理，尚非已验证修复

`erl_lint.erl` 的 receive 处理（本机第 3139 行起）分别以入口变量环境 `Vt` 检查 timeout、超时体和消息子句；子体分支汇合后，再合并 timeout 的变量事实。这支持 §4.3 的可见性结论。

二次审核提出：

1. 消息子句与超时体组成分支组；
2. 该分支组与 timeout 从相同的入口环境开始，各自隔离绑定和重命名；
3. 两者之间保留名称占用，退出后合并导出。

这个方向同时考虑了可见性与名称冲突，优于直接丢弃 timeout 导出。但不能仅凭它与 `argument` 作用域相似，就认定现有 `add_argument/3` 可以直接复用为完整实现：两侧导出相同原变量但最终名不同的情况，仍需明确合并规则。

本次未实现该分组方案。后续除 §4.3 的正反例外，还应覆盖 timeout 与消息体、超时体同时绑定同一原变量，以及分支外已有同名绑定的场景。

### 8.3 try 提前闭合 group：还必须阻止分支绑定进入 after

二次审核提出在 `Handlers` 之后闭合 group，使暂存的占用名在进入 `After` 前合并。这个方向针对了 §4.1 的名称占用问题，但仅提前闭合仍不足以构成修复。

在临时模块中，将 group 的进入边界保留在成功路径前，退出边界移动到 handlers 结束后，`After` 随后遍历，其他环境合并规则保持原样。反例为：

```erlang
try ok of
    ok -> B = 1
catch
    _:_ -> B = 2
after
    B
end.
```

分析结果：

| 实现 | `free` |
| --- | --- |
| 当前 `astranaut_vars` | `['B']` |
| 仅提前闭合 group 的临时对照 | `[]` |

原因是 `exit_scope_type(scope_group)` 不只合并 `allocated`，也会把共同分支绑定并入 `env`。提前退出后，这些绑定成为 `After` 的可见入口；随后包裹 `with_shadowed_leaky(After)` 只负责退出恢复，不会清除进入时已存在的绑定。

因此后续修复应让跨区段占用名及时生效，同时让 `After` 从正确的入口绑定/重命名环境开始。不能只改变 group 结束时机而沿用全部导出合并语义。

`erl_lint.erl` 的 try 处理（本机第 3220 行起）也区分了变量事实及其 unsafe 状态；不能把它的事实合并直接等价为 vars 中“所有这些名字均可读取”。

### 8.4 分支最终名汇合：严格单遍存在限制，不是结构上不可能

二次审核指出不同分支的最终名不一致时，不能在分支后任取一个名字。这个约束成立。

但“不插入桥接赋值或改写为返回值，就不可能汇合”的表述过强。以下代码只调整第二分支绑定点的变量名，未增加赋值或改变分支结构：

```erlang
f(Tag) ->
    case Tag of
        1 -> B = 1, B_1 = B + 1;
        2 -> B_1 = 10
    end,
    B_1.
```

本机原生编译器无警告编译通过，运行结果为 `{f(1), f(2)} = {2, 10}`。第二分支不需要也绑定 `B`，因为该名字仅在第一分支内部使用。

准确边界是：若绑定点一经输出便不能重访，且没有预先知道各分支最终绑定的策略，严格流式单遍难以完成这种名称协调。延迟输出、二次改写或基于绑定点的定向重命名仍可能在保持分支结构的情况下实现；桥接赋值或返回值改写是其他可选方案，而非唯一方案。

二次审核提出在唯一最终名的情况下，直接把 `Orig => Final` 写回 `renames`，与 §4.5 的映射一致性目标相符。但应只处理允许导出的新绑定，不能覆盖进入分支前已有的外层映射；不同最终名的情况也不能因此被忽略。移除 `current_name/2` 的反查前，需先证明所有相关汇合路径都已正确维护映射。

### 8.5 补充校验的结果边界

- 确认消息命中后的运行期错误，并收窄其触发条件。
- 确认 receive 的入口环境隔离方向，但没有实现或验证完整修复。
- 对 try 的提前闭合建议给出新的可见性反例，不能认定为已完成的精确修复。
- 对“必须改变分支结构”的断言给出可编译、可执行的反例。
- 所有临时模块仅位于 `/tmp`；正式源码与测试未修改。

## 9. 已实施的修复与 OTP 21–29 验证

本节记录在 `f293365` 基线上实施的工作区修复，尚未提交。

本节为第一轮结果；追加审核发现的缺口及最终验证由 §10 补充和修正。

### 9.1 实现变化

修改 [astranaut_vars.erl](../../src/astranaut_vars.erl)：

1. **try 区段隔离**：在进入 after 前合并主体/成功子句和异常子句的名称占用，同时恢复入口绑定和重命名环境，避免名称冲突及分支绑定泄漏。
2. **receive timeout 隔离**：将消息子句与超时体组成内层分支组，再将该组与 timeout 作为隔离成员处理；两侧共享占用信息但不共享新增可见绑定，组退出后保留合法导出。
3. **分支名称协调**：为共同导出的新变量建立显式映射。不同分支最终名不一致时预留共同名称，在实际读取导出变量时协调对应的到达绑定点，不改变已被替换的早期绑定或闭包捕获。
4. **延迟还原变量名**：内部变量注解暂存 Binding ID 与原始位置；作用域遍历结束后还原最终名及原始位置，内部标记不出现在返回 AST 中。这个阶段不重复调用用户回调。
5. **保留未使用绑定的独立改名**：未读取的分支绑定不强制统一名称，保留 `silence_unused_vars/1` 的既有警告抑制行为。结果 `rename_map` 不为尚未协调的多个最终名报告虚假的唯一映射。
6. **统计与类型约定**：需要统计时记录绑定点引用，最终按协调后的名称计算指标；`collect=[]` 不保存统计事件。`rename_history` 使用头插并在返回时反转。Scope 类型中的必填标记与实际构造保持一致。

设计文档 [Plan 02](02-astranaut-vars-scope-analysis.md) 已同步上述规则。没有删除公开 API、展开协议、map generator 角色特判或 benchmark 文件；它们不属于已确认必须删除的缺陷。

### 9.2 回归测试

新增 [astranaut_vars_scope_regression_tests.erl](../../test/astranaut_vars_scope_regression_tests.erl)，共 8 项 EUnit：

| 测试 | 覆盖内容 |
| --- | --- |
| `try_section_allocation_test` | of/after 名称冲突、after 不读取分支新绑定、try 不导出新绑定 |
| `receive_timeout_allocation_test` | 消息命中时不再 badmatch、timeout 重绑定在外部正确读取 |
| `receive_timeout_visibility_test` | 消息体和超时体不读取 timeout 新绑定，receive 之后仍可读取 |
| `branch_final_binding_test` | case/if 中不同重绑定次数后的最新值 |
| `branch_binding_identity_test` | 分支协调不改变闭包捕获的早期绑定 |
| `nested_branch_binding_test` | 嵌套 case、receive 消息与超时分支的名称协调 |
| `branch_active_map_test` | 分支后的 current_var、active_map、rename_map 一致 |
| `branch_metrics_projection_test` | 最终绑定名、导出和历史统计正确，collect 子集不改变 AST |

最初 7 项测试中有 6 项在修复前失败，复现了 unsafe_var、运行期 badmatch、旧值读取及映射缺失等问题。修复后补充嵌套分支测试；最终 8 项在所有验证版本通过。已有未使用分支变量改名测试也通过。

### 9.3 完整测试矩阵

使用本地 `erlang:21` 至 `erlang:29` Docker 镜像，在每个独立容器内依次执行：

```text
rebar3 compile
rebar3 eunit
rebar3 ct
```

| OTP | 编译 | EUnit | Common Test | 退出状态 |
| --- | --- | --- | --- | --- |
| 21 | 通过 | 242 通过，0 失败 | 287 通过，1 跳过 | 全部 0 |
| 22 | 通过 | 242 通过，0 失败 | 287 通过，1 跳过 | 全部 0 |
| 23 | 通过 | 242 通过，0 失败 | 287 通过，1 跳过 | 全部 0 |
| 24 | 通过 | 242 通过，0 失败 | 287 通过，1 跳过 | 全部 0 |
| 25 | 通过 | 242 通过，0 失败 | 287 通过，1 跳过 | 全部 0 |
| 26 | 通过 | 243 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |
| 27 | 通过 | 243 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |
| 28 | 通过 | 243 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |
| 29 | 通过 | 243 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |

OTP 21–25 少一项 EUnit，是 map comprehension 测试按已有 OTP ≥26 条件编译。Common Test 的跳过来自已有 `test_import_macro_during_code_server_registration_lag`：OTP 26 之前 `code:is_loaded/1` 会被暂停的 code_server 阻塞。没有为本次修复新增跳过项。

### 9.4 验证环境与日志

- 每个容器以只读方式挂载仓库，将 src/include/test/test_data/common_tests 和 rebar.config 复制到容器临时目录；构建互不共享。
- 容器断网。临时 rebar.config 仅删除 `project_plugins`，避免下载与项目库测试无关的发布/容器插件；保留测试 profile、CT 配置和 cover hook。
- 使用容器自带 rebar3，设置 `ERL_FLAGS=+S 2:2` 控制并行资源。
- 本次完整矩阵的脚本保存在 `/tmp/astranaut-scope-matrix.sh`；各版本的 `summary.txt`、`compile.log`、`eunit.log` 和 `ct.log` 保存在 `/tmp/astranaut-scope-matrix/<OTP>/`。这些是本机临时证据，不作为永久路径承诺。
- 本机 OTP 29 的 `rebar3 dialyzer` 通过，无警告；已运行 `rebar3 fmt -w`，并以 `rebar3 fmt --check` 核验。

未对每个 OTP 版本分别运行 Dialyzer，未重测性能。分支协调增加了一次内部名称还原遍历，原有性能报告不能作为本次修复后的性能结论。容器验证不覆盖已移除的 project_plugins 自身兼容性。

## 10. 追加边界审核、修复与最终验证

### 10.1 新报告的核实结果

对用户追加审核中的四项问题，先新增反例再运行测试：当时 12 项作用域测试中有 4 项失败，分别复现 receive 导出优先级、参数绑定引用丢失、非法注解和未读取时预留汇合名。

| 审核项 | 实测结果与边界 |
| --- | --- |
| receive 中 timeout 覆盖新变量的分支导出 | 确认。原先未绑定的 X 同时在 timeout、所有消息/超时分支绑定时，分支后错误返回 2，而不是消息分支的 1 或超时体的 3。 |
| 已有外层 X 也必须由分支重绑定覆盖 | 不采纳。既有方言规则是分支不导出对外层变量的重绑定；timeout 的重绑定可以导出。已有 `X=0` 的对照在两个路径都保持返回 2，不能与新变量共同导出混同。 |
| 同名兄弟参数丢失 BindId、未使用变量改写移除匹配 | 确认。`{A=1,A=2},A` 的首个绑定引用被丢失；没有末尾读取时，两个绑定都会独立改名，同样破坏匹配。 |
| annotation 三元组违反 OTP 契约 | 确认，`erl_anno:is_anno/1` 返回 false。添加自定义二元组也不能解决，因为 tag 本身必须受 OTP 支持。 |
| 所有 annotation setter 都会因三元组崩溃 | 表述过强。本机 OTP 29 的 `erl_anno:set_generated(true, OldAnno)` 仍返回结果，但结果依然不是合法 annotation。本轮以合法性断言及实际遍历边界测试锁定问题。 |
| 未读取时提前预留汇合名 | 确认。分支后的普通节点曾观察到 AST 中不存在的 B@1 映射和占用。 |
| 兄弟参数应无条件合并所有最终名 | 不采纳。标准共享最终名需要合并 BindId；调用方已经生成不同最终名时，它们是独立的方言绑定，不能强制改成同名匹配。 |
| receive 必须三侧都绑定才能安全导出 | 不成立。§4.3 的四版本原生编译器正例已证明 timeout 单独绑定即可在 receive 之后读取。 |

新报告中关于 `walk_list`、try 无 of 时向外暴露主体绑定的源码描述与本轮修复前的实际实现不符，不作为修改 try 行为的依据；§8.3 已验证的 try 可见性边界继续保留。

### 10.2 已实施的追加修复

1. **receive 按运行顺序处理**：先遍历 timeout，再遍历独立的消息/超时分支组，保持两侧入口环境隔离和名称占用传递。对于原先未绑定的变量，后执行的共同分支绑定优先导出；已有外层变量仍遵循分支重绑定不导出的规则。
2. **保留兄弟参数的匹配约束**：相同原名且最终名相同的参数绑定保留全部 BindId，并登记隐式匹配引用；即使后面没有读取，也不会把有语义作用的共享变量拆成两个独立的下划线变量。
3. **保留方言参数的独立绑定**：若调用方为兄弟参数生成不同最终名，仍由后一个参数绑定向后生效。标准分析的约束修复不会把 rebinding 的 `{A=1,A=2}` 改成必须相等。
4. **使用合法 annotation**：仅可能参与分支协调的绑定及其引用，使用 OTP 支持的 `text` 字段保存内部身份和原始 annotation；每次遍历有独立前缀。还原后保留原始 location、file、generated 和 text，不留下内部标记。
5. **普通遍历快速路径**：没有上述延期绑定时，不产生内部标记，也不执行最终还原扫描。不能只按“是否真正汇合过”跳过清理，否则已经生成的标记会遗留到结果。
6. **实际读取时才协调名称**：组退出不为不同最终名预留新名字、不写虚假映射；实际读取时优先选取调用方已有且不与其他绑定点冲突的最终名，没有安全候选时才使用通用 fresh_var。常见 rebinding 示例保持 B_1，而不是额外生成 B@1。

### 10.3 回归证据

作用域测试模块现有 15 项，新增边界包括：

- **receive 新变量优先级**：`receive a -> X=1 after begin X=2,0 end -> X=3 end, X`，消息与超时路径分别返回 1、3。
- **receive 已有外层变量**：相同结构前置 `X=0`，两个路径均返回 timeout 的 2，保留已有方言规则。
- **匹配不能被警告抑制删除**：对 `{A=1,A=2},A` 及 `{A=1,A=2}` 执行 `silence_unused_vars/1` 后，运行仍抛出 `{badmatch,2}`。
- **不同最终名仍独立**：isolated 参数模式下经 rebinding 命名策略改写，`V={A=1,A=2},{V,A}` 返回 `{{1,2},2}`。
- **内部 annotation 合法**：在自定义 uniplate 边界检查 `erl_anno:is_anno/1`，调用 `erl_anno:set_generated/2`，并开启 output validation，全部通过。
- **位置元数据保留**：使用带 file、generated、text 和行列信息的变量 annotation，最终输出逐项保留；普通无分支遍历不出现内部 text。
- **未读取不预留**：分支后普通原子节点的回调没有 B 的虚假 active_map 项，allocated 只包含实际绑定名 B、B_1。
- **优先复用已有最终名**：常见两分支示例使用 B_1；候选名与其他绑定冲突时采用安全回退，仍正确返回 2、11。

§9 的闭包捕获、嵌套分支、统计投影及 try/receive 可见性测试继续通过。

### 10.4 最终 OTP 21–29 完整矩阵

使用与 §9 相同的隔离容器方法，对本轮最终源码重新执行 `rebar3 compile`、`rebar3 eunit` 和 `rebar3 ct`。

| OTP | 编译 | EUnit | Common Test | 退出状态 |
| --- | --- | --- | --- | --- |
| 21 | 通过 | 249 通过，0 失败 | 287 通过，1 原有跳过 | 全部 0 |
| 22 | 通过 | 249 通过，0 失败 | 287 通过，1 原有跳过 | 全部 0 |
| 23 | 通过 | 249 通过，0 失败 | 287 通过，1 原有跳过 | 全部 0 |
| 24 | 通过 | 249 通过，0 失败 | 287 通过，1 原有跳过 | 全部 0 |
| 25 | 通过 | 249 通过，0 失败 | 287 通过，1 原有跳过 | 全部 0 |
| 26 | 通过 | 250 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |
| 27 | 通过 | 250 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |
| 28 | 通过 | 250 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |
| 29 | 通过 | 250 通过，0 失败 | 288 通过，0 跳过 | 全部 0 |

各版本日志保存于 `/tmp/astranaut-scope-review-matrix/<OTP>/`，包含 summary、compile、eunit、ct 四份记录，与 §9 的第一轮日志分开。

已重新执行 `rebar3 fmt -w`、`rebar3 fmt --check` 及本机 OTP 29 的 `rebar3 dialyzer`，全部通过。未新增版本条件跳过；未运行新的性能基准或逐版本 Dialyzer；容器仍不加载 project_plugins。代码及文档尚未提交。
