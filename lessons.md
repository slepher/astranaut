# Lessons Learned

在 macro-passes 实现过程中反复出现的错误模式及正确做法。

---

## 不要混淆 traverse 和 return monad

`astranaut_traverse:lift_m/2` 不能桥接 `astranaut_return:struct()`。

```erlang
%% 错误
Expanded <- astranaut_traverse:lift_m(
              fun(V) -> V end,
              astranaut_traverse:eval(expand_macro(...), ...))
%% lift_m 期望 traverse monad，收到 return monad 会静默失败

%% 正确（如果必须走 return monad）
Expanded <- astranaut:traverse_return(
              astranaut_traverse:eval(expand_macro(...), ...))
%% traverse_return 内部调用 astranaut_traverse:astranaut_traverse/1，
%% 正确处理 RETURN_OK / RETURN_FAIL / TRAVERSE_M 三种类型
```

**规则：** 任何返回 `astranaut_return:struct()` 的函数（`used_macros`、`validate`、`eval`）桥接到 traverse monad 时，用 `astranaut:traverse_return/1`，不用 `lift_m`。

但是需要隔离 State 的场景（如 `expand_macro`），应优先用 `scoped_state/2`，避免进出 return monad 的桥接开销。

## State 操作必须在 do/bind 中串联

traverse monad 中 `put`、`modify`、`get` 返回的是 monad 值，用普通逗号 `,` 会丢弃：

```erlang
%% 错误 — put 的返回值被丢弃，State 不更新
fun(Form) ->
    astranaut_traverse:put(NewState),
    astranaut_traverse:return({splice, []})
end.

%% 正确 — 通过 do 串联
fun(Form) ->
    do([ traverse ||
           astranaut_traverse:put(NewState),
           return({splice, []})
       ])
end.
```

## 不要用 `_ <-` 表示仅执行 monadic action

在 `do` 块内，如果一个 monadic action 只需要被串联，而其返回值不参与后续计算或模式匹配，直接写 action 即可。`_ <- Action` 中的 `_` 匹配任何值，不提供验证或数据传递，只会增加绑定噪音。

```erlang
%% 冗余 — 返回值被无条件丢弃
do([ traverse ||
       _ <- astranaut_traverse:warning(invalid_macro_attribute),
       return(Form)
   ]).

%% 正确 — 直接表达“发出 warning 后继续”
do([ traverse ||
       astranaut_traverse:warning(invalid_macro_attribute),
       return(Form)
   ]).
```

只有在返回值会被使用、需要通过模式匹配约束结果，或当前 `do` 语法明确要求 bind 时才使用 `<-`。这不改变上一节规则：monadic action 仍必须位于 `do`/bind 流程内；在普通函数体中用逗号分隔仍会丢弃 monad 状态。

## Traversal callback 中让框架补充诊断位置

`astranaut:map_m`、`mapfold` 等 traversal 会在调用 callback 时，用当前节点的
位置包装 callback 返回的 monad，并使用当前 traversal formatter 格式化 pending
error/warning。因此 callback 报告当前节点的诊断时，应使用
`astranaut_traverse:error/1`、`warning/1` 或 `fail/1`，不要重复构造
`formatted_errors/1` 或 `formatted_warnings/1`。

```erlang
%% 冗余 — 手动重复当前节点的位置和 formatter
Pos = erl_syntax:get_pos(Node),
astranaut_traverse:then(
  astranaut_traverse:formatted_errors(
    [{Pos, ?MODULE, {invalid_node, Node}}]),
  astranaut_traverse:return(Node)).

%% 正确 — 非终止错误保留节点并继续遍历
astranaut_traverse:then(
  astranaut_traverse:error({invalid_node, Node}),
  astranaut_traverse:return(Node)).

%% 正确 — 终止当前 traversal 分支
astranaut_traverse:fail({invalid_node, Node}).
```

`error/1` 返回携带错误的 `state_ok`，需要通过 `then`、`bind` 或 `do` 与后续
`return` 串联，适用于保留节点并继续收集同一分支中的错误。`fail/1` 返回
`state_fail`，适用于当前分支不能继续处理的情况。不要仅为了自动设置位置而把
原有 `fail/1` 改成 `error/1`；二者都会由外层 traversal 补充位置，但错误恢复
语义不同。

只有错误属于另一个节点、需要指定不同位置或不同 formatter，或者正在转发已经
格式化的 `error_marker` 信息时，才使用 `formatted_errors/1`。

## Return monad 递归处理应在节点边界格式化诊断

如果递归 AST 处理使用 `astranaut_return` 而不是 traversal monad，外层 traversal
只能看到整个入口节点，不能自动为内部节点的 warning/error 设置精确位置。不要在
每个业务分支中散落 `formatted_warning`；应在每层递归节点的统一入口用
`astranaut_return:with_error/2` 和 `astranaut_error:update_pos/3` 格式化该节点产生
的 pending 诊断，业务分支只调用普通 `warning/1` 或 `error/1`。

```erlang
quote_node_boundary(Node, Opts) ->
    Pos = node_pos(Node, Opts),
    astranaut_return:with_error(
      fun(ErrorStruct) ->
              astranaut_error:update_pos(Pos, ?MODULE, ErrorStruct)
      end,
      quote_node(Node, Opts)).
```

子节点边界已经格式化的诊断不会被父节点的 `update_pos/3` 重写；父节点只格式化
仍处于 pending 状态的诊断。这样直接调用 API 和经 traversal 调用时能得到一致的
warning，同时保留内部节点位置。

当 return monad 分支只是产生 warning 后返回普通值时，使用
`astranaut_return:warning_ok(Warning, Value)`，不要展开写成
`then(warning(Warning), return(Value))`。如果后续是一个可能失败的 monadic
computation，则应保留 `then(warning(...), NextReturn)`；改成先执行 `NextReturn`、
再通过 `bind` 调用 `warning_ok`，会导致 `NextReturn` 失败时丢失原本已经产生的
warning。

## expand_macro 必须用 scoped_state 隔离 State

`expand_macro` 内部使用 traverse State 做 depth tracking（`put(1)`），与 handler 的 State 冲突。

```erlang
%% 错误 — expand_macro 的 put(1) 会覆盖 handler 的 ExternalEnv
Expanded <- expand_macro(Macro, #{expected_role => form})

%% 正确 — scoped_state 在同一 traverse 流中临时换成内部 State
Expanded <- astranaut_traverse:scoped_state(
              ok,
              expand_macro(Macro, #{expected_role => form}))
%% 结束后内部 State 丢弃，外层 State 恢复。
%% 错误/warning/formatter/attr/file 照常沿当前 traverse 管线传播。
%% 不需要 eval + traverse_return 的 return monad 桥接。
```

如果需要保留内部最终 State，用 `scoped_state_run/2`：

```erlang
{Expanded, InnerState} <- astranaut_traverse:scoped_state_run(
                            ok,
                            expand_macro(Macro, #{expected_role => form}))
```

## 不要全局拆分后一次性 insert_forms

最初在扫描末尾 `partition` 所有 form 为 Generated / Original → `insert_forms(Generated, Original)` → 错误移动了非冲突的生成函数。

**正确：** 逐个处理标记 form，只在确需 `__original__` 重命名时执行合并，非冲突 form 保持原地位置。

## 先写单元测试

在确认 `astranaut_macro:map_forms_splice/3` 正确之前反复尝试集成整个 pass 导致了大量无效迭代。应首先对该 macro 专属扫描循环写独立测试验证：
- form 透传
- splice-back
- State 持久化
- return→traverse 桥接

验证通过后再集成到外部 pass。
