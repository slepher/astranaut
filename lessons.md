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

在确认 `map_forms_splice` 正确之前反复尝试集成整个 pass 导致了大量无效迭代。应首先对 `map_forms_splice/3` 写独立测试验证：
- form 透传
- splice-back
- State 持久化
- return→traverse 桥接

验证通过后再集成到外部 pass。
