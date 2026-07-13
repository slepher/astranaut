# Local Macro White List 设计

## 设计结论

白名单是 local-macro function expansion 的可选观察和校验策略，不是所有 function 展开的通用语义。调用方必须显式传入控制参数；通用展开器不得根据 MacroEnv、FormId 或调用阶段隐式推断是否启用。

```text
LocalMacroWhitelistControl =
    disabled
  | #{mode := collect,
      form_id := FormId}
  | #{mode := verify,
      form_id := FormId,
      expected := ordsets:ordset(FA)}
```

- `disabled`：不分配白名单 accumulator，不观察 local macro match，不做完成检查。
- `collect`：累计实际匹配的 local FAs，完整 function expansion 成功后返回 canonical whitelist。
- `verify`：累计实际匹配的 local FAs；发现 `expected` 之外的 FA 时立即失败，完成后检查是否缺少 expected FA。

布尔参数不足以区分首次收集和后续校验，因此不采用 `check_whitelist => true | false`。

## 启用边界

| 调用场景 | 参数 |
|---|---|
| local declaration 的 frozen function 预展开 | `collect` 或已有 canonical 时 `verify` |
| 同一 frozen FormId 在另一个 local declaration context 下处理 | `verify` |
| retained local-macro head 或 frozen helper 的 final-context 处理 | `verify` |
| local macro 展开生成的 replacement AST | 继承当前 control |
| 普通 Step 2 function form | `disabled` |
| 不属于 local frozen closure 的普通 retained function | `disabled` |
| attribute macro 调用 | `disabled`；它不是 local-macro 定义 function 的展开 |

启用依据由 `astranaut_local_macro` 的 frozen FormId 生命周期决定，而不是由 function 是否碰巧调用 local macro 决定。

## 展开接口

通用 function 展开操作增加显式控制参数，并用单一结果形状返回观察结果：

```text
ExpandFunction(MacroEnv, InjectForms, Forms, TargetFA, WhitelistControl)
  -> {
       forms := ExpandedForms,
       local_macro_whitelist := disabled | ordsets:ordset(FA)
     }
  | Error
```

普通 function 传 `disabled`，结果字段固定为 `disabled`。local-macro 工作流使用返回的 ordset 建立或校验 ExpansionRecord。不得通过回调、process dictionary 或另一份隐式 traverse state 把结果传回调用方。

## 单 traversal 接入

`process_macro_return` 已经对宏返回 AST 执行结构规范化、位置和 quoted-variable 更新。local match 观察必须接入这次已有 traversal：

```erlang
Node1 <- process_macro_return(Node, Macro, Opts)
```

其中 `Opts` 传播 `local_macro_whitelist` control。处理每个返回节点时：

1. 按现有 pre/post 规则调用统一 `match_macro_call`。
2. 未匹配或匹配 external macro：白名单不变。
3. 匹配 `macro_source := local_macro`：在调用宏之前观察其 `{Function, Arity}`。
4. 宏返回的 replacement AST 继续通过其自身 `process_macro_return` 处理，并继承同一个 control/accumulator。
5. replacement 完成后恢复原 traversal continuation，不重扫父节点、已处理 siblings 或完整 function forms。

原始 frozen function 的首次 traversal 和宏返回 AST 的后续处理必须使用同一个观察规则。不得在 `process_macro_return` 返回后再次调用 `transform_exprs(Node1)` 只为扫描白名单。

宏私有 State 继续由 `scoped_state` 隔离；白名单 accumulator 属于框架 expansion state，不能暴露给用户宏 computation。

## 候选环境与观察集合

必须区分：

```text
CandidateLocalEnv
  = declaration 时冻结、可能被该 local-macro function 匹配的 local entries

ObservedLocalFAs
  = 本次完整递归展开中实际匹配的 CandidateLocalEnv 子集
```

local-macro 工作流负责为 declaration 和后续 final 处理构造相同能力边界的 CandidateLocalEnv；通用展开器只观察实际匹配，不解释 declaration order、self、retain 或 generation。

普通 function 在 `disabled` 模式使用完整 FinalMacroEnv，不受 CandidateLocalEnv/ObservedLocalFAs 约束。

## 冲突检测

首次成功的 `collect` 结果建立：

```text
CanonicalWhitelist(FormId) = ObservedLocalFAs
```

`verify` 模式采用非对称的提前检测：

```text
每次观察到 FA:
  Observed1 = Observed0 + FA
  Unexpected = Observed1 - Expected
  Unexpected 非空
    -> 立即 conflicting_local_macro_whitelist

完整 function expansion 成功后:
  Missing = Expected - Observed
  Missing 非空
    -> conflicting_local_macro_whitelist
```

缺失项不能在 traversal 中途报告，因为后续 replacement AST 仍可能生成对应调用。白名单只单调增长，因此多出的 FA 可以立即报告。

错误应携带：

```text
{conflicting_local_macro_whitelist,
 FormId,
 #{expected => Expected,
   observed => Observed,
   unexpected => Unexpected,
   missing => Missing}}
```

白名单相同后，仍执行现有最终 AST 一致性比较：

- whitelist 不同：`conflicting_local_macro_whitelist`；
- whitelist 相同但 expanded form 不同：`conflicting_local_macro_closure_environment`；
- 两者都相同：接受并缓存。

不同白名单即使产生相同 AST 也视为冲突，因为本变更把调用能力集合稳定性定义为独立语义，而不是单纯性能优化。

## ExpansionRecord

```text
ExpansionRecord = #{
  canonical_whitelist := ordsets:ordset(FA),
  canonical_result := Form,
  results_by_input := #{InputFingerprint => #{
    whitelist := ordsets:ordset(FA),
    result := Form
  }}
}
```

InputFingerprint 继续覆盖当前展开可观察的 external macro map、候选 local descriptors/versions、macro options 和 inject forms。白名单是展开输出，不能作为首次 cache lookup 所需的唯一输入 key。

白名单外 local generation 不得进入该 local-macro function 的有效 fingerprint。普通 function 为 `disabled`，继续使用现有普通 function fingerprint/展开规则。

## 循环与错误传播

白名单观察不建立新的递归驱动器。现有 macro depth、origin/current macro、异常恢复、warning/formatter 和结构验证继续由同一 traversal/monad 管线处理。

任何 whitelist 或 expansion-result 冲突都必须使当前 local declaration/preparation 事务失败，不提交部分 ExpansionRecord、canonical forms 或 generation。普通 function 的 `disabled` 路径不得产生 whitelist 错误。

## 复杂度约束

本变更允许增加一个显式 control、一个 expansion accumulator 和 ExpansionRecord 中的一份 canonical whitelist。不得同时保留 final order/self/member 排除链作为第二套正确性机制；白名单上线后应删除已失去职责依据的排除与 owner-union 路径。

