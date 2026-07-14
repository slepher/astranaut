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
- `verify`：累计实际匹配的 local FAs；declaration/非-final context 中发现 `expected` 之外的 FA 时立即记录冲突并跳过该 macro 调用，完成后检查是否缺少 expected FA。final retained 展开先用 `expected` 过滤 FinalEnv，因此名单外调用不匹配为 local macro。

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
       local_macro_whitelist := disabled | ordsets:ordset(FA),
       needed_local_macros := ordsets:ordset(FA)
     }
  | Error
```

普通 function 传 `disabled`，白名单结果固定为 `disabled` 且 callable 请求为空。local-macro 工作流使用返回的 whitelist 建立或校验 ExpansionRecord，并消费 `needed_local_macros` 完成按需调度。不得通过回调、process dictionary 或另一份隐式 traverse state 把结果传回调用方。

## 统一发现—执行接入

白名单观察必须位于既有 macro 发现—执行路径，而不是 `process_macro_return`：

```text
match_macro_call
  -> observe local FA
  -> conflict: add error and skip invocation
  -> uncallable: request dependency
  -> callable: invoke macro
```

`process_macro_return` 保持单一职责，只执行结构规范化、位置和 quoted-variable 更新。它不匹配或调用 replacement 中的 macro。宏返回 AST 随后按原有 pre/post 递归展开路径进入 `transform_exprs`，并在真正发现、即将执行 macro 时继承相同 whitelist control 和 accumulator。

因此原始 frozen function 和所有 replacement AST 共用同一个观察规则；不得为了 whitelist 增加独立 scanner、完整 function 重扫或 expanded/original AST diff。原有 macro expansion 对 replacement AST 的递归处理不属于 whitelist 扫描。

宏私有 State 继续由 `scoped_state` 隔离；白名单 accumulator 属于框架 expansion state，不能暴露给用户宏 computation。

## 候选环境与观察集合

必须区分：

```text
CandidateLocalEnv
  = declaration 时冻结、可能被该 local-macro function 匹配的 local entries

ObservedLocalFAs
  = 本次完整递归展开中实际匹配的 CandidateLocalEnv 子集
```

whitelist 是 local-macro 环境的组成部分，不是 FinalEnv 之外的事后结果过滤器。local-macro 工作流按阶段构造能力边界：

- declaration `collect` 使用冻结的 CandidateLocalEnv；
- declaration/非-final `verify` 使用当前 CandidateLocalEnv，并由观察器在调用前拒绝 whitelist 外匹配；
- final retained `verify` 使用 `FinalEnv + canonical whitelist` 构造有效 local env，名单外 local entries 不参与匹配，其调用保持普通 Erlang 调用。

通用展开器只观察传入环境中的实际匹配，不解释 declaration order、self、retain 或 generation。

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

缺失项不能在 traversal 中途报告，因为后续 replacement AST 仍可能生成对应调用。白名单只单调增长，因此每个多出的 FA 可以在匹配点立即报告并跳过调用；遍历可继续收集其他独立冲突，不需要额外的全局 conflict state。

上述 unexpected 检测适用于非-final verify。final retained 的有效环境已经按 canonical whitelist 过滤，名单外 FA 不会成为 local macro match，因此不会产生 unexpected 错误。

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

final retained 已有 canonical whitelist 时，fingerprint 使用过滤后的有效 MacroEnv；名单外 local descriptor 与 generation 都不进入该 form 的 final input key。

## 动态 callable 调度

`collect` 或非-final `verify` 可能在 external/local replacement AST 中首次匹配到尚未 callable 的候选 local macro。该情况不是 whitelist 冲突。通用展开器返回 `needed_local_macros` 调度请求，不调用该宏，也不提交本次部分 expansion；`astranaut_local_macro` 通过 `need_callable` 编译所需累计 boundary，然后从 frozen form 重新展开。这样保留按真实匹配驱动的最小编译策略，无需预编译全部 candidates。

## 循环与错误传播

白名单观察不建立新的递归驱动器。现有 macro depth、origin/current macro、异常恢复、warning/formatter 和结构验证继续由同一 traversal/monad 管线处理。

任何 whitelist 或 expansion-result 冲突都必须使当前 local declaration/preparation 事务失败，不提交部分 ExpansionRecord、canonical forms 或 generation。普通 function 的 `disabled` 路径不得产生 whitelist 错误。

## 复杂度约束

本变更允许增加一个显式 control、一个 expansion accumulator 和 ExpansionRecord 中的一份 canonical whitelist。不得同时保留 final order/self/member 排除链作为第二套正确性机制；白名单上线后应删除已失去职责依据的排除与 owner-union 路径。
