# Macro Expansion Spec Delta

## ADDED Requirements

### Requirement: 白名单检查必须显式启用

通用 function 展开接口 MUST 接收显式 `LocalMacroWhitelistControl`。控制值 MUST 区分 `disabled`、首次 `collect` 和后续 `verify(Expected)`；系统 MUST NOT 根据 MacroEnv 或 function 名称隐式启用白名单。

#### Scenario: 普通 function 禁用白名单

- **给定** 普通 Step 2 function 调用一个或多个 local macros
- **当** 通用 function 展开器处理该 form
- **那么** 调用方传入 `disabled`
- **并且** 系统不创建 observed whitelist、不比较 canonical whitelist，也不产生 whitelist conflict
- **并且** function 仍按完整 FinalMacroEnv 的普通规则展开

#### Scenario: local frozen function 启用收集

- **给定** frozen FormId 首次作为 local-macro closure function 展开
- **当** `astranaut_local_macro` 调用通用 function 展开器
- **那么** 调用方传入 `collect` 和该 FormId
- **并且** 成功结果返回本次实际匹配的 local macro FA ordset

#### Scenario: local frozen function 后续处理启用校验

- **给定** frozen FormId 已有 canonical whitelist
- **当** 它在另一个 declaration context 或 final context 下再次处理
- **那么** 调用方传入 `verify(Expected)`
- **并且** retained local-macro head 与 frozen helper 使用相同校验规则

#### Scenario: final retained 环境按 canonical whitelist 过滤

- **给定** retained frozen FormId 已有 canonical whitelist
- **并且** FinalEnv 包含其他同声明或后声明 local macros
- **当** 系统构造该 FormId 的 final expansion env
- **那么** 只有 canonical whitelist 中的 local entries 进入有效 MacroEnv
- **并且** 名单外调用保持普通 Erlang 调用，不产生 unexpected whitelist 错误

### Requirement: 宏返回 AST 在统一发现—执行点观察

启用白名单时，系统 MUST 在原始 function 和所有递归宏返回 AST 中观察实际匹配的 local macro FA。观察 MUST 位于 `match_macro_call` 成功后、macro 调用前的统一路径。`process_macro_return` MUST 只负责返回 AST 的规范化、位置和变量更新，不负责 macro 匹配或展开。系统 MUST NOT 为白名单重新 traverse 完整 function forms 或对 expanded/original forms 做 AST diff。

#### Scenario: 宏返回值生成新的 local macro 调用

- **给定** local frozen function 首先调用 external macro A
- **并且** A 的 replacement AST 包含 local macro B 调用
- **当** A 的返回 AST 通过原有递归展开路径发现 B
- **那么** B 在统一发现—执行点、调用前进入当前 FormId 的 observed whitelist
- **并且** B 随后由同一发现—执行路径展开
- **并且** original function 的其他节点和已处理 siblings 不被重扫

#### Scenario: 多层 replacement 继承同一控制参数

- **给定** A 的 replacement 调用 B，B 的 replacement 又调用 C
- **当** local frozen function 以 `collect` 或 `verify` 展开
- **那么** A、B、C 的发现—执行路径继承同一 whitelist control 和 accumulator
- **并且** `process_macro_return` 不执行 B 或 C，也不启动独立 whitelist scan pass

#### Scenario: replacement 首次发现尚未 callable 的 local macro

- **给定** external macro replacement 首次生成候选 local macro B 调用
- **并且** B 尚未进入 callable generation
- **当** 展开器匹配到 B
- **那么** 展开器返回 B 的 callable 调度请求且不调用 B
- **并且** local-macro workflow 通过 `need_callable` 编译所需 boundary
- **并且** 系统从 frozen form 重试并在成功后才提交 whitelist 与 expanded form

### Requirement: 白名单不匹配必须独立报告

首次成功的 `collect` 结果 MUST 成为该 FormId 的 canonical whitelist。后续 `verify` MUST 在观察到 expected 之外的 local FA 时立即记录错误并跳过该 macro 调用，并在完整 function expansion 结束后检查缺失 FA。

#### Scenario: 多出 FA 时提前失败

- **给定** canonical whitelist 是 `[a/1]`
- **当** 后续 traversal 实际匹配到 `b/1`
- **那么** 系统立即报告 `conflicting_local_macro_whitelist` 并且不调用 `b/1`
- **并且** traversal 可以继续报告后续独立冲突，无需维护全局 conflict state
- **并且** 不要求先展开完整 function forms
- **并且** 本场景适用于 declaration/非-final verify；final retained 已先过滤环境

#### Scenario: 缺失 FA 只在完成后失败

- **给定** canonical whitelist 是 `[a/1, b/1]`
- **并且** 当前 traversal 暂时只观察到 `a/1`
- **当** traversal 尚未完成
- **那么** 系统不得提前报告缺失 `b/1`
- **当** 完整 function expansion 结束仍未观察到 `b/1`
- **那么** 系统报告 `conflicting_local_macro_whitelist`

#### Scenario: 白名单不同但 AST 相同仍冲突

- **给定** 两个环境的最终 expanded form 相同
- **但是** observed local macro whitelist 不同
- **那么** 系统报告 `conflicting_local_macro_whitelist`

### Requirement: 展开结果一致性检查必须保留

白名单一致只证明 local macro 调用集合一致。系统 MUST 继续比较不同 external/options/inject contexts 下的最终 expanded form。

#### Scenario: 白名单相同但 AST 不同

- **给定** 两个环境具有相同 canonical whitelist
- **并且** 不同 `inject_attrs` 值产生不同 expanded form
- **那么** 系统报告 `conflicting_local_macro_closure_environment`

#### Scenario: 普通 function 不参与两类白名单比较

- **给定** 普通 function 以 `disabled` 展开
- **当** 其匹配或递归生成 local macro 调用
- **那么** 系统不读取或更新 local-macro whitelist ExpansionRecord

## MODIFIED Requirements

### Requirement: local-macro ExpansionRecord

local-macro ExpansionRecord MUST 同时保存 canonical whitelist 与 canonical expanded form。cache input key MUST 基于展开前可知的运行环境；白名单作为展开结果保存，不得作为首次 lookup 的唯一 key。

#### Scenario: 同一输入复用完整 expansion 结果

- **给定** 某个 FormId 和 input fingerprint 已缓存 whitelist 与 expanded form
- **当** 相同 input fingerprint 再次请求该 local-macro function expansion
- **那么** 系统复用缓存的 whitelist 和 expanded form
- **并且** 不为 cache lookup 预先执行白名单扫描
