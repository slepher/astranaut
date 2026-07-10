# Local Macro 规范增量

## 需求：按 FA 注册 local macro

### 场景：重复 declaration 失败

- **给定** 两个 `-local_macro` declaration 注册相同 FA
- **当** 注册第二个 declaration 时
- **那么** 编译以 `duplicate_local_macro_declaration` 失败

### 场景：后声明 macro 属于先声明闭包

- **给定** A 在 B 前声明，且 B 是 A 的静态闭包成员
- **当** 计算 A 的闭包时
- **那么** B 按 helper 处理，可同时属于 A 与自身的闭包

## 需求：声明位点环境

每个 local macro 的闭包必须使用 declaration 前已 pass 的外部环境，以及闭包实际引用的 local macro 组成的环境快照。

### 场景：声明前 import 对闭包可见

- **给定** `-import_macro(macro_a)` 在 `-local_macro([foo/1])` 之前已 pass
- **并且** foo 的闭包实际调用 macro_a 中的宏
- **当** 编译 foo 时
- **那么** 其展开环境包含 macro_a

### 场景：声明后 import 不回溯进入闭包环境

- **给定** `-local_macro([foo/1])` 已注册
- **并且** 后续属性生成 `-import_macro(macro_b)`
- **当** 编译 foo 时
- **那么** foo 的 declaration 环境不包含 macro_b

### 场景：仅实际引用的 local macro 进入环境

- **给定** 多个 local macro 已注册或已编译
- **并且** foo 的闭包仅实际使用其中 a/1
- **当** 构造 foo 的展开环境时
- **那么** LocalEnv 部分仅包含 a/1

### 场景：自身递归调用不是宏依赖

- **给定** `-local_macro([foo/1])` 且 foo/1 函数体调用 foo/1
- **当** 计算 foo 的闭包和引用 local macro 集合时
- **那么** 该调用按普通 Erlang 函数调用处理
- **并且** foo/1 不进入自己的 local macro 依赖集合

## 需求：冻结原始闭包 forms

### 场景：闭包在注册时冻结

- **给定** 扫描遇到 `-local_macro([foo/1])`
- **当** foo 的闭包计算完成
- **那么** 闭包 function/spec 的原始 forms 保存为冻结输入

### 场景：后续 splice 不可改写冻结输入

- **给定** function/spec 已被冻结
- **当** 属性宏 splice 输出生成相同 form ID
- **那么** 报 `illegal_locked_form_mutation`
- **并且** 不插入该 attribute 的 splice 结果
- **并且** 统一扫描继续处理后续 forms

## 需求：跨环境展开比对与缓存

### 场景：相同环境复用缓存

- **给定** 同一原始 form 再次以相同 EnvFingerprint 需要展开
- **当** 构造累计模块时
- **那么** 复用缓存结果，不重复展开

### 场景：不同环境结果不一致报错

- **给定** helper 同时属于两个闭包
- **并且** 两个环境下的展开结果不同
- **当** 结果被比较时
- **那么** 报 `conflicting_local_macro_closure_environment`

### 场景：不同环境结果一致时成功

- **给定** 同一 helper 属于两个 declaration 环境不同的闭包
- **并且** 两个环境下从原始 form 展开的结果一致
- **当** 构造累计模块时
- **那么** 编译成功并复用该一致结果

## 需求：按声明顺序最小累计编译

### 场景：B 需要 A

- **给定** A 在 B 前声明，且 B 的闭包实际使用 A 作为宏
- **当** 首次需要调用 B 时
- **那么** 先编译 `{A}`，再编译 `{A,B}`

### 场景：B 不需要 A

- **给定** A 在 B 前声明，且 B 的闭包不使用 A 作为宏
- **当** 首次需要调用 B 时
- **那么** 直接编译 `{A,B}`

### 场景：scan 收尾编译全部 local macro

- **给定** 某些 local macro 已在 earlier attribute 调用时编译，另一些从未被调用
- **当** local-macro 工作流收尾时
- **那么** 按注册顺序构造包含全部 local macro 的最终累计模块
- **并且** 已展开的 form/environment 组合从缓存复用

## 需求：extra_functions 与 internal_function

### 场景：extra_functions 补充闭包

- **给定** local macro options 包含 `{extra_functions, [helper/1]}`
- **当** 计算闭包时
- **那么** helper/1 进入闭包并按同一冻结、展开和比对规则处理

### 场景：extra_functions 引用不存在函数失败

- **给定** `{extra_functions, [missing/1]}`
- **当** 注册 local macro 时
- **那么** 编译以 `invalid_extra_functions` 失败

### 场景：共享闭包函数的 internal_function 策略冲突

- **给定** 同一 helper 属于两个 local macro 闭包
- **并且** 两个 declaration 对该 helper 的 internal direct-call 策略不同
- **当** 校验策略时
- **那么** 编译以 `conflicting_internal_function_policy` 失败

## 需求：retain 与最终跳过集合

### 场景：retain 根保留完整闭包

- **给定** local macro 根或 helper 被 `local_macro_retain`、`export` 或 `export_macro` 命中
- **当** 计算 retain 集合时
- **那么** 该根的完整闭包及其 spec forms 均被保留

### 场景：非冻结 retain root 无额外效果

- **给定** `-local_macro_retain([ordinary/0])` 且 ordinary/0 不属于任何 local macro 闭包
- **当** 收尾计算 retain 集合时
- **那么** 不报错，ordinary/0 仍按普通 form 处理

### 场景：最终跳过未 retain 的已展开 forms

- **给定** local macro form 已展开且不属于 retain 闭包
- **当** 收尾计算 FinalSkipIds 时
- **那么** 该 form 出现在 FinalSkipIds 中

### 场景：retain form 参与最终展开

- **给定** frozen form 属于 retain 闭包
- **并且** 最终环境比对通过
- **当** 最终函数体展开执行时
- **那么** 该 form 不在 FinalSkipIds 中并参与递归展开

### 场景：retain helper 的最终环境比对不一致失败

- **给定** retain 闭包中的 helper 在 declaration 环境与最终环境下展开结果不同
- **当** 收尾执行最终环境比对时
- **那么** 报 `conflicting_local_macro_closure_environment`

### 场景：local macro 宏头跳过最终环境比对

- **给定** local macro 宏头属于 retain 闭包
- **当** 收尾执行最终环境比对时
- **那么** 宏头本身跳过比对
- **并且** 仍不在 FinalSkipIds 中并参与最终函数体展开

## 需求：安全覆盖加载

### 场景：old code 被引用时失败

- **给定** 覆盖加载前 `code:soft_purge(<Module>__local_macro)` 返回 `false`
- **当** 尝试加载新的累计模块时
- **那么** 报 `local_macro_module_in_use`
- **并且** 不调用 `code:purge/1`
