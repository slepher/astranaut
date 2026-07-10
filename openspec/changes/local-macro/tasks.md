# Tasks

## 规格

- [ ] 记录闭包为静态函数依赖集合，而非词法 closure。
- [ ] 记录按 FA 注册、重复声明与声明顺序。
- [ ] 记录完整源码视图、已 pass 环境快照及原始 form 冻结。
- [ ] 记录 declaration 前 import 可见、后续环境不回溯，以及仅实际引用 local macro 进入环境。
- [ ] 记录多环境展开缓存与冲突规则。
- [ ] 记录按 declaration 顺序的最小累计编译。
- [ ] 记录 scan 收尾重新构造全部 local macro 的最终累计模块。
- [ ] 记录同模块安全覆盖加载。
- [ ] 记录 retain 根、闭包 retain 与最终跳过集合。
- [ ] 记录 `extra_functions`、`internal_function` 和 local macro 自身递归的闭包规则。

## 实现

- [ ] 新增 `astranaut_local_macro.erl`，实现按 FA 的注册表和状态转换。
- [ ] 实现闭包计算、`extra_functions`、`internal_function` 策略和重复声明检查。
- [ ] 实现冻结 form 改写保护及按环境展开缓存。
- [ ] 实现多环境结果比较和 `conflicting_local_macro_closure_environment`。
- [ ] 实现最小累计编译计划与最终全量累计编译。
- [ ] 实现 declaration 环境快照，且不受后续外部环境更新影响。
- [ ] 实现 `<Module>__local_macro` 的安全覆盖加载和模块级互斥。
- [ ] 实现 retain 根闭包展开、最终环境比对和 FinalSkipIds。

## 测试

- [ ] 同一 FA 重复 declaration 报错。
- [ ] 后声明 local macro 属于先声明闭包时按 helper 多环境规则处理。
- [ ] 共享 form 的不同环境结果一致时成功，不一致时报错。
- [ ] 声明前 import 可见、声明后 import 不可见，且闭包仅取得实际引用 local macro。
- [ ] local macro 自身递归调用保持普通函数调用。
- [ ] B 依赖 A 时按 `{A}`、`{A,B}` 编译；无依赖时仅 `{A,B}`。
- [ ] scan 收尾重新编译全部 local macro，并复用既有展开缓存。
- [ ] 相同 form/environment 命中缓存，不重复展开。
- [ ] extra_functions 成功补充 helper；不存在的 helper 报 `invalid_extra_functions`。
- [ ] 共享闭包函数的 internal_function 策略冲突报错。
- [ ] `local_macro_retain`、`export`、`export_macro` 保留完整闭包及 spec forms。
- [ ] retain helper 的最终环境比对不一致报错；retain 宏头跳过该比对。
- [ ] 已展开未 retain forms 进入 FinalSkipIds；retain 闭包参与最终展开。
- [ ] old code 仍被引用时安全加载以 `local_macro_module_in_use` 失败。
