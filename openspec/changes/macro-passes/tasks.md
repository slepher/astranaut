# Tasks

## 规格

- [x] 记录外部属性宏按源码顺序展开的规则。
- [x] 记录外部属性宏生成 forms 后的 scan-and-splice 行为。
- [x] 记录外部属性阶段中外部宏环境前向增长的规则。
- [x] 记录后续环境增长不会回溯重展开已扫描属性。
- [x] 记录生成出的外部属性宏调用会在同一阶段立即重新扫描。
- [x] 记录生成出的非环境 attribute 会在当前阶段递归重扫。
- [x] 记录生成出的普通 forms 会被保留，但不会在外部属性阶段提前做最终宏展开。
- [x] 记录 scan-and-splice 不能全局拆分 Generated/Base 后统一重插入。
- [x] 记录只有生成的 function/spec 需要参与最小化插入整理。
- [x] 记录无重复函数冲突时生成的 function/spec 必须保持原地相对位置。
- [x] 记录 return monad 桥接到 traverse 时必须保留 error state。
- [x] 记录本地属性宏生成的 attribute 会在同一本地属性阶段继续展开。
- [x] 记录宏环境变更 attribute 的黑名单。
- [x] 记录外部属性阶段结束后建立本地宏快照边界。
- [x] 记录 `extra_functions` 作为本地宏定义选项的语义。
- [x] 记录 `extra_functions` 采用并集语义。
- [x] 记录 `local_macro` 与 `export_macro` 都参与宏定义闭包分析。
- [x] 记录 `internal_function` 是宏定义策略，而不是声明身份策略。
- [x] 记录共享闭包函数上的 `internal_function` 冲突按具体函数检测。
- [x] 记录本地宏与本地属性宏不得继续修改宏环境。
- [x] 记录本地属性宏可以改写普通 forms，但不能改写锁定快照。
- [x] 记录锁定快照至少包含 function 和 spec forms。
- [x] 记录最终展开基于 `FrozenExternalEnv + LoadedLocalMacroEnv`。

## 实现

- [x] 重构外部属性展开路径，使其按源码顺序扫描 forms，并携带可变的外部宏环境状态。
- [x] 为外部属性宏生成的 forms 实现 queue / splice 回插机制。
- [x] 增加 `map_forms_splice/3`，统一支持 queue/splice 重扫与 per-form 错误累积。
- [x] 修正 `map_forms_splice/3` 的整理逻辑，避免全量 Generated/Base 拆分。
- [x] 仅对生成的 function/spec 保留内部标记，并在确有重复函数和 `__original__` 时执行重命名。
- [x] 保留非冲突生成 function/spec 的原地相对位置。
- [x] 在外部属性扫描过程中重新发现外部属性宏生成的宏环境项。
- [x] 对生成出的外部属性宏调用在插入点立即重新扫描。
- [x] 对生成出的非环境 attribute 立即重新扫描，并在当前环境可解析为外部属性宏时继续递归展开。
- [x] 保证新的外部环境只影响后续 forms，不回扫已处理结果。
- [x] 让生成出的普通 forms 留在结果 forms 流中，但不在外部属性阶段提前执行最终函数体展开。
- [x] 在外部属性阶段结束后冻结外部宏环境。
- [x] 将外部属性阶段的环境状态改为 traverse State 传递。
- [x] 将 handler 内 state 更新改为 traverse `do` / bind 串联，避免 `put` 被普通表达式丢弃。
- [x] 使用保留 error state 的 return-to-traverse 桥接处理 `used_macros`、`macro_options` 校验和外部属性宏展开。
- [x] 在本地宏定义校验中加入 `extra_functions`。
- [x] 将 `extra_functions` 合并进本地 helper 闭包分析。
- [x] 将多个 `extra_functions` 声明按集合并集处理。
- [x] 使用 clause map 校验 `extra_functions` 中的函数是否存在。
- [x] 对 `export_macro` 也执行宏定义 helper 闭包扫描。
- [x] 为宏定义元数据增加 `internal_function` 策略表示。
- [x] 将 `internal_function` 策略作用到闭包成员，而非仅作用于宏头函数。
- [x] 在共享闭包函数上检测 `internal_function` 冲突，并在快照锁定前报错。
- [x] 标记或追踪锁定本地宏快照 forms，以便后续 Pass 拒绝非法改写。
- [x] 将 function 与 spec forms 一并纳入锁定快照追踪。
- [x] 拒绝本地属性输出中的宏环境变更 attribute。
- [x] 拒绝本地属性输出对锁定本地快照 forms 的改写。
- [x] 将本地属性宏展开改为当前位置 scan-and-splice 递归扫描。
- [x] 保持最终递归展开仅作用于非本地快照区域。
- [x] 保持当前 `outer` / `inner` 遍历语义不变。
- [x] 对新的 Pass 边界失败使用新的显式错误名。

## 测试

- [x] 增加测试：外部属性宏生成新的 `-import_macro(...)`，供后续属性使用。
- [x] 增加测试：外部属性宏生成新的 `-macro_options(...)`，供后续 import 使用。
- [x] 增加测试：外部属性宏生成新的 `-use_macro(...)` alias，供后续属性使用。
- [x] 增加测试：外部属性宏生成另一个外部属性宏调用，并在同阶段立即展开。
- [x] 增加测试：外部属性宏同时生成新导入和依赖该导入的后续属性。
- [x] 增加测试：生成出的非环境 attribute 被重新扫描，并递归展开为外部属性宏调用。
- [x] 增加测试：后续外部环境更新不会回溯重展开较早属性。
- [x] 增加测试：外部属性生成的普通函数 forms 会被保留，但只在最终阶段才完成宏展开。
- [x] 增加测试：非冲突生成函数在 scan-and-splice 后保持原地相对位置。
- [x] 增加测试：本地宏发现能看见更早外部属性宏生成的 forms。
- [x] 增加测试：`extra_functions` 能补充静态分析遗漏的 helper。
- [x] 增加测试：`extra_functions` 引用未定义函数时编译失败。
- [x] 增加测试：多个 `extra_functions` 声明按并集合并。
- [x] 增加测试：`export_macro` 的 helper 闭包会参与 `internal_function` 分析，但不会因此进入锁定快照。
- [x] 增加测试：两个宏的 `internal_function` 名单不同，但没有共享闭包函数时编译成功。
- [x] 增加测试：共享闭包函数在不同宏下的 `internal_function` 处理冲突时，报 `conflicting_internal_function_policy`。
- [x] 增加测试：`internal_function` 策略会让宏定义内部的某个宏函数调用按直接调用处理。
- [x] 增加测试：本地宏函数体试图生成宏环境变更输出时编译失败。
- [x] 增加测试：本地属性宏生成 `-import_macro(...)` 时编译失败。
- [x] 增加测试：本地属性宏生成 `-local_macro(...)` 时编译失败。
- [x] 增加测试：本地属性宏生成另一个本地属性宏调用时会继续展开。
- [x] 增加测试：本地属性宏改写锁定 helper 时编译失败。
- [x] 增加测试：本地属性宏改写锁定 spec 时编译失败。
- [x] 增加测试：最终展开仍会展开锁定快照之外普通代码中生成的宏调用。
- [x] 重新运行现有 uniform macro 与 macro validation 测试套件。
