# Tasks

local macro 专属任务移至 [local-macro/tasks.md](../local-macro/tasks.md)。

## 规格

- [x] 记录外部与本地属性宏统一参与 scan-and-splice。
- [x] 记录环境变更的前向生效与不回扫规则。
- [x] 记录生成属性、环境 form 与普通 forms 的当前位置处理及延后函数体展开规则。
- [x] 记录 splice 的局部顺序与禁止全局 Generated/Base 重排规则。
- [x] 记录 attribute injection 只读取当前位置之前的 passed forms。
- [x] 记录 import/use 的消费语义、macro_options 的保留语义及宏 key 冲突规则。
- [x] 记录 traverse/return 桥接和用户宏 traverse state 隔离。
- [x] 记录 `local_macro` declaration 和未就绪本地属性调用委托 local-macro 工作流。
- [x] 记录 scan 收尾使用 local-macro 提供的最终环境和跳过集合。
- [x] 记录 local 与普通 function 使用同构展开，以及统一 local 引用匹配边界。

## 实现

- [x] 重构 attribute pass，使其携带 ExternalEnv 与 LocalMacroState。
- [x] 将 `local_macro` 注册和按需可调用性检查接入 local-macro 模块。
- [x] 保留 scan-and-splice 对生成属性的当前位置重扫语义。
- [x] 让生成的 import/use/macro_options 在处理下一 form 前更新 ExternalEnv。
- [x] 维护 passed forms 与 remaining queue 两种不同的扫描视图。
- [x] 仅对需要 `__original__/Arity` 合并的生成 function/spec 做最小整理。
- [x] 通过冲突检查合并宏映射，仅在 `force_override` 时覆盖不同定义。
- [x] 以 scoped state 执行用户宏返回的 traverse computation。
- [x] 在 attribute pass 收尾剔除 FinalSkipIds，再运行 function pass。
- [x] 将 FinalLocalEnv 接入 function pass，并过滤未编译 local macro。
- [x] 提供不含 local 专属策略的统一 function 展开和 local 引用解析操作。
- [x] 扫描器只调用 local-macro 注册、确保可调用和收尾接口，不自行执行编译计划。

## 测试

- [x] 增加测试：外部与已就绪本地属性宏按同一源码顺序扫描。
- [x] 增加测试：本地属性宏生成环境变更后，后续 form 可见且先前 form 不回扫。
- [x] 增加测试：尚未就绪的本地属性宏调用会触发 local-macro 工作流。
- [x] 增加测试：属性宏生成 import 与依赖该 import 的后续属性时按 splice 顺序展开。
- [x] 增加测试：attribute injection 只包含已经通过扫描的 attributes。
- [x] 增加测试：生成的 macro_options 对后续展开生效。
- [x] 增加测试：宏 key 冲突失败及 `force_override` 成功。
- [x] 增加测试：生成的普通 function 延后到最终函数体展开，且无冲突 function/spec 不被全局重排。
- [x] 增加测试：attribute/function 宏的 traverse state 与框架 state 隔离。
- [x] 重新运行现有 uniform macro 与 macro validation 测试套件。
- [x] 增加测试：local macro function 与普通 function 复用同一展开语义。
- [x] 增加测试：目标 FA 自身移除和 internal_function 不在通用展开器中实现。

## Hierarchy_final 后续任务（新增，保留既有任务状态）

### 规格

- [x] 明确 `env_snapshot` 与 `inject_forms_snapshot` 共同组成一份 local function-form 编译上下文，`closure_source_view` 仅是闭包结构输入而非宏上下文。
- [x] 明确 local macro 唯一特殊规则是 function-form 编译上下文仅限 declaration 前 passed forms；attribute 运行期规则对 external/local 宏通用。
- [x] 将 `Hierarchy_final.md` 识别的 P0–P3 差距转化为实现和测试任务。

### 实现

- [ ] **P0：声明位点注入快照。** 注册 local declaration 时单独保存 declaration 前 `passed_forms`；展开 frozen local forms 时用它执行 `inject_attrs` 和构造环境 fingerprint，不再把包含 remaining queue 的 closure source view 作为 `InjectForms`。
- [ ] **P0：隔离编译期与运行期。** attribute 触发 `ensure_available` 时，只允许 declaration 前 passed forms 进入 local function-form 编译上下文；编译完成后的 attribute 调用继续走 external/local 共用的运行期 MacroEnv/PassedForms 规则，不新增 local 专用运行路径。
- [ ] **P1：统一跨来源有效宏环境。** 让 external/local 宏 entry 按源码位置走同一冲突与 `force_override` 更新规则，避免固定 `maps:merge(External, Local)` 决定 winner 或延迟冲突。
- [ ] **P2：明确并实现 `__original__` 的 spec merge。** 按 `Hierarchy_final.md` 的 spec 归属规则处理原 spec、生成 spec 与重命名原函数。
- [ ] **P3：封装 local declaration 单次语义校验。** 注册和 local macro map 构造共享同一份成功校验结果，同时保留失败不回滚先前注册且诊断不重复的行为。

### 测试

- [ ] local declaration 前后存在目标 attribute 时，frozen local forms 的 `inject_attrs` 只包含 declaration 前已 pass 的值。
- [ ] declaration 后 `use_macro` 修改 alias、调用参数或 `inject_attrs` 配置时，frozen local forms 仍使用 declaration-time 配置。
- [ ] 更晚 attribute 触发按需编译时，验证 local function forms 仍只使用 declaration 前 passed forms，并验证后续 attribute 与 external attribute 使用同一运行期规则。
- [ ] remaining queue 中 helper 可进入 local closure，但尚未 pass 的 attributes 不进入 local forms 注入。
- [ ] external → local 与 local → external 的冲突、双方 `force_override`、生成环境 form 交错均按源码顺序裁决。
- [ ] `__original__` 合并覆盖原函数带 spec、wrapper 自带 spec、原/生成 spec 同时存在三类场景。
- [ ] local declaration 无效、重复及部分失败场景只产生一次诊断且不破坏先前成功注册。
