# Tasks

local macro 专属任务移至 [local-macro/tasks.md](../local-macro/tasks.md)。

## 规格

- [x] 记录外部与本地属性宏统一参与 scan-and-splice。
- [x] 记录环境变更的前向生效与不回扫规则。
- [x] 记录生成属性、环境 form 与普通 forms 的当前位置处理及延后函数体展开规则。
- [x] 记录 splice 的局部顺序与禁止全局 Generated/Base 重排规则。
- [x] 记录 `local_macro` declaration 和未就绪本地属性调用委托 local-macro 工作流。
- [x] 记录 scan 收尾使用 local-macro 提供的最终环境和跳过集合。

## 实现

- [ ] 重构 attribute pass，使其携带 ExternalEnv 与 LocalMacroState。
- [ ] 将 `local_macro` 注册和按需可调用性检查接入 local-macro 模块。
- [ ] 保留 scan-and-splice 对生成属性的当前位置重扫语义。
- [ ] 让生成的 import/use/macro_options 在处理下一 form 前更新 ExternalEnv。
- [ ] 在 attribute pass 收尾剔除 FinalSkipIds，再运行 function pass。
- [ ] 将 FinalLocalEnv 接入 function pass。

## 测试

- [ ] 增加测试：外部与已就绪本地属性宏按同一源码顺序扫描。
- [ ] 增加测试：本地属性宏生成环境变更后，后续 form 可见且先前 form 不回扫。
- [ ] 增加测试：尚未就绪的本地属性宏调用会触发 local-macro 工作流。
- [ ] 增加测试：属性宏生成 import 与依赖该 import 的后续属性时按 splice 顺序展开。
- [ ] 增加测试：生成的普通 function 延后到最终函数体展开，且无冲突 function/spec 不被全局重排。
- [ ] 重新运行现有 uniform macro 与 macro validation 测试套件。
