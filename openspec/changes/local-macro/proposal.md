# Local Macro

## 摘要

2026-10-28 采用“首次声明前环境绑定 + 延迟单次 canonical 展开 + 策略 A 前序就绪”。本地宏闭包函数的原始 AST、已解析注入值与宏映射在首次进入闭包时永久绑定；共享、retain 和 export 直接复用 canonical form。

## 范围

- declaration 只校验与登记闭包，零展开、零编译。
- 属性目标需要可调用或 scan 收尾时，按声明顺序准备累计 prefix。
- 无宏闭包直接复用原始 form；含宏闭包在展开前确保前序本地候选全部可调用。
- 删除多环境指纹、whitelist、重放与挂起重试；将闭包分析和本地诊断留在 provider。
- 保留冻结保护、closure_roots、完整闭包 retain、唯一临时模块、安全加载和 generation 原子提交。

## 取舍

含外部宏的闭包也可能提前编译未实际调用的前序本地宏，以覆盖动态 replacement。独立无宏调用的声明合并编译一次。最终环境差异不再导致闭包冲突；详细契约见 [设计](design.md) 和 [规范](specs/macro-expansion/spec.md)。
