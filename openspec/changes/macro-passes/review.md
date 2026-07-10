# Macro Passes — 待完成

## 测试 (tasks.md)

| # | 内容 | 状态 |
|---|---|---|
| L67 | use_macro alias 前向可见 | **已测试** |
| L68 | 外部属性宏生成另一个外部属性宏调用→同阶段展开 | **已测试** |
| L69 | 外部属性宏同时生成新导入和依赖该导入的后续属性 | **已测试** |
| L70 | 生成出的非环境 attribute 被重扫并递归展开为外部属性宏调用 | **已测试** |
| L72 | 外部属性生成的普通函数 forms 保留，只在最终阶段完成宏展开 | **已测试** |
| L74 | 本地宏发现能看见更早外部属性宏生成的 forms | **已测试** |
| L75 | extra_functions 补充静态分析遗漏的 helper | **已测试** |
| L76 | extra_functions 引用未定义函数时编译失败 | **已测试** |
| L77 | 多个 extra_functions 声明按并集合并 | **已测试** |
| L79 | 两个宏的 internal_function 名单不同、无共享闭包函数时编译成功 | **已测试** |
| L81 | internal_function 策略让宏定义内宏函数调用按直接调用处理 | **已测试** |
| L82 | 本地宏函数体试图生成宏环境变更输出时编译失败 | **已测试** |
| L87 | 本地属性宏改写锁定 spec 时编译失败 | **已测试** |
| L88 | 最终展开仍会展开锁定快照之外普通代码中生成的宏调用 | **已测试** |

## 已完成

| 类别 | 内容 |
|---|---|
| 外部 pass | `map_forms_splice` + traverse State + traverse_return 桥接 + eval 隔离 + catch_on_error |
| 本地 pass | `map_forms_splice` + `local_form_handler/1` + mutation/locked 检查，删除旧递归扫描 |
| map_forms_splice | `{form, _}` / `{generated_insert, _}` 标记，仅 function/spec 参与整理，`__original__` 按需重命名 |
| 合并 | `merge_macro_maps_pure/2` |
| 内部功能 | extra_functions、internal_function、锁定快照、mutation 禁止 |
| 错误 | format_error 新增 4 子句 |
| 全部测试 | 252/252 通过 |
