# Review: local_macro list syntax 处理差异

## 背景

`-local_macro([foo/0, bar/1])` 的 list 语法在重构后存在两条代码路径，处理方式不同。

## 路径 1: Phase 2 — `forms_with_attribute`

`load_local_macro_attributes` → `astranaut_lib:forms_with_attribute/5` → `values_apply_fun_m`

```text
forms_with_attribute(local_macro) 扫描 forms
  └─ values_apply_fun_m([{foo,0}, {bar,1}], ...)
       └─ deep_attr => true (默认)
            └─ 逐元素解包: {foo,0}, {bar,1}
                 └─ update_local_macro_attribute 各调用一次
                      └─ macro_without_module_attr({foo,0}) → {[{foo,0}], []}
```

**行为**: 每个 FA 独立走校验链，独立生成 `nowarn_unused_function` 节点，独立检查 `macro_override`。若某 FA 校验失败，后续 FA 可能未被处理。

## 路径 2: Phase 1 — `handle_local_macro_declaration`

`handle_local_macro_declaration` → `validate_local_macro_attribute` → `macro_without_module_attr`

```text
handle_local_macro_declaration([{foo,0}, {bar,1}], ...)
  └─ macro_without_module_attr([{foo,0}, {bar,1}]) when is_list(FAs) → {[{foo,0}, {bar,1}], []}
       └─ astranaut_local_macro:register([{foo,0}, {bar,1}], ...)
```

**行为**: 所有 FA 作为 batch 原子处理。`duplicate_local_macro_declaration` 检查整个 FA 列表，任一重复即全部失败。`internal_function` 策略跨所有 closure 联合校验。所有 FA 共享相同的 `env_snapshot` 和 `order`。

## 差异总结

| 维度 | Phase 2 (forms_with_attribute) | Phase 1 (handle_local_macro_declaration) |
|------|-------------------------------|------------------------------------------|
| 粒度 | 逐个 FA 处理 | batch 原子处理 |
| 失败语义 | 单 FA 失败，后续可能未处理 | 任一失败，全部不注册 |
| env_snapshot | 从 Phase 2 传入的 Ctx 构建 | 从 Phase 1 的 ExternalEnv 快照 |
| error context | 每个 FA 独立 Pos | 共享 declaration Pos |

## 建议

`macro-passes-adjusted` 实现时应当统一到一条路径：

- 保留 Phase 1 的 batch 语义（符合 local-macro design 中"原子操作"的要求）
- 移除 Phase 2 中对 `-local_macro` 的重复处理，或改为调用 `astranaut_local_macro` 接口
- `forms_with_attribute` 的 `deep_attr` unwrap 行为对 `-local_macro` list 语法是冗余的，可在统一后移除对该属性的 `deep_attr` 依赖
