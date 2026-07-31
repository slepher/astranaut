# Synchronizing astranaut local CI

`astranaut/ci_scripts` is the source of the shared local-CI implementation.
Projects that depend on `astranaut` can synchronize these files from the exact
dependency selected by Rebar3.

## Initial setup

Create `ci_scripts/` in the target project, then manually copy **one** launcher
from this directory:

- Windows / PowerShell: `sync_ci.ps1`
- Linux or macOS / Bash: `sync_ci.sh`

Also copy `ci-env.conf.example` into the target project's `ci_scripts/`
directory. Create the local configuration from that example:

```powershell
Copy-Item .\ci_scripts\ci-env.conf.example .\ci_scripts\ci-env.conf
```

```bash
cp ./ci_scripts/ci-env.conf.example ./ci_scripts/ci-env.conf
```

Add `ci_scripts/ci-env.conf` to the target project's `.gitignore`. The example
configuration is project-owned and is not overwritten by synchronization.

## Synchronize

Run the launcher from the target project root:

```powershell
.\ci_scripts\sync_ci.ps1
```

```bash
bash ./ci_scripts/sync_ci.sh
```

The launcher:

1. Detects `src/astranaut.app.src`. If it exists, the current project is
   `astranaut`, so synchronization stops without changing files.
2. In other projects, runs `rebar3 get-deps`.
3. Resolves the selected `astranaut` dependency below `_build/default`,
   including a local checkout when one is active.
4. Recreates shared CI files as hard links and removes obsolete shared links.
5. Preserves `ci-env.conf`, `ci-env.conf.example`, `sync_ci.ps1`, and
   `sync_ci.sh`.

Run synchronization again whenever the upstream CI file set changes.

---

# 同步 astranaut 本地 CI

`astranaut/ci_scripts` 是共享本地 CI 实现的源目录。依赖 `astranaut` 的项目
可以从 Rebar3 实际选中的依赖版本同步这些文件。

## 首次设置

在目标项目中创建 `ci_scripts/`，然后从本目录手动复制一个启动脚本：

- Windows / PowerShell：`sync_ci.ps1`
- Linux 或 macOS / Bash：`sync_ci.sh`

同时把 `ci-env.conf.example` 复制到目标项目的 `ci_scripts/`，再从 example
建立本机配置：

```powershell
Copy-Item .\ci_scripts\ci-env.conf.example .\ci_scripts\ci-env.conf
```

```bash
cp ./ci_scripts/ci-env.conf.example ./ci_scripts/ci-env.conf
```

把 `ci_scripts/ci-env.conf` 加入目标项目的 `.gitignore`。example 配置由目标
项目维护，同步时不会覆盖。

## 执行同步

在目标项目根目录运行：

```powershell
.\ci_scripts\sync_ci.ps1
```

```bash
bash ./ci_scripts/sync_ci.sh
```

同步脚本会：

1. 检查 `src/astranaut.app.src`；存在时说明当前项目就是 `astranaut`，直接
   退出且不修改任何文件。
2. 仅在其他项目中运行 `rebar3 get-deps`。
3. 从 `_build/default` 定位 Rebar3 实际选中的 `astranaut`，包括本地
   checkout。
4. 将共享 CI 文件重建为硬链接，并移除已经废弃的共享链接。
5. 保留 `ci-env.conf`、`ci-env.conf.example`、`sync_ci.ps1` 和
   `sync_ci.sh`。

上游 CI 文件集合变化后重新运行同步即可。
