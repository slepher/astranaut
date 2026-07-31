# astranaut local CI

这套脚本用于在 Docker 中针对多个 Erlang/OTP 版本运行 `astranaut`
本地 CI。源码会被复制到容器内的临时目录，不会复用宿主机 `_build`。

The scripts run `astranaut` against multiple Erlang/OTP versions in isolated
Docker worktrees without reusing the host `_build` directory.

## Checks

每个 OTP 版本默认依次运行：

1. `rebar3 compile`
2. `rebar3 xref`
3. `rebar3 ct`

`rebar3 dialyzer` 可通过 `RUN_DIALYZER=true` 开启。由于项目目前仍有历史
Dialyzer warning，默认配置将其关闭。

Each OTP version runs compile, xref, and Common Test. Dialyzer is opt-in
because the project currently has historical warnings.

## Prerequisites

- Docker Desktop or Docker Engine
- PowerShell 7 on Windows, or Bash on Linux

## Configuration

配置文件位于 `ci_scripts/`：

- `ci_scripts/ci-env.conf.example`：提交到仓库的默认配置模板
- `ci_scripts/ci-env.conf`：本机覆盖配置，已加入 `.gitignore`
- `ci_scripts/Dockerfile.local-ci`：构建通用 OTP 测试镜像

如果 `ci-env.conf` 不存在，构建或运行脚本会从示例配置自动创建。主要选项：

- `ERLANG_VSNS`: 逗号分隔的 OTP 版本
- `TEST_SUITE` / `TEST_CASE`: 可选的 Common Test 范围
- `RUN_XREF`: 是否运行 xref
- `RUN_DIALYZER`: 是否运行 Dialyzer
- `USE_CHECKOUTS`: `auto` 时检测宿主机 `_checkouts`，也可强制设为
  `true` 或 `false`
- `OUTPUT_LANG`: `auto`、`en` 或 `cn`
- `LOG_PORT`: 日志浏览器端口

默认值为 `USE_CHECKOUTS=auto`。根目录存在非空 `_checkouts` 时，runner
自动将其中的 checkout 纳入隔离快照；目录不存在或为空时自动跳过。

## Build images

```powershell
.\ci_scripts\build.ps1
.\ci_scripts\build.ps1 -TargetVer 28
```

```bash
./ci_scripts/build.sh
./ci_scripts/build.sh 28
```

镜像名为 `local-ci:<OTP>`；镜像只提供 Erlang/OTP 环境，可由多个项目复用。
修改项目源码后不需要重新构建镜像。

## Run CI

运行默认矩阵且不启动日志查看器：

```powershell
.\ci_scripts\run.ps1 -NoView
```

```bash
./ci_scripts/run.sh --noview
```

运行单个版本、suite 或 case：

```powershell
.\ci_scripts\run.ps1 -TargetVer 28 `
  -TestSuite astranaut_design_SUITE `
  -TestCase lib_form_source_contracts -NoView
```

```bash
./ci_scripts/run.sh 28 --suite astranaut_design_SUITE \
  --case lib_form_source_contracts --noview
```

PowerShell 可用 `-RunDialyzer` 临时开启 Dialyzer、`-SkipXref` 临时关闭
xref，或用 `-NoCheckouts` 忽略宿主机 checkout。Bash 对应参数是
`--dialyzer`、`--skip-xref` 和 `--no-checkouts`。

启用 checkout 时，脚本会解析 `_checkouts` 中每个目录的真实路径并分别
只读挂载，因此 Windows junction 和 Linux symlink 都可使用。容器通过
`git ls-files --cached --others --exclude-standard` 建立源码快照：已跟踪
修改和未忽略的新文件会进入 CI，构建产物及忽略文件不会复制。CI 不会修改
宿主机源码或依赖。

## Logs

Common Test 日志、覆盖率报告和 `ci-summary.txt` 保存在 Docker volume
`astranaut-local-ci-data` 中，并按 OTP 版本分目录。

```powershell
.\ci_scripts\view_logs.ps1
```

```bash
./ci_scripts/view_logs.sh
```

日志浏览器使用 Nginx，并在 `LOG_PORT` 配置的端口提供报告。按
<kbd>Ctrl+C</kbd> 停止。

## Sharing with dependent projects

依赖 `astranaut` 的其他项目可通过同步脚本复用这套 CI。手动安装
PowerShell/Bash 启动脚本、从 example 创建本机配置以及硬链接同步规则见
[README.sync.md](README.sync.md)。
