<#
.SYNOPSIS
ASTranaut CI Batch Builder
批量构建 Docker 镜像
#>
param (
    [string]$TargetVer = "" # 可选：指定特定版本进行构建
)

$ErrorActionPreference = "Stop"
$ScriptDir = $PSScriptRoot

$LangDict = @{
    "en" = @{
        "AUTO_COPY"     = "Warning: ci-env.conf not found. Created from ci-env.conf.example."
        "START_BATCH"   = "=== Batch Build Started ==="
        "TARGET_LIST"   = "Versions to build: {0}"
        "START_BUILD"   = ">>> [{0}] Building Image..."
        "SUCCESS"       = "Build Success: {0}"
        "FAIL"          = "Build Failed: {0} (Exit Code: {1})"
    }
    "cn" = @{
        "AUTO_COPY"     = "警告: 未找到 ci-env.conf。已从 ci-env.conf.example 自动创建默认配置。"
        "START_BATCH"   = "=== 开始批量构建 ==="
        "TARGET_LIST"   = "待构建版本: {0}"
        "START_BUILD"   = ">>> [{0}] 正在构建镜像..."
        "SUCCESS"       = "构建成功: {0}"
        "FAIL"          = "构建失败: {0} (错误码: {1})"
    }
}

try {
    # 1. 配置文件检测与自动拷贝
    $ConfigPath = Join-Path $ScriptDir "ci-env.conf"
    $ExamplePath = Join-Path $ScriptDir "ci-env.conf.example"

    # 预先定义简单的语言提示 (配置还没读到，只能先猜或默认英文/系统语言)
    $SysLangIsCn = (Get-Culture).Name -match "^zh"
    
    if (-not (Test-Path $ConfigPath)) {
        if (Test-Path $ExamplePath) {
            Copy-Item $ExamplePath $ConfigPath
            # 输出提示
            if ($SysLangIsCn) { Write-Warning "警告: 未找到 ci-env.conf。已从 ci-env.conf.example 自动创建默认配置。" }
            else { Write-Warning "Warning: ci-env.conf not found. Created from ci-env.conf.example." }
        } else {
            throw "Critical Error: Neither ci-env.conf nor ci-env.conf.example found."
        }
    }

    # 2. 正式读取配置
    $Config = Get-Content $ConfigPath -Raw -ErrorAction Stop | ConvertFrom-StringData
    
    # 3. 确定版本列表
    if ($TargetVer) {
        $VerList = @($TargetVer)
    } elseif ($Config.ERLANG_VSNS) {
        # 解析逗号分隔的列表
        $VerList = $Config.ERLANG_VSNS.Split(",") | ForEach-Object { $_.Trim() } | Where-Object { $_ -ne "" }
    } else {
        throw "ERLANG_VSNS not defined in config."
    }

    # 4. 语言判定
    $UserLang  = if ($Config.OUTPUT_LANG) { $Config.OUTPUT_LANG.Trim() } else { "auto" }
    $LangKey = "en"
    if ($UserLang -eq "cn") { $LangKey = "cn" }
    elseif ($UserLang -eq "auto" -and $SysLangIsCn) { $LangKey = "cn" }
    $Msg = $LangDict[$LangKey]

    Write-Host "`n$($Msg.START_BATCH)" -ForegroundColor Magenta
    Write-Host ($Msg.TARGET_LIST -f ($VerList -join ", ")) -ForegroundColor Gray

    $DockerfilePath = Join-Path $ScriptDir "Dockerfile.local-ci"
    $ProjectRoot = Split-Path -Parent $ScriptDir

    # 5. 循环构建
    foreach ($Ver in $VerList) {
        Write-Host "`n$($Msg.START_BUILD -f $Ver)" -ForegroundColor Cyan
        
        docker build -t "local-ci:$Ver" `
            --build-arg "ERLANG_VER=$Ver" `
            -f $DockerfilePath `
            $ProjectRoot

        if ($LASTEXITCODE -eq 0) {
            Write-Host ($Msg.SUCCESS -f $Ver) -ForegroundColor Green
        } else {
            # 构建失败通常是严重错误，停止脚本
            throw ($Msg.FAIL -f $Ver, $LASTEXITCODE)
        }
    }

} catch {
    Write-Error "`nError: $($_.Exception.Message)"
    exit 1
}