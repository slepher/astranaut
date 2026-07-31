<#
.SYNOPSIS
Build reusable local-ci Erlang/OTP Docker images.
#>
param (
    [string]$TargetVer = "",
    [Alias("h", "?")]
    [switch]$Help
)

if ($TargetVer -eq "--help") {
    $TargetVer = ""
    $Help = $true
}

if ($Help) {
    @"
Usage:
  .\ci_scripts\build.ps1 [-TargetVer <version>]

Examples:
  .\ci_scripts\build.ps1
  .\ci_scripts\build.ps1 -TargetVer 28
"@
    exit 0
}

$ErrorActionPreference = "Stop"
$ScriptDir = $PSScriptRoot
$ProjectRoot = Split-Path -Parent $ScriptDir
$ConfigPath = Join-Path $ScriptDir "ci-env.conf"
$ExamplePath = Join-Path $ScriptDir "ci-env.conf.example"

try {
    if (-not (Test-Path -LiteralPath $ConfigPath)) {
        Copy-Item -LiteralPath $ExamplePath -Destination $ConfigPath
        Write-Warning "ci-env.conf not found; created it from ci-env.conf.example."
    }

    if (-not (Get-Command docker -ErrorAction SilentlyContinue)) {
        throw "Docker was not found in PATH."
    }

    $Config = Get-Content -Raw -LiteralPath $ConfigPath |
        ConvertFrom-StringData
    if ($TargetVer) {
        $Versions = @($TargetVer.Trim())
    } elseif ($Config.ERLANG_VSNS) {
        $Versions = @(
            $Config.ERLANG_VSNS.Split(",") |
                ForEach-Object { $_.Trim() } |
                Where-Object { $_ }
        )
    } else {
        throw "ERLANG_VSNS is not defined in ci-env.conf."
    }

    $UseChinese =
        $Config.OUTPUT_LANG -eq "cn" -or
        ($Config.OUTPUT_LANG -eq "auto" -and
         (Get-Culture).Name -match "^zh")
    $Dockerfile = Join-Path $ScriptDir "Dockerfile.local-ci"

    if ($UseChinese) {
        Write-Host "`n=== 构建本地 CI 镜像 ===" -ForegroundColor Magenta
        Write-Host "OTP 版本: $($Versions -join ', ')" -ForegroundColor Gray
    } else {
        Write-Host "`n=== Building local CI images ===" -ForegroundColor Magenta
        Write-Host "OTP versions: $($Versions -join ', ')" -ForegroundColor Gray
    }

    foreach ($Version in $Versions) {
        Write-Host "`n>>> local-ci:$Version" -ForegroundColor Cyan
        & docker build `
            --tag "local-ci:$Version" `
            --build-arg "ERLANG_VER=$Version" `
            --file $Dockerfile `
            $ProjectRoot
        if ($LASTEXITCODE -ne 0) {
            throw "Docker build failed for Erlang/OTP $Version."
        }
    }
} catch {
    Write-Error $_.Exception.Message
    exit 1
}
