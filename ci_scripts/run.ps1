<#
.SYNOPSIS
ASTranaut CI Batch Runner
批量运行测试并查看结果

.PARAMETER TargetVer
Only run the specified Erlang/OTP version.

.PARAMETER TestSuite
Only run the specified Common Test suite.

.PARAMETER TestCase
Only run the specified Common Test case. Requires TestSuite.

.PARAMETER NoView
Do not open the log viewer after the test.

.PARAMETER Help
Show command usage.
#>
param (
    [string]$TargetVer = "", # 可选：指定特定版本运行
    [string]$TestSuite = "", # 可选：指定 Common Test Suite
    [string]$TestCase = "",  # 可选：指定 Common Test Case
    [switch]$NoView,         # 可选：运行后不打开日志查看器
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
  .\ci_scripts\run.ps1 [options]

Options:
  -TargetVer <version>   Run one Erlang/OTP version, for example 23.
  -TestSuite <suite>     Run one Common Test suite.
  -TestCase <case>       Run one case from -TestSuite.
  -NoView                Do not open the log viewer.
  --help, -h, -?         Show this help.

Example:
  .\ci_scripts\run.ps1 -TargetVer 23 -TestSuite astranaut_design_SUITE -TestCase lib_form_source_contracts -NoView
"@
    exit 0
}

$ErrorActionPreference = "Stop"
$ScriptDir = $PSScriptRoot

$LangDict = @{
    "en" = @{
        "START_BATCH"   = "=== Batch Test Started ==="
        "TARGET_LIST"   = "Versions to test: {0}"
        "PREP_ENV"      = "Preparing Volume..."
        "START_TEST"    = ">>> [{0}] Running Tests..."
        "TEST_PASS"     = "Version {0}: Tests Passed."
        "TEST_FAIL"     = "Version {0}: Tests Failed. Check logs."
        "ALL_FINISH"    = "=== All Tests Finished ==="
        "CALL_VIEWER"   = "Opening Log Viewer..."
    }
    "cn" = @{
        "START_BATCH"   = "=== 开始批量测试 ==="
        "TARGET_LIST"   = "待测试版本: {0}"
        "PREP_ENV"      = "准备数据卷..."
        "START_TEST"    = ">>> [{0}] 正在运行测试..."
        "TEST_PASS"     = "版本 {0}: 测试通过。"
        "TEST_FAIL"     = "版本 {0}: 测试失败，请查看日志。"
        "ALL_FINISH"    = "=== 所有测试已结束 ==="
        "CALL_VIEWER"   = "正在打开日志查看器..."
    }
}

try {
    # 1. 配置读取
    $ConfigPath = Join-Path $ScriptDir "ci-env.conf"
    if (-not (Test-Path $ConfigPath)) { throw "ci-env.conf missing." }
    $Config = Get-Content $ConfigPath -Raw -ErrorAction Stop | ConvertFrom-StringData
    
    # 2. 确定版本列表
    if ($TargetVer) {
        $VerList = @($TargetVer)
    } elseif ($Config.ERLANG_VSNS) {
        $VerList = $Config.ERLANG_VSNS.Split(",")[0].Trim() # [修正] 这里不需要 Split[0]，要全部
        $VerList = $Config.ERLANG_VSNS.Split(",") | ForEach-Object { $_.Trim() } | Where-Object { $_ -ne "" }
    } else {
        throw "ERLANG_VSNS not defined."
    }
    
    $EffectiveTestSuite =
        if ($TestSuite) { $TestSuite.Trim() }
        elseif ($Config.TEST_SUITE) { $Config.TEST_SUITE.Trim() }
        else { $null }
    $EffectiveTestCase = if ($TestCase) { $TestCase.Trim() } else { $null }
    if ($EffectiveTestCase -and -not $EffectiveTestSuite) {
        throw "-TestCase requires -TestSuite."
    }
    $UserLang  = if ($Config.OUTPUT_LANG) { $Config.OUTPUT_LANG.Trim() } else { "auto" }

    $LangKey = "en"
    if ($UserLang -eq "cn") { $LangKey = "cn" }
    elseif ($UserLang -eq "auto" -and (Get-Culture).Name -match "^zh") { $LangKey = "cn" }
    $Msg = $LangDict[$LangKey]

    $ProjectRoot = Split-Path -Parent $ScriptDir
    $GlobalVolumeName = "local-ci-data"

    Write-Host "`n$($Msg.START_BATCH)" -ForegroundColor Magenta
    Write-Host ($Msg.TARGET_LIST -f ($VerList -join ", ")) -ForegroundColor Gray

    # 3. 准备 Volume (只做一次)
    Write-Host $Msg.PREP_ENV -ForegroundColor DarkGray
    if (-not (docker volume ls -q -f name=$GlobalVolumeName)) {
        docker volume create $GlobalVolumeName | Out-Null
    }

    # 4. 循环运行
    $AnyTestFailed = $false
    foreach ($Ver in $VerList) {
        Write-Host "`n$($Msg.START_TEST -f $Ver)" -ForegroundColor Cyan
        
        # 即使容器内测试失败，run.ps1 也不应立即退出，以便后续版本继续运行
        # 容器退出码 != 0 表示测试有失败
        docker run --rm `
            -e "ERLANG_VER=$Ver" `
            -e "TEST_SUITE=$EffectiveTestSuite" `
            -e "TEST_CASE=$EffectiveTestCase" `
            -e "OUTPUT_LANG=$LangKey" `
            -v "${ProjectRoot}:/mnt/source" `
            -v "${ScriptDir}:/mnt/scripts" `
            -v "${GlobalVolumeName}:/mnt/logs" `
            "local-ci:$Ver" `
            bash /mnt/scripts/inner_test.sh
        
        if ($LASTEXITCODE -eq 0) {
            Write-Host ($Msg.TEST_PASS -f $Ver) -ForegroundColor Green
        } else {
            $AnyTestFailed = $true
            Write-Warning ($Msg.TEST_FAIL -f $Ver)
        }
    }

    Write-Host "`n$($Msg.ALL_FINISH)" -ForegroundColor Magenta
    
    # 5. 调用查看器
    if (-not $NoView) {
        Write-Host $Msg.CALL_VIEWER -ForegroundColor Gray
        & "$ScriptDir\view_logs.ps1"
    }

    if ($AnyTestFailed) {
        exit 1
    }

} catch {
    Write-Error "`nError: $($_.Exception.Message)"
    exit 1
}