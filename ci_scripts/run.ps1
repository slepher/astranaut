<#
.SYNOPSIS
Run astranaut CI checks in reusable local-ci Erlang/OTP images.
#>
param (
    [string]$TargetVer = "",
    [string]$TestSuite = "",
    [string]$TestCase = "",
    [switch]$RunDialyzer,
    [switch]$SkipXref,
    [switch]$NoCheckouts,
    [switch]$NoView,
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
  -TargetVer <version>   Run one Erlang/OTP version.
  -TestSuite <suite>     Run one Common Test suite.
  -TestCase <case>       Run one case from -TestSuite.
  -RunDialyzer           Enable Dialyzer for this run.
  -SkipXref              Disable xref for this run.
  -NoCheckouts           Ignore the host _checkouts directory.
  -NoView                Do not start the log viewer.
  --help, -h, -?         Show this help.

Example:
  .\ci_scripts\run.ps1 -TargetVer 28 -TestSuite astranaut_design_SUITE `
    -TestCase lib_form_source_contracts -NoView
"@
    exit 0
}

$ErrorActionPreference = "Stop"
$ScriptDir = $PSScriptRoot
$ProjectRoot = Split-Path -Parent $ScriptDir
$ConfigPath = Join-Path $ScriptDir "ci-env.conf"
$ExamplePath = Join-Path $ScriptDir "ci-env.conf.example"
$LogVolume = "astranaut-local-ci-data"

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

    $EffectiveSuite =
        if ($TestSuite) { $TestSuite.Trim() }
        elseif ($Config.TEST_SUITE) { $Config.TEST_SUITE.Trim() }
        else { "" }
    $EffectiveCase =
        if ($TestCase) { $TestCase.Trim() }
        elseif ($Config.TEST_CASE) { $Config.TEST_CASE.Trim() }
        else { "" }
    if ($EffectiveCase -and -not $EffectiveSuite) {
        throw "A Common Test case requires a suite."
    }

    $XrefValue =
        if ($SkipXref) { "false" }
        elseif ($Config.RUN_XREF) {
            $Config.RUN_XREF.Trim().ToLowerInvariant()
        } else { "true" }
    $DialyzerValue =
        if ($RunDialyzer) { "true" }
        elseif ($Config.RUN_DIALYZER) {
            $Config.RUN_DIALYZER.Trim().ToLowerInvariant()
        } else { "false" }
    $RequestedCheckouts =
        if ($NoCheckouts) { "false" }
        elseif ($Config.USE_CHECKOUTS) {
            $Config.USE_CHECKOUTS.Trim().ToLowerInvariant()
        } else { "auto" }
    $CheckoutsPath = Join-Path $ProjectRoot "_checkouts"
    $CheckoutEntries =
        if (Test-Path -LiteralPath $CheckoutsPath -PathType Container) {
            @(
                Get-ChildItem -LiteralPath $CheckoutsPath |
                    Where-Object { $_.PSIsContainer }
            )
        } else {
            @()
        }
    $UseCheckoutsValue =
        switch -Regex ($RequestedCheckouts) {
            "^(auto)?$" {
                if ($CheckoutEntries.Count -gt 0) { "true" } else { "false" }
                break
            }
            "^(true|1|yes)$" {
                if ($CheckoutEntries.Count -eq 0) {
                    throw "USE_CHECKOUTS is enabled but $CheckoutsPath is missing or empty."
                }
                "true"
                break
            }
            "^(false|0|no)$" {
                "false"
                break
            }
            default {
                throw "USE_CHECKOUTS must be auto, true, or false."
            }
        }
    $UseChinese =
        $Config.OUTPUT_LANG -eq "cn" -or
        ($Config.OUTPUT_LANG -eq "auto" -and
         (Get-Culture).Name -match "^zh")
    $OutputLanguage = if ($UseChinese) { "cn" } else { "en" }

    if (-not (& docker volume ls -q -f "name=$LogVolume")) {
        & docker volume create $LogVolume | Out-Null
        if ($LASTEXITCODE -ne 0) {
            throw "Failed to create Docker volume $LogVolume."
        }
    }

    Write-Host "`n=== astranaut local CI ===" -ForegroundColor Magenta
    Write-Host "OTP versions: $($Versions -join ', ')" -ForegroundColor Gray
    Write-Host "Common Test: $(
        if ($EffectiveSuite) {
            $EffectiveSuite + $(if ($EffectiveCase) { ":$EffectiveCase" })
        } else { "ALL" }
    )" -ForegroundColor Gray
    Write-Host "xref=$XrefValue, dialyzer=$DialyzerValue, checkouts=$UseCheckoutsValue" -ForegroundColor Gray

    $CheckoutMounts = @()
    if ($UseCheckoutsValue -eq "true") {
        foreach ($Checkout in $CheckoutEntries) {
            $CheckoutSource =
                if ($Checkout.LinkType -and $Checkout.Target) {
                    [string](@($Checkout.Target)[0])
                } else {
                    $Checkout.FullName
                }
            if (-not [System.IO.Path]::IsPathRooted($CheckoutSource)) {
                $CheckoutSource =
                    Join-Path $Checkout.Parent.FullName $CheckoutSource
            }
            $CheckoutSource = [System.IO.Path]::GetFullPath($CheckoutSource)
            if (-not (Test-Path -LiteralPath $CheckoutSource -PathType Container)) {
                throw "Checkout target does not exist: $CheckoutSource"
            }
            $CheckoutMounts += @{
                Source = $CheckoutSource
                Name = $Checkout.Name
            }
        }
    }

    $AnyFailed = $false
    foreach ($Version in $Versions) {
        & docker image inspect "local-ci:$Version" | Out-Null
        if ($LASTEXITCODE -ne 0) {
            throw "Image local-ci:$Version does not exist. Run build.ps1 first."
        }

        Write-Host "`n>>> Erlang/OTP $Version" -ForegroundColor Cyan
        $DockerArgs = @(
            "run", "--rm",
            "--env", "ERLANG_VER=$Version",
            "--env", "TEST_SUITE=$EffectiveSuite",
            "--env", "TEST_CASE=$EffectiveCase",
            "--env", "RUN_XREF=$XrefValue",
            "--env", "RUN_DIALYZER=$DialyzerValue",
            "--env", "USE_CHECKOUTS=$UseCheckoutsValue",
            "--env", "OUTPUT_LANG=$OutputLanguage",
            "--volume", "${ProjectRoot}:/mnt/source:ro",
            "--volume", "${ScriptDir}:/mnt/scripts:ro",
            "--volume", "${LogVolume}:/mnt/logs"
        )
        if ($UseCheckoutsValue -eq "true") {
            foreach ($CheckoutMount in $CheckoutMounts) {
                $DockerArgs += @(
                    "--volume",
                    "$($CheckoutMount.Source):/mnt/checkouts/$($CheckoutMount.Name):ro"
                )
            }
        }
        $DockerArgs += @(
            "local-ci:$Version",
            "bash", "/mnt/scripts/inner_test.sh"
        )
        & docker @DockerArgs

        if ($LASTEXITCODE -eq 0) {
            Write-Host "OTP $Version passed." -ForegroundColor Green
        } else {
            $AnyFailed = $true
            Write-Warning "OTP $Version failed. See its exported CI summary and logs."
        }
    }

    if (-not $NoView) {
        & "$ScriptDir\view_logs.ps1"
    }
    if ($AnyFailed) {
        exit 1
    }
} catch {
    Write-Error $_.Exception.Message
    exit 1
}
