<#
.SYNOPSIS
Synchronize hard links to the shared astranaut local-CI scripts.

.DESCRIPTION
The CI implementation remains in astranaut/ci_scripts. This script recreates
the shared files in the local ci_scripts directory as hard links. Project
configuration files and synchronization launchers are never linked or removed.
When run from astranaut itself, the script exits without making changes.
#>
param (
    [Alias("h", "?")]
    [switch]$Help
)

if ($Help) {
    @"
Usage:
  .\ci_scripts\sync_ci.ps1

The script first checks whether the current project is astranaut. For other
projects it runs:
  rebar3 get-deps

The shared CI source is then resolved from _build/default.
"@
    exit 0
}

$ErrorActionPreference = "Stop"
$TargetDir = $PSScriptRoot
$ProjectRoot = Split-Path -Parent $TargetDir
$AstranautAppFile = Join-Path $ProjectRoot "src\astranaut.app.src"

if (Test-Path -LiteralPath $AstranautAppFile -PathType Leaf) {
    Write-Host "Current project is astranaut; no CI synchronization is needed." `
        -ForegroundColor Green
    exit 0
}

try {
    Push-Location $ProjectRoot
    try {
        & rebar3 get-deps
        if ($LASTEXITCODE -ne 0) {
            throw "rebar3 get-deps failed with exit code $LASTEXITCODE."
        }
    } finally {
        Pop-Location
    }

    $LibSourceDir = Join-Path `
        $ProjectRoot `
        "_build\default\lib\astranaut\ci_scripts"
    $CheckoutBuildDir = Join-Path `
        $ProjectRoot `
        "_build\default\checkouts\astranaut"
    $CheckoutBuildSource = Join-Path $CheckoutBuildDir "src"

    if (Test-Path -LiteralPath $LibSourceDir -PathType Container) {
        $SourceDir = $LibSourceDir
    } elseif (Test-Path -LiteralPath $CheckoutBuildSource -PathType Container) {
        $SourceItem = Get-Item -LiteralPath $CheckoutBuildSource
        if ($SourceItem.LinkType -and $SourceItem.Target) {
            $AppSourceDir = [string](@($SourceItem.Target)[0])
            if (-not [System.IO.Path]::IsPathRooted($AppSourceDir)) {
                $AppSourceDir = Join-Path `
                    $SourceItem.Parent.FullName `
                    $AppSourceDir
            }
            $CheckoutRoot = Split-Path -Parent `
                ([System.IO.Path]::GetFullPath($AppSourceDir))
            $SourceDir = Join-Path $CheckoutRoot "ci_scripts"
        } else {
            $SourceDir = Join-Path $CheckoutBuildDir "ci_scripts"
        }
    } else {
        throw @"
get-deps produced neither an astranaut library nor checkout under:
  $(Join-Path $ProjectRoot '_build\default')
"@
    }

    if (-not (Test-Path -LiteralPath $SourceDir -PathType Container)) {
        throw @"
The astranaut dependency selected by get-deps has no ci_scripts directory:
  $SourceDir
"@
    }
    if (-not (Test-Path -LiteralPath $TargetDir)) {
        New-Item -ItemType Directory -Path $TargetDir | Out-Null
    } elseif (-not (Test-Path -LiteralPath $TargetDir -PathType Container)) {
        throw "CI target is not a directory: $TargetDir"
    }

    $ProjectOwnedNames = @(
        "ci-env.conf",
        "ci-env.conf.example",
        "sync_ci.ps1",
        "sync_ci.sh"
    )
    $SourceFiles = @(
        Get-ChildItem -LiteralPath $SourceDir -File |
            Where-Object { $_.Name -notin $ProjectOwnedNames } |
            Sort-Object Name
    )
    if ($SourceFiles.Count -eq 0) {
        throw "Shared CI source contains no files: $SourceDir"
    }
    $SourceNames = @($SourceFiles | ForEach-Object { $_.Name })

    foreach ($TargetFile in Get-ChildItem -LiteralPath $TargetDir -File) {
        if ($TargetFile.Name -in $ProjectOwnedNames) {
            continue
        }
        if ($TargetFile.Name -notin $SourceNames) {
            Remove-Item -LiteralPath $TargetFile.FullName
            Write-Host "Removed obsolete link: $($TargetFile.Name)"
        }
    }

    foreach ($SourceFile in $SourceFiles) {
        $TargetFile = Join-Path $TargetDir $SourceFile.Name
        if (Test-Path -LiteralPath $TargetFile) {
            Remove-Item -LiteralPath $TargetFile
        }
        New-Item -ItemType HardLink `
            -Path $TargetFile `
            -Target $SourceFile.FullName | Out-Null
        Write-Host "Linked: $($SourceFile.Name)"
    }

    Write-Host ""
    Write-Host "Shared CI scripts synchronized from _build:" `
        -ForegroundColor Green
    Write-Host "  $SourceDir"
    Write-Host "Project configuration preserved in:" `
        -ForegroundColor Green
    Write-Host "  $TargetDir"
} catch {
    Write-Error $_.Exception.Message
    exit 1
}
