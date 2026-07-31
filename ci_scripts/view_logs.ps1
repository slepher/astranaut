<#
.SYNOPSIS
Serve astranaut local-CI logs from the astranaut-local-ci-data Docker volume.
#>

$ErrorActionPreference = "Stop"
$ScriptDir = $PSScriptRoot
$ProjectRoot = Split-Path -Parent $ScriptDir
$ConfigPath = Join-Path $ScriptDir "ci-env.conf"
$LogVolume = "astranaut-local-ci-data"

try {
    if (-not (Test-Path -LiteralPath $ConfigPath)) {
        throw "ci-env.conf is missing."
    }
    if (-not (Get-Command docker -ErrorAction SilentlyContinue)) {
        throw "Docker was not found in PATH."
    }

    $Config = Get-Content -Raw -LiteralPath $ConfigPath |
        ConvertFrom-StringData
    if (-not $Config.ERLANG_VSNS) {
        throw "ERLANG_VSNS is not defined in ci-env.conf."
    }
    $Versions = @(
        $Config.ERLANG_VSNS.Split(",") |
            ForEach-Object { $_.Trim() } |
            Where-Object { $_ }
    )
    $Port = if ($Config.LOG_PORT) { $Config.LOG_PORT.Trim() } else { "8081" }

    if (-not (& docker volume ls -q -f "name=$LogVolume")) {
        throw "Docker volume $LogVolume does not exist. Run local CI first."
    }

    Write-Host "`n=== astranaut local CI logs ===" -ForegroundColor Cyan
    Write-Host "--------------------------------------------------------" -ForegroundColor Green
    foreach ($Version in $Versions) {
        Write-Host ">>> Erlang/OTP $Version" -ForegroundColor Magenta

        & docker run --rm `
            --volume "${LogVolume}:/data:ro" `
            nginx:alpine `
            /bin/sh -c "test -f /data/$Version/ci-summary.txt"
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  Summary: http://localhost:$Port/$Version/ci-summary.txt" -ForegroundColor Cyan
        }

        & docker run --rm `
            --volume "${LogVolume}:/data:ro" `
            nginx:alpine `
            /bin/sh -c "test -f /data/$Version/logs/index.html"
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  Logs:    http://localhost:$Port/$Version/logs/index.html" -ForegroundColor Green
        } else {
            Write-Host "  No Common Test logs found." -ForegroundColor DarkGray
        }

        & docker run --rm `
            --volume "${LogVolume}:/data:ro" `
            nginx:alpine `
            /bin/sh -c "test -f /data/$Version/cover/index.html"
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  Cover:   http://localhost:$Port/$Version/cover/index.html" -ForegroundColor Cyan
        }
        Write-Host ""
    }

    Write-Host "--------------------------------------------------------" -ForegroundColor Green
    Write-Host "Press Ctrl+C to stop the viewer." -ForegroundColor White
    & docker run --rm --interactive --tty `
        --publish "${Port}:80" `
        --volume "${LogVolume}:/usr/share/nginx/html:ro" `
        nginx:alpine `
        /bin/sh -c "nginx -g 'daemon off;'"
} catch {
    Write-Error $_.Exception.Message
    exit 1
}
