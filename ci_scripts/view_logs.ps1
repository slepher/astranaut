<#
.SYNOPSIS
ASTranaut CI Log Viewer
#>

$ErrorActionPreference = "Stop"
$ScriptDir = $PSScriptRoot

$LangDict = @{
    "en" = @{
        "START_VIEWER"  = "=== Log Viewer ==="
        "VOL_MISSING"   = "Error: Data volume '{0}' not found."
        "START_NGINX"   = "Starting Log Viewer (Silent Mode)..."
        "VER_HEADER"    = ">>> [Erlang {0}]"
        "URL_LOGS"      = "  Logs:    "
        "URL_COVER"     = "  Cover:   "
        "NO_DATA"       = "  (No data found)"
        "STOP_TIP"      = "(Press Ctrl+C to stop server)"
    }
    "cn" = @{
        "START_VIEWER"  = "=== 日志查看器 ==="
        "VOL_MISSING"   = "错误: 未找到数据卷 '{0}'。"
        "START_NGINX"   = "正在启动日志预览服务器 (静默模式)..."
        "VER_HEADER"    = ">>> [Erlang {0}]"
        "URL_LOGS"      = "  日志:    "
        "URL_COVER"     = "  覆盖率:  "
        "NO_DATA"       = "  (未找到数据)"
        "STOP_TIP"      = "(按 Ctrl+C 停止服务器)"
    }
}

try {
    $ConfigPath = Join-Path $ScriptDir "ci-env.conf"
    if (-not (Test-Path $ConfigPath)) { throw "ci-env.conf missing." }
    $Config = Get-Content $ConfigPath -Raw -ErrorAction Stop | ConvertFrom-StringData
    
    $VsnsRaw = $Config.ERLANG_VSNS
    if (-not $VsnsRaw) { throw "ERLANG_VSNS not found." }
    $VerList = $VsnsRaw.Split(",") | ForEach-Object { $_.Trim() } | Where-Object { $_ -ne "" }

    $UserLang  = if ($Config.OUTPUT_LANG) { $Config.OUTPUT_LANG.Trim() } else { "auto" }
    $NginxPort = if ($Config.LOG_PORT) { $Config.LOG_PORT.Trim() } else { "8080" }

    $LangKey = "en"
    if ($UserLang -eq "cn") { $LangKey = "cn" }
    elseif ($UserLang -eq "auto" -and (Get-Culture).Name -match "^zh") { $LangKey = "cn" }
    $Msg = $LangDict[$LangKey]

    $GlobalVolumeName = "local-ci-data"

    Write-Host "`n$($Msg.START_VIEWER)" -ForegroundColor Cyan

    if (-not (docker volume ls -q -f name=$GlobalVolumeName)) {
        Write-Error ($Msg.VOL_MISSING -f $GlobalVolumeName)
        exit 1
    }

    Write-Host $Msg.START_NGINX -ForegroundColor Yellow
    Write-Host "--------------------------------------------------------" -ForegroundColor Green

    foreach ($Ver in $VerList) {
        Write-Host ($Msg.VER_HEADER -f $Ver) -ForegroundColor Magenta
        
        # Check Logs
        docker run --rm -v "${GlobalVolumeName}:/data" nginx:alpine /bin/sh -c "[ -f /data/$Ver/logs/index.html ]"
        $HasLogs = $LASTEXITCODE -eq 0

        if ($HasLogs) {
            Write-Host "$($Msg.URL_LOGS)http://localhost:$NginxPort/$Ver/logs/index.html" -ForegroundColor Green
            
            # Check Cover
            docker run --rm -v "${GlobalVolumeName}:/data" nginx:alpine /bin/sh -c "[ -f /data/$Ver/cover/index.html ]"
            $HasCover = $LASTEXITCODE -eq 0
            
            if ($HasCover) {
                Write-Host "$($Msg.URL_COVER)http://localhost:$NginxPort/$Ver/cover/index.html" -ForegroundColor Cyan
            }
        } else {
            Write-Host $Msg.NO_DATA -ForegroundColor DarkGray
        }
        Write-Host ""
    }

    Write-Host "--------------------------------------------------------" -ForegroundColor Green
    Write-Host $Msg.STOP_TIP -ForegroundColor White

    docker run --rm -it -p "${NginxPort}:80" `
        -v "${GlobalVolumeName}:/usr/share/nginx/html:ro" `
        nginx:alpine `
        /bin/sh -c "ln -sf /dev/null /var/log/nginx/access.log && sed -i 's/notice/error/' /etc/nginx/nginx.conf && nginx -g 'daemon off;'"

} catch {
    Write-Error "`nError: $($_.Exception.Message)"
    exit 1
}