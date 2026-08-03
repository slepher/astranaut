# fetch_absforms.ps1 — 从 erlang/otp 下载 absform 文档 (maint-21 ~ maint-29)

param(
    [string]$OutDir = "absforms"
)

$ErrorActionPreference = "Stop"
New-Item -ItemType Directory -Path $OutDir -Force | Out-Null

$results = @()
$errors  = @()

for ($v = 21; $v -le 29; $v++) {
    $mdUrl  = "https://raw.githubusercontent.com/erlang/otp/refs/heads/maint-$v/erts/doc/guides/absform.md"
    $xmlUrl = "https://raw.githubusercontent.com/erlang/otp/refs/heads/maint-$v/erts/doc/src/absform.xml"
    $got = $false

    try {
        $code = (Invoke-WebRequest -Uri $mdUrl -Method Head -ErrorAction Stop).StatusCode
        if ($code -eq 200) {
            Write-Host "[$v] Downloading .md..." -ForegroundColor Green
            Invoke-WebRequest -Uri $mdUrl -OutFile "$OutDir\absform-$v.md" -ErrorAction Stop
            $results += "${v}:md"
            $got = $true
        }
    } catch { }

    if (-not $got) {
        try {
            $code = (Invoke-WebRequest -Uri $xmlUrl -Method Head -ErrorAction Stop).StatusCode
            if ($code -eq 200) {
                Write-Host "[$v] Downloading .xml..." -ForegroundColor Green
                Invoke-WebRequest -Uri $xmlUrl -OutFile "$OutDir\absform-$v.xml" -ErrorAction Stop
                $results += "${v}:xml"
                $got = $true
            }
        } catch { }
    }

    if (-not $got) {
        Write-Host "[$v] NOT FOUND" -ForegroundColor Red
        $errors += $v
    }
}

Write-Host ""
Write-Host "=== Summary ===" -ForegroundColor Yellow
foreach ($r in $results) { Write-Host "OK: $r" -ForegroundColor Green }
if ($errors.Count -gt 0) { Write-Host "MISSING: $($errors -join ', ')" -ForegroundColor Red }
