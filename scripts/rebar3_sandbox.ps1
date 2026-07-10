# Used by Codex sandbox mode to avoid Erlang starting the external inet_gethost helper.
# It points Erlang host lookup at scripts\codex_inetrc before running rebar3.
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$env:ERL_INETRC = Join-Path $repoRoot "scripts\codex_inetrc"

Set-Location $repoRoot

& rebar3 @args
exit $LASTEXITCODE
