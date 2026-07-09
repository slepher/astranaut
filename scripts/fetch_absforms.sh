#!/usr/bin/env bash
# fetch_absforms.sh — 从 erlang/otp 下载 absform 文档 (maint-19 ~ maint-29)

set -euo pipefail
OUTDIR="absforms"
mkdir -p "$OUTDIR"

RESULTS=()
MISSING=()

for v in $(seq 19 29); do
    MD_URL="https://raw.githubusercontent.com/erlang/otp/refs/heads/maint-${v}/erts/doc/guides/absform.md"
    XML_URL="https://raw.githubusercontent.com/erlang/otp/refs/heads/maint-${v}/erts/doc/src/absform.xml"

    if curl -fsSLo /dev/null -w "%{http_code}" "$MD_URL" 2>/dev/null | grep -q 200; then
        echo "[$v] Downloading .md..."
        curl -fsSLo "$OUTDIR/absform-${v}.md" "$MD_URL"
        RESULTS+=("${v}:md")
    elif curl -fsSLo /dev/null -w "%{http_code}" "$XML_URL" 2>/dev/null | grep -q 200; then
        echo "[$v] Downloading .xml..."
        curl -fsSLo "$OUTDIR/absform-${v}.xml" "$XML_URL"
        RESULTS+=("${v}:xml")
    else
        echo "[$v] NOT FOUND"
        MISSING+=("$v")
    fi
done

echo ""
echo "=== Summary ==="
for r in "${RESULTS[@]}"; do echo "OK: $r"; done
if [ ${#MISSING[@]} -gt 0 ]; then
    echo "MISSING: ${MISSING[*]}"
fi
