#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
CONFIG_FILE="$SCRIPT_DIR/ci-env.conf"
EXAMPLE_FILE="$SCRIPT_DIR/ci-env.conf.example"

usage() {
    cat <<'EOF'
Usage:
  ./ci_scripts/build.sh [Erlang/OTP version]

Examples:
  ./ci_scripts/build.sh
  ./ci_scripts/build.sh 28
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    usage
    exit 0
fi

if [[ ! -f "$CONFIG_FILE" ]]; then
    cp "$EXAMPLE_FILE" "$CONFIG_FILE"
    echo "Warning: ci-env.conf was created from ci-env.conf.example."
fi

if ! command -v docker >/dev/null 2>&1; then
    echo "Error: Docker was not found in PATH." >&2
    exit 1
fi

read_conf() {
    grep -E "^$1=" "$CONFIG_FILE" |
        cut -d'=' -f2- |
        tr -d '\r' |
        xargs || true
}

if [[ -n "${1:-}" ]]; then
    TARGET_VERSIONS=("$1")
else
    ERLANG_VSNS="$(read_conf ERLANG_VSNS)"
    if [[ -z "$ERLANG_VSNS" ]]; then
        echo "Error: ERLANG_VSNS is not defined in ci-env.conf." >&2
        exit 1
    fi
    IFS=',' read -r -a TARGET_VERSIONS <<< "$ERLANG_VSNS"
fi

echo
echo "=== Building local CI images ==="
for VERSION in "${TARGET_VERSIONS[@]}"; do
    VERSION="$(echo "$VERSION" | xargs)"
    [[ -z "$VERSION" ]] && continue

    echo
    echo ">>> local-ci:$VERSION"
    docker build \
        --tag "local-ci:$VERSION" \
        --build-arg "ERLANG_VER=$VERSION" \
        --file "$SCRIPT_DIR/Dockerfile.local-ci" \
        "$PROJECT_ROOT"
done
