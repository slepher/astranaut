#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
CONFIG_FILE="$SCRIPT_DIR/ci-env.conf"
LOG_VOLUME="astranaut-local-ci-data"

if [[ ! -f "$CONFIG_FILE" ]]; then
    echo "Error: ci-env.conf is missing." >&2
    exit 1
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

ERLANG_VSNS="$(read_conf ERLANG_VSNS)"
LOG_PORT="$(read_conf LOG_PORT)"
LOG_PORT="${LOG_PORT:-8081}"
[[ -n "$ERLANG_VSNS" ]] || {
    echo "Error: ERLANG_VSNS is not defined in ci-env.conf." >&2
    exit 1
}
IFS=',' read -r -a TARGET_VERSIONS <<< "$ERLANG_VSNS"

if ! docker volume ls -q -f "name=$LOG_VOLUME" |
     grep -Fxq "$LOG_VOLUME"; then
    echo "Error: Docker volume $LOG_VOLUME does not exist." >&2
    exit 1
fi

echo
echo "=== astranaut local CI logs ==="
echo "--------------------------------------------------------"
for VERSION in "${TARGET_VERSIONS[@]}"; do
    VERSION="$(echo "$VERSION" | xargs)"
    [[ -z "$VERSION" ]] && continue
    echo ">>> Erlang/OTP $VERSION"

    if docker run --rm \
        --volume "$LOG_VOLUME:/data:ro" \
        nginx:alpine \
        /bin/sh -c "test -f /data/$VERSION/ci-summary.txt"; then
        echo "  Summary: http://localhost:$LOG_PORT/$VERSION/ci-summary.txt"
    fi
    if docker run --rm \
        --volume "$LOG_VOLUME:/data:ro" \
        nginx:alpine \
        /bin/sh -c "test -f /data/$VERSION/logs/index.html"; then
        echo "  Logs:    http://localhost:$LOG_PORT/$VERSION/logs/index.html"
    else
        echo "  No Common Test logs found."
    fi
    if docker run --rm \
        --volume "$LOG_VOLUME:/data:ro" \
        nginx:alpine \
        /bin/sh -c "test -f /data/$VERSION/cover/index.html"; then
        echo "  Cover:   http://localhost:$LOG_PORT/$VERSION/cover/index.html"
    fi
    echo
done

echo "--------------------------------------------------------"
echo "Press Ctrl+C to stop the viewer."
docker run --rm --interactive --tty \
    --publish "$LOG_PORT:80" \
    --volume "$LOG_VOLUME:/usr/share/nginx/html:ro" \
    nginx:alpine \
    /bin/sh -c "nginx -g 'daemon off;'"
