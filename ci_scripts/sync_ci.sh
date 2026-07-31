#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
ASTRANAUT_APP_FILE="$PROJECT_ROOT/src/astranaut.app.src"

usage() {
    cat <<'EOF'
Usage:
  bash ./ci_scripts/sync_ci.sh

The script first checks whether the current project is astranaut. For other
projects it runs:
  rebar3 get-deps

The shared CI source is then resolved from _build/default.
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    usage
    exit 0
fi
if [[ $# -gt 0 ]]; then
    echo "Error: unknown argument $1." >&2
    usage >&2
    exit 1
fi

if [[ -f "$ASTRANAUT_APP_FILE" ]]; then
    echo "Current project is astranaut; no CI synchronization is needed."
    exit 0
fi

if ! command -v rebar3 >/dev/null 2>&1; then
    echo "Error: rebar3 was not found in PATH." >&2
    exit 1
fi

(
    cd "$PROJECT_ROOT"
    rebar3 get-deps
)

LIB_SOURCE_DIR="$PROJECT_ROOT/_build/default/lib/astranaut/ci_scripts"
CHECKOUT_BUILD_DIR="$PROJECT_ROOT/_build/default/checkouts/astranaut"
CHECKOUT_BUILD_SOURCE="$CHECKOUT_BUILD_DIR/src"

if [[ -d "$LIB_SOURCE_DIR" ]]; then
    SOURCE_DIR="$LIB_SOURCE_DIR"
elif [[ -d "$CHECKOUT_BUILD_SOURCE" ]]; then
    APP_SOURCE_DIR="$(cd "$CHECKOUT_BUILD_SOURCE" && pwd -P)"
    CHECKOUT_ROOT="$(dirname "$APP_SOURCE_DIR")"
    if [[ -d "$CHECKOUT_ROOT/ci_scripts" ]]; then
        SOURCE_DIR="$CHECKOUT_ROOT/ci_scripts"
    else
        SOURCE_DIR="$CHECKOUT_BUILD_DIR/ci_scripts"
    fi
else
    echo "Error: get-deps produced neither an astranaut library nor checkout under:" >&2
    echo "  $PROJECT_ROOT/_build/default" >&2
    exit 1
fi

if [[ ! -d "$SOURCE_DIR" ]]; then
    echo "Error: the selected astranaut dependency has no ci_scripts directory:" >&2
    echo "  $SOURCE_DIR" >&2
    exit 1
fi
if [[ -e "$SCRIPT_DIR" && ! -d "$SCRIPT_DIR" ]]; then
    echo "Error: CI target is not a directory: $SCRIPT_DIR" >&2
    exit 1
fi
mkdir -p "$SCRIPT_DIR"

is_project_owned() {
    case "$1" in
        ci-env.conf|ci-env.conf.example|sync_ci.ps1|sync_ci.sh)
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

shopt -s nullglob
SOURCE_FILES=()
declare -A SOURCE_NAMES=()
for SOURCE_FILE in "$SOURCE_DIR"/*; do
    [[ -f "$SOURCE_FILE" ]] || continue
    SOURCE_NAME="$(basename "$SOURCE_FILE")"
    is_project_owned "$SOURCE_NAME" && continue
    SOURCE_FILES+=("$SOURCE_FILE")
    SOURCE_NAMES["$SOURCE_NAME"]=1
done

if [[ ${#SOURCE_FILES[@]} -eq 0 ]]; then
    echo "Error: shared CI source contains no files: $SOURCE_DIR" >&2
    exit 1
fi

for TARGET_FILE in "$SCRIPT_DIR"/*; do
    [[ -f "$TARGET_FILE" ]] || continue
    TARGET_NAME="$(basename "$TARGET_FILE")"
    is_project_owned "$TARGET_NAME" && continue
    if [[ -z "${SOURCE_NAMES[$TARGET_NAME]+present}" ]]; then
        rm -f -- "$TARGET_FILE"
        echo "Removed obsolete link: $TARGET_NAME"
    fi
done

for SOURCE_FILE in "${SOURCE_FILES[@]}"; do
    SOURCE_NAME="$(basename "$SOURCE_FILE")"
    TARGET_FILE="$SCRIPT_DIR/$SOURCE_NAME"
    rm -f -- "$TARGET_FILE"
    ln "$SOURCE_FILE" "$TARGET_FILE"
    echo "Linked: $SOURCE_NAME"
done

echo
echo "Shared CI scripts synchronized from _build:"
echo "  $SOURCE_DIR"
echo "Project configuration preserved in:"
echo "  $SCRIPT_DIR"
