#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
CONFIG_FILE="$SCRIPT_DIR/ci-env.conf"
EXAMPLE_FILE="$SCRIPT_DIR/ci-env.conf.example"
LOG_VOLUME="astranaut-local-ci-data"

TARGET_VERSION=""
TEST_SUITE=""
TEST_CASE=""
NO_VIEW=false
FORCE_DIALYZER=false
SKIP_XREF=false
NO_CHECKOUTS=false

usage() {
    cat <<'EOF'
Usage:
  ./ci_scripts/run.sh [Erlang/OTP version] [options]

Options:
  --suite <suite>   Run one Common Test suite.
  --case <case>     Run one case from --suite.
  --dialyzer        Enable Dialyzer for this run.
  --skip-xref       Disable xref for this run.
  --no-checkouts    Ignore the host _checkouts directory.
  --noview          Do not start the log viewer.
  --help, -h        Show this help.

Example:
  ./ci_scripts/run.sh 28 --suite astranaut_design_SUITE \
    --case lib_form_source_contracts --noview
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --suite)
            [[ $# -ge 2 ]] || {
                echo "Error: --suite requires a value." >&2
                exit 1
            }
            TEST_SUITE="$2"
            shift 2
            ;;
        --case)
            [[ $# -ge 2 ]] || {
                echo "Error: --case requires a value." >&2
                exit 1
            }
            TEST_CASE="$2"
            shift 2
            ;;
        --dialyzer)
            FORCE_DIALYZER=true
            shift
            ;;
        --skip-xref)
            SKIP_XREF=true
            shift
            ;;
        --no-checkouts)
            NO_CHECKOUTS=true
            shift
            ;;
        --noview)
            NO_VIEW=true
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        -*)
            echo "Error: unknown option $1." >&2
            exit 1
            ;;
        *)
            [[ -z "$TARGET_VERSION" ]] || {
                echo "Error: specify only one Erlang/OTP version." >&2
                exit 1
            }
            TARGET_VERSION="$1"
            shift
            ;;
    esac
done

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

if [[ -z "$TEST_SUITE" ]]; then
    TEST_SUITE="$(read_conf TEST_SUITE)"
fi
if [[ -z "$TEST_CASE" ]]; then
    TEST_CASE="$(read_conf TEST_CASE)"
fi
if [[ -n "$TEST_CASE" && -z "$TEST_SUITE" ]]; then
    echo "Error: a Common Test case requires a suite." >&2
    exit 1
fi

if [[ -n "$TARGET_VERSION" ]]; then
    TARGET_VERSIONS=("$TARGET_VERSION")
else
    ERLANG_VSNS="$(read_conf ERLANG_VSNS)"
    [[ -n "$ERLANG_VSNS" ]] || {
        echo "Error: ERLANG_VSNS is not defined in ci-env.conf." >&2
        exit 1
    }
    IFS=',' read -r -a TARGET_VERSIONS <<< "$ERLANG_VSNS"
fi

RUN_XREF="$(read_conf RUN_XREF)"
RUN_DIALYZER="$(read_conf RUN_DIALYZER)"
USE_CHECKOUTS="$(read_conf USE_CHECKOUTS)"
OUTPUT_LANGUAGE="$(read_conf OUTPUT_LANG)"
RUN_XREF="${RUN_XREF:-true}"
RUN_DIALYZER="${RUN_DIALYZER:-false}"
USE_CHECKOUTS="${USE_CHECKOUTS:-auto}"
if $SKIP_XREF; then RUN_XREF=false; fi
if $FORCE_DIALYZER; then RUN_DIALYZER=true; fi
if [[ "$OUTPUT_LANGUAGE" == "auto" ]]; then
    if [[ "${LANG:-}" == zh* ]]; then
        OUTPUT_LANGUAGE=cn
    else
        OUTPUT_LANGUAGE=en
    fi
fi

bool_enabled() {
    local value="${1,,}"
    [[ "$value" == "true" || "$value" == "1" || "$value" == "yes" ]]
}

CHECKOUT_MOUNTS=()
CHECKOUTS_DIR="$PROJECT_ROOT/_checkouts"
if [[ -d "$CHECKOUTS_DIR" ]]; then
    for CHECKOUT in "$PROJECT_ROOT/_checkouts"/*; do
        [[ -d "$CHECKOUT" ]] || continue
        CHECKOUT_SOURCE="$(realpath "$CHECKOUT")"
        CHECKOUT_NAME="$(basename "$CHECKOUT")"
        CHECKOUT_MOUNTS+=(
            --volume "$CHECKOUT_SOURCE:/mnt/checkouts/$CHECKOUT_NAME:ro"
        )
    done
fi

if $NO_CHECKOUTS; then
    USE_CHECKOUTS=false
else
    case "${USE_CHECKOUTS,,}" in
        auto|"")
            if [[ ${#CHECKOUT_MOUNTS[@]} -gt 0 ]]; then
                USE_CHECKOUTS=true
            else
                USE_CHECKOUTS=false
            fi
            ;;
        true|1|yes)
            if [[ ${#CHECKOUT_MOUNTS[@]} -eq 0 ]]; then
                echo "Error: USE_CHECKOUTS is enabled but $CHECKOUTS_DIR is missing or empty." >&2
                exit 1
            fi
            USE_CHECKOUTS=true
            ;;
        false|0|no)
            USE_CHECKOUTS=false
            ;;
        *)
            echo "Error: USE_CHECKOUTS must be auto, true, or false." >&2
            exit 1
            ;;
    esac
fi

if ! docker volume ls -q -f "name=$LOG_VOLUME" |
     grep -Fxq "$LOG_VOLUME"; then
    docker volume create "$LOG_VOLUME" >/dev/null
fi

echo
echo "=== astranaut local CI ==="
echo "OTP versions: ${TARGET_VERSIONS[*]}"
echo "Common Test: ${TEST_SUITE:-ALL}${TEST_CASE:+:$TEST_CASE}"
echo "xref=$RUN_XREF, dialyzer=$RUN_DIALYZER, checkouts=$USE_CHECKOUTS"

if bool_enabled "$USE_CHECKOUTS" &&
   [[ ${#CHECKOUT_MOUNTS[@]} -eq 0 ]]; then
        echo "Error: no checkout directories were detected." >&2
        exit 1
fi

ANY_FAILED=0
for VERSION in "${TARGET_VERSIONS[@]}"; do
    VERSION="$(echo "$VERSION" | xargs)"
    [[ -z "$VERSION" ]] && continue

    if ! docker image inspect "local-ci:$VERSION" >/dev/null 2>&1; then
        echo "Error: image local-ci:$VERSION does not exist." >&2
        echo "Run ./ci_scripts/build.sh $VERSION first." >&2
        exit 1
    fi

    echo
    echo ">>> Erlang/OTP $VERSION"
    set +e
    DOCKER_ARGS=(
        run --rm
        --env "ERLANG_VER=$VERSION"
        --env "TEST_SUITE=$TEST_SUITE"
        --env "TEST_CASE=$TEST_CASE"
        --env "RUN_XREF=$RUN_XREF"
        --env "RUN_DIALYZER=$RUN_DIALYZER"
        --env "USE_CHECKOUTS=$USE_CHECKOUTS"
        --env "OUTPUT_LANG=$OUTPUT_LANGUAGE"
        --volume "$PROJECT_ROOT:/mnt/source:ro"
        --volume "$SCRIPT_DIR:/mnt/scripts:ro"
        --volume "$LOG_VOLUME:/mnt/logs"
    )
    if bool_enabled "$USE_CHECKOUTS"; then
        DOCKER_ARGS+=("${CHECKOUT_MOUNTS[@]}")
    fi
    DOCKER_ARGS+=(
        "local-ci:$VERSION"
        bash /mnt/scripts/inner_test.sh
    )
    docker "${DOCKER_ARGS[@]}"
    STATUS=$?
    set -e

    if [[ $STATUS -eq 0 ]]; then
        echo "OTP $VERSION passed."
    else
        ANY_FAILED=1
        echo "OTP $VERSION failed. See its exported CI summary and logs." >&2
    fi
done

if ! $NO_VIEW; then
    "$SCRIPT_DIR/view_logs.sh"
fi
exit "$ANY_FAILED"
