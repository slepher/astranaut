#!/usr/bin/env bash
set -uo pipefail

SRC_MOUNT="/mnt/source"
LOG_VOLUME="/mnt/logs"
WORK_DIR="/tmp/build/astranaut"

ERLANG_VER="${ERLANG_VER:-$(erl -noshell -eval \
    'io:format("~s", [erlang:system_info(otp_release)]), halt().')}"
TEST_SUITE="${TEST_SUITE:-}"
TEST_CASE="${TEST_CASE:-}"
RUN_XREF="${RUN_XREF:-true}"
RUN_DIALYZER="${RUN_DIALYZER:-false}"
USE_CHECKOUTS="${USE_CHECKOUTS:-auto}"
OUTPUT_LANG="${OUTPUT_LANG:-en}"

VOL_VER_BASE="$LOG_VOLUME/$ERLANG_VER"
VOL_LOGS_DIR="$VOL_VER_BASE/logs"
VOL_COVER_DIR="$VOL_VER_BASE/cover"
SUMMARY_FILE="$VOL_VER_BASE/ci-summary.txt"

if [[ "$OUTPUT_LANG" == "cn" ]]; then
    MSG_PREPARE="准备隔离工作目录"
    MSG_COMPILE="编译项目"
    MSG_XREF="运行交叉引用检查"
    MSG_DIALYZER="运行 Dialyzer"
    MSG_TEST="运行 Common Test"
    MSG_EXPORT="导出测试日志与覆盖率"
else
    MSG_PREPARE="Preparing isolated work directory"
    MSG_COMPILE="Compiling project"
    MSG_XREF="Running cross-reference checks"
    MSG_DIALYZER="Running Dialyzer"
    MSG_TEST="Running Common Test"
    MSG_EXPORT="Exporting test logs and coverage"
fi

step() {
    echo
    echo "================================================================"
    echo ">>> $1"
    echo "================================================================"
}

run_check() {
    local name="$1"
    shift

    echo "CMD: $*"
    "$@"
    local status=$?
    printf '%s=%s\n' "$name" "$status" >> "$SUMMARY_FILE"
    return "$status"
}

bool_enabled() {
    [[ "${1,,}" == "true" || "$1" == "1" || "${1,,}" == "yes" ]]
}

if [[ "${USE_CHECKOUTS,,}" == "auto" ]]; then
    USE_CHECKOUTS=false
    if [[ -d "/mnt/checkouts" ]]; then
        for checkout_path in /mnt/checkouts/*; do
            if [[ -d "$checkout_path" ]]; then
                USE_CHECKOUTS=true
                break
            fi
        done
    fi
fi

copy_worktree() {
    local source_dir="$1"
    local target_dir="$2"

    mkdir -p "$target_dir"
    if git -C "$source_dir" rev-parse --is-inside-work-tree \
        >/dev/null 2>&1; then
        (
            cd "$source_dir"
            git ls-files --cached --others --exclude-standard -z |
                while IFS= read -r -d '' tracked_path; do
                    if [[ -e "$tracked_path" || -L "$tracked_path" ]]; then
                        printf '%s\0' "$tracked_path"
                    fi
                done |
                tar --null --verbatim-files-from --files-from=- -cf -
        ) |
            tar -C "$target_dir" -xf -
    else
        tar -C "$source_dir" \
            --exclude='.git' \
            --exclude='_build' \
            --exclude='_checkouts' \
            -cf - . |
            tar -C "$target_dir" -xf -
    fi
}

echo "Erlang/OTP:      $ERLANG_VER"
echo "Common Test:     ${TEST_SUITE:-ALL}${TEST_CASE:+:$TEST_CASE}"
echo "Run xref:        $RUN_XREF"
echo "Run Dialyzer:    $RUN_DIALYZER"
echo "Use _checkouts:  $USE_CHECKOUTS"

step "$MSG_PREPARE"
rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR" "$VOL_VER_BASE"

if ! copy_worktree "$SRC_MOUNT" "$WORK_DIR"; then
    echo "Error: failed to copy the project worktree." >&2
    exit 1
fi

if bool_enabled "$USE_CHECKOUTS"; then
    if [[ ! -d "/mnt/checkouts" ]]; then
        echo "Error: USE_CHECKOUTS is enabled but /mnt/checkouts is not mounted." >&2
        exit 1
    fi
    mkdir -p "$WORK_DIR/_checkouts"
    CHECKOUT_NAMES=()
    for CHECKOUT in /mnt/checkouts/*; do
        [[ -d "$CHECKOUT" ]] || continue
        CHECKOUT_NAME="$(basename "$CHECKOUT")"
        if ! copy_worktree \
            "$CHECKOUT" \
            "$WORK_DIR/_checkouts/$CHECKOUT_NAME"; then
            echo "Error: failed to copy checkout $CHECKOUT_NAME." >&2
            exit 1
        fi
        CHECKOUT_NAMES+=("$CHECKOUT_NAME")
    done
else
    CHECKOUT_NAMES=()
fi

mkdir -p "$WORK_DIR/_build/test/logs"
if [[ -d "$VOL_LOGS_DIR" ]] &&
   [[ -n "$(ls -A "$VOL_LOGS_DIR" 2>/dev/null)" ]]; then
    cp -r "$VOL_LOGS_DIR"/. "$WORK_DIR/_build/test/logs/"
fi

rm -f "$SUMMARY_FILE"
cat > "$SUMMARY_FILE" <<EOF
project=astranaut
erlang_otp=$ERLANG_VER
test_suite=${TEST_SUITE:-ALL}
test_case=${TEST_CASE:-ALL}
use_checkouts=$USE_CHECKOUTS
checkouts=${CHECKOUT_NAMES[*]:-none}
EOF

cd "$WORK_DIR"
CI_EXIT_CODE=0

step "$MSG_COMPILE"
run_check compile rebar3 compile || CI_EXIT_CODE=$?

if [[ $CI_EXIT_CODE -eq 0 ]] && bool_enabled "$RUN_XREF"; then
    step "$MSG_XREF"
    run_check xref rebar3 xref || CI_EXIT_CODE=$?
else
    printf 'xref=skipped\n' >> "$SUMMARY_FILE"
fi

if [[ $CI_EXIT_CODE -eq 0 ]] && bool_enabled "$RUN_DIALYZER"; then
    step "$MSG_DIALYZER"
    run_check dialyzer rebar3 dialyzer || CI_EXIT_CODE=$?
else
    printf 'dialyzer=skipped\n' >> "$SUMMARY_FILE"
fi

if [[ $CI_EXIT_CODE -eq 0 ]]; then
    CT_COMMAND=(rebar3 ct)
    if [[ -n "$TEST_SUITE" ]]; then
        CT_COMMAND+=(--suite "$TEST_SUITE")
    fi
    if [[ -n "$TEST_CASE" ]]; then
        CT_COMMAND+=(--case "$TEST_CASE")
    fi

    step "$MSG_TEST"
    run_check common_test "${CT_COMMAND[@]}" || CI_EXIT_CODE=$?
else
    printf 'common_test=skipped\n' >> "$SUMMARY_FILE"
fi

step "$MSG_EXPORT"
mkdir -p "$VOL_LOGS_DIR" "$VOL_COVER_DIR"

if [[ -d "_build/test/logs" ]]; then
    rm -rf "$VOL_LOGS_DIR"/*
    cp -r _build/test/logs/. "$VOL_LOGS_DIR/"
fi

if [[ -d "_build/test/cover" ]]; then
    rm -rf "$VOL_COVER_DIR"/*
    cp -r _build/test/cover/. "$VOL_COVER_DIR/"
fi

printf 'result=%s\n' "$CI_EXIT_CODE" >> "$SUMMARY_FILE"
cat "$SUMMARY_FILE"
exit "$CI_EXIT_CODE"
