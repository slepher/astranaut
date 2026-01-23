#!/bin/bash
set -e

# === 路径定义 ===
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
CONFIG_FILE="$SCRIPT_DIR/ci-env.conf"

if [ ! -f "$CONFIG_FILE" ]; then echo "Error: ci-env.conf not found."; exit 1; fi

# === 1. 配置读取 ===
read_conf() {
    grep "^$1=" "$CONFIG_FILE" | cut -d'=' -f2- | tr -d '\r' | xargs || true
}

ERL_VSNS_RAW=$(read_conf "ERLANG_VSNS")
TEST_SUITE=$(read_conf "TEST_SUITE")
USER_LANG=$(read_conf "OUTPUT_LANG")

# === 2. 参数解析 ===
TARGET_VER=""
NO_VIEW=0

for arg in "$@"; do
    case $arg in
        --noview) NO_VIEW=1 ;;
        *) TARGET_VER="$arg" ;; # 假设非 flag 参数为版本号
    esac
done

if [ -n "$TARGET_VER" ]; then
    TARGET_VERSIONS=("$TARGET_VER")
elif [ -n "$ERL_VSNS_RAW" ]; then
    IFS=',' read -r -a TARGET_VERSIONS <<< "$ERL_VSNS_RAW"
else
    echo "Error: ERLANG_VSNS not defined."
    exit 1
fi

# === 3. 语言判定 ===
LANG_KEY="en"
if [ "$USER_LANG" == "cn" ]; then LANG_KEY="cn"
elif [ "$USER_LANG" == "auto" ] && [[ "$LANG" == zh* ]]; then LANG_KEY="cn"; fi

# === 4. 字典与颜色 ===
CYAN='\033[0;36m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
MAGENTA='\033[0;35m'
GRAY='\033[0;90m'
NC='\033[0m'

if [ "$LANG_KEY" == "cn" ]; then
    MSG_START="=== 开始批量测试 ==="
    MSG_LIST="待测试版本: "
    MSG_PREP="准备数据卷..."
    MSG_RUN=">>> [%s] 正在运行测试..."
    MSG_PASS="版本 %s: 测试通过 (或部分通过)。"
    MSG_FAIL="版本 %s: 测试失败，请查看日志。"
    MSG_FINISH="=== 所有测试已结束 ==="
    MSG_VIEW="正在调用日志查看器..."
else
    MSG_START="=== Batch Test Started ==="
    MSG_LIST="Versions to test: "
    MSG_PREP="Preparing Volume..."
    MSG_RUN=">>> [%s] Running Tests..."
    MSG_PASS="Version %s: Tests Passed (or partial pass)."
    MSG_FAIL="Version %s: Tests Failed. Check logs."
    MSG_FINISH="=== All Tests Finished ==="
    MSG_VIEW="Invoking Log Viewer..."
fi

GLOBAL_VOLUME="local-ci-data"

# === 5. 执行逻辑 ===
echo -e "${MAGENTA}\n$MSG_START${NC}"
echo -e "${GRAY}$MSG_LIST${TARGET_VERSIONS[*]}${NC}"

echo -e "${GRAY}$MSG_PREP${NC}"
if ! docker volume ls -q -f name="$GLOBAL_VOLUME" | grep -q "$GLOBAL_VOLUME"; then
    docker volume create "$GLOBAL_VOLUME" > /dev/null
fi

for VER in "${TARGET_VERSIONS[@]}"; do
    VER=$(echo "$VER" | xargs)
    if [ -z "$VER" ]; then continue; fi

    printf "${CYAN}\n$MSG_RUN${NC}\n" "$VER"

    # 使用 set +e 允许 docker run 返回错误码而不中断脚本
    set +e
    docker run --rm \
        -e "ERLANG_VER=$VER" \
        -e "TEST_SUITE=$TEST_SUITE" \
        -e "OUTPUT_LANG=$LANG_KEY" \
        -v "$PROJECT_ROOT:/mnt/source" \
        -v "$SCRIPT_DIR:/mnt/scripts" \
        -v "$GLOBAL_VOLUME:/mnt/logs" \
        "local-ci:$VER" \
        bash /mnt/scripts/inner_test.sh
    
    EXIT_CODE=$?
    set -e

    if [ $EXIT_CODE -eq 0 ]; then
        printf "${GREEN}$MSG_PASS${NC}\n" "$VER"
    else
        printf "${YELLOW}$MSG_FAIL${NC}\n" "$VER"
    fi
done

echo -e "${MAGENTA}\n$MSG_FINISH${NC}"

if [ $NO_VIEW -eq 0 ]; then
    echo -e "${GRAY}$MSG_VIEW${NC}"
    "$SCRIPT_DIR/view_logs.sh"
fi