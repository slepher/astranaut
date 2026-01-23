#!/bin/bash
set -e

# === 路径定义 ===
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
CONFIG_FILE="$SCRIPT_DIR/ci-env.conf"
EXAMPLE_FILE="$SCRIPT_DIR/ci-env.conf.example"

# === 0. 自动复制配置 ===
if [ ! -f "$CONFIG_FILE" ]; then
    if [ -f "$EXAMPLE_FILE" ]; then
        cp "$EXAMPLE_FILE" "$CONFIG_FILE"
        echo -e "\033[1;33mWarning: ci-env.conf not found. Created from ci-env.conf.example.\033[0m"
        # 英文: Warning: ci-env.conf not found. Created from ci-env.conf.example.
    else
        echo "Error: Neither ci-env.conf nor ci-env.conf.example found."
        exit 1
    fi
fi

# === 1. 读取配置 ===
# 清理 Windows 回车符 \r
read_conf() {
    grep "^$1=" "$CONFIG_FILE" | cut -d'=' -f2- | tr -d '\r' | xargs
}

ERL_VSNS_RAW=$(read_conf "ERLANG_VSNS")
USER_LANG=$(read_conf "OUTPUT_LANG")

# 确定版本列表: 如果脚本有传参 (e.g. ./build.sh 25) 则只构建该版本，否则构建列表所有
if [ -n "$1" ]; then
    TARGET_VERSIONS=("$1")
elif [ -n "$ERL_VSNS_RAW" ]; then
    # 将逗号分隔字符串转换为数组
    IFS=',' read -r -a TARGET_VERSIONS <<< "$ERL_VSNS_RAW"
else
    echo "Error: ERLANG_VSNS not defined in config."
    exit 1
fi

# === 2. 语言判定 ===
LANG_KEY="en"
if [ "$USER_LANG" == "cn" ]; then LANG_KEY="cn"
elif [ "$USER_LANG" == "auto" ] && [[ "$LANG" == zh* ]]; then LANG_KEY="cn"; fi

# === 3. 字典与颜色 ===
CYAN='\033[0;36m'
GREEN='\033[0;32m'
MAGENTA='\033[0;35m'
GRAY='\033[0;90m'
NC='\033[0m'

if [ "$LANG_KEY" == "cn" ]; then
    MSG_START="=== 开始批量构建 ==="
    MSG_LIST="待构建版本: "
    MSG_BUILD=">>> [%s] 正在构建镜像..."
    MSG_SUCCESS="构建成功: %s"
else
    MSG_START="=== Batch Build Started ==="
    MSG_LIST="Versions to build: "
    MSG_BUILD=">>> [%s] Building Image..."
    MSG_SUCCESS="Build Success: %s"
fi

# === 4. 执行构建 ===
echo -e "${MAGENTA}\n$MSG_START${NC}"
echo -e "${GRAY}$MSG_LIST${TARGET_VERSIONS[*]}${NC}"

for VER in "${TARGET_VERSIONS[@]}"; do
    # 去除可能存在的空格
    VER=$(echo "$VER" | xargs)
    if [ -z "$VER" ]; then continue; fi

    printf "${CYAN}\n$MSG_BUILD${NC}\n" "$VER"

    docker build -t "local-ci:$VER" \
        --build-arg "ERLANG_VER=$VER" \
        -f "$SCRIPT_DIR/Dockerfile.local-ci" \
        "$PROJECT_ROOT"

    printf "${GREEN}$MSG_SUCCESS${NC}\n" "$VER"
done