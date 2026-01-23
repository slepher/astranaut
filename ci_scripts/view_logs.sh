#!/bin/bash
set -e

# === 路径定义 ===
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/ci-env.conf"

if [ ! -f "$CONFIG_FILE" ]; then echo "Error: ci-env.conf not found."; exit 1; fi

# === 1. 配置读取 ===
read_conf() {
    grep "^$1=" "$CONFIG_FILE" | cut -d'=' -f2- | tr -d '\r' | xargs || true
}

ERL_VSNS_RAW=$(read_conf "ERLANG_VSNS")
USER_LANG=$(read_conf "OUTPUT_LANG")
LOG_PORT=$(read_conf "LOG_PORT")
if [ -z "$LOG_PORT" ]; then LOG_PORT="8080"; fi

if [ -n "$ERL_VSNS_RAW" ]; then
    IFS=',' read -r -a TARGET_VERSIONS <<< "$ERL_VSNS_RAW"
else
    echo "Error: ERLANG_VSNS not found."
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
WHITE='\033[1;37m'
NC='\033[0m'

if [ "$LANG_KEY" == "cn" ]; then
    MSG_START="=== 日志查看器 ==="
    MSG_ERR_VOL="错误: 未找到数据卷 local-ci-data。"
    MSG_NGINX="正在启动日志预览服务器 (静默模式)..."
    MSG_HEADER=">>> [Erlang %s]"
    MSG_TIP="(按 Ctrl+C 停止)"
    MSG_NO_DATA="  (未找到数据)"
else
    MSG_START="=== Log Viewer ==="
    MSG_ERR_VOL="Error: Volume local-ci-data not found."
    MSG_NGINX="Starting Log Viewer (Silent Mode)..."
    MSG_HEADER=">>> [Erlang %s]"
    MSG_TIP="(Press Ctrl+C to stop)"
    MSG_NO_DATA="  (No data found)"
fi

GLOBAL_VOLUME="local-ci-data"

# === 4. 执行逻辑 ===
echo -e "${CYAN}\n$MSG_START${NC}"

if ! docker volume ls -q -f name="$GLOBAL_VOLUME" | grep -q "$GLOBAL_VOLUME"; then
    echo "$MSG_ERR_VOL"
    exit 1
fi

echo -e "${YELLOW}$MSG_NGINX${NC}"
echo -e "${GREEN}--------------------------------------------------------${NC}"

for VER in "${TARGET_VERSIONS[@]}"; do
    VER=$(echo "$VER" | xargs)
    if [ -z "$VER" ]; then continue; fi

    printf "${MAGENTA}$MSG_HEADER${NC}\n" "$VER"

    # Check Logs
    set +e
    docker run --rm -v "$GLOBAL_VOLUME:/data" nginx:alpine /bin/sh -c "[ -f /data/$VER/logs/index.html ]"
    HAS_LOGS=$?
    set -e

    if [ $HAS_LOGS -eq 0 ]; then
        echo -e "${GREEN}  Logs:    http://localhost:$LOG_PORT/$VER/logs/index.html${NC}"
        
        # Check Cover
        set +e
        docker run --rm -v "$GLOBAL_VOLUME:/data" nginx:alpine /bin/sh -c "[ -f /data/$VER/cover/index.html ]"
        HAS_COVER=$?
        set -e
        
        if [ $HAS_COVER -eq 0 ]; then
            echo -e "${CYAN}  Cover:   http://localhost:$LOG_PORT/$VER/cover/index.html${NC}"
        fi
    else
        echo -e "${GRAY}$MSG_NO_DATA${NC}"
    fi
    echo ""
done

echo -e "${GREEN}--------------------------------------------------------${NC}"
echo -e "${WHITE}$MSG_TIP${NC}"

# 启动 Nginx (静默)
docker run --rm -it -p "$LOG_PORT:80" \
    -v "$GLOBAL_VOLUME:/usr/share/nginx/html:ro" \
    nginx:alpine \
    /bin/sh -c "ln -sf /dev/null /var/log/nginx/access.log && sed -i 's/notice/error/' /etc/nginx/nginx.conf && nginx -g 'daemon off;'"