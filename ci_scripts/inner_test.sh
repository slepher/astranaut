#!/bin/bash
set -e

# ==============================================================================
# 多语言字典定义
# ==============================================================================
# 默认为英文
MSG_ENV_INFO="Build Environment Info:"
MSG_STEP_INIT="Initializing work directory..."
MSG_STEP_COPY="Copying source code..."
MSG_STEP_CLEAN="Cleaning Windows build artifacts..."
MSG_STEP_RESTORE="Restoring test history from volume..."
MSG_HIST_LOADED="Done: History loaded."
MSG_HIST_EMPTY="Info: No history found (First run)."
MSG_STEP_TEST="Running Common Test..."
MSG_MODE_SINGLE=">>> Mode: Single Suite"
MSG_MODE_ALL=">>> Mode: All Tests"
MSG_TEST_FAIL="!!! Tests failed, proceeding to log export..."
MSG_STEP_EXPORT="Exporting results to Volume..."
MSG_SUCC_LOGS="SUCCESS: Test logs updated."
MSG_ERR_LOGS="ERROR: No test logs found."
MSG_FOUND_COVER="Info: Coverage data found and exported."

# 如果环境变量 OUTPUT_LANG 为 cn，则覆盖为中文
if [ "$OUTPUT_LANG" == "cn" ]; then
    MSG_ENV_INFO="构建环境信息:"
    MSG_STEP_INIT="初始化工作目录..."
    MSG_STEP_COPY="复制源码到工作区..."
    MSG_STEP_CLEAN="清理 Windows 编译残留..."
    MSG_STEP_RESTORE="从卷中恢复测试历史..."
    MSG_HIST_LOADED="完成: 历史记录已加载。"
    MSG_HIST_EMPTY="提示: 无历史记录 (首次运行)。"
    MSG_STEP_TEST="执行编译与测试 (Common Test)..."
    MSG_MODE_SINGLE=">>> 模式: 单个 Suite 测试"
    MSG_MODE_ALL=">>> 模式: 全量测试"
    MSG_TEST_FAIL="!!! 测试中有用例失败，继续导出日志..."
    MSG_STEP_EXPORT="导出结果到数据卷..."
    MSG_SUCC_LOGS="成功: 测试日志已更新。"
    MSG_ERR_LOGS="错误: 未生成测试日志。"
    MSG_FOUND_COVER="提示: 检测到覆盖率数据，已导出。"
fi

# ==============================================================================
# 辅助函数
# ==============================================================================
CURRENT_STEP=0
TOTAL_STEPS=6

log_step() {
    CURRENT_STEP=$((CURRENT_STEP + 1))
    echo ""
    echo "================================================================"
    echo ">>> [$CURRENT_STEP/$TOTAL_STEPS] $1"
    echo "================================================================"
}

# ==============================================================================
# 路径定义
# ==============================================================================
SRC_MOUNT="/mnt/source"
LOG_VOLUME="/mnt/logs"
WORK_DIR="/tmp/build/astranaut"

# 1. 确定 Erlang 版本
if [ -z "$ERLANG_VER" ]; then
    ERLANG_VER=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().')
fi

# 2. 定义基于版本的子目录结构
# 结构: /mnt/logs/21/logs 和 /mnt/logs/21/cover (如果有)
VOL_VER_BASE="$LOG_VOLUME/$ERLANG_VER"
VOL_LOGS_DIR="$VOL_VER_BASE/logs"
VOL_COVER_DIR="$VOL_VER_BASE/cover"

# ==============================================================================
# 脚本主体
# ==============================================================================

# 0. 环境检查
echo "----------------------------------------"
echo "$MSG_ENV_INFO"
echo "Target Version: $ERLANG_VER"
echo "Target Suite:   ${TEST_SUITE:-ALL}"
echo "----------------------------------------"

# 1. 准备目录
log_step "$MSG_STEP_INIT"
if [ -d "$WORK_DIR" ]; then rm -rf "$WORK_DIR"; fi
mkdir -p "$WORK_DIR"

# 2. 复制代码
log_step "$MSG_STEP_COPY"
tar -C "$SRC_MOUNT" \
    --exclude='.git' --exclude='_build' --exclude='_ci_test_logs' --exclude='ci_scripts' \
    -cf - . | tar -C "$WORK_DIR" -xf -

# 3. 清理残留
log_step "$MSG_STEP_CLEAN"
rm -rf "$WORK_DIR/_build"

# 4. 恢复历史日志
log_step "$MSG_STEP_RESTORE"
mkdir -p "$WORK_DIR/_build/test/logs"

if [ -d "$VOL_LOGS_DIR" ] && [ "$(ls -A $VOL_LOGS_DIR)" ]; then
    cp -r "$VOL_LOGS_DIR"/* "$WORK_DIR/_build/test/logs/" 2>/dev/null || true
    echo "$MSG_HIST_LOADED"
else
    echo "$MSG_HIST_EMPTY"
fi

# 5. 执行测试
log_step "$MSG_STEP_TEST"
cd "$WORK_DIR"

if [ -n "$TEST_SUITE" ]; then
    echo "$MSG_MODE_SINGLE ($TEST_SUITE)"
    CT_CMD="rebar3 ct --suite $TEST_SUITE"
else
    echo "$MSG_MODE_ALL"
    CT_CMD="rebar3 ct"
fi

echo "CMD: $CT_CMD"
# 执行测试，允许失败以便导出日志
$CT_CMD || echo "$MSG_TEST_FAIL"

# 6. 导出结果
log_step "$MSG_STEP_EXPORT"

mkdir -p "$VOL_LOGS_DIR"
mkdir -p "$VOL_COVER_DIR"

# (A) 智能导出覆盖率 (如果 rebar.config 开启了 cover，这里会有数据)
if [ -d "_build/test/cover" ] && [ "$(ls -A _build/test/cover)" ]; then
    rm -rf "$VOL_COVER_DIR"/*
    cp -r _build/test/cover/* "$VOL_COVER_DIR/"
    echo "$MSG_FOUND_COVER"
fi

# (B) 导出日志
if [ -d "_build/test/logs" ]; then
    cp -rf _build/test/logs/* "$VOL_LOGS_DIR/"
    echo "$MSG_SUCC_LOGS"
else
    echo "$MSG_ERR_LOGS"
    exit 1
fi