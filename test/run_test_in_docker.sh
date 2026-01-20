#!/bin/bash
set -e

# 定义路径
SRC_MOUNT="/mnt/source"          # Windows 挂载进来的只读源码
WORK_DIR="/tmp/build/astranaut"  # 容器内的工作目录
LOG_EXPORT="/mnt/logs"           # 日志导出目录

# --- 新增：打印 Erlang 版本信息 ---
echo "========================================"
echo ">>> 当前环境检查:"
# 打印 OTP 版本 (例如: 21)
echo -n "OTP Release: "
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'
# 打印详细模拟器版本
erl -version
echo "========================================"
# ------------------------------------

echo ">>> [1/5] 准备工作目录..."
# 确保目标目录存在且为空
if [ -d "$WORK_DIR" ]; then
    rm -rf "$WORK_DIR"
fi
mkdir -p "$WORK_DIR"

echo ">>> [2/5] 复制源码 (Copy Mode)..."
# 使用 cp -a 递归复制所有文件 (包括隐藏文件如 .git，但不包括挂载点本身)
# 注意：结尾的 /. 是为了把目录下内容拷进去，而不是把目录本身拷进去
tar -C "$SRC_MOUNT" \
    --exclude='.git' \
    --exclude='_build' \
    -cf - . | tar -C "$WORK_DIR" -xf -
    
echo ">>> [3/5] 清理 Windows 编译产物..."
# 【关键步骤】
# 因为你是从 Windows 拷过来的，_build 文件夹里可能包含 Windows 的二进制文件。
# 在 Linux 容器里直接用会报错，所以必须删掉它，强制让 rebar3 在 Linux 下重新编译。
rm -rf "$WORK_DIR/_build"

cd "$WORK_DIR"

echo ">>> [4/5] 编译并运行 Common Test..."
# 编译
rebar3 compile
# 运行测试 (允许失败以便导出日志)
rebar3 ct || echo "!!! 测试中有失败项，继续导出日志..."

echo ">>> [5/5] 导出日志到宿主机..."
mkdir -p "$LOG_EXPORT"

# 检查日志是否存在并导出
if [ -d "_build/test/logs" ]; then
    # -u 选项仅更新较新的文件，或者直接覆盖
    cp -r _build/test/logs/* "$LOG_EXPORT/"
    echo "SUCCESS: 日志已导出。"
    echo "请查看 Windows 目录下的 index.html"
else
    echo "WARNING: 未找到日志文件，可能是编译阶段就挂了。"
fi