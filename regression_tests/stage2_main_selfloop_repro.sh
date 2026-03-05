#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2_compiler> [probe_wait_sec] [source]" >&2
  echo "  probe_wait_sec: seconds to wait before LLDB probe (default: 15)" >&2
  echo "  source: source to compile (default: src/crystal_v2.cr)" >&2
  exit 2
fi

COMPILER="$1"
PROBE_WAIT_SEC="${2:-15}"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SOURCE="${3:-$ROOT_DIR/src/crystal_v2.cr}"

OUT_DIR="${TMPDIR:-/tmp}/stage2_main_selfloop_repro"
RUN_LOG="$OUT_DIR/compile.log"
LLDB_LOG="$OUT_DIR/lldb.txt"
OUT_BIN="$OUT_DIR/stage3_probe"

mkdir -p "$OUT_DIR"

if [ ! -x "$COMPILER" ]; then
  echo "compiler not executable: $COMPILER" >&2
  exit 2
fi

if [ ! -f "$SOURCE" ]; then
  echo "source not found: $SOURCE" >&2
  exit 2
fi

(
  CRYSTAL_CACHE_DIR_STAGE2_RELEASE="${CRYSTAL_CACHE_DIR_STAGE2_RELEASE:-$OUT_DIR/cache_release}" \
    "$COMPILER" "$SOURCE" --release -o "$OUT_BIN"
) >"$RUN_LOG" 2>&1 &
PID=$!

sleep "$PROBE_WAIT_SEC"

if ! kill -0 "$PID" 2>/dev/null; then
  set +e
  wait "$PID"
  rc=$?
  set -e
  echo "status: $rc"
  echo "run_log: $RUN_LOG"
  echo "lldb_log: $LLDB_LOG"
  echo "not reproduced (process exited before probe window)"
  exit 1
fi

lldb -p "$PID" \
  -o 'thread backtrace all' \
  -o 'disassemble --frame' \
  -o detach \
  -o quit >"$LLDB_LOG" 2>&1 || true

kill -TERM "$PID" 2>/dev/null || true
sleep 1
kill -KILL "$PID" 2>/dev/null || true
wait "$PID" 2>/dev/null || true

main_hits="$(rg -c 'frame #0: .*Crystal\$Dmain\$\$Int32_Pointer' "$LLDB_LOG" || true)"
self_loop_hits="$(
  perl -ne '
    if (/^\s*(?:->\s*)?(0x[0-9A-Fa-f]+)[^:]*:\s+b\s+(0x[0-9A-Fa-f]+)/i && lc($1) eq lc($2)) { $c++ }
    END { print 0 + $c }
  ' "$LLDB_LOG"
)"

echo "status: 124"
echo "probe_wait_sec: $PROBE_WAIT_SEC"
echo "run_log: $RUN_LOG"
echo "lldb_log: $LLDB_LOG"
echo "main_frame_hits: ${main_hits:-0}"
echo "self_branch_hits: ${self_loop_hits:-0}"

if [ "${main_hits:-0}" -ge 1 ] && [ "${self_loop_hits:-0}" -ge 1 ]; then
  echo "reproduced (main self-loop in stage2 compiler)"
  exit 0
fi

echo "timeout/hang observed, but signature mismatch"
exit 1
