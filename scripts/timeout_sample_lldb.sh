#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  scripts/timeout_sample_lldb.sh [options] -- <command> [args...]

Options:
  --timeout SEC       Timeout before sampling (default: 180)
  --sample SEC        sample(1) duration in seconds (default: 3)
  --top N             Number of hot symbols to convert into lldb breakpoints (default: 8)
  --out-dir DIR       Output directory (default: /tmp/stage2_hang_probe_<timestamp>)
  --keep-running      Do not kill target process after probe
  --help              Show this help

Example:
  scripts/timeout_sample_lldb.sh --timeout 180 --sample 2 --top 10 -- \
    /tmp/stage2_rel_current --release src/crystal_v2.cr -o /tmp/stage2.bin
USAGE
}

TIMEOUT_SEC=180
SAMPLE_SEC=3
TOP_N=8
KEEP_RUNNING=0
OUT_DIR=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --timeout)
      TIMEOUT_SEC="${2:-}"
      shift 2
      ;;
    --sample)
      SAMPLE_SEC="${2:-}"
      shift 2
      ;;
    --top)
      TOP_N="${2:-}"
      shift 2
      ;;
    --out-dir)
      OUT_DIR="${2:-}"
      shift 2
      ;;
    --keep-running)
      KEEP_RUNNING=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ $# -eq 0 ]]; then
  usage >&2
  exit 2
fi

if ! [[ "$TIMEOUT_SEC" =~ ^[0-9]+$ ]] || ! [[ "$SAMPLE_SEC" =~ ^[0-9]+$ ]] || ! [[ "$TOP_N" =~ ^[0-9]+$ ]]; then
  echo "timeout/sample/top must be non-negative integers" >&2
  exit 2
fi

if [[ -z "$OUT_DIR" ]]; then
  OUT_DIR="/tmp/stage2_hang_probe_$(date +%Y%m%d_%H%M%S)"
fi
mkdir -p "$OUT_DIR"

if ! command -v sample >/dev/null 2>&1; then
  echo "sample command not found (macOS required)" >&2
  exit 1
fi
if ! command -v lldb >/dev/null 2>&1; then
  echo "lldb command not found" >&2
  exit 1
fi

CMD=("$@")
CMD_PRINT="$(printf '%q ' "${CMD[@]}")"
echo "$CMD_PRINT" > "$OUT_DIR/command.txt"

STDOUT_LOG="$OUT_DIR/stdout.log"
STDERR_LOG="$OUT_DIR/stderr.log"
SAMPLE_LOG="$OUT_DIR/sample.txt"
SAMPLE_CMD_LOG="$OUT_DIR/sample_cmd.log"
HOTSPOT_RAW="$OUT_DIR/hotspots_raw.txt"
HOTSPOT_LIST="$OUT_DIR/hotspots.txt"
LLDB_CMDS="$OUT_DIR/lldb_cmds.txt"
LLDB_LOG="$OUT_DIR/lldb.log"
SUMMARY="$OUT_DIR/summary.txt"

echo "[probe] command: $CMD_PRINT"
echo "[probe] out_dir: $OUT_DIR"

"${CMD[@]}" >"$STDOUT_LOG" 2>"$STDERR_LOG" &
PID=$!
START_TS=$(date +%s)
TIMED_OUT=0
STATUS=0

while kill -0 "$PID" 2>/dev/null; do
  NOW_TS=$(date +%s)
  ELAPSED=$((NOW_TS - START_TS))
  if (( ELAPSED >= TIMEOUT_SEC )); then
    TIMED_OUT=1
    break
  fi
  sleep 1
done

if (( TIMED_OUT == 0 )); then
  wait "$PID" || STATUS=$?
  {
    echo "status=completed"
    echo "exit_code=$STATUS"
    echo "elapsed_sec=$(( $(date +%s) - START_TS ))"
  } > "$SUMMARY"
  echo "[probe] process completed before timeout (exit=$STATUS)"
  echo "[probe] logs: $STDOUT_LOG $STDERR_LOG"
  echo "[probe] summary: $SUMMARY"
  exit "$STATUS"
fi

echo "[probe] timeout reached (${TIMEOUT_SEC}s), collecting sample..."
sample "$PID" "$SAMPLE_SEC" -file "$SAMPLE_LOG" >"$SAMPLE_CMD_LOG" 2>&1 || true

awk '
  /^Sort by top of stack, same collapsed/ {in_section = 1; next}
  /^Binary Images:/ {in_section = 0}
  in_section {
    line = $0
    sub(/^[[:space:]]+/, "", line)
    if (line ~ /  \(in [^)]+\)[[:space:]]+[0-9]+[[:space:]]*$/) {
      count = line
      sub(/^.*\)[[:space:]]+/, "", count)
      sym = line
      sub(/[[:space:]]+\(in [^)]+\)[[:space:]]+[0-9]+[[:space:]]*$/, "", sym)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", sym)
      if (sym != "" && count ~ /^[0-9]+$/) print count "\t" sym
    }
  }
' "$SAMPLE_LOG" | sort -rn -k1,1 | head -n "$TOP_N" > "$HOTSPOT_RAW"

cut -f2 "$HOTSPOT_RAW" > "$HOTSPOT_LIST" || true

{
  echo "settings set target.process.stop-on-exec false"
  while IFS= read -r sym; do
    [[ -z "$sym" ]] && continue
    esc="${sym//\"/\\\"}"
    echo "breakpoint set --name \"$esc\""
  done < "$HOTSPOT_LIST"
  echo "process interrupt"
  echo "thread backtrace all"
  echo "breakpoint list"
  echo "detach"
  echo "quit"
} > "$LLDB_CMDS"

lldb -p "$PID" --batch -s "$LLDB_CMDS" >"$LLDB_LOG" 2>&1 || true

if (( KEEP_RUNNING == 0 )); then
  kill "$PID" 2>/dev/null || true
  wait "$PID" 2>/dev/null || true
fi

{
  echo "status=timed_out"
  echo "pid=$PID"
  echo "timeout_sec=$TIMEOUT_SEC"
  echo "sample_sec=$SAMPLE_SEC"
  echo "top_n=$TOP_N"
  echo "kept_running=$KEEP_RUNNING"
  echo "out_dir=$OUT_DIR"
  echo "sample_log=$SAMPLE_LOG"
  echo "hotspots=$HOTSPOT_RAW"
  echo "lldb_log=$LLDB_LOG"
} > "$SUMMARY"

echo "[probe] timeout probe completed"
echo "[probe] sample:  $SAMPLE_LOG"
echo "[probe] hotspots:"
if [[ -s "$HOTSPOT_RAW" ]]; then
  cat "$HOTSPOT_RAW"
else
  echo "  (no hotspots parsed)"
fi
echo "[probe] lldb log: $LLDB_LOG"
echo "[probe] summary:  $SUMMARY"

exit 124
