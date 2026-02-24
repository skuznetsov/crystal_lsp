#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  scripts/timeout_sample_lldb.sh [options] -- <command> [args...]

Options:
  -t, --timeout SEC       Timeout before sampling (default: 180)
  -s, --sample SEC        sample(1) duration in seconds (default: 10)
  -n, --top N             Number of hotspot symbols (default: 5)
  -o, --out DIR           Output directory (default: /tmp/timeout_sample_<ts>_<pid>)
  -b, --breakpoints LIST  Comma-separated LLDB breakpoint symbols
      --no-lldb           Skip LLDB attach/backtrace
  -h, --help              Show this help
USAGE
}

TIMEOUT_SECS=180
SAMPLE_SECS=10
TOP_N=5
OUT_DIR=""
BREAKPOINTS=""
USE_LLDB=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    -t|--timeout)
      TIMEOUT_SECS="${2:-}"; shift 2 ;;
    -s|--sample)
      SAMPLE_SECS="${2:-}"; shift 2 ;;
    -n|--top)
      TOP_N="${2:-}"; shift 2 ;;
    -o|--out)
      OUT_DIR="${2:-}"; shift 2 ;;
    -b|--breakpoints)
      BREAKPOINTS="${2:-}"; shift 2 ;;
    --no-lldb)
      USE_LLDB=0; shift ;;
    -h|--help)
      usage; exit 0 ;;
    --)
      shift
      break ;;
    *)
      echo "unknown option: $1" >&2
      usage >&2
      exit 2 ;;
  esac
done

if [[ $# -eq 0 ]]; then
  usage >&2
  exit 2
fi

if [[ -z "$OUT_DIR" ]]; then
  OUT_DIR="/tmp/timeout_sample_$(date +%Y%m%d_%H%M%S)_$$"
fi
mkdir -p "$OUT_DIR"

CMD=("$@")
CMD_LOG="$OUT_DIR/command.log"
SAMPLE_LOG="$OUT_DIR/sample.txt"
HOTSPOT_LOG="$OUT_DIR/hotspots.txt"
LLDB_LOG="$OUT_DIR/lldb.txt"
LLDB_CMDS="$OUT_DIR/lldb.commands"

echo "[run] ${CMD[*]}" | tee "$OUT_DIR/summary.txt"
"${CMD[@]}" >"$CMD_LOG" 2>&1 &
PID=$!
START_TS=$(date +%s)
TIMED_OUT=0

while kill -0 "$PID" 2>/dev/null; do
  NOW_TS=$(date +%s)
  if (( NOW_TS - START_TS >= TIMEOUT_SECS )); then
    TIMED_OUT=1
    break
  fi
  sleep 1
done

if (( TIMED_OUT == 0 )); then
  wait "$PID"
  STATUS=$?
  {
    echo "[exit] status=$STATUS"
    echo "[log] $CMD_LOG"
  } | tee -a "$OUT_DIR/summary.txt"
  exit "$STATUS"
fi

echo "[timeout] ${TIMEOUT_SECS}s (pid=$PID)" | tee -a "$OUT_DIR/summary.txt"

if command -v sample >/dev/null 2>&1; then
  sample "$PID" "$SAMPLE_SECS" -file "$SAMPLE_LOG" >/dev/null 2>&1 || true
else
  echo "sample tool not found" > "$SAMPLE_LOG"
fi

if [[ -s "$SAMPLE_LOG" ]]; then
  grep -Eo '[A-Za-z_][A-Za-z0-9_:$#.<>\-]+' "$SAMPLE_LOG" \
    | rg -v '^(Thread|Dispatch|kernel|libsystem|sample|All|Total|self|start|main)$' \
    | sort | uniq -c | sort -nr | head -n "$TOP_N" > "$HOTSPOT_LOG" || true
fi

BP_SYMBOLS=()
if [[ -n "$BREAKPOINTS" ]]; then
  IFS=',' read -r -a BP_SYMBOLS <<< "$BREAKPOINTS"
elif [[ -s "$HOTSPOT_LOG" ]]; then
  while read -r _count sym; do
    [[ -n "${sym:-}" ]] && BP_SYMBOLS+=("$sym")
  done < "$HOTSPOT_LOG"
fi

if (( USE_LLDB == 1 )) && command -v lldb >/dev/null 2>&1 && kill -0 "$PID" 2>/dev/null; then
  {
    echo "process attach --pid $PID"
    for sym in "${BP_SYMBOLS[@]}"; do
      echo "breakpoint set --name $sym"
    done
    echo "thread backtrace all"
    echo "process detach"
    echo "quit"
  } > "$LLDB_CMDS"
  lldb -b -s "$LLDB_CMDS" > "$LLDB_LOG" 2>&1 || true
fi

kill "$PID" 2>/dev/null || true
wait "$PID" 2>/dev/null || true

{
  echo "[killed] pid=$PID"
  echo "[log] $CMD_LOG"
  echo "[sample] $SAMPLE_LOG"
  if [[ -s "$HOTSPOT_LOG" ]]; then
    echo "[hotspots] $HOTSPOT_LOG"
  fi
  if [[ -f "$LLDB_LOG" ]]; then
    echo "[lldb] $LLDB_LOG"
  fi
} | tee -a "$OUT_DIR/summary.txt"

if [[ -s "$HOTSPOT_LOG" ]]; then
  echo "Top symbols:"
  cat "$HOTSPOT_LOG"
fi

exit 124
