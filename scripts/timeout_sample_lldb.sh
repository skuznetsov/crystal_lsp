#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  scripts/timeout_sample_lldb.sh [options] -- <command> [args...]

Options:
  -t, --timeout SEC       Timeout before sampling (default: 180)
  -s, --sample SEC        sample(1) duration in seconds (default: 10)
  -l, --lldb-timeout SEC  LLDB attach/backtrace timeout (default: 20)
  -n, --top N             Number of hotspot symbols (default: 5)
  -o, --out DIR           Output directory (default: /tmp/timeout_sample_<ts>_<pid>)
  -b, --breakpoints LIST  Comma-separated LLDB breakpoint symbols
      --no-lldb           Skip LLDB attach/backtrace
  -h, --help              Show this help
USAGE
}

TIMEOUT_SECS=180
SAMPLE_SECS=10
LLDB_TIMEOUT_SECS=20
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
    -l|--lldb-timeout)
      LLDB_TIMEOUT_SECS="${2:-}"; shift 2 ;;
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
SAMPLE_CHILD_LOG="$OUT_DIR/sample.child.txt"
HOTSPOT_LOG="$OUT_DIR/hotspots.txt"
LLDB_LOG="$OUT_DIR/lldb.txt"
LLDB_CMDS="$OUT_DIR/lldb.commands"
PROC_LOG="$OUT_DIR/processes.txt"

echo "[run] ${CMD[*]}" | tee "$OUT_DIR/summary.txt"
"${CMD[@]}" >"$CMD_LOG" 2>&1 &
PID=$!
PGID="$(ps -o pgid= -p "$PID" 2>/dev/null | tr -d '[:space:]' || true)"
SELF_PGID="$(ps -o pgid= -p "$$" 2>/dev/null | tr -d '[:space:]' || true)"
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
  set +e
  wait "$PID"
  STATUS=$?
  set -e
  {
    echo "[exit] status=$STATUS"
    echo "[log] $CMD_LOG"
  } | tee -a "$OUT_DIR/summary.txt"
  exit "$STATUS"
fi

echo "[timeout] ${TIMEOUT_SECS}s (pid=$PID)" | tee -a "$OUT_DIR/summary.txt"

collect_descendants() {
  local parent="$1"
  local child=""
  while read -r child; do
    [[ -z "${child:-}" ]] && continue
    echo "$child"
    collect_descendants "$child"
  done < <(pgrep -P "$parent" || true)
}

DESC_PIDS=()
if command -v pgrep >/dev/null 2>&1; then
  while read -r cpid; do
    [[ -n "${cpid:-}" ]] && DESC_PIDS+=("$cpid")
  done < <(collect_descendants "$PID")
fi

SAMPLE_PID="$PID"
if [[ -n "${DESC_PIDS[*]:-}" ]]; then
  while read -r cpid _cpu comm; do
    [[ -z "${cpid:-}" ]] && continue
    if [[ "$comm" != "bash" && "$comm" != "sh" ]] && kill -0 "$cpid" 2>/dev/null; then
      SAMPLE_PID="$cpid"
      break
    fi
  done < <(ps -o pid=,%cpu=,comm= -p "${DESC_PIDS[@]}" 2>/dev/null | sort -k2,2nr)
fi
if [[ "$SAMPLE_PID" == "$PID" && -n "${DESC_PIDS[*]:-}" ]]; then
  while read -r cpid _cpu _comm; do
    [[ -z "${cpid:-}" ]] && continue
    if kill -0 "$cpid" 2>/dev/null; then
      SAMPLE_PID="$cpid"
      break
    fi
  done < <(ps -o pid=,%cpu=,comm= -p "${DESC_PIDS[@]}" 2>/dev/null | sort -k2,2nr)
fi

ps -o pid,ppid,pgid,stat,etime,%cpu,%mem,command -p "$PID" > "$PROC_LOG" 2>/dev/null || true
if [[ -n "${DESC_PIDS[*]:-}" ]]; then
  ps -o pid,ppid,pgid,stat,etime,%cpu,%mem,command -p "${DESC_PIDS[@]}" >> "$PROC_LOG" 2>/dev/null || true
fi

if command -v sample >/dev/null 2>&1; then
  sample "$PID" "$SAMPLE_SECS" -file "$SAMPLE_LOG" >/dev/null 2>&1 || true
  if [[ "$SAMPLE_PID" != "$PID" ]]; then
    sample "$SAMPLE_PID" "$SAMPLE_SECS" -file "$SAMPLE_CHILD_LOG" >/dev/null 2>&1 || true
  fi
else
  echo "sample tool not found" > "$SAMPLE_LOG"
fi

HOTSPOT_SOURCE="$SAMPLE_LOG"
if [[ -s "$SAMPLE_CHILD_LOG" ]]; then
  HOTSPOT_SOURCE="$SAMPLE_CHILD_LOG"
fi

if [[ -s "$HOTSPOT_SOURCE" ]]; then
  awk '
    /\(in / {
      line = $0
      sub(/^[[:space:]]*[0-9]+[[:space:]]+/, "", line)
      gsub(/^[+!|:[:space:]]+/, "", line)
      while (sub(/^[0-9]+[[:space:]]+/, "", line)) {}
      sub(/[[:space:]]+\(in .*/, "", line)
      sub(/^[*~]+/, "", line)
      while (sub(/^[0-9]+[[:space:]]+/, "", line)) {}
      if (length(line) > 0) print line
    }
  ' "$HOTSPOT_SOURCE" \
    | rg -v '^(Thread_|DispatchQueue_|Call|start|main|kevent)$' \
    | sort | uniq -c | sort -nr | head -n "$TOP_N" > "$HOTSPOT_LOG" || true
fi

BP_SYMBOLS=()
if [[ -n "$BREAKPOINTS" ]]; then
  IFS=',' read -r -a BP_SYMBOLS <<< "$BREAKPOINTS"
elif [[ -s "$HOTSPOT_LOG" ]]; then
  while read -r _count sym; do
    sym=$(printf '%s' "$sym" | sed -E 's/^[[:space:]]+//; s/^[0-9]+[[:space:]]+//; s/^[*~]+//')
    [[ -n "${sym:-}" ]] && BP_SYMBOLS+=("$sym")
  done < "$HOTSPOT_LOG"
fi

if (( USE_LLDB == 1 )) && command -v lldb >/dev/null 2>&1 && kill -0 "$SAMPLE_PID" 2>/dev/null; then
  {
    echo "process attach --pid $SAMPLE_PID"
    for sym in "${BP_SYMBOLS[@]}"; do
      qsym="${sym//\\/\\\\}"
      qsym="${qsym//\"/\\\"}"
      echo "breakpoint set --name \"$qsym\""
    done
    echo "thread backtrace all"
    echo "process detach"
    echo "quit"
  } > "$LLDB_CMDS"
  lldb -b -s "$LLDB_CMDS" > "$LLDB_LOG" 2>&1 &
  LLDB_PID=$!
  LLDB_START_TS=$(date +%s)
  LLDB_TIMED_OUT=0
  while kill -0 "$LLDB_PID" 2>/dev/null; do
    NOW_TS=$(date +%s)
    if (( NOW_TS - LLDB_START_TS >= LLDB_TIMEOUT_SECS )); then
      LLDB_TIMED_OUT=1
      kill -TERM "$LLDB_PID" 2>/dev/null || true
      sleep 1
      kill -KILL "$LLDB_PID" 2>/dev/null || true
      break
    fi
    sleep 1
  done
  wait "$LLDB_PID" 2>/dev/null || true
  if (( LLDB_TIMED_OUT == 1 )); then
    echo "[lldb-timeout] ${LLDB_TIMEOUT_SECS}s" >> "$LLDB_LOG"
  fi
fi

if [[ -n "${PGID:-}" && "${PGID:-}" != "${SELF_PGID:-}" ]]; then
  kill -TERM "-$PGID" 2>/dev/null || true
fi
kill "$PID" 2>/dev/null || true
for cpid in "${DESC_PIDS[@]:-}"; do
  kill "$cpid" 2>/dev/null || true
done
sleep 1
if [[ -n "${PGID:-}" && "${PGID:-}" != "${SELF_PGID:-}" ]]; then
  kill -KILL "-$PGID" 2>/dev/null || true
fi
kill -KILL "$PID" 2>/dev/null || true
for cpid in "${DESC_PIDS[@]:-}"; do
  kill -KILL "$cpid" 2>/dev/null || true
done
wait "$PID" 2>/dev/null || true

{
  echo "[killed] pid=$PID"
  if [[ "$SAMPLE_PID" != "$PID" ]]; then
    echo "[sample-pid] $SAMPLE_PID (descendant)"
    echo "[sample-child] $SAMPLE_CHILD_LOG"
  else
    echo "[sample-pid] $SAMPLE_PID (parent)"
  fi
  echo "[log] $CMD_LOG"
  echo "[ps] $PROC_LOG"
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
