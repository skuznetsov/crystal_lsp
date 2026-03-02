#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  scripts/timeout_sample_lldb.sh [options] -- <command> [args...]

Options:
  -t, --timeout SEC       Timeout before sampling (default: 300)
  -m, --memory-limit MB   Kill if process RSS (parent + children) exceeds MB (default: 0, disabled)
      --memory-percent PCT Kill if process RSS exceeds a percentage of total RAM (default: disabled, range 1-100)
      --memory-prewarn-pct PCT  Start early memory diagnostics when RSS exceeds this percentage of limit (disabled, range 1-99)
      --memory-prewarn-lldb-timeout SEC  prewarn LLDB timeout seconds (default: 5)
      --memory-prewarn-sample-secs SEC  prewarn sample duration in seconds (default: --sample)
      --memory-check-sec SEC  Interval between memory checks in seconds (default: 1)
  -s, --sample SEC        sample(1) duration in seconds (default: 10)
  -l, --lldb-timeout SEC  LLDB attach/backtrace timeout (default: 20)
  -n, --top N             Number of hotspot symbols (default: 5)
  -o, --out DIR           Output directory (default: /tmp/timeout_sample_<ts>_<pid>)
      --out-dir DIR       Alias for --out
      --series-start SEC  Periodic sample start offset (default: 30)
      --series-interval SEC  Periodic sample interval (default: 60)
      --series-duration SEC  Periodic sample duration (default: --sample)
      --no-series         Disable periodic pre-timeout sampling
  -b, --breakpoints LIST  Comma-separated LLDB breakpoint symbols
      --no-lldb           Skip LLDB attach/backtrace
  -h, --help              Show this help

Examples:
  scripts/timeout_sample_lldb.sh -m 2048 --memory-prewarn-pct 85 --memory-prewarn-sample-secs 3 --memory-prewarn-lldb-timeout 8 --timeout 30 -- ./build/my_app
  scripts/timeout_sample_lldb.sh --memory-percent 25 --memory-check-sec 2 --memory-prewarn-pct 80 --timeout 120 -- ./gradlew test
USAGE
}

TIMEOUT_SECS=300
MEMORY_LIMIT_MB=0
MEMORY_LIMIT_PCT=0
MEMORY_PREWARN_PCT=0
MEMORY_PREWARN_MB=0
MEMORY_PREWARN_TRIGGERED=0
MEMORY_CHECK_SECS=1
MEMORY_PREWARN_LLDB_TIMEOUT=5
MEMORY_PREWARN_SAMPLE_SECS=0
SAMPLE_SECS=10
LLDB_TIMEOUT_SECS=20
TOP_N=5
OUT_DIR=""
BREAKPOINTS=""
USE_LLDB=1
SERIES_ENABLED=1
SERIES_START_SECS=30
SERIES_INTERVAL_SECS=60
SERIES_SAMPLE_SECS=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    -t|--timeout)
      TIMEOUT_SECS="${2:-}"; shift 2 ;;
    -m|--memory-limit)
      MEMORY_LIMIT_MB="${2:-}"; shift 2 ;;
    --memory-percent)
      MEMORY_LIMIT_PCT="${2:-}"; shift 2 ;;
    --memory-prewarn-pct)
      MEMORY_PREWARN_PCT="${2:-}"; shift 2 ;;
    --memory-prewarn-lldb-timeout)
      MEMORY_PREWARN_LLDB_TIMEOUT="${2:-}"; shift 2 ;;
    --memory-prewarn-sample-secs)
      MEMORY_PREWARN_SAMPLE_SECS="${2:-}"; shift 2 ;;
    --memory-check-sec)
      MEMORY_CHECK_SECS="${2:-}"; shift 2 ;;
    -s|--sample)
      SAMPLE_SECS="${2:-}"; shift 2 ;;
    -l|--lldb-timeout)
      LLDB_TIMEOUT_SECS="${2:-}"; shift 2 ;;
    -n|--top)
      TOP_N="${2:-}"; shift 2 ;;
    -o|--out|--out-dir)
      OUT_DIR="${2:-}"; shift 2 ;;
    --series-start)
      SERIES_START_SECS="${2:-}"; shift 2 ;;
    --series-interval)
      SERIES_INTERVAL_SECS="${2:-}"; shift 2 ;;
    --series-duration)
      SERIES_SAMPLE_SECS="${2:-}"; shift 2 ;;
    --no-series)
      SERIES_ENABLED=0; shift ;;
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
if (( SERIES_SAMPLE_SECS <= 0 )); then
  SERIES_SAMPLE_SECS="$SAMPLE_SECS"
fi
MEMORY_LIMIT_MODE="disabled"

if (( MEMORY_LIMIT_MB > 0 && MEMORY_LIMIT_PCT > 0 )); then
  echo "[error] use either --memory-limit or --memory-percent, not both" >&2
  usage >&2
  exit 2
fi

if (( MEMORY_LIMIT_PCT > 0 )); then
  if (( MEMORY_LIMIT_PCT < 1 || MEMORY_LIMIT_PCT > 100 )); then
    echo "[error] --memory-percent must be in 1..100" >&2
    usage >&2
    exit 2
  fi
  get_total_ram_mb() {
    if command -v sysctl >/dev/null 2>&1; then
      local mem_bytes
      mem_bytes="$(sysctl -n hw.memsize 2>/dev/null || true)"
      if [[ -n "${mem_bytes:-}" ]]; then
        printf '%s\n' "$((mem_bytes / 1024 / 1024))"
        return 0
      fi
    fi

    if [[ -r /proc/meminfo ]]; then
      awk '/^MemTotal:/ { print int($2 / 1024); exit }' /proc/meminfo 2>/dev/null || true
      return 0
    fi

    return 1
  }

  TOTAL_RAM_MB="$(get_total_ram_mb || true)"
  if [[ -z "${TOTAL_RAM_MB:-}" || "${TOTAL_RAM_MB}" = "0" ]]; then
    echo "[error] failed to detect physical RAM for --memory-percent" >&2
    usage >&2
    exit 2
  fi

  MEMORY_LIMIT_MB=$((TOTAL_RAM_MB * MEMORY_LIMIT_PCT / 100))
  if (( MEMORY_LIMIT_MB <= 0 )); then
    echo "[error] invalid computed memory limit" >&2
    exit 2
  fi
  MEMORY_LIMIT_MODE="percent"
fi

if (( MEMORY_LIMIT_MB > 0 && MEMORY_LIMIT_PCT == 0 )); then
  MEMORY_LIMIT_MODE="fixed"
fi

if (( MEMORY_PREWARN_PCT > 0 )); then
  if [[ "$MEMORY_LIMIT_MODE" == "disabled" ]]; then
    echo "[error] --memory-prewarn-pct requires either --memory-limit or --memory-percent" >&2
    usage >&2
    exit 2
  fi
  if (( MEMORY_PREWARN_PCT < 1 || MEMORY_PREWARN_PCT > 99 )); then
    echo "[error] --memory-prewarn-pct must be in 1..99" >&2
    usage >&2
    exit 2
  fi
  MEMORY_PREWARN_MB=$((MEMORY_LIMIT_MB * MEMORY_PREWARN_PCT / 100))
  if (( MEMORY_PREWARN_MB <= 0 )); then
    MEMORY_PREWARN_MB=1
  fi
fi

if (( MEMORY_PREWARN_LLDB_TIMEOUT < 0 )); then
  echo "[error] --memory-prewarn-lldb-timeout must be >= 0" >&2
  usage >&2
  exit 2
fi
if (( MEMORY_PREWARN_SAMPLE_SECS < 0 )); then
  echo "[error] --memory-prewarn-sample-secs must be >= 0" >&2
  usage >&2
  exit 2
fi
if (( MEMORY_PREWARN_SAMPLE_SECS <= 0 )); then
  MEMORY_PREWARN_SAMPLE_SECS="$SAMPLE_SECS"
fi

CMD=("$@")
CMD_LOG="$OUT_DIR/command.log"
SAMPLE_LOG="$OUT_DIR/sample.txt"
SAMPLE_CHILD_LOG="$OUT_DIR/sample.child.txt"
MEMORY_PREWARN_SAMPLE="$OUT_DIR/memory_prewarn.sample.txt"
MEMORY_PREWARN_LLDB_CMD="$OUT_DIR/memory_prewarn.lldb.commands"
MEMORY_PREWARN_LLDB_LOG="$OUT_DIR/memory_prewarn.lldb.txt"
HOTSPOT_LOG="$OUT_DIR/hotspots.txt"
LLDB_LOG="$OUT_DIR/lldb.txt"
LLDB_CMDS="$OUT_DIR/lldb.commands"
PROC_LOG="$OUT_DIR/processes.txt"
SERIES_DIR="$OUT_DIR/sample_series"
SERIES_INDEX=0

echo "[run] ${CMD[*]}" | tee "$OUT_DIR/summary.txt"
"${CMD[@]}" >"$CMD_LOG" 2>&1 &
PID=$!
PGID="$(ps -o pgid= -p "$PID" 2>/dev/null | tr -d '[:space:]' || true)"
SELF_PGID="$(ps -o pgid= -p "$$" 2>/dev/null | tr -d '[:space:]' || true)"
START_TS=$(date +%s)
if (( SERIES_ENABLED == 1 )); then
  mkdir -p "$SERIES_DIR"
  NEXT_SERIES_TS=$((START_TS + SERIES_START_SECS))
else
  NEXT_SERIES_TS=0
fi
TIMED_OUT=0
MEMORY_LIMIT_EXCEEDED=0
MEMORY_BYTES=0
LAST_MEMORY_CHECK_TS=0
EXIT_CODE=124

collect_descendants_pids() {
  local parent="$1"
  local child=""
  while read -r child; do
    [[ -z "${child:-}" ]] && continue
    echo "$child"
    collect_descendants_pids "$child"
  done < <(pgrep -P "$parent" || true)
}

total_rss_kb() {
  local root_pid="$1"
  local -a pids=("$root_pid")
  local child=""
  while read -r child; do
    [[ -z "${child:-}" ]] && continue
    pids+=("$child")
  done < <(collect_descendants_pids "$root_pid")

  local total=0
  for child in "${pids[@]}"; do
    if kill -0 "$child" 2>/dev/null; then
      local rss
      rss="$(ps -o rss= -p "$child" 2>/dev/null | tr -d '[:space:]' || true)"
      [[ -z "$rss" ]] && rss=0
      total=$((total + rss))
    fi
  done
  printf '%s' "$total"
}

collect_descendants_live() {
  local parent="$1"
  local child=""
  while read -r child; do
    [[ -z "${child:-}" ]] && continue
    echo "$child"
    collect_descendants_live "$child"
  done < <(pgrep -P "$parent" || true)
}

run_periodic_sample() {
  local now_ts="$1"
  local sample_pid="$PID"
  local desc=()

  if command -v pgrep >/dev/null 2>&1; then
    while read -r cpid; do
      [[ -n "${cpid:-}" ]] && desc+=("$cpid")
    done < <(collect_descendants_live "$PID")
  fi

  if [[ -n "${desc[*]:-}" ]]; then
    while read -r cpid _cpu comm; do
      [[ -z "${cpid:-}" ]] && continue
      if [[ "$comm" != "bash" && "$comm" != "sh" ]] && kill -0 "$cpid" 2>/dev/null; then
        sample_pid="$cpid"
        break
      fi
    done < <(ps -o pid=,%cpu=,comm= -p "${desc[@]}" 2>/dev/null | sort -k2,2nr)
  fi

  if ! kill -0 "$sample_pid" 2>/dev/null; then
    return
  fi

  local sample_file="$SERIES_DIR/sample.${SERIES_INDEX}.txt"
  local hotspot_file="$SERIES_DIR/hotspots.${SERIES_INDEX}.txt"
  sample "$sample_pid" "$SERIES_SAMPLE_SECS" -file "$sample_file" >/dev/null 2>&1 || true
  if [[ -s "$sample_file" ]]; then
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
    ' "$sample_file" \
      | rg -v '^(Thread_|DispatchQueue_|Call|start|main|kevent)$' \
      | sort | uniq -c | sort -nr | head -n "$TOP_N" > "$hotspot_file" || true
    echo "[series] idx=${SERIES_INDEX} t=$((now_ts - START_TS))s pid=${sample_pid} sample=${sample_file}" >> "$OUT_DIR/summary.txt"
  else
    echo "[series] idx=${SERIES_INDEX} t=$((now_ts - START_TS))s pid=${sample_pid} sample_failed" >> "$OUT_DIR/summary.txt"
  fi
  SERIES_INDEX=$((SERIES_INDEX + 1))
}

while kill -0 "$PID" 2>/dev/null; do
  NOW_TS=$(date +%s)
  if (( SERIES_ENABLED == 1 )) && (( NOW_TS >= NEXT_SERIES_TS )); then
    run_periodic_sample "$NOW_TS"
    NEXT_SERIES_TS=$((NOW_TS + SERIES_INTERVAL_SECS))
  fi
  if (( MEMORY_CHECK_SECS > 0 && MEMORY_LIMIT_MB > 0 )); then
    if (( NOW_TS - LAST_MEMORY_CHECK_TS >= MEMORY_CHECK_SECS )); then
      MEMORY_BYTES=$(total_rss_kb "$PID")
  if (( MEMORY_PREWARN_MB > 0 && MEMORY_PREWARN_TRIGGERED == 0 )) && (( MEMORY_BYTES >= MEMORY_PREWARN_MB * 1024 )); then
        MEMORY_PREWARN_TRIGGERED=1
        if command -v sample >/dev/null 2>&1; then
          sample "$PID" "$MEMORY_PREWARN_SAMPLE_SECS" -file "$MEMORY_PREWARN_SAMPLE" >/dev/null 2>&1 || true
        fi
        if command -v lldb >/dev/null 2>&1; then
          {
            echo "process attach --pid $PID"
            echo "thread backtrace all"
            echo "process detach"
            echo "quit"
          } > "$MEMORY_PREWARN_LLDB_CMD"
          lldb -b -s "$MEMORY_PREWARN_LLDB_CMD" > "$MEMORY_PREWARN_LLDB_LOG" 2>&1 &
          PREWARN_LC_PID=$!
          PREWARN_LC_TS=$(date +%s)
          while kill -0 "$PREWARN_LC_PID" 2>/dev/null; do
            PREWARN_NOW_TS=$(date +%s)
            if (( PREWARN_NOW_TS - PREWARN_LC_TS >= MEMORY_PREWARN_LLDB_TIMEOUT )); then
              kill -TERM "$PREWARN_LC_PID" 2>/dev/null || true
              sleep 1
              kill -KILL "$PREWARN_LC_PID" 2>/dev/null || true
              break
            fi
            sleep 1
          done
          wait "$PREWARN_LC_PID" 2>/dev/null || true
        fi
        PREWARN_MB=$((MEMORY_BYTES / 1024))
        echo "[memory-prewarn] threshold=${MEMORY_PREWARN_MB}MB pct=${MEMORY_PREWARN_PCT}% used=${PREWARN_MB}MB sample=$MEMORY_PREWARN_SAMPLE lldb=$MEMORY_PREWARN_LLDB_LOG" | tee -a "$OUT_DIR/summary.txt"
      fi
      if (( MEMORY_BYTES >= MEMORY_LIMIT_MB * 1024 )); then
        MEMORY_LIMIT_EXCEEDED=1
        break
      fi
      LAST_MEMORY_CHECK_TS=$NOW_TS
    fi
  fi
  if (( NOW_TS - START_TS >= TIMEOUT_SECS )); then
    TIMED_OUT=1
    break
  fi
  sleep 1
done

if (( TIMED_OUT == 0 && MEMORY_LIMIT_EXCEEDED == 0 )); then
  set +e
  wait "$PID"
  STATUS=$?
  set -e
  {
    echo "[exit] status=$STATUS"
    echo "[log] $CMD_LOG"
    if (( SERIES_ENABLED == 1 && SERIES_INDEX > 0 )); then
      echo "[series] dir=$SERIES_DIR count=$SERIES_INDEX"
    fi
  } | tee -a "$OUT_DIR/summary.txt"
  exit "$STATUS"
fi

if (( MEMORY_LIMIT_EXCEEDED == 1 )); then
  EXIT_CODE=125
  echo "[memory-limit] threshold=${MEMORY_LIMIT_MB}MB exceeded" | tee -a "$OUT_DIR/summary.txt"
  echo "[memory-source] mode=${MEMORY_LIMIT_MODE}" | tee -a "$OUT_DIR/summary.txt"
  LIMIT_MB=$((MEMORY_BYTES / 1024))
  echo "[memory-kill] pid=$PID used=${LIMIT_MB}MB limit=${MEMORY_LIMIT_MB}MB" | tee -a "$OUT_DIR/summary.txt"
  if (( MEMORY_PREWARN_TRIGGERED == 1 )); then
    echo "[memory-prewarn] triggered=1" | tee -a "$OUT_DIR/summary.txt"
  fi
else
  echo "[timeout] ${TIMEOUT_SECS}s (pid=$PID)" | tee -a "$OUT_DIR/summary.txt"
fi

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

SAMPLE_TARGETS=()
add_sample_target() {
  local target="$1"
  local existing=""
  [[ -z "${target:-}" ]] && return
  for existing in "${SAMPLE_TARGETS[@]:-}"; do
    [[ "$existing" == "$target" ]] && return
  done
  SAMPLE_TARGETS+=("$target")
}

add_sample_target "$SAMPLE_PID"
add_sample_target "$PID"
if [[ -n "${DESC_PIDS[*]:-}" ]]; then
  while read -r cpid _cpu _comm; do
    [[ -z "${cpid:-}" ]] && continue
    add_sample_target "$cpid"
  done < <(ps -o pid=,%cpu=,comm= -p "${DESC_PIDS[@]}" 2>/dev/null | sort -k2,2nr)
fi

ps -o pid,ppid,pgid,stat,etime,%cpu,%mem,command -p "$PID" > "$PROC_LOG" 2>/dev/null || true
if [[ -n "${DESC_PIDS[*]:-}" ]]; then
  ps -o pid,ppid,pgid,stat,etime,%cpu,%mem,command -p "${DESC_PIDS[@]}" >> "$PROC_LOG" 2>/dev/null || true
fi

if command -v sample >/dev/null 2>&1; then
  rm -f "$SAMPLE_LOG" "$SAMPLE_CHILD_LOG"
  SAMPLED_PID=""
  for target in "${SAMPLE_TARGETS[@]:-}"; do
    [[ -z "${target:-}" ]] && continue
    if ! kill -0 "$target" 2>/dev/null; then
      continue
    fi
    if [[ "$target" == "$PID" ]]; then
      sample "$target" "$SAMPLE_SECS" -file "$SAMPLE_LOG" >/dev/null 2>&1 || true
      if [[ -s "$SAMPLE_LOG" ]]; then
        SAMPLED_PID="$target"
        SAMPLE_PID="$target"
        break
      fi
    else
      sample "$target" "$SAMPLE_SECS" -file "$SAMPLE_CHILD_LOG" >/dev/null 2>&1 || true
      if [[ -s "$SAMPLE_CHILD_LOG" ]]; then
        SAMPLED_PID="$target"
        SAMPLE_PID="$target"
        break
      fi
    fi
  done
  if [[ -z "$SAMPLED_PID" ]]; then
    SAMPLE_PID="$PID"
    echo "sample failed for candidate pids: ${SAMPLE_TARGETS[*]:-none}" > "$SAMPLE_LOG"
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
  if (( SERIES_ENABLED == 1 && SERIES_INDEX > 0 )); then
    echo "[series] dir=$SERIES_DIR count=$SERIES_INDEX"
  fi
} | tee -a "$OUT_DIR/summary.txt"

if [[ -s "$HOTSPOT_LOG" ]]; then
  echo "Top symbols:"
  cat "$HOTSPOT_LOG"
fi

exit "$EXIT_CODE"
