#!/usr/bin/env bash
# Bootstrap ladder: stage1 = host Crystal builds crystal_v2; stage2+ = previous
# binary builds crystal_v2 again (plain flags only — no --stats in the ladder).
#
# Usage (from repo root):
#   scripts/bootstrap_chain.sh [--stages N] [--host PATH] [--source PATH] [--out DIR]
#       [--timeout SEC] [--mem MB]
#
# Environment (defaults if flags omitted):
#   CRYSTAL_HOST   — host compiler for stage1 (default: crystal)
#   BOOTSTRAP_CHAIN_STAGES / BOOTSTRAP_CHAIN_SOURCE / BOOTSTRAP_CHAIN_OUT
#   BOOTSTRAP_TIMEOUT_SEC — run_safe timeout for stage2+ (default 900)
#   BOOTSTRAP_MEM_MB      — run_safe RSS cap for stage2+ (default 12288)
#   BOOTSTRAP_SMOKE_PLAIN_MEM_MB — RSS cap for plain smoke only (default 8192).
#       Plain smoke uses the full prelude; 1024MB triggers false OOM under run_safe.
#
# Stage1 uses:  host build SOURCE -o OUT/cv2_s1 --error-trace
# Stage2+:      scripts/run_safe.sh PREV timeout mem SOURCE -o OUT/cv2_sN
#
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

STAGES="${BOOTSTRAP_CHAIN_STAGES:-5}"
HOST_CRYSTAL="${CRYSTAL_HOST:-crystal}"
SOURCE_REL="${BOOTSTRAP_CHAIN_SOURCE:-src/crystal_v2.cr}"
OUT_DIR="${BOOTSTRAP_CHAIN_OUT:-}"
TIMEOUT_SEC="${BOOTSTRAP_TIMEOUT_SEC:-900}"
MEM_MB="${BOOTSTRAP_MEM_MB:-12288}"
SMOKE_PLAIN_MEM_MB="${BOOTSTRAP_SMOKE_PLAIN_MEM_MB:-8192}"

usage() {
  cat <<'USAGE'
Bootstrap ladder (stage1 = host Crystal builds crystal_v2; stage2+ = previous
binary builds crystal_v2 again). Plain self-host only — no --stats on the ladder.

Usage:
  scripts/bootstrap_chain.sh [--stages N] [--host PATH] [--source PATH] [--out DIR]
      [--timeout SEC] [--mem MB]

Environment (optional):
  CRYSTAL_HOST          Host compiler for stage1 (default: crystal)
  BOOTSTRAP_CHAIN_STAGES / BOOTSTRAP_CHAIN_SOURCE / BOOTSTRAP_CHAIN_OUT
  BOOTSTRAP_TIMEOUT_SEC run_safe timeout for stage2+ (default: 900)
  BOOTSTRAP_MEM_MB      run_safe RSS cap for stage2+ (default: 12288)
  BOOTSTRAP_SMOKE_PLAIN_MEM_MB  RSS cap for plain smoke only (default: 8192)

Stage1:  host build SOURCE -o OUT/cv2_s1 --error-trace
Stage2+: scripts/run_safe.sh PREV timeout mem SOURCE -o OUT/cv2_sN
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --stages)
      STAGES="$2"
      shift 2
      ;;
    --host)
      HOST_CRYSTAL="$2"
      shift 2
      ;;
    --source)
      SOURCE_REL="$2"
      shift 2
      ;;
    --out)
      OUT_DIR="$2"
      shift 2
      ;;
    --timeout)
      TIMEOUT_SEC="$2"
      shift 2
      ;;
    --mem)
      MEM_MB="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      if [[ "$1" == -* ]]; then
        echo "error: unknown option: $1" >&2
        exit 1
      fi
      HOST_CRYSTAL="$1"
      shift
      ;;
  esac
done

if ! [[ "$STAGES" =~ ^[1-9][0-9]*$ ]]; then
  echo "error: --stages must be a positive integer" >&2
  exit 1
fi

if [[ -z "$OUT_DIR" ]]; then
  OUT_DIR="/tmp/crystal_v2_bootstrap_chain_${USER:-user}_$$"
fi
mkdir -p "$OUT_DIR"

SOURCE_ABS="$REPO_ROOT/$SOURCE_REL"
NO_PRELUDE_ORACLE="$REPO_ROOT/regression_tests/combined/test_no_prelude_interpolation.cr"
SMOKE_SRC="$OUT_DIR/_smoke_puts42.cr"

if [[ ! -f "$SOURCE_ABS" ]]; then
  echo "error: source not found: $SOURCE_ABS" >&2
  exit 1
fi
if [[ ! -f "$NO_PRELUDE_ORACLE" ]]; then
  echo "error: no-prelude oracle not found: $NO_PRELUDE_ORACLE" >&2
  exit 1
fi

cat > "$SMOKE_SRC" <<'EOF'
puts 42
EOF

parse_time_l_real() {
  local logfile="$1"
  # time -l prints "  N.NN real  N.NN user  N.NN sys" after child output
  awk '$2 == "real" && $4 == "user" && $6 == "sys" { v = $1 } END { if (v != "") print v }' "$logfile"
}

parse_time_l_max_rss_bytes() {
  local logfile="$1"
  awk '/maximum resident set size/ { r = $1 } END { print r }' "$logfile"
}

bytes_to_mb() {
  local b="$1"
  if [[ -z "$b" || "$b" == "0" ]]; then
    echo ""
    return
  fi
  awk -v n="$b" 'BEGIN { printf "%.2f", n / 1048576.0 }'
}

# Returns: exit code of build in $1; sets WALL_REAL, PEAK_MB, LOG
run_stage1_host() {
  local out_bin="$1"
  local logfile="$2"
  (
    cd "$REPO_ROOT"
    /usr/bin/time -l "$HOST_CRYSTAL" build "$SOURCE_REL" -o "$out_bin" --error-trace
  ) >"$logfile" 2>&1
}

run_stageN_selfhost() {
  local compiler="$1"
  local out_bin="$2"
  local logfile="$3"
  (
    cd "$REPO_ROOT"
    /usr/bin/time -l "$SCRIPT_DIR/run_safe.sh" "$compiler" "$TIMEOUT_SEC" "$MEM_MB" \
      "$SOURCE_REL" -o "$out_bin"
  ) >"$logfile" 2>&1
}

run_smoke_plain() {
  local compiler="$1"
  local logfile="$2"
  (
    cd "$REPO_ROOT"
    /usr/bin/time -l "$SCRIPT_DIR/run_safe.sh" "$compiler" 60 "$SMOKE_PLAIN_MEM_MB" \
      "$SMOKE_SRC" -o "$OUT_DIR/_smoke_puts42.bin"
  ) >"$logfile" 2>&1
}

run_smoke_noprelude() {
  local compiler="$1"
  local logfile="$2"
  (
    cd "$REPO_ROOT"
    /usr/bin/time -l "$SCRIPT_DIR/run_safe.sh" "$compiler" 120 1024 \
      "$NO_PRELUDE_ORACLE" --no-prelude -o "$OUT_DIR/_smoke_noprel.bin"
  ) >"$logfile" 2>&1
}

stderr_tail() {
  local logfile="$1"
  local n="${2:-80}"
  if [[ -f "$logfile" ]]; then
    echo "--- tail ($n lines): $logfile ---"
    tail -n "$n" "$logfile"
  fi
}

echo "=== Bootstrap chain ==="
echo "repo:       $REPO_ROOT"
echo "stages:     $STAGES"
echo "host (s1):  $HOST_CRYSTAL"
echo "source:     $SOURCE_REL"
echo "out dir:    $OUT_DIR"
echo "stage2+ run_safe: timeout=${TIMEOUT_SEC}s mem=${MEM_MB}MB"
echo "smoke plain run_safe: timeout=60s mem=${SMOKE_PLAIN_MEM_MB}MB (override BOOTSTRAP_SMOKE_PLAIN_MEM_MB)"
echo ""

declare -a ST_OK ST_BIN ST_WALL ST_PEAK ST_SMOKE_P ST_SMOKE_N ST_LOG
FIRST_FAIL=""
FIRST_FAIL_KIND=""
FIRST_SYMPTOM=""

for ((s = 1; s <= STAGES; s++)); do
  OUT_BIN="$OUT_DIR/cv2_s${s}"
  LOG_B="$OUT_DIR/stage${s}_build.log"
  LOG_P="$OUT_DIR/stage${s}_smoke_plain.log"
  LOG_N="$OUT_DIR/stage${s}_smoke_noprelude.log"

  if [[ $s -eq 1 ]]; then
    echo "--- Stage $s (host build) ---"
    run_stage1_host "$OUT_BIN" "$LOG_B"
    RC=$?
    PREV="$OUT_BIN"
  else
    echo "--- Stage $s (self-host via cv2_s$((s - 1))) ---"
    run_stageN_selfhost "$PREV" "$OUT_BIN" "$LOG_B"
    RC=$?
    PREV="$OUT_BIN"
  fi

  WALL_REAL="$(parse_time_l_real "$LOG_B" || true)"
  RSS_B="$(parse_time_l_max_rss_bytes "$LOG_B" || true)"
  PEAK_MB="$(bytes_to_mb "$RSS_B")"

  if [[ $RC -ne 0 ]]; then
    ST_OK+=("fail")
    ST_BIN+=("$OUT_BIN")
    ST_WALL+=("${WALL_REAL:-?}")
    ST_PEAK+=("${PEAK_MB:-?}")
    ST_SMOKE_P+=("-")
    ST_SMOKE_N+=("-")
    ST_LOG+=("$LOG_B")
    if [[ -z "$FIRST_FAIL" ]]; then
      FIRST_FAIL="$s"
      FIRST_FAIL_KIND="build"
      FIRST_SYMPTOM="$(stderr_tail "$LOG_B" 40)"
    fi
    echo "STAGE $s BUILD: FAIL (exit $RC)"
    stderr_tail "$LOG_B" 50
    break
  fi

  if [[ ! -x "$OUT_BIN" ]]; then
    ST_OK+=("fail")
    ST_BIN+=("$OUT_BIN")
    ST_WALL+=("${WALL_REAL:-?}")
    ST_PEAK+=("${PEAK_MB:-?}")
    ST_SMOKE_P+=("-")
    ST_SMOKE_N+=("-")
    ST_LOG+=("$LOG_B")
    if [[ -z "$FIRST_FAIL" ]]; then
      FIRST_FAIL="$s"
      FIRST_FAIL_KIND="missing_binary"
      FIRST_SYMPTOM="expected executable missing: $OUT_BIN"
    fi
    echo "STAGE $s: binary not executable after reported success"
    break
  fi

  echo "STAGE $s BUILD: ok  wall=${WALL_REAL:-?}s  peak_rss≈${PEAK_MB:-?}MB  -> $OUT_BIN"

  SP_OK="ok"
  SN_OK="ok"
  run_smoke_plain "$OUT_BIN" "$LOG_P"
  RCP=$?
  if [[ $RCP -ne 0 ]]; then
    SP_OK="fail"
    if [[ -z "$FIRST_FAIL" ]]; then
      FIRST_FAIL="$s"
      FIRST_FAIL_KIND="smoke_plain"
      FIRST_SYMPTOM="$(stderr_tail "$LOG_P" 40)"
    fi
  fi
  run_smoke_noprelude "$OUT_BIN" "$LOG_N"
  RCN=$?
  if [[ $RCN -ne 0 ]]; then
    SN_OK="fail"
    if [[ -z "$FIRST_FAIL" ]]; then
      FIRST_FAIL="$s"
      FIRST_FAIL_KIND="smoke_noprelude"
      FIRST_SYMPTOM="$(stderr_tail "$LOG_N" 40)"
    fi
  fi

  ST_OK+=("ok")
  ST_BIN+=("$OUT_BIN")
  ST_WALL+=("${WALL_REAL:-?}")
  ST_PEAK+=("${PEAK_MB:-?}")
  ST_SMOKE_P+=("$SP_OK")
  ST_SMOKE_N+=("$SN_OK")
  ST_LOG+=("$LOG_B")

  echo "  smoke plain:     $SP_OK"
  echo "  smoke no-prelude: $SN_OK"
  if [[ "$SP_OK" != "ok" ]]; then
    stderr_tail "$LOG_P" 30
  fi
  if [[ "$SN_OK" != "ok" ]]; then
    stderr_tail "$LOG_N" 30
  fi

  if [[ "$SP_OK" != "ok" || "$SN_OK" != "ok" ]]; then
    break
  fi
done

echo ""
echo "================================================================"
echo "SUMMARY"
echo "================================================================"

printf "%-6s %-8s %-10s %-12s %-12s %-12s %s\n" \
  "Stage" "Build" "Wall(s)" "PeakRSS(MB)" "Smoke+" "SmokeNP" "Binary"
for ((i = 0; i < ${#ST_OK[@]}; i++)); do
  sn=$((i + 1))
  printf "%-6s %-8s %-10s %-12s %-12s %-12s %s\n" \
    "s${sn}" "${ST_OK[$i]}" "${ST_WALL[$i]}" "${ST_PEAK[$i]}" "${ST_SMOKE_P[$i]}" "${ST_SMOKE_N[$i]}" "${ST_BIN[$i]}"
done

echo ""
if [[ -n "$FIRST_FAIL" ]]; then
  echo "Earliest failing stage: $FIRST_FAIL ($FIRST_FAIL_KIND)"
  echo "First symptom:"
  echo "$FIRST_SYMPTOM"
  echo ""
  echo "Logs under: $OUT_DIR"
  exit 1
fi

LAST_IDX=$((${#ST_OK[@]} - 1))
if [[ ${#ST_OK[@]} -eq "$STAGES" ]] && [[ "${ST_OK[$LAST_IDX]:-}" == "ok" ]]; then
  echo "Bootstrap ladder: ALL GREEN through stage ${STAGES} (s${STAGES} landmark)."
  echo "Logs under: $OUT_DIR"
  exit 0
fi

echo "Incomplete ladder (expected $STAGES stages, recorded ${#ST_OK[@]})."
echo "Logs under: $OUT_DIR"
exit 1
