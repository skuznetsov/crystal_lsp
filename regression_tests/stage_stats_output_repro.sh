#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d /tmp/stage_stats_output.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
OUT_STATS="$TMP_DIR/stats.out"
ERR_STATS="$TMP_DIR/stats.err"
OUT_VERBOSE="$TMP_DIR/stats_verbose.out"
ERR_VERBOSE="$TMP_DIR/stats_verbose.err"
trap 'rm -rf "$TMP_DIR"' EXIT

cat >"$SRC" <<'CR'
puts 1 + 2
CR

set +e
CRYSTAL_V2_PIPELINE_CACHE=0 "$COMPILER" "$SRC" -s --no-link -o "$BIN" >"$OUT_STATS" 2>"$ERR_STATS"
status_stats=$?
CRYSTAL_V2_PIPELINE_CACHE=0 "$COMPILER" "$SRC" -s --verbose --no-link -o "$BIN" >"$OUT_VERBOSE" 2>"$ERR_VERBOSE"
status_verbose=$?
set -e

if [[ $status_stats -ne 0 ]]; then
  echo "failed: non-verbose -s run exited with $status_stats"
  echo "--- stderr ---"
  cat "$ERR_STATS"
  exit 1
fi

if [[ $status_verbose -ne 0 ]]; then
  echo "failed: verbose -s run exited with $status_verbose"
  echo "--- stderr ---"
  cat "$ERR_VERBOSE"
  exit 1
fi

for stage in 1 2 3 4 5 6; do
  if ! grep -q "^Stage ${stage}/6 " "$OUT_STATS"; then
    echo "failed: missing Stage ${stage}/6 line in -s output"
    echo "--- stdout ---"
    cat "$OUT_STATS"
    exit 1
  fi
  if ! grep -q "^Stage ${stage}/6 " "$OUT_VERBOSE"; then
    echo "failed: missing Stage ${stage}/6 line in -s --verbose output"
    echo "--- stdout ---"
    cat "$OUT_VERBOSE"
    exit 1
  fi
done

if grep -q "elapsed=" "$OUT_STATS"; then
  echo "failed: -s output unexpectedly contains verbose elapsed details"
  echo "--- stdout ---"
  cat "$OUT_STATS"
  exit 1
fi

if ! grep -q "^Stage 1/6 parse .*elapsed=" "$OUT_VERBOSE"; then
  echo "failed: verbose parse stage line does not include elapsed details"
  echo "--- stdout ---"
  cat "$OUT_VERBOSE"
  exit 1
fi

if ! grep -q "^Stage 4/6 mir .*lower=" "$OUT_VERBOSE"; then
  echo "failed: verbose MIR stage line does not include lower/opt details"
  echo "--- stdout ---"
  cat "$OUT_VERBOSE"
  exit 1
fi

if ! grep -q "^Stage 6/6 compile .*opt=" "$OUT_VERBOSE"; then
  echo "failed: verbose compile stage line does not include subphase details"
  echo "--- stdout ---"
  cat "$OUT_VERBOSE"
  exit 1
fi

if ! grep -q "^Timing (ms): " "$OUT_STATS"; then
  echo "failed: final timing summary missing in -s output"
  echo "--- stdout ---"
  cat "$OUT_STATS"
  exit 1
fi

if ! grep -q "^Timing (ms): " "$OUT_VERBOSE"; then
  echo "failed: final timing summary missing in -s --verbose output"
  echo "--- stdout ---"
  cat "$OUT_VERBOSE"
  exit 1
fi

echo "ok: stage timing output is present for -s and expanded for --verbose"
echo "compiler: $COMPILER"
