#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2-compiler>" >&2
  exit 2
fi

STAGE2_COMPILER=$1
LOG_FILE="${TMPDIR:-/tmp}/stage2_bootstrap_repro.log"
OUTPUT_BIN="${TMPDIR:-/tmp}/stage2_bootstrap_repro_bin"

TARGET="${2:-regression_tests/basic_sanity.cr}"

set +e
/usr/bin/time -p "$STAGE2_COMPILER" --release "$TARGET" -o "$OUTPUT_BIN" >"$LOG_FILE" 2>&1
status=$?
set -e

if [ $status -ne 0 ]; then
  echo "stage2 bootstrap repro failed (exit $status) for target $TARGET"
  echo "log: $LOG_FILE"
  tail -n 80 "$LOG_FILE"
  exit $status
fi

echo "stage2 bootstrap repro: PASS (compiler output: $OUTPUT_BIN)"
