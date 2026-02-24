#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2-compiler>" >&2
  exit 2
fi

STAGE2_COMPILER=$1
LOG_FILE="${TMPDIR:-/tmp}/stage2_parse_prelude_nocodegen_repro.log"
OUTPUT_BIN="${TMPDIR:-/tmp}/stage2_parse_prelude_nocodegen_repro_bin"

set +e
/usr/bin/time -p "$STAGE2_COMPILER" --no-codegen --no-prelude src/stdlib/prelude.cr -o "$OUTPUT_BIN" >"$LOG_FILE" 2>&1
status=$?
set -e

if [ $status -ne 0 ]; then
  echo "stage2 parse/no-codegen prelude repro failed (exit $status)"
  echo "log: $LOG_FILE"
  tail -n 80 "$LOG_FILE"
  exit $status
fi

echo "stage2 parse/no-codegen prelude repro: PASS (output: $OUTPUT_BIN)"
