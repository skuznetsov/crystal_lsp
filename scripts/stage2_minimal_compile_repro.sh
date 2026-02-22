#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2-compiler> [source.cr]" >&2
  echo "If source is omitted, uses an inline minimal repro: puts 1" >&2
  exit 2
fi

STAGE2_COMPILER=$1
TMP_ROOT="${TMPDIR:-/tmp}"
TMP_SRC="$TMP_ROOT/stage2_minimal_repro.cr"
SOURCE_FILE="${2:-$TMP_SRC}"
OUTPUT_BIN="${TMP_ROOT}/stage2_minimal_repro_bin"
LOG_FILE="${TMP_ROOT}/stage2_minimal_repro.log"
SOURCE_WAS_DEFAULT=0
if [ "$SOURCE_FILE" = "$TMP_SRC" ]; then
  SOURCE_WAS_DEFAULT=1
fi

if [ ! -f "$SOURCE_FILE" ]; then
  if [ "$SOURCE_WAS_DEFAULT" -eq 0 ]; then
    echo "Source file not found: $SOURCE_FILE" >&2
    exit 2
  fi
  cat > "$TMP_SRC" <<'CR'
puts 1
CR
fi

echo "Repro source: $SOURCE_FILE"

set +e
/usr/bin/time -p "$STAGE2_COMPILER" --release "$SOURCE_FILE" -o "$OUTPUT_BIN" >"$LOG_FILE" 2>&1
status=$?
set -e

if [ $status -ne 0 ]; then
  echo "stage2 minimal compile repro failed (exit $status)."
  echo "compiler: $STAGE2_COMPILER"
  echo "log: $LOG_FILE"
  tail -n 80 "$LOG_FILE"
  exit $status
fi

echo "PASS: stage2 compiler produced $OUTPUT_BIN"
