#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler> [source]" >&2
  exit 2
fi

COMPILER=$1
SOURCE=${2:-"$(dirname "$0")/basic_sanity.cr"}
OUT="${TMPDIR:-/tmp}/stage2_cli_parser_repro_bin"
LOG="${TMPDIR:-/tmp}/stage2_cli_parser_repro.log"
START_TS=$(date +%s)

set +e
"$COMPILER" --release "$SOURCE" -o "$OUT" > "$LOG" 2>&1
status=$?
END_TS=$(date +%s)
set -e

if [ $status -ne 0 ]; then
  echo "stage2 CLI parser repro failed (exit $status)" >&2
  echo "source: $SOURCE" >&2
  echo "log: $LOG" >&2
  tail -n 120 "$LOG" >&2
  exit $status
fi

ELAPSED=$((END_TS - START_TS))
echo "stage2 CLI parser repro: PASS (compiler output: $OUT, elapsed: ${ELAPSED}s)"
