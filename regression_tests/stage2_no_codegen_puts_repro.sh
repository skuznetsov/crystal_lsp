#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2_compiler> [timeout_sec]" >&2
  exit 2
fi

COMPILER="$1"
TIMEOUT_SEC="${2:-12}"
OUT_DIR="${TMPDIR:-/tmp}/stage2_no_codegen_puts_repro"
SRC="$OUT_DIR/repro.cr"
BIN="$OUT_DIR/repro.bin"
PROBE_DIR="$OUT_DIR/probe"

mkdir -p "$OUT_DIR"

cat > "$SRC" <<'CR'
puts 1
CR

set +e
scripts/timeout_sample_lldb.sh --timeout "$TIMEOUT_SEC" --sample 2 --top 12 --out-dir "$PROBE_DIR" -- \
  "$COMPILER" --release --no-codegen --no-ast-cache "$SRC" -o "$BIN"
status=$?
set -e

echo "status: $status"
echo "probe: $PROBE_DIR"

if [ "$status" -eq 0 ]; then
  echo "UNEXPECTED: regression did not reproduce"
  exit 1
fi

echo "expected failure reproduced"
exit 0
