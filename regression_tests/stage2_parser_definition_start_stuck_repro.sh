#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2_compiler> [timeout_sec]" >&2
  exit 2
fi

COMPILER="$1"
TIMEOUT_SEC="${2:-20}"
OUT_DIR="${TMPDIR:-/tmp}/stage2_parser_definition_start_stuck"
SRC="$OUT_DIR/repro.cr"
BIN="$OUT_DIR/repro.bin"
ERR="$OUT_DIR/err.txt"

mkdir -p "$OUT_DIR"

cat > "$SRC" <<'CR'
x = 1
puts x
CR

set +e
PARSER_DEBUG=1 timeout "$TIMEOUT_SEC" "$COMPILER" "$SRC" -o "$BIN" >"$OUT_DIR/out.txt" 2>"$ERR"
rc=$?
set -e

echo "status: $rc"
echo "log: $ERR"

if [ "$rc" -eq 124 ]; then
  loops=$(rg -c "parse_program: current=0, checking macro_control_start\\?" "$ERR" || true)
  stmt_hits=$(rg -c "parse_statement:" "$ERR" || true)
  echo "parse_program_identifier_loops: ${loops:-0}"
  echo "parse_statement_hits: ${stmt_hits:-0}"
  if [ "${loops:-0}" -ge 20 ] && [ "${stmt_hits:-0}" -eq 0 ]; then
    echo "reproduced (stuck in definition_start?/program loop on Identifier)"
    exit 0
  fi
  echo "timeout reproduced, but signature mismatch"
  exit 1
fi

echo "not reproduced"
exit 1
