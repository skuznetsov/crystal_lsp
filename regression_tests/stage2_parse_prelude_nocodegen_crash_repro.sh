#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d /tmp/stage2_parse_prelude_nocodegen_crash.XXXXXX)"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"
BIN="$TMP_DIR/out.bin"

set +e
"$COMPILER" --no-codegen --no-prelude src/stdlib/prelude.cr -o "$BIN" >"$OUT" 2>"$ERR"
status=$?
set -e

if [[ $status -eq 139 ]] || \
   grep -q "Index out of bounds" "$ERR" || grep -q "Index out of bounds" "$OUT" || \
   grep -q "ExprId out of bounds" "$ERR" || grep -q "ExprId out of bounds" "$OUT"; then
  echo "reproduced: stage2 prelude no-codegen crash/index signature"
  echo "compiler: $COMPILER"
  echo "status: $status"
  echo "tmp_dir: $TMP_DIR"
  exit 0
fi

echo "not reproduced"
echo "compiler: $COMPILER"
echo "status: $status"
echo "tmp_dir: $TMP_DIR"
echo "--- stderr ---"
cat "$ERR"
echo "--- stdout ---"
cat "$OUT"
exit 1
