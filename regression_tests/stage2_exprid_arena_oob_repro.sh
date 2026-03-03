#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <stage2_compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d /tmp/stage2_exprid_arena_oob.XXXXXX)"
SRC="$TMP_DIR/puts1.cr"
BIN="$TMP_DIR/puts1.bin"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"

cat >"$SRC" <<'CR'
puts 1
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$OUT" 2>"$ERR"
status=$?
set -e

if [[ $status -ne 0 ]] && grep -q "ExprId out of bounds" "$ERR"; then
  echo "reproduced: ExprId out of bounds (arena=:0) on minimal compile"
  echo "compiler: $COMPILER"
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
