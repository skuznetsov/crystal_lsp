#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler> [source.cr]" >&2
  exit 2
fi

COMPILER="$1"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="${2:-$ROOT_DIR/regression_tests/basic_sanity.cr}"
TMP_DIR="$(mktemp -d /tmp/stage1_ttyname_r_redefinition.XXXXXX)"
BIN="$TMP_DIR/repro.bin"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$OUT" 2>"$ERR"
status=$?
set -e

if [[ $status -ne 0 ]] && rg -q "invalid redefinition of function 'ttyname_r'|@ttyname_r\(\.\.\.\)" "$ERR"; then
  echo "reproduced: invalid LLVM redefinition for ttyname_r"
  echo "compiler: $COMPILER"
  echo "source: $SRC"
  echo "status: $status"
  echo "tmp_dir: $TMP_DIR"
  exit 0
fi

echo "not reproduced"
echo "compiler: $COMPILER"
echo "source: $SRC"
echo "status: $status"
echo "tmp_dir: $TMP_DIR"
if [[ -s "$ERR" ]]; then
  echo "--- stderr ---"
  cat "$ERR"
fi
if [[ -s "$OUT" ]]; then
  echo "--- stdout ---"
  cat "$OUT"
fi
exit 1
