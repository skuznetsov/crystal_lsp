#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <stage2_compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d /tmp/stage2_mir_setup_post_lowering.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"

cat >"$SRC" <<'CR'
macro x
end
CR

set +e
STAGE2_DEBUG=1 CRYSTAL_V2_MIR_SETUP_TRACE=1 \
  "$COMPILER" "$SRC" --no-prelude --no-link -o "$BIN" >"$OUT" 2>"$ERR"
status=$?
set -e

if [[ $status -eq 139 ]] && \
   grep -q "\\[MIR_SETUP\\] lowering bodies done funcs=1" "$ERR"; then
  echo "reproduced: stage2 segfault occurs after MIR setup/lowering bodies"
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
tail -n 200 "$ERR"
echo "--- stdout ---"
tail -n 120 "$OUT"
exit 1
