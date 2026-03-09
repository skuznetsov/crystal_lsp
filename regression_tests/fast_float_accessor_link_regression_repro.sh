#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "$ROOT_DIR/tmp/fast-float-ec.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
ERR="$TMP_DIR/repro.err"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
puts Float::FastFloat.to_f64?("1.0", true, true)
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$TMP_DIR/repro.out" 2>"$ERR"
compile_rc=$?
set -e

echo "compiler: $COMPILER"
echo "compile_rc: $compile_rc"
echo "stderr_tail:"
tail -n 20 "$ERR" 2>/dev/null || true

if [[ $compile_rc -ne 0 ]] &&
   rg -q 'Undefined symbols.*_ec|"_ec"|_ec, referenced from' "$ERR" &&
   rg -q 'Undefined symbols.*_ptr|"_ptr"|_ptr, referenced from' "$ERR"; then
  echo "reproduced: Float::FastFloat accessors regressed to bare _ec/_ptr link symbols"
  exit 0
fi

echo "not reproduced"
exit 1
