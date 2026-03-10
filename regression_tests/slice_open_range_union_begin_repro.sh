#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "$ROOT_DIR/tmp/slice-open-range-union.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
LOG="$TMP_DIR/repro.run"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
key_value = "foo=bar".to_slice
split_index = key_value.index!(0x3d_u8)
key = String.new(key_value[0, split_index])
value = String.new(key_value[split_index + 1..])
puts key
puts value
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$LOG.compile.out" 2>"$LOG.compile.err"
compile_rc=$?
if [[ $compile_rc -eq 0 ]]; then
  "$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 256 >"$LOG"
  run_rc=$?
else
  run_rc=-1
fi
set -e

stdout=""
if [[ $run_rc -eq 0 ]]; then
  stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$LOG" 2>/dev/null | tr -d '\r')"
fi

expected=$'foo\nbar'

echo "compiler: $COMPILER"
echo "compile_rc: $compile_rc"
echo "run_rc: $run_rc"
echo "stdout:"
printf '%s\n' "$stdout"

if [[ $compile_rc -eq 0 && $run_rc -eq 0 && "$stdout" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced: broad Slice(UInt8)#[](Range) lost the open-range begin payload"
exit 0
