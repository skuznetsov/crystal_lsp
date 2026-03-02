#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_current}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_slice_add_union_abi.XXXXXX)"
SRC="$TMP_DIR/slice_add_union_abi_repro.cr"
BIN="$TMP_DIR/slice_add_union_abi_repro"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
bytes = Bytes.new(64, 65_u8)
slice = bytes
sum = 0_i32

while !slice.empty?
  sum += slice[0]
  slice = slice + 1
end

puts sum
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 20 "$TMP_DIR/compile.err"
  exit 1
fi

if [[ ! -x "$BIN" ]]; then
  if [[ -x "$TMP_DIR/slice_add_union_abi_repro" ]]; then
    BIN="$TMP_DIR/slice_add_union_abi_repro"
  fi
fi

set +e
run_output="$("$ROOT/scripts/run_safe.sh" "$BIN" 5 512 2>&1)"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  echo "$run_output" | tail -n 20
  exit 1
fi

if echo "$run_output" | rg -q "^4160$"; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced (unexpected output)"
echo "$run_output"
exit 1
