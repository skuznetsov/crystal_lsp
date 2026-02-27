#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_enum_keyhash_fix}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_hash_enum_key.XXXXXX)"
SRC="$TMP_DIR/hash_enum_key_repro.cr"
BIN="$TMP_DIR/hash_enum_key_repro"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
enum TK
  A
  B
end

h = {} of TK => Int32
h[TK::A] = 11
h[TK::B] = 22
puts h[TK::A]
puts h[TK::B]
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 40 "$TMP_DIR/compile.err"
  exit 1
fi

set +e
run_output="$($ROOT/scripts/run_safe.sh "$BIN" 10 768 2>&1)"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  echo "$run_output" | tail -n 40
  exit 1
fi

if ! echo "$run_output" | rg -q '^11$'; then
  echo "reproduced (missing first value)"
  echo "$run_output"
  exit 1
fi

if ! echo "$run_output" | rg -q '^22$'; then
  echo "reproduced (missing second value)"
  echo "$run_output"
  exit 1
fi

echo "not reproduced"
