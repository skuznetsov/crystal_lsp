#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_nonlocal_return_fix}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_hash_has_key_nonlocal_return.XXXXXX)"
SRC="$TMP_DIR/hash_has_key_nonlocal_return_repro.cr"
BIN="$TMP_DIR/hash_has_key_nonlocal_return_repro"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
h = {} of String => Int32
h["x"] = 1
puts h.has_key?("x")
CR

set +e
"$COMPILER" --output "$BIN" "$SRC" >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 80 "$TMP_DIR/compile.err"
  exit 1
fi

set +e
run_output="$("$ROOT/scripts/run_safe.sh" "$BIN" 10 768 2>&1)"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  echo "$run_output" | tail -n 80
  exit 1
fi

if ! echo "$run_output" | rg -q '^true$'; then
  echo "reproduced (unexpected runtime output)"
  echo "$run_output"
  exit 1
fi

echo "not reproduced"
