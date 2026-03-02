#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_hash_delete_fix}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_hash_delete_missing.XXXXXX)"
SRC="$TMP_DIR/hash_delete_missing_repro.cr"
BIN="$TMP_DIR/hash_delete_missing_repro"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
h = {"a" => 1}
x = h.delete("z")
puts x.nil? ? "nil" : "not_nil"
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
run_output="$("$ROOT/scripts/run_safe.sh" "$BIN" 10 768 2>&1)"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  echo "$run_output" | tail -n 40
  exit 1
fi

if ! echo "$run_output" | rg -q '^nil$'; then
  echo "reproduced (unexpected runtime output)"
  echo "$run_output"
  exit 1
fi

echo "not reproduced"
