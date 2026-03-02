#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_hash_literal_index_fix}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_hash_literal_index.XXXXXX)"
SRC="$TMP_DIR/hash_literal_index_repro.cr"
BIN="$TMP_DIR/hash_literal_index_repro"
HIR="${BIN}.hir"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
h = {"a" => "x"}
puts h.size
puts h["a"]
CR

set +e
"$COMPILER" "$SRC" --emit hir -o "$BIN" >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 40 "$TMP_DIR/compile.err"
  exit 1
fi

if [[ ! -f "$HIR" ]]; then
  echo "reproduced (hir not emitted)"
  exit 1
fi

find_entry_call_type_id="$(awk '
  /func @Hash\(String, String\)#find_entry\$String/ {in_fn=1}
  in_fn && /^}/ {in_fn=0}
  in_fn && /find_entry_with_index\$String/ {
    line=$0
    sub(/.*: /, "", line)
    sub(/[^0-9].*/, "", line)
    if (line != "") {
      print line
      exit
    }
  }
' "$HIR")"

if [[ -z "$find_entry_call_type_id" ]]; then
  echo "reproduced (failed to capture find_entry call return type id)"
  exit 1
fi

if rg -q "^  type\\.${find_entry_call_type_id} = .*Entry\\(K, V\\)" "$HIR"; then
  echo "reproduced (find_entry callsite still uses unresolved Entry(K, V) union)"
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

if ! echo "$run_output" | rg -q '^1$'; then
  echo "reproduced (missing size output)"
  echo "$run_output"
  exit 1
fi

if ! echo "$run_output" | rg -q '^x$'; then
  echo "reproduced (missing hash lookup output)"
  echo "$run_output"
  exit 1
fi

echo "not reproduced"
