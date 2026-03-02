#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_union_tuple_fix}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_hash_find_entry_union_idx.XXXXXX)"
SRC="$TMP_DIR/hash_find_entry_union_idx_repro.cr"
BIN="$TMP_DIR/hash_find_entry_union_idx_repro"
HIR="${BIN}.hir"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
h = {} of String => String
int_types = ["Int8", "Int16", "Int32", "Int64", "Int128", "UInt8", "UInt16", "UInt32", "UInt64", "UInt128"]
float_types = ["Float32", "Float64"]
num_types = int_types + float_types
binary_ops = ["+", "-", "*", "//"]
comparison_ops = ["==", "!=", "<", "<=", ">", ">="]

num_types.each do |t1|
  num_types.each do |t2|
    comparison_ops.each do |op|
      h["#{t1}##{op}"] ||= "binary"
    end
    binary_ops.each do |op|
      h["#{t1}##{op}"] ||= "binary"
    end
  end
end

puts h.size
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

if awk '
  /func @Hash\(String, String\)#find_entry\$String/ {in_fn=1}
  in_fn && /^}/ {in_fn=0}
  in_fn && /#\[\]\$Int32/ {bad=1}
  END {exit bad ? 0 : 1}
' "$HIR"; then
  echo "reproduced (find_entry uses virtual #[] call instead of index_get)"
  exit 1
fi

if ! awk '
  /func @Hash\(String, String\)#find_entry\$String/ {in_fn=1}
  in_fn && /^}/ {in_fn=0}
  in_fn && /index_get/ {ok=1}
  END {exit ok ? 0 : 1}
' "$HIR"; then
  echo "reproduced (find_entry missing index_get fast-path)"
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

if ! echo "$run_output" | rg -q '^120$'; then
  echo "reproduced (unexpected runtime output)"
  echo "$run_output"
  exit 1
fi

echo "not reproduced"
