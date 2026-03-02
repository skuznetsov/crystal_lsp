#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_hash_const_fix}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_const_hash_or_chain.XXXXXX)"
SRC="$TMP_DIR/const_hash_or_chain_repro.cr"
BIN="$TMP_DIR/const_hash_or_chain_repro"
HIR="${BIN}.hir"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
class A
  CONST = {"a" => "x"}

  def foo(seed : String) : String
    h = {"z" => "q"}
    h[seed]? || CONST[seed]? || seed
  end
end

puts A.new.foo("a")
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
  /func @A#foo\$String/ {in_fn=1}
  in_fn && /^}/ {in_fn=0}
  in_fn && /classvar_get A\.@@CONST : 0/ {bad=1}
  in_fn && /literal nil : Pointer/ {bad=1}
  END {exit bad ? 0 : 1}
' "$HIR"; then
  echo "reproduced (constant hash receiver degraded to VOID/Pointer in foo)"
  exit 1
fi

if ! awk '
  /func @A#foo\$String/ {in_fn=1}
  in_fn && /^}/ {in_fn=0}
  in_fn && /classvar_get A\.@@CONST/ {seen_const_get=1}
  in_fn && /Hash\(String, String\)#\[\]\?\$String/ {seen_lookup=1}
  END {exit (seen_const_get && seen_lookup) ? 0 : 1}
' "$HIR"; then
  echo "reproduced (missing typed CONST hash lookup in foo)"
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

if ! echo "$run_output" | rg -q '^x$'; then
  echo "reproduced (unexpected runtime output)"
  echo "$run_output"
  exit 1
fi

echo "not reproduced"
