#!/usr/bin/env bash
# Regression test for HIR escape analysis missing PointerStore.
# Before fix: when a tuple literal is coerced to the formal element type
# inside a push function (e.g. {1, nil, nil} -> Tuple(Int32, Nil|String, Nil|String)),
# the coerced Allocate stayed StackLocal, MIR mapped that to a stack alloca,
# and storing the alloca pointer into the heap-allocated Array buffer left
# a dangling pointer once push returned. Reading t[0] / t[1] / t[2] back
# produced garbage values.
#
# Root cause: src/compiler/hir/escape_analysis.cr never traced PointerStore.
# The buffer write (PointerStore) of the coerced tuple was invisible to the
# worklist, so escape never propagated to the source Allocate.
#
# Fix: track PointerStore in record_uses and seed the stored value as
# HeapEscape (raw pointer stores typically target heap buffers that outlive
# the current frame).
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/tuple_push_pointer_store.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_OUT="$TMP_DIR/compile.out"
COMPILE_ERR="$TMP_DIR/compile.err"
RUN_OUT="$TMP_DIR/run.out"

cleanup() {
  if [[ "$KEEP_TMP" != "1" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
arr = [] of Tuple(Int32, Nil | String, Nil | String)
arr << {1, nil, nil}
t = arr.pop
puts t[0]
puts t[1].nil?
puts t[2].nil?
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_OUT" 2>"$COMPILE_ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "compile failed"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- stderr ---"
  cat "$COMPILE_ERR"
  echo "--- stdout ---"
  cat "$COMPILE_OUT"
  exit 2
fi

./scripts/run_safe.sh "$BIN" 5 256 >"$RUN_OUT"
stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT" | tr -d '\r')"

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "stdout:"
printf '%s\n' "$stdout_text"

expected=$'1\ntrue\ntrue'

if [[ "$stdout_text" == "$expected" ]]; then
  echo "fixed: tuple coerced inside push escapes to heap correctly"
  exit 0
fi

echo "unexpected output (expected: 1/true/true)"
cat "$RUN_OUT"
exit 1
