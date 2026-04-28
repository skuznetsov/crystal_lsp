#!/usr/bin/env bash
# Regression: Array(Union) buffer stride mismatch
#
# Push/realloc/get used three different per-element strides for inline-stored
# unions: realloc multiplied by 8 (treating the slot as a pointer), PointerStore
# wrote at the MIR size (incl. alignment padding), and Array#[] read at the
# LLVM-natural sizeof of `{i32, [N x i32]}` (which omits the padding).
# Indexes ≥ 1 silently returned garbage. See commit
# fix(codegen): unify Array(Union) buffer stride across realloc/get/set.
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/array_inline_union_struct_stride.XXXXXX")"
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
record A, x : Int32, y : Int32
record B, x : Int32

alias U = A | B

arr = [] of U
arr << A.new(1, 2)
arr << B.new(3)
arr << A.new(4, 5)

puts arr.size

a0 = arr[0]
case a0
when A then puts a0.x
end

b1 = arr[1]
case b1
when B then puts b1.x
end

a2 = arr[2]
case a2
when A then puts "#{a2.x},#{a2.y}"
end
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

set +e
./scripts/run_safe.sh "$BIN" 10 512 >"$RUN_OUT"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "run failed"
  echo "status: $run_status"
  echo "tmp_dir: $TMP_DIR"
  cat "$RUN_OUT"
  exit 1
fi

EXPECTED=$'3\n1\n3\n4,5'
ACTUAL="$(grep -v '^\[' "$RUN_OUT" | grep -v '^=== ')"

if [[ "$ACTUAL" != "$EXPECTED" ]]; then
  echo "output mismatch"
  echo "tmp_dir: $TMP_DIR"
  echo "--- expected ---"
  echo "$EXPECTED"
  echo "--- actual ---"
  echo "$ACTUAL"
  exit 1
fi

echo "ok"
