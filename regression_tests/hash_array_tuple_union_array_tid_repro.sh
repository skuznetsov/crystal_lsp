#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hash_array_tuple_union_tid.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT_BASE="$TMP_DIR/repro"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"
LL_FILE="$OUT_BASE.ll"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
alias ArenaLike = Int32 | Int64

h = {} of String => Array(Tuple(Int32, ArenaLike))
h["x"] = [{1, 1_i32.as(ArenaLike)}]

puts h["x"].size
CR

set +e
env DEBUG_CONTAINER_REGISTER=1 DEBUG_ARRAY_TID=1 DEBUG_ARRAY_TID_VERBOSE=1 \
  "$COMPILER" "$SRC" --emit llvm-ir -o "$OUT_BASE" >"$STDOUT_LOG" 2>"$STDERR_LOG"
status=$?
set -e

echo "compiler: $COMPILER"
echo "status: $status"
echo "tmp_dir: $TMP_DIR"

if [[ $status -ne 0 ]]; then
  echo "compile failed"
  echo "--- stderr ---"
  cat "$STDERR_LOG"
  echo "--- stdout ---"
  cat "$STDOUT_LOG"
  exit 2
fi

target_name='Array(Tuple(Int32, Int32 | Int64))'
hit_line="$(rg -n -F "[ARRAY_TID] hit array_name=${target_name}" "$STDERR_LOG" | tail -n 1 || true)"
miss_line="$(rg -n -F "[ARRAY_TID] miss array_name=${target_name}" "$STDERR_LOG" | tail -n 1 || true)"

if [[ -n "$hit_line" ]]; then
  echo "trace_hit: $hit_line"
fi
if [[ -n "$miss_line" ]]; then
  echo "trace_miss: $miss_line"
fi

if [[ -n "$hit_line" ]]; then
  array_id="$(printf '%s\n' "$hit_line" | sed -E 's/.*array_id=([0-9]+).*/\1/')"
  store_line="$(rg -n "store i32 ${array_id}, ptr %.*tid_ptr" "$LL_FILE" | tail -n 1 || true)"
  if [[ -n "$store_line" ]]; then
    echo "ir_store: $store_line"
    echo "not reproduced"
    exit 1
  fi
fi

if [[ -n "$miss_line" ]]; then
  echo "reproduced: Array(Tuple(Int32, Int32|Int64)) runtime type id lookup missed canonical MIR slot"
  exit 0
fi

echo "reproduced: unexpected signature"
echo "--- stderr ---"
cat "$STDERR_LOG"
exit 0
