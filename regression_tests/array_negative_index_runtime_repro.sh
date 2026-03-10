#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "$ROOT_DIR/tmp/array-negative-index.XXXXXX")"
GET_SRC="$TMP_DIR/get.cr"
GET_BIN="$TMP_DIR/get.bin"
GET_LOG="$TMP_DIR/get.run"
SET_SRC="$TMP_DIR/set.cr"
SET_BIN="$TMP_DIR/set.bin"
SET_LOG="$TMP_DIR/set.run"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$GET_SRC" <<'CR'
a = [{true, false, true}] of {Bool, Bool, Bool}
t = a[-1]
puts t[0]
puts t[1]
puts t[2]
CR

cat >"$SET_SRC" <<'CR'
a = [10, 20, 30]
a[-1] = 99
puts a[0]
puts a[1]
puts a[2]
CR

compile_and_run() {
  local src="$1"
  local bin="$2"
  local log="$3"

  "$COMPILER" "$src" -o "$bin" >"$log.compile.out" 2>"$log.compile.err"
  "$ROOT_DIR/scripts/run_safe.sh" "$bin" 5 256 >"$log"
}

set +e
compile_and_run "$GET_SRC" "$GET_BIN" "$GET_LOG"
get_status=$?
compile_and_run "$SET_SRC" "$SET_BIN" "$SET_LOG"
set_status=$?
set -e

get_stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$GET_LOG" 2>/dev/null | tr -d '\r')"
set_stdout="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$SET_LOG" 2>/dev/null | tr -d '\r')"

echo "compiler: $COMPILER"
echo "get_status: $get_status"
echo "get stdout:"
printf '%s\n' "$get_stdout"
echo "set_status: $set_status"
echo "set stdout:"
printf '%s\n' "$set_stdout"

if [[ $get_status -eq 0 && $set_status -eq 0 &&
      "$get_stdout" == $'true\nfalse\ntrue' &&
      "$set_stdout" == $'10\n20\n99' ]]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced: dynamic Array negative indexing bypassed Crystal index normalization"
exit 0
