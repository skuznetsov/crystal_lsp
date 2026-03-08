#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/abs_path_pointer_tuple_index.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_STDOUT="$TMP_DIR/compile.stdout"
COMPILE_STDERR="$TMP_DIR/compile.stderr"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

compile_cmd=()
if [[ "$(basename "$COMPILER")" == "crystal" ]]; then
  compile_cmd=("$COMPILER" build "$SRC" -o "$BIN")
else
  compile_cmd=("$COMPILER" "$SRC" -o "$BIN")
fi

cat >"$SRC" <<'CR'
s = begin
  __macro_ptr_1 = ::Pointer(typeof({1_u32, 3_u32}, {1_u32, 1_u32}, {1_u32, 2_u32})).malloc(3)
  __macro_ptr_1[0] = {1_u32, 3_u32}
  __macro_ptr_1[1] = {1_u32, 1_u32}
  __macro_ptr_1[2] = {1_u32, 2_u32}
  ::Slice.new(__macro_ptr_1, 3, read_only: false)
end
puts s[0][1]
CR

set +e
"${compile_cmd[@]}" >"$COMPILE_STDOUT" 2>"$COMPILE_STDERR"
compile_status=$?
set -e

echo "compiler: $COMPILER"
echo "compile_status: $compile_status"

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced: absolute-path pointer tuple index sample failed to compile"
  echo "--- compile stdout ---"
  cat "$COMPILE_STDOUT"
  echo "--- compile stderr ---"
  cat "$COMPILE_STDERR"
  exit 0
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$BIN" 10 512 >"$RUN_LOG" 2>&1
run_status=$?
set -e

echo "run_status: $run_status"
cat "$RUN_LOG"

stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_LOG" | tr -d '\r')"
expected='3'

if [[ $run_status -eq 0 ]] && [[ "$stdout_text" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ "$stdout_text" == 'Pointer().null' ]]; then
  echo "reproduced: absolute-path pointer tuple index degrades to Pointer().null"
  exit 0
fi

echo "reproduced: unexpected absolute-path pointer tuple index signature"
exit 0
