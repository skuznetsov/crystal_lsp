#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/slice_tuple_reverse.XXXXXX")"
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
s = Slice[{1_u32, 3_u32}, {1_u32, 1_u32}, {1_u32, 2_u32}]
s.reverse!
puts s[0][1]
CR

set +e
"${compile_cmd[@]}" >"$COMPILE_STDOUT" 2>"$COMPILE_STDERR"
compile_status=$?
set -e

echo "compiler: $COMPILER"
echo "compile_status: $compile_status"

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced: slice tuple reverse sample failed to compile"
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
expected='2'

if [[ $run_status -eq 0 ]] && [[ "$stdout_text" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ $run_status -eq 139 ]] || grep -Eq 'Segmentation fault|Segfault|signal 11|status: 139|exit 139|EXC_BAD_ACCESS' "$RUN_LOG"; then
  echo "reproduced: slice tuple reverse sample crashes at runtime"
  exit 0
fi

echo "reproduced: unexpected slice tuple reverse signature"
exit 0
