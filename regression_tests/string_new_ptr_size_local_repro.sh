#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/string_new_ptr_size_local.XXXXXX")"
SRC="$TMP_DIR/main.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_STDOUT="$TMP_DIR/compile.stdout"
COMPILE_STDERR="$TMP_DIR/compile.stderr"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
size = 5
str = "./dep"
puts String.new(str.to_unsafe, size).bytesize
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_STDOUT" 2>"$COMPILE_STDERR"
compile_status=$?
set -e

echo "compiler: $COMPILER"
echo "compile_status: $compile_status"

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced: compile failed"
  echo "--- compile stdout ---"
  cat "$COMPILE_STDOUT"
  echo "--- compile stderr ---"
  cat "$COMPILE_STDERR"
  exit 0
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 256 >"$RUN_LOG" 2>&1
run_status=$?
set -e

echo "run_status: $run_status"
cat "$RUN_LOG"

if [[ $run_status -eq 0 ]] && grep -Fq "=== STDOUT ===" "$RUN_LOG" && grep -Fxq "5" "$RUN_LOG"; then
  echo "not reproduced"
  exit 1
fi

if [[ $run_status -eq 0 ]] && grep -Fq "=== STDOUT ===" "$RUN_LOG" && grep -Fxq "0" "$RUN_LOG"; then
  echo "reproduced: direct String.new(ptr, local_size) collapsed bytesize to 0"
  exit 0
fi

echo "reproduced: unexpected runtime signature"
exit 0
