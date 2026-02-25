#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
if [[ ! -x "$COMPILER" ]]; then
  echo "error: compiler is not executable: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/stage2_index_void_receiver.XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
COMPILE_LOG="$TMP_DIR/compile.log"
RUN_LOG="$TMP_DIR/run.log"

cat > "$SRC" <<'CR'
puts "abc".check_no_null_byte
CR

if ! "$COMPILER" "$SRC" >"$COMPILE_LOG" 2>&1; then
  echo "reproduced: compiler failed on minimal check_no_null_byte repro"
  tail -n 40 "$COMPILE_LOG"
  exit 1
fi

BIN="${SRC%.cr}"
if [[ ! -x "$BIN" ]]; then
  echo "reproduced: compiler did not produce binary at expected path: $BIN"
  tail -n 40 "$COMPILE_LOG"
  exit 1
fi

if ! scripts/run_safe.sh "$BIN" 5 256 >"$RUN_LOG" 2>&1; then
  echo "reproduced: runtime crash/hang (expected clean String#byte_index path)"
  tail -n 40 "$RUN_LOG"
  exit 1
fi

if rg -q "^abc$" "$RUN_LOG"; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced: output mismatch for minimal check_no_null_byte repro"
echo "[expected] abc"
echo "[actual tail]"
tail -n 40 "$RUN_LOG"
exit 1
