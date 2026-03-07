#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_no_prelude_pointer.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro.o"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
x : Pointer(UInt8) = Pointer(UInt8).null
CR

set +e
"$COMPILER" "$SRC" --no-prelude --no-link -o "$OUT" >"$STDOUT_LOG" 2>"$STDERR_LOG"
status=$?
set -e

echo "compiler: $COMPILER"
echo "status: $status"

if [ "$status" -eq 0 ]; then
  echo "not reproduced"
  exit 1
fi

echo "--- stderr ---"
cat "$STDERR_LOG"

if grep -Fq "Missing named tuple key: :args" "$STDERR_LOG"; then
  echo "reproduced: no-prelude Pointer generic compile hit args-key failure"
  exit 0
fi

echo "reproduced: unexpected failure signature"
exit 0
