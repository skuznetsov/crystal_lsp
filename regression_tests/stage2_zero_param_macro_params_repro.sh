#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_zero_param_macro.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro.bin"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
macro x
end
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
echo "reproduced: zero-param macro registration crashed"
exit 0
