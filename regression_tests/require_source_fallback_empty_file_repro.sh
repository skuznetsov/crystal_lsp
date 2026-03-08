#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/require_fallback_empty.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro.o"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
puts 1
CR

set +e
"$COMPILER" "$SRC" --no-prelude --no-link --verbose -o "$OUT" >"$STDOUT_LOG" 2>"$STDERR_LOG"
compile_status=$?
set -e

echo "compiler: $COMPILER"
echo "status: $compile_status"

if [[ $compile_status -ne 0 ]]; then
  echo "compile failed"
  echo "--- stdout ---"
  cat "$STDOUT_LOG"
  echo "--- stderr ---"
  cat "$STDERR_LOG"
  exit 2
fi

if rg -Fq "Source require fallback entries=0" "$STDOUT_LOG" "$STDERR_LOG"; then
  echo "reproduced: empty source file still paid full require fallback scan"
  exit 0
fi

echo "not reproduced"
exit 1
