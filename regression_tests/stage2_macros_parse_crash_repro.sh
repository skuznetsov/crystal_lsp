#!/usr/bin/env bash
# Regression: stage2 crashes with SIGSEGV when parsing macros.cr
# Crash in MacroPiece#control_keyword (bad self pointer from tuple destructuring)
# Uses pre-built stage2 binary; rebuild stage2 if source changes.
set -euo pipefail

STAGE2="${1:-/private/tmp/stage2_dbg}"
if [[ ! -x "$STAGE2" ]]; then
  echo "SKIP: stage2 binary not found at $STAGE2"
  exit 0
fi

TMPFILE=$(mktemp /tmp/s2_macro_crash_XXXXXX.cr)
trap "rm -f $TMPFILE ${TMPFILE%.cr}" EXIT

echo 'puts "hello"' > "$TMPFILE"

# Stage2 should be able to parse prelude (including macros.cr) without crashing
if timeout 30 "$STAGE2" "$TMPFILE" -o "${TMPFILE%.cr}" 2>&1 | tail -3; then
  echo "PASS: stage2 parsed macros.cr without crash"
else
  EXIT_CODE=$?
  if [[ $EXIT_CODE -eq 139 ]] || [[ $EXIT_CODE -eq 134 ]]; then
    echo "FAIL: stage2 SIGSEGV/SIGABRT during macros.cr parse (exit $EXIT_CODE)"
    exit 1
  else
    echo "FAIL: stage2 exited with $EXIT_CODE"
    exit 1
  fi
fi
