#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_heredoc_eager_debug.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro.bin"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
msg = <<-TXT
hello
TXT
msg
CR

set +e
"$COMPILER" --no-codegen --no-prelude "$SRC" -o "$OUT" >"$STDOUT_LOG" 2>"$STDERR_LOG"
status=$?
set -e

echo "compiler: $COMPILER"
echo "status: $status"

if [[ $status -eq 139 ]]; then
  echo "reproduced: stage2 heredoc parsing still segfaults"
  echo "--- stdout ---"
  cat "$STDOUT_LOG"
  echo "--- stderr ---"
  cat "$STDERR_LOG"
  exit 0
fi

echo "not reproduced: no segfault on heredoc parse"
echo "--- stdout ---"
cat "$STDOUT_LOG"
echo "--- stderr ---"
cat "$STDERR_LOG"
exit 1
