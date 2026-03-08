#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_no_prelude_literal.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro.o"
STDOUT_LOG="$TMP_DIR/stdout.log"
STDERR_LOG="$TMP_DIR/stderr.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

printf '1\n' >"$SRC"

set +e
"$COMPILER" "$SRC" --no-prelude --no-link -o "$OUT" >"$STDOUT_LOG" 2>"$STDERR_LOG"
rc=$?
set -e

echo "compiler: $COMPILER"
echo "status: $rc"

if [ "$rc" -eq 0 ]; then
  echo "not reproduced"
  exit 1
fi

echo "--- stderr ---"
cat "$STDERR_LOG"

if grep -Fq "Unsupported AST node type: CrystalV2::Compiler::Frontend::Node" "$STDERR_LOG"; then
  echo "reproduced: no-prelude literal compile hit unsupported Frontend::Node lowering"
  exit 0
fi

echo "reproduced: unexpected failure signature"
exit 0
