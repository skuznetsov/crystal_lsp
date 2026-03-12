#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="src/stdlib/lib_c/aarch64-darwin/c/sys/types.cr"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-lib-struct-bodyless.XXXXXX")"
LOG_FILE="$WORKDIR/repro.log"
OUT_BIN="$WORKDIR/repro.bin"

trap 'rm -rf "$WORKDIR"' EXIT

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cd "$ROOT"

set +e
DEBUG_LIB_CLASS_REPAIR=LibC::PthreadAttrT \
CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if [[ "$RC" -ne 0 ]]; then
  echo "--- repro log ---" >&2
  tail -n 80 "$LOG_FILE" >&2
  echo "unexpected status: $RC" >&2
  exit 3
fi

if grep -E -q '\[LIB_CLASS_REPAIR\] class=LibC::PthreadAttrT .*body=0' "$LOG_FILE"; then
  echo "reproduced: stage2 parsed LibC::PthreadAttrT with empty body"
  exit 0
fi

echo "not reproduced"
exit 1
