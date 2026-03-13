#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="src/stdlib/errno.cr"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-errno-percent-words.XXXXXX")"
LOG_FILE="$WORKDIR/repro.log"
OUT_BIN="$WORKDIR/repro.bin"

trap 'rm -rf "$WORKDIR"' EXIT

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cd "$ROOT"

set +e
DEBUG_MACRO_FOR=1 \
DEBUG_PERCENT_WORDS=1 \
CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if grep -q '\[PERCENT_WORDS_ENTRY\]' "$LOG_FILE" &&
   ! grep -q '\[PERCENT_WORDS_FLUSH\] count=1' "$LOG_FILE"; then
  echo "reproduced: stage2 crashed while splitting Errno %w(...) before the first word flush"
  exit 0
fi

if grep -q '\[MACRO_FOR_HEADER_DONE\]' "$LOG_FILE"; then
  echo "not reproduced (compiler reached Errno %w(...) header completion)"
  exit 1
fi

if [[ "$RC" -ne 0 ]]; then
  echo "not reproduced (compiler exited $RC after the old percent-word signature disappeared)"
  exit 1
fi

echo "not reproduced"
exit 1
