#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="src/stdlib/errno.cr"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-errno-macro-body-parse.XXXXXX")"
LOG_FILE="$WORKDIR/repro.log"
OUT_BIN="$WORKDIR/repro.bin"

trap 'rm -rf "$WORKDIR"' EXIT

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cd "$ROOT"

set +e
DEBUG_PERCENT_WORDS=1 \
CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if grep -q '\[PERCENT_WORDS_DONE\]' "$LOG_FILE" &&
   ! grep -q "\[PARSE_OK\] $ROOT/$SRC" "$LOG_FILE"; then
  echo "reproduced: stage2 finished Errno %w(...) but crashed before PARSE_OK"
  exit 0
fi

if grep -q "\[PARSE_OK\] $ROOT/$SRC" "$LOG_FILE"; then
  echo "not reproduced (compiler parsed Errno and moved past the old macro-body parse crash)"
  exit 1
fi

if [[ "$RC" -ne 0 ]]; then
  echo "not reproduced (compiler exited $RC after the old macro-body parse signature disappeared)"
  exit 1
fi

echo "not reproduced"
exit 1
