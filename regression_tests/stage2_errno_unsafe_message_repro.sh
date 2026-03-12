#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="src/stdlib/errno.cr"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-errno-unsafe-message.XXXXXX")"
LOG_FILE="$WORKDIR/repro.log"
OUT_BIN="$WORKDIR/repro.bin"

trap 'rm -rf "$WORKDIR"' EXIT

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cd "$ROOT"

set +e
DEBUG_ENUM_ARENA=Errno \
DEBUG_DEF_ARENA=Errno \
CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if grep -q '\[DEF_ARENA\] base=Errno#unsafe_message' "$LOG_FILE" &&
   ! grep -q '\[DEF_ARENA\] base=Errno.value' "$LOG_FILE" &&
   grep -q 'CrystalV2::Compiler::Frontend::Node:last=CrystalV2::Compiler::Frontend::Node' "$LOG_FILE"; then
  echo "reproduced: stage2 corrupted Errno#unsafe_message before reaching Errno.value"
  exit 0
fi

if [[ "$RC" -ne 0 ]]; then
  echo "not reproduced (compiler exited $RC after the old Errno signature disappeared)"
  exit 1
fi

echo "not reproduced"
exit 1
