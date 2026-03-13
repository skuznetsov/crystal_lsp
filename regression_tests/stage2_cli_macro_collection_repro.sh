#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="$1"

if [[ ! -x "$BIN" ]]; then
  echo "Compiler is not executable: $BIN" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_cli_macro_collection_repro.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
OUT_BIN="$TMP_DIR/repro_bin"
LOG="$TMP_DIR/compile.log"

cat >"$SRC" <<'CR'
puts 1
CR

set +e
env STAGE2_DEBUG=1 CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --release "$SRC" -o "$OUT_BIN" >"$LOG" 2>&1
rc=$?
set -e

if rg -Fq "[STAGE2_DEBUG] pre-scan constants done" "$LOG"; then
  echo "not reproduced (compiler moved past the old top-level macro collection frontier)"
  exit 0
fi

if rg -Fq "[STAGE2_DEBUG] top-level collection walk start" "$LOG"; then
  echo "reproduced: stage2 still crashed before top-level macro collection completed"
  exit 1
fi

if [[ $rc -eq 0 ]]; then
  echo "not reproduced (compiler finished stop-after-HIR cleanly)"
  exit 0
fi

echo "inconclusive: compiler exited $rc before reaching the top-level collection markers" >&2
tail -n 40 "$LOG" >&2 || true
exit 2
