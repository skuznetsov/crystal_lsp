#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_SRC="${TMPDIR:-/tmp}/stage2_lib_global_decl_repro.cr"
OUT_BIN="${TMPDIR:-/tmp}/stage2_lib_global_decl_repro.bin"
LOG_FILE="${TMPDIR:-/tmp}/stage2_lib_global_decl_repro.log"

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cat > "$TMP_SRC" <<'CR'
lib LibStage2GlobalDecl
  $daylight : Int
end

1
CR

cd "$ROOT"

set +e
/usr/bin/time -p "$BIN" --no-codegen "$TMP_SRC" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if [[ "$RC" -ne 0 ]]; then
  echo "stage2 lib global decl repro failed (exit $RC)"
  echo "log: $LOG_FILE"
  tail -n 80 "$LOG_FILE"
  exit "$RC"
fi

echo "stage2 lib global decl repro: PASS"
