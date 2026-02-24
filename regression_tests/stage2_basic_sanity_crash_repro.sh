#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_rel_fix35}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT="/tmp/stage2_basic_sanity_repro.bin"

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cd "$ROOT"

set +e
"$BIN" regression_tests/basic_sanity.cr -o "$OUT"
RC=$?
set -e

if [[ "$RC" -eq 0 ]]; then
  echo "unexpected success (no crash): $BIN"
  exit 1
fi

if [[ "$RC" -eq 139 ]]; then
  echo "reproduced: stage2 crashes with exit 139 on basic_sanity"
  exit 0
fi

echo "reproduced non-zero failure: exit $RC"
exit 0
