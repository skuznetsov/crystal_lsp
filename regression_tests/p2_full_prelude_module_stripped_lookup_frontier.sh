#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/p2_module_stripped_lookup.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/puts42.cr"
OUT="$TMP_DIR/puts42"
LOG="$TMP_DIR/compile.log"

cat > "$SRC" <<'CR'
puts 42
CR

set +e
STAGE2_BOOTSTRAP_TRACE=1 "$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 30 4096 \
  "$SRC" -o "$OUT" >"$LOG" 2>&1
status=$?
set -e

if ! grep -q '\[STAGE2_DEBUG\] class register done' "$LOG"; then
  echo "compiler did not pass class registration" >&2
  tail -120 "$LOG" >&2
  exit 1
fi

if ! grep -q '\[STAGE2_TRACE\] lower_main: exprs=' "$LOG"; then
  echo "compiler did not reach lower_main" >&2
  tail -120 "$LOG" >&2
  exit 1
fi

echo "p2_full_prelude_module_stripped_lookup_frontier_ok"
