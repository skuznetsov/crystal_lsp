#!/usr/bin/env bash
# Guard backend-owned runtime intrinsics emitted as HIR Call instructions.
#
# These calls must remain visible in HIR for MIR/LLVM lowering, but they must
# not be demand-driven as source-level HIR functions by lower_missing_call_targets.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_backend_intrinsic_boundary_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/intrinsic_boundary.cr"
OUT="$TMP_DIR/out"
LOG="$TMP_DIR/run.log"

printf '%s\n' \
  'x = "same"' \
  'y = "same"' \
  'z = x == y' \
  'w = x.as?(String)' \
  > "$SRC"

DEBUG_MISSING_SUMMARY=1 \
DEBUG_MISSING_TOP=30 \
CRYSTAL_V2_STOP_AFTER_HIR=1 \
CRYSTAL_V2_PHASE_STATS=1 \
  "$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 30 1024 \
    "$SRC" --no-prelude --emit hir --no-link -o "$OUT" >"$LOG" 2>&1

HIR="$OUT.hir"
if [[ ! -s "$HIR" ]]; then
  echo "p2 backend intrinsic boundary regression: missing HIR artifact" >&2
  cat "$LOG" >&2
  exit 1
fi

for intrinsic in __crystal_v2_string_eq __crystal_v2_select_ptr; do
  if ! grep -q "$intrinsic" "$HIR"; then
    echo "p2 backend intrinsic boundary regression: $intrinsic missing from HIR" >&2
    cat "$HIR" >&2
    exit 1
  fi
  if grep -q "$intrinsic" "$LOG"; then
    echo "p2 backend intrinsic boundary regression: $intrinsic was treated as missing source demand" >&2
    grep "$intrinsic" "$LOG" >&2
    exit 1
  fi
done

echo "p2_backend_intrinsic_boundary_no_prelude_ok"
