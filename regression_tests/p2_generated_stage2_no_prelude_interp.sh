#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
SOURCE="$ROOT_DIR/regression_tests/combined/test_no_prelude_interpolation.cr"
TMP_DIR="$(mktemp -d /tmp/p2_generated_stage2_interp_XXXXXX)"
GENERATED_S2="$TMP_DIR/generated_s2"
BUILD_LOG="$TMP_DIR/build.log"
SMOKE_LOG="$TMP_DIR/smoke.log"

cleanup() {
  if [[ "${KEEP_TMP:-0}" != "1" ]]; then
    rm -rf "$TMP_DIR"
  else
    echo "[p2_generated_stage2_no_prelude_interp] kept tmp: $TMP_DIR" >&2
  fi
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "p2_generated_stage2_no_prelude_interp_failed: compiler not found: $COMPILER" >&2
  exit 2
fi

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 420 4096 \
  "$ROOT_DIR/src/crystal_v2.cr" -o "$GENERATED_S2" >"$BUILD_LOG" 2>&1

if [[ ! -x "$GENERATED_S2" ]]; then
  echo "p2_generated_stage2_no_prelude_interp_failed: missing generated stage2 compiler" >&2
  tail -80 "$BUILD_LOG" >&2 || true
  exit 1
fi

"$ROOT_DIR/scripts/run_safe.sh" "$GENERATED_S2" 60 1024 \
  "$SOURCE" --no-prelude --no-codegen >"$SMOKE_LOG" 2>&1

if grep -Eq 'STUB CALLED:|error\[E[0-9]+\]:' "$SMOKE_LOG"; then
  echo "p2_generated_stage2_no_prelude_interp_failed: unexpected runtime or semantic failure" >&2
  tail -120 "$SMOKE_LOG" >&2 || true
  exit 1
fi

if ! grep -q 'Parsed 5 top-level expressions' "$SMOKE_LOG"; then
  echo "p2_generated_stage2_no_prelude_interp_failed: missing parse success signal" >&2
  tail -120 "$SMOKE_LOG" >&2 || true
  exit 1
fi

echo "p2_generated_stage2_no_prelude_interp_ok"
