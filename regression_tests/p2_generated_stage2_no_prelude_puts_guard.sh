#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
TMP_DIR="$(mktemp -d /tmp/p2_generated_stage2_puts_guard_XXXXXX)"
GENERATED_S2="$TMP_DIR/generated_s2"
SOURCE="$TMP_DIR/repro.cr"
OUT_BIN="$TMP_DIR/repro_bin"
BUILD_LOG="$TMP_DIR/build.log"
COMPILE_LOG="$TMP_DIR/compile.log"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  if [[ "${KEEP_TMP:-0}" != "1" ]]; then
    rm -rf "$TMP_DIR"
  else
    echo "[p2_generated_stage2_no_prelude_puts_guard] kept tmp: $TMP_DIR" >&2
  fi
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: compiler not found: $COMPILER" >&2
  exit 2
fi

cat >"$SOURCE" <<'CR'
puts 7
CR

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 420 4096 \
  "$ROOT_DIR/src/crystal_v2.cr" -o "$GENERATED_S2" >"$BUILD_LOG" 2>&1

if [[ ! -x "$GENERATED_S2" ]]; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: missing generated stage2 compiler" >&2
  tail -80 "$BUILD_LOG" >&2 || true
  exit 1
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$GENERATED_S2" 60 1024 \
  "$SOURCE" --no-prelude -o "$OUT_BIN" >"$COMPILE_LOG" 2>&1
compile_status=$?
set -e

if grep -q 'Tuple\$Heach\$\$block' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old Tuple#each block frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if [[ $compile_status -eq 0 && -x "$OUT_BIN" ]]; then
  "$ROOT_DIR/scripts/run_safe.sh" "$OUT_BIN" 5 512 >"$RUN_LOG" 2>&1
  if grep -q '^7$' "$RUN_LOG"; then
    echo "p2_generated_stage2_no_prelude_puts_guard_ok"
    exit 0
  fi
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: generated binary printed unexpected output" >&2
  cat "$RUN_LOG" >&2
  exit 1
fi

if grep -q 'Missing hash key: __crystal_main' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old __crystal_main hash-key frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if grep -q 'MIR function stub not found for: __crystal_main' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old __crystal_main MIR-stub frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if grep -q 'STUB CALLED: IO\$CCFileDescriptor\$Htell' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_ok frontier=io_filedescriptor_tell"
  exit 0
fi

echo "p2_generated_stage2_no_prelude_puts_guard_failed: unexpected generated stage2 compile failure" >&2
tail -120 "$COMPILE_LOG" >&2 || true
exit 1
