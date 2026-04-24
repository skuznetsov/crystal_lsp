#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
TMP_DIR="$(mktemp -d /tmp/p2_u64_to_s_base16_XXXXXX)"
SOURCE="$TMP_DIR/repro.cr"
OUT_BIN="$TMP_DIR/repro_bin"
COMPILE_LOG="$TMP_DIR/compile.log"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  if [[ "${KEEP_TMP:-0}" != "1" ]]; then
    rm -rf "$TMP_DIR"
  else
    echo "[p2_u64_to_s_base16_no_null] kept tmp: $TMP_DIR" >&2
  fi
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "p2_u64_to_s_base16_no_null_failed: compiler not found: $COMPILER" >&2
  exit 2
fi

cat >"$SOURCE" <<'CR'
s = 0xcbf29ce484222325_u64.to_s(16)
puts s
puts s.bytesize
puts s.byte_index(0).nil?
CR

"$COMPILER" "$SOURCE" -o "$OUT_BIN" >"$COMPILE_LOG" 2>&1

if [[ ! -x "$OUT_BIN" ]]; then
  echo "p2_u64_to_s_base16_no_null_failed: compiler did not produce binary" >&2
  tail -80 "$COMPILE_LOG" >&2 || true
  exit 1
fi

"$ROOT_DIR/scripts/run_safe.sh" "$OUT_BIN" 5 256 >"$RUN_LOG" 2>&1

if grep -qx 'cbf29ce484222325' "$RUN_LOG" &&
   grep -qx '16' "$RUN_LOG" &&
   grep -qx 'true' "$RUN_LOG"; then
  echo "p2_u64_to_s_base16_no_null_ok"
  exit 0
fi

echo "p2_u64_to_s_base16_no_null_failed: unexpected runtime output" >&2
cat "$RUN_LOG" >&2
exit 1
