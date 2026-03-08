#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/generated_env_keys.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro_bin"
RUN_OUT="$TMP_DIR/run.out"
COMPILE_ERR="$TMP_DIR/compile.err"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
puts ENV.keys.any? { |k| k.starts_with?("DEBUG_") }
puts !ENV["DEBUG_MAIN"]?.nil?
puts ENV.keys.includes?("DEBUG_MAIN")
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" > /dev/null 2> "$COMPILE_ERR"
rc=$?
set -e

if [ "$rc" -ne 0 ]; then
  echo "compiler: $COMPILER"
  echo "status: compile_failed($rc)"
  echo "--- compile stderr ---"
  cat "$COMPILE_ERR"
  exit 1
fi

DEBUG_MAIN=1 "$ROOT/scripts/run_safe.sh" "$BIN" 5 256 > "$RUN_OUT"

echo "compiler: $COMPILER"
echo "--- run output ---"
cat "$RUN_OUT"

if grep -Fq "=== STDOUT ===" "$RUN_OUT" && \
   grep -Fxq "false" <(awk '/=== STDOUT ===/{flag=1; next} /=== STDERR ===/{flag=0} flag {print}' "$RUN_OUT" | sed -n '1p') && \
   grep -Fxq "true" <(awk '/=== STDOUT ===/{flag=1; next} /=== STDERR ===/{flag=0} flag {print}' "$RUN_OUT" | sed -n '2p') && \
   grep -Fxq "false" <(awk '/=== STDOUT ===/{flag=1; next} /=== STDERR ===/{flag=0} flag {print}' "$RUN_OUT" | sed -n '3p'); then
  echo "reproduced: generated binary loses DEBUG_MAIN in ENV.keys enumeration while direct lookup still works"
  exit 0
fi

echo "not reproduced"
exit 1
