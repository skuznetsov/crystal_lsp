#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2_compiler> [timeout_sec]" >&2
  exit 2
fi

COMPILER="$1"
TIMEOUT_SEC="${2:-180}"
OUT_DIR="${TMPDIR:-/tmp}/stage2_env_optional_hang_probe"
SRC="$OUT_DIR/env_optional.cr"
BIN="$OUT_DIR/env_optional.bin"
PROBE_DIR="$OUT_DIR/probe"

mkdir -p "$OUT_DIR"

cat > "$SRC" <<'CR'
key = "CRYSTAL_V2_MISSING_KEY_REPRO_#{Process.pid}"
val = ENV[key]?
puts(val.nil? ? "nil" : val)
puts ENV.fetch(key, "fallback")
CR

set +e
scripts/timeout_sample_lldb.sh --timeout "$TIMEOUT_SEC" --sample 8 --top 12 --out "$PROBE_DIR" -- \
  "$COMPILER" "$SRC" -o "$BIN"
st=$?
set -e

echo "status: $st"
echo "probe: $PROBE_DIR"

if [ "$st" -eq 124 ]; then
  echo "expected hang reproduced"
  exit 0
fi

if [ "$st" -eq 0 ]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced (non-timeout failure)"
exit 1
