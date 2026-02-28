#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <stage2_compiler> [timeout_sec] [mode] [n] [phase]" >&2
  echo "  mode: release|debug (default: release)" >&2
  echo "  n: number of generated concrete types (default: 600)" >&2
  echo "  phase: full|no_codegen (default: full)" >&2
  exit 2
fi

COMPILER="$1"
TIMEOUT_SEC="${2:-120}"
MODE="${3:-release}"
N="${4:-600}"
PHASE="${5:-full}"
OUT_DIR="${TMPDIR:-/tmp}/stage2_yield_scan_hang_probe"
SRC="$OUT_DIR/yield_scan_stress.cr"
BIN="$OUT_DIR/yield_scan_stress.bin"
PROBE_DIR="$OUT_DIR/probe"

mkdir -p "$OUT_DIR"

cat > "$SRC" <<'CR'
class Box(T)
  @value : T
  def initialize(@value : T)
  end

  def each_twice
    yield @value
    yield @value
  end
end
CR

i=0
while [ "$i" -lt "$N" ]; do
  cat >> "$SRC" <<CR
struct S${i}
  getter v : Int32
  def initialize(@v : Int32)
  end
end
CR
  i=$((i + 1))
done

cat >> "$SRC" <<'CR'
sum = 0
CR

i=0
while [ "$i" -lt "$N" ]; do
  cat >> "$SRC" <<CR
b${i} = Box(S${i}).new(S${i}.new(${i}))
b${i}.each_twice { |x| sum += x.v }
CR
  i=$((i + 1))
done

cat >> "$SRC" <<'CR'
result = sum
result
CR

case "$MODE" in
  release)
    COMPILER_FLAGS=(--release)
    ;;
  debug)
    COMPILER_FLAGS=()
    ;;
  *)
    echo "unknown mode: $MODE (expected release|debug)" >&2
    exit 2
    ;;
esac

case "$PHASE" in
  full)
    ;;
  no_codegen)
    COMPILER_FLAGS+=(--no-codegen --no-ast-cache)
    ;;
  *)
    echo "unknown phase: $PHASE (expected full|no_codegen)" >&2
    exit 2
    ;;
esac

set +e
scripts/timeout_sample_lldb.sh --timeout "$TIMEOUT_SEC" --sample 6 --top 12 --out "$PROBE_DIR" -- \
  /usr/bin/time -p "$COMPILER" "${COMPILER_FLAGS[@]}" "$SRC" -o "$BIN"
st=$?
set -e

echo "status: $st"
echo "probe: $PROBE_DIR"

if [ "$st" -eq 124 ]; then
  echo "reproduced (timeout)"
  exit 0
fi

if [ "$st" -eq 0 ]; then
  echo "completed (no timeout)"
  exit 0
fi

echo "reproduced (non-timeout failure)"
exit 1
