#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <stage2_compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d /tmp/stage2_container_clear_oob.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"

cat >"$SRC" <<'CR'
class Probe
  @h1 : Hash(UInt32, String) = {} of UInt32 => String

  def run
    @h1[1_u32] = "a"
    @h1.clear
  end
end

p = Probe.new
p.run
puts "probe_ok"
CR

set +e
"$COMPILER" "$SRC" --release --no-link -o "$BIN" >"$OUT" 2>"$ERR"
status=$?
set -e

if [[ $status -ne 0 ]] && grep -q "Index out of bounds" "$ERR"; then
  echo "reproduced: stage2 fails on container clear stress with Index out of bounds"
  echo "compiler: $COMPILER"
  echo "status: $status"
  echo "tmp_dir: $TMP_DIR"
  exit 0
fi

echo "not reproduced"
echo "compiler: $COMPILER"
echo "status: $status"
echo "tmp_dir: $TMP_DIR"
echo "--- stderr ---"
cat "$ERR"
echo "--- stdout ---"
cat "$OUT"
exit 1
