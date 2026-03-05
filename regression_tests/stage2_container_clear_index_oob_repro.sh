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
struct TR
  getter id : UInt32
  def initialize(@id : UInt32)
  end
end

class Probe
  @h1 : Hash(UInt32, String) = {} of UInt32 => String
  @h2 : Hash(UInt32, String) = {} of UInt32 => String
  @h3 : Hash(UInt32, TR) = {} of UInt32 => TR
  @s1 : Set(UInt32) = Set(UInt32).new
  @h4 : Hash(UInt32, Int32) = {} of UInt32 => Int32
  @arr : Array(Bool) = [] of Bool
  @h5 : Hash(UInt32, {Int32, Int32}) = {} of UInt32 => {Int32, Int32}
  @h6 : Hash(UInt32, Set(UInt32)) = {} of UInt32 => Set(UInt32)
  @h7 : Hash(UInt32, {String, Int32, Int32}) = {} of UInt32 => {String, Int32, Int32}

  def seed(n : Int32)
    i = 0
    while i < n
      id = i.to_u32
      @h1[id] = "a#{i}"
      @h2[id] = "b#{i}"
      @h3[id] = TR.new(id)
      @s1 << id
      @h4[id] = i
      @arr << i.odd?
      @h5[id] = {i, i + 1}
      @h6[id] = Set(UInt32){id}
      @h7[id] = {"x#{i}", i, i + 2}
      i += 1
    end
  end

  def reset
    @h1.clear
    @h2.clear
    @h3.clear
    @s1.clear
    @h4.clear
    @arr.clear
    @h5.clear
    @h6.clear
    @h7.clear
  end
end

p = Probe.new
p.seed(200)
p.reset
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
