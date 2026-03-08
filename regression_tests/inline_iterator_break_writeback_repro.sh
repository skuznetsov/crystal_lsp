#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/inline_iterator_break_writeback.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_OUT="$TMP_DIR/compile.out"
COMPILE_ERR="$TMP_DIR/compile.err"
RUN_OUT="$TMP_DIR/run.out"

cleanup() {
  if [[ "$KEEP_TMP" != "1" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
class Probe
  @src : String
  @offset : Int32

  def initialize(@src : String)
    @offset = 0
  end

  def current_byte : UInt8
    @src.to_unsafe[@offset]
  end

  def advance
    @offset += 1
  end

  def test(delimiter : String)
    matches = true
    delimiter.each_byte do |byte|
      if @offset >= @src.bytesize || current_byte != byte
        matches = false
        break
      end
      advance
    end
    puts matches
    puts @offset
  end
end

Probe.new("TXT").test("TXT")
Probe.new("TXZ").test("TXT")
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_OUT" 2>"$COMPILE_ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "compile failed"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- stderr ---"
  cat "$COMPILE_ERR"
  echo "--- stdout ---"
  cat "$COMPILE_OUT"
  exit 2
fi

./scripts/run_safe.sh "$BIN" 5 256 >"$RUN_OUT"
stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT")"
stdout_trimmed="$(printf '%s' "$stdout_text" | tr -d '\r')"
expected=$'true\n3\nfalse\n2'

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "stdout:"
printf '%s\n' "${stdout_trimmed:-<empty>}"

if [[ "$stdout_trimmed" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ "$stdout_trimmed" == $'true\n3\ntrue\n2' ]]; then
  echo "reproduced: break inside iterator block loses outer local writeback"
  exit 0
fi

echo "unexpected output"
cat "$RUN_OUT"
exit 2
