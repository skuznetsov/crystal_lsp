#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/proc_block_value_store.XXXXXX")"
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
class EventEmitter
  @on_event : Proc(String, Nil)?

  def initialize
    @on_event = nil
  end

  def on_event(&block : String ->)
    @on_event = block
  end

  def callback_set? : Bool
    !@on_event.nil?
  end
end

emitter = EventEmitter.new
puts "before=#{emitter.callback_set?}"
emitter.on_event { |msg| puts msg }
puts "after=#{emitter.callback_set?}"
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
stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT" | tr -d '\r')"

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "stdout:"
printf '%s\n' "$stdout_text"

if grep -Fxq "before=false" <<<"$stdout_text" && grep -Fxq "after=true" <<<"$stdout_text"; then
  echo "not reproduced"
  exit 1
fi

if grep -Fxq "before=false" <<<"$stdout_text" && grep -Fxq "after=false" <<<"$stdout_text"; then
  echo "reproduced: non-inline &block value param does not store proc"
  exit 0
fi

echo "unexpected output"
cat "$RUN_OUT"
exit 2
