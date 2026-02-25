#!/usr/bin/env bash
# Repro: `uninitialized StaticArray(Struct, N)` produced invalid storage and crashed.
# Usage: regression_tests/stage1_uninitialized_staticarray_struct_repro.sh <compiler>

set -euo pipefail

COMPILER="${1:-}"
if [[ -z "$COMPILER" || ! -x "$COMPILER" ]]; then
  echo "usage: $0 <compiler>"
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/stage1_staticarray_struct.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro_bin"
BUILD_LOG="$TMP_DIR/build.log"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
struct E
  getter index : Int32
  def initialize(@index : Int32)
  end
end

class Holder
  @buf : StaticArray(E, 2)

  def initialize
    @buf = uninitialized StaticArray(E, 2)
    @buf[0] = E.new(11)
    @buf[1] = E.new(22)
  end

  def sum : Int32
    @buf[0].index + @buf[1].index
  end
end

puts Holder.new.sum
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$BUILD_LOG" 2>&1
build_status=$?
set -e

if [[ $build_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  tail -n 40 "$BUILD_LOG"
  exit 1
fi

if [[ ! -x "$BIN" ]]; then
  # Current stage2 compiler path may ignore -o and emit next to source.
  if [[ -x "${SRC%.cr}" ]]; then
    BIN="${SRC%.cr}"
  else
    echo "reproduced (no output binary)"
    tail -n 40 "$BUILD_LOG"
    exit 1
  fi
fi

set +e
scripts/run_safe.sh "$BIN" 5 512 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if [[ $run_status -eq 0 ]] && rg -q "^33$" "$RUN_LOG"; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced"
tail -n 80 "$RUN_LOG"
exit 1

