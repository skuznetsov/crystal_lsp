#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/untyped-getter-ivar-layout.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_STDOUT="$TMP_DIR/compile.stdout"
COMPILE_STDERR="$TMP_DIR/compile.stderr"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
class UntypedGetterLayout
  getter first, second, third

  def initialize(x : Int32, *, first : Int32, second : String, third : Bool)
    @first = first
    @second = second
    @third = third
  end
end

x = UntypedGetterLayout.new(1, first: 11, second: "two", third: true)
puts x.first
puts x.second
puts x.third
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_STDOUT" 2>"$COMPILE_STDERR"
compile_status=$?
set -e

echo "compiler: $COMPILER"
echo "compile_status: $compile_status"

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced: compile failed"
  echo "--- stdout ---"
  cat "$COMPILE_STDOUT"
  echo "--- stderr ---"
  cat "$COMPILE_STDERR"
  exit 0
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 256 >"$RUN_LOG" 2>&1
run_status=$?
set -e

echo "run_status: $run_status"
cat "$RUN_LOG"

stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_LOG" | tr -d '\r')"
expected=$'11\ntwo\ntrue'

if [[ $run_status -eq 0 && "$stdout_text" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced: late-typed getter ivars kept stale field offsets"
exit 0
