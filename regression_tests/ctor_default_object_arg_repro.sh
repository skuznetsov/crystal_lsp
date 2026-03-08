#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/ctor_default_object_arg.XXXXXX")"
SRC="$TMP_DIR/main.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_STDOUT="$TMP_DIR/compile.stdout"
COMPILE_STDERR="$TMP_DIR/compile.stderr"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
class Ctx
  getter value

  def initialize(@value : Int32 = 42)
  end
end

class EngineLike
  getter c, ctx

  def initialize(@a : Int32, @b : Int32, @c : Int32? = nil, @ctx : Ctx = Ctx.new)
  end
end

x = EngineLike.new(1, 2, 3)
puts x.c.nil? ? 1 : 0
puts x.ctx.nil? ? 1 : 0
puts x.ctx ? x.ctx.value : -1
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_STDOUT" 2>"$COMPILE_STDERR"
compile_status=$?
set -e

echo "compiler: $COMPILER"
echo "compile_status: $compile_status"

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced: compile failed"
  echo "--- compile stdout ---"
  cat "$COMPILE_STDOUT"
  echo "--- compile stderr ---"
  cat "$COMPILE_STDERR"
  exit 0
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 256 >"$RUN_LOG" 2>&1
run_status=$?
set -e

echo "run_status: $run_status"
cat "$RUN_LOG"

stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_LOG")"
stdout_trimmed="$(printf '%s' "$stdout_text" | tr -d '\r')"
expected=$'0\n0\n42'
broken=$'0\n0\n-1'

if [[ $run_status -eq 0 ]] && [[ "$stdout_trimmed" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ "$stdout_trimmed" == "$broken" ]]; then
  echo "reproduced: constructor wrapper padded object default arg with null"
  exit 0
fi

echo "reproduced: unexpected runtime signature"
exit 0
