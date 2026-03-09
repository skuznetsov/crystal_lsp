#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/named_ctor_init_fallback.XXXXXX")"
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
class Shape
  getter name : String
  getter node_id : Int32
  getter params : Array(Int32)
  getter return_annotation : String?
  getter scope : String
  getter type_parameters : Array(String)?
  getter is_class_method : Bool

  def initialize(name : String, node_id : Int32, *, params : Array(Int32) = [] of Int32, return_annotation : String? = nil, scope : String, type_parameters : Array(String)? = nil, is_class_method : Bool = false)
    @name = name
    @node_id = node_id
    @params = params
    @return_annotation = return_annotation
    @scope = scope
    @type_parameters = type_parameters
    @is_class_method = is_class_method
  end
end

x = Shape.new("foo", 7, scope: "scope")
puts x.name
puts x.node_id
puts x.params.size
puts x.return_annotation.nil?
puts x.scope
puts x.type_parameters.nil?
puts x.is_class_method
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
expected=$'foo\n7\n0\ntrue\nscope\ntrue\nfalse'
broken_prefix=$'foo\n7\n5\ntrue'

if [[ $run_status -eq 0 ]] && [[ "$stdout_trimmed" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ "$stdout_trimmed" == "$broken_prefix" ]] || grep -q "\\[CRASH\\]" "$RUN_LOG"; then
  echo "reproduced: named constructor call bound against raw .new arity instead of initialize signature"
  exit 0
fi

echo "reproduced: unexpected runtime signature"
exit 0
