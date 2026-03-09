#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/namespaced_symbol_shadow.XXXXXX")"
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
module M
  abstract class Symbol
    getter name

    def initialize(@name : String)
    end
  end

  class MethodSymbol < Symbol
    getter value

    def initialize(name : String, @value : Int32)
      super(name)
    end
  end

  class SymbolTable
    @symbols = {} of String => Symbol

    def define(name : String, symbol : Symbol)
      @symbols[name] = symbol
    end

    def lookup(name : String)
      @symbols[name]?
    end
  end
end

t = M::SymbolTable.new
sym = M::MethodSymbol.new("foo", 123)
t.define("foo", sym)
v = t.lookup("foo")
puts v.nil?
puts v.not_nil!.name
puts v.class.name
puts v.is_a?(M::MethodSymbol)
puts v.as(M::MethodSymbol).value
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

set +e
./scripts/run_safe.sh "$BIN" 5 256 >"$RUN_OUT"
run_status=$?
set -e
stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT" | tr -d '\r')"
line1="$(printf '%s\n' "$stdout_text" | sed -n '1p')"
line2="$(printf '%s\n' "$stdout_text" | sed -n '2p')"
line4="$(printf '%s\n' "$stdout_text" | sed -n '4p')"
line5="$(printf '%s\n' "$stdout_text" | sed -n '5p')"

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "stdout:"
printf '%s\n' "$stdout_text"

if [[ $run_status -eq 0 && "$line1" == "false" && "$line2" == "foo" && "$line4" == "true" && "$line5" == "123" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ $run_status -ne 0 ]] && grep -Fq "[CRASH]" "$RUN_OUT"; then
  echo "reproduced: namespaced Symbol shadow still degrades to builtin Symbol ABI/layout"
  exit 0
fi

echo "reproduced: namespaced Symbol shadow returned unexpected runtime result"
exit 0
