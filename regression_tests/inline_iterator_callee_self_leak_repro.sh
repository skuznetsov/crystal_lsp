#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/inline_iterator_callee_self_leak.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_OUT="$TMP_DIR/compile.out"
COMPILE_ERR="$TMP_DIR/compile.err"
RUN_OUT="$TMP_DIR/run.out"

cat >"$SRC" <<'CR'
class Iter
  def each_token(&block : Int32 ->)
    i = 0
    while true
      block.call i
      break if i == 2
      i += 1
    end
  end
end

class Holder
  def initialize
    @tokens = [] of Int32
  end

  def run(iter : Iter)
    iter.each_token { |token| @tokens << token }
    puts @tokens.size
    puts @tokens.join(",")
  end
end

Holder.new.run(Iter.new)
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_OUT" 2>"$COMPILE_ERR"
status=$?
set -e

if [[ $status -ne 0 ]]; then
  echo "compile failed"
  echo "compiler: $COMPILER"
  echo "status: $status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- stderr ---"
  cat "$COMPILE_ERR"
  echo "--- stdout ---"
  cat "$COMPILE_OUT"
  exit 2
fi

set +e
./scripts/run_safe.sh "$BIN" 5 256 >"$RUN_OUT" 2>&1
status=$?
set -e

if [[ $status -eq 139 ]] || grep -q "\\[CRASH\\]" "$RUN_OUT"; then
  echo "reproduced: callee self leaked into caller after iterator break"
  echo "compiler: $COMPILER"
  echo "tmp_dir: $TMP_DIR"
  echo "--- runtime ---"
  cat "$RUN_OUT"
  exit 0
fi

expected=$'=== STDOUT ===\n3\n0,1,2\n=== STDERR ===\n[EXIT: 0] after ~0s'
actual="$(cat "$RUN_OUT")"
if [[ "$actual" == "$expected" ]]; then
  echo "not reproduced"
  echo "compiler: $COMPILER"
  echo "tmp_dir: $TMP_DIR"
  echo "stdout:"
  echo "3"
  echo "0,1,2"
  exit 1
fi

echo "unexpected output"
echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "--- runtime ---"
cat "$RUN_OUT"
exit 3
