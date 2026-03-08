#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/case_type_narrowing.XXXXXX")"
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
module Outer
  abstract class Node
  end

  class NumberNode < Node
  end

  class OtherNode < Node
  end

  def self.kind(node : NumberNode)
    "number"
  end

  def self.kind(node : OtherNode)
    "other"
  end

  def self.kind(node : Node)
    "base"
  end

  def self.run
    node = NumberNode.new.as(Node)
    puts node.is_a?(NumberNode)
    puts(case node
         when NumberNode
           kind(node)
         else
           "miss"
         end)
    puts(case node
         when Outer::NumberNode
           kind(node)
         else
           "miss"
         end)
  end
end

Outer.run
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

expected_fixed=$'true\nnumber\nnumber'
expected_bug=$'true\nmiss\nmiss'

if [[ "$stdout_text" == "$expected_fixed" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ "$stdout_text" == "$expected_bug" ]]; then
  echo "reproduced: case when Type misses type matching and narrowing for nested identifier/path conditions"
  exit 0
fi

echo "unexpected output"
cat "$RUN_OUT"
exit 2
