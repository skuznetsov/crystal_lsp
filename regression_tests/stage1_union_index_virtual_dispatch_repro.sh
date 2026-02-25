#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_current}"
TMP_DIR="$(mktemp -d /tmp/stage1_union_index_vdispatch.XXXXXX)"
SRC="$TMP_DIR/union_index_vdispatch_repro.cr"
BIN="$TMP_DIR/union_index_vdispatch_repro"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
class A
  def [](i : Int32)
    puts "A:#{i}"
  end
end

class B
  def [](i : Int32)
    puts "B:#{i}"
  end
end

u = B.new.as(A | B)
u[7]
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 20 "$TMP_DIR/compile.err"
  exit 1
fi

if [[ ! -x "$BIN" ]]; then
  # Current stage2 path can ignore -o and emit next to source.
  if [[ -x "$TMP_DIR/union_index_vdispatch_repro" ]]; then
    BIN="$TMP_DIR/union_index_vdispatch_repro"
  fi
fi

set +e
output="$("$BIN" 2>&1)"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  echo "$output" | head -n 20
  exit 1
fi

if [[ "$output" == "B:7" ]]; then
  echo "not reproduced"
  exit 0
fi

if [[ "$output" == "A:7" || -z "$output" ]]; then
  echo "reproduced (wrong virtual dispatch output)"
  echo "$output"
  exit 1
fi

echo "reproduced (unexpected output)"
echo "$output"
exit 1
