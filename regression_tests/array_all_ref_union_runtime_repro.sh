#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/array_all_ref_union_runtime.XXXXXX")"
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
class A
  def initialize(@x : Int32)
  end
end

class B
  getter y

  def initialize(@y : Int32)
  end
end

alias U = A | B

arr = [] of U
arr << A.new(1)
arr << B.new(2)

value0 = arr[0]
value1 = arr[1]

puts value0.is_a?(A)
puts value1.is_a?(B)
puts value1.class.name

case value1
when B
  puts value1.y
else
  puts "miss"
end
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
./scripts/run_safe.sh "$BIN" 10 512 >"$RUN_OUT"
run_status=$?
set -e
stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT" | tr -d '\r')"
stderr_text="$(awk '/^=== STDERR ===/{flag=1;next}flag' "$RUN_OUT" | tr -d '\r')"
combined_text="$(cat "$RUN_OUT")"

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "run_status: $run_status"
echo "stdout:"
printf '%s\n' "$stdout_text"

expected=$'true\ntrue\nB\n2'
if [[ $run_status -eq 0 && "$stdout_text" == "$expected" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ $run_status -eq 0 ]]; then
  line1="$(printf '%s\n' "$stdout_text" | sed -n '1p')"
  line2="$(printf '%s\n' "$stdout_text" | sed -n '2p')"
  line3="$(printf '%s\n' "$stdout_text" | sed -n '3p')"
  line4="$(printf '%s\n' "$stdout_text" | sed -n '4p')"
  if [[ "$line1" == "true" && "$line2" == "true" && "$line4" == "2" ]]; then
    if [[ "$line3" == "A | B" ]]; then
      echo "not reproduced: stride crash fixed, residual class.name still reports union"
      exit 1
    fi
  fi
fi

if grep -Fxq "true" <<<"$stdout_text" && grep -Eq 'Segmentation fault|Segfault|signal 11|status: 139|exit 139|EXC_BAD_ACCESS' <<<"$stderr_text"$'\n'"$combined_text"; then
  echo "reproduced: Array(A | B) mixes pointer-sized and union-sized element strides"
  exit 0
fi

echo "reproduced: unexpected output"
cat "$RUN_OUT"
exit 0
