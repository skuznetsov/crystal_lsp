#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hash_array_tuple_union.XXXXXX")"
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
alias ArenaLike = Int32 | Int64

h = {} of String => Array(Tuple(Int32, ArenaLike))
h["x"] = [{1, 1_i32.as(ArenaLike)}]

puts h["x"].size
entry = h["x"][0]
value = entry[1]
puts(value.is_a?(Int32) ? 100 + value : 0)
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

expected_fixed=$'1\n101'
expected_bug=$'4\n101'

if [[ "$stdout_text" == "$expected_fixed" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ "$stdout_text" == "$expected_bug" ]]; then
  echo "reproduced: Hash(String, Array(Tuple(Int32, Int32|Int64))) corrupts stored array shape"
  exit 0
fi

echo "unexpected output"
cat "$RUN_OUT"
exit 2
