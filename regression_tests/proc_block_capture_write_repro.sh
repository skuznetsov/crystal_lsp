#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/proc_block_capture_write.XXXXXX")"
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
class Registry
  def on(&block : String ->)
    block.call("ok")
  end
end

result = ""
Registry.new.on { |v| result = v }
puts result
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
stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT")"
stdout_trimmed="$(printf '%s' "$stdout_text" | tr -d '\r')"

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "stdout: ${stdout_trimmed:-<empty>}"

if [[ "$stdout_trimmed" == "ok" ]]; then
  echo "not reproduced"
  exit 1
fi

if [[ -z "$stdout_trimmed" ]]; then
  echo "reproduced: capturing block->Proc loses outer write"
  exit 0
fi

echo "unexpected output"
cat "$RUN_OUT"
exit 2
