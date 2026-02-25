#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-}"
if [[ -z "$COMPILER" || ! -x "$COMPILER" ]]; then
  echo "usage: $0 <compiler>"
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/stage1_namedtuple_literal_index.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro_bin"
BUILD_LOG="$TMP_DIR/build.log"
RUN_LOG="$TMP_DIR/run.log"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
def split(name : String) : NamedTuple(base: String, args: String)?
  if idx = name.index('(')
    {base: name[0, idx], args: name[(idx + 1)...-1]}
  else
    nil
  end
end

def test(name : String)
  if info = split(name)
    puts info[:args]
  else
    puts "nil"
  end
end

test("Foo(Bar)")
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$BUILD_LOG" 2>&1
build_status=$?
set -e

if [[ $build_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  tail -n 40 "$BUILD_LOG"
  exit 1
fi

if [[ ! -x "$BIN" ]]; then
  if [[ -x "${SRC%.cr}" ]]; then
    BIN="${SRC%.cr}"
  else
    echo "reproduced (no output binary)"
    tail -n 40 "$BUILD_LOG"
    exit 1
  fi
fi

set +e
scripts/run_safe.sh "$BIN" 5 256 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if [[ $run_status -eq 0 ]] && rg -q "^Bar$" "$RUN_LOG"; then
  echo "not reproduced"
  exit 0
fi

echo "reproduced"
tail -n 80 "$RUN_LOG"
exit 1
