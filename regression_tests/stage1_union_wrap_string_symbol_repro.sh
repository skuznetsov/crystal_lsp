#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
OUT_DIR="${TMPDIR:-/tmp}/stage1_union_wrap_string_symbol_repro"
SRC="$OUT_DIR/repro.cr"
BIN="$OUT_DIR/repro.bin"
BIN_FALLBACK="${SRC%.cr}"
RUN_OUT="$OUT_DIR/run.out"
RUN_ERR="$OUT_DIR/run.err"
BUILD_ERR="$OUT_DIR/build.err"

mkdir -p "$OUT_DIR"

cat > "$SRC" <<'CR'
def split_generic_type_args(params_str : String) : Array(String)
  args = [] of String
  args << params_str
  args
end

def sanitize_type_name(name : String) : String
  name
end

def sanitize_type_name_part(name : String) : String
  if info = {base: "A", args: "B,C"}
    args = split_generic_type_args(info[:args]).map do |arg|
      sanitize_type_name(arg.strip)
    end
    name = "#{info[:base]}(#{args.join(", ")})"
  end
  name
end

puts sanitize_type_name_part("X")
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$OUT_DIR/build.out" 2>"$BUILD_ERR"
build_rc=$?
set -e

if [ "$build_rc" -ne 0 ]; then
  echo "reproduced (compile failed)"
  echo "build stderr: $BUILD_ERR"
  exit 0
fi

if [ ! -x "$BIN" ] && [ -x "$BIN_FALLBACK" ]; then
  BIN="$BIN_FALLBACK"
fi

if [ ! -x "$BIN" ]; then
  echo "reproduced (binary not produced)"
  exit 0
fi

set +e
"$BIN" >"$RUN_OUT" 2>"$RUN_ERR"
run_rc=$?
set -e

if [ "$run_rc" -eq 139 ] || grep -qi "segmentation fault" "$RUN_ERR"; then
  echo "reproduced (runtime segfault)"
  echo "runtime stderr: $RUN_ERR"
  exit 0
fi

echo "not reproduced"
exit 1
