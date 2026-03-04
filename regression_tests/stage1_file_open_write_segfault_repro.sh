#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <stage1_compiler>" >&2
  exit 2
fi

COMPILER="$1"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_file_open_write_segfault.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro_bin"
COMPILE_OUT="$TMP_DIR/compile.out"
COMPILE_ERR="$TMP_DIR/compile.err"
RUN_LOG="$TMP_DIR/run.log"

cat >"$SRC" <<'CR'
path = "/tmp/stage1_file_open_write_segfault_probe.txt"
File.open(path, "w") do |io|
  io << "ok\n"
end
puts "done"
CR

set +e
"$COMPILER" "$SRC" --release -o "$BIN" >"$COMPILE_OUT" 2>"$COMPILE_ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "not reproduced (compile failed before runtime)"
  echo "compiler: $COMPILER"
  echo "compile_status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- compile stderr ---"
  tail -n 120 "$COMPILE_ERR"
  exit 1
fi

set +e
bash "$ROOT/scripts/run_safe.sh" "$BIN" 5 512 >"$RUN_LOG" 2>&1
run_status=$?
set -e

if [[ $run_status -eq 139 ]] && grep -q "\\[CRASH\\] Segfault (exit 139)" "$RUN_LOG"; then
  echo "reproduced: stage1-compiled File.open(\"w\") runtime segfault"
  echo "compiler: $COMPILER"
  echo "compile_status: $compile_status"
  echo "run_status: $run_status"
  echo "tmp_dir: $TMP_DIR"
  exit 0
fi

echo "not reproduced"
echo "compiler: $COMPILER"
echo "compile_status: $compile_status"
echo "run_status: $run_status"
echo "tmp_dir: $TMP_DIR"
echo "--- run log ---"
tail -n 120 "$RUN_LOG"
exit 1
