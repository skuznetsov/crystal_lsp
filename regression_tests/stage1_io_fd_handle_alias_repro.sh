#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-/tmp/stage1_dbg_aliasfix}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d /tmp/stage1_io_fd_handle_alias.XXXXXX)"
SRC="$TMP_DIR/io_fd_handle_alias_repro.cr"
BIN="$TMP_DIR/io_fd_handle_alias_repro"
HIR="${BIN}.hir"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
puts "ok"
CR

set +e
"$COMPILER" build "$SRC" --emit hir -o "$BIN" >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "reproduced (compile failed)"
  echo "compiler status=$compile_status"
  head -n 30 "$TMP_DIR/compile.err"
  exit 1
fi

if [[ ! -f "$HIR" ]]; then
  echo "reproduced (hir not emitted)"
  exit 1
fi

if ! rg -q 'func @Atomic\(Int32\)#get\$Atomic::Ordering' "$HIR"; then
  echo "reproduced (Atomic(Int32)#get specialization missing)"
  exit 1
fi

if awk '
  /func @IO::FileDescriptor#fd/ {in_fd=1}
  in_fd && /^}/ {in_fd=0}
  in_fd && /Atomic\(Handle\)#get\$Atomic::Ordering/ {bad=1}
  END {exit bad ? 0 : 1}
' "$HIR"; then
  echo "reproduced (IO::FileDescriptor#fd resolved to Atomic(Handle)#get)"
  exit 1
fi

set +e
run_output="$("$ROOT/scripts/run_safe.sh" "$BIN" 5 512 2>&1)"
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  echo "reproduced (runtime failed)"
  echo "runtime status=$run_status"
  echo "$run_output" | tail -n 30
  exit 1
fi

if ! echo "$run_output" | rg -q '^ok$'; then
  echo "reproduced (unexpected runtime output)"
  echo "$run_output"
  exit 1
fi

echo "not reproduced"
