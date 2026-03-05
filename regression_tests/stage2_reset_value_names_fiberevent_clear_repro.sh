#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <stage2_debug_compiler>" >&2
  exit 2
fi

COMPILER="$1"
if [[ ! -x "$COMPILER" ]]; then
  echo "Compiler is not executable: $COMPILER" >&2
  exit 2
fi

if ! command -v lldb >/dev/null 2>&1; then
  echo "lldb is required for this repro script" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/stage2_reset_value_names_fiberevent.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"
DIS="$TMP_DIR/disasm.txt"
LLDB_CMDS="$TMP_DIR/lldb.cmd"

cat >"$SRC" <<'CR'
macro x
end
CR

set +e
STAGE2_DEBUG=1 CRYSTAL_V2_MIR_SETUP_TRACE=1 CRYSTAL_V2_LLVM_SETUP_TRACE=1 \
  "$COMPILER" "$SRC" --no-prelude --no-link -o "$BIN" >"$OUT" 2>"$ERR"
compile_status=$?
set -e

cat >"$LLDB_CMDS" <<'LL'
disassemble -n Crystal$CCMIR$CCLLVMIRGenerator$Hreset_value_names$$Crystal$CCMIR$CCFunction
quit
LL

# disassemble may fail (e.g., no symbol/debug info); treat as non-repro, not script failure
if ! lldb -b -s "$LLDB_CMDS" -- "$COMPILER" >"$DIS" 2>&1; then
  true
fi

fiber_call_count=$(grep -c 'Crystal\$CCEventLoop\$CCPolling\$CCFiberEvent\$Hclear' "$DIS" || true)

if [[ $compile_status -eq 139 ]] && \
   grep -q "\\[MIR_SETUP\\] before lowering.new" "$ERR" && \
   ! grep -q "\\[LLVM_SETUP\\] generate(io) done" "$ERR" && \
   grep -q "reset_value_names" "$DIS" && \
   [[ "$fiber_call_count" -gt 0 ]]; then
  echo "reproduced: stage2 segfault + reset_value_names disasm shows FiberEvent#clear target drift"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "fiber_clear_calls_in_reset_value_names: $fiber_call_count"
  echo "tmp_dir: $TMP_DIR"
  exit 0
fi

echo "not reproduced"
echo "compiler: $COMPILER"
echo "status: $compile_status"
echo "fiber_clear_calls_in_reset_value_names: $fiber_call_count"
echo "tmp_dir: $TMP_DIR"
echo "--- stderr tail ---"
tail -n 120 "$ERR" || true
echo "--- disasm tail ---"
tail -n 120 "$DIS" || true
exit 1
