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

TMP_DIR="$(mktemp -d /tmp/stage2_emit_function_blockid_clear.XXXXXX)"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
OUT="$TMP_DIR/out.txt"
ERR="$TMP_DIR/err.txt"
BT="$TMP_DIR/lldb_bt.txt"
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

cat >"$LLDB_CMDS" <<LL
settings set target.env-vars STAGE2_DEBUG=1 CRYSTAL_V2_MIR_SETUP_TRACE=1 CRYSTAL_V2_LLVM_SETUP_TRACE=1
run "$SRC" --no-prelude --no-link -o "$BIN"
bt
quit
LL

# LLDB run may fail if symbols are unavailable; treat as non-repro, not script failure.
if ! lldb -b -s "$LLDB_CMDS" -- "$COMPILER" >"$BT" 2>&1; then
  true
fi

if [[ $compile_status -eq 139 ]] && \
   grep -q "\\[MIR_SETUP\\] before lowering.new" "$ERR" && \
   grep -q "\\[LLVM_GEN\\] emit_function start __crystal_main" "$ERR" && \
   ! grep -q "\\[LLVM_SETUP\\] generate(io) done" "$ERR" && \
   grep -q 'Hash\$LCrystal\$CCMIR\$CCBlockId\$C\$_Nil\$R\$Hclear_impl' "$BT" && \
   grep -q 'Set\$LCrystal\$CCHIR\$CCBlockId\$R\$Hclear' "$BT" && \
   grep -q 'Crystal\$CCMIR\$CCLLVMIRGenerator\$Hemit_function' "$BT"; then
  echo "reproduced: stage2 segfault in emit_function clear path (BlockId hash/set clear)"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  exit 0
fi

echo "not reproduced"
echo "compiler: $COMPILER"
echo "status: $compile_status"
echo "tmp_dir: $TMP_DIR"
echo "--- stderr tail ---"
tail -n 140 "$ERR" || true
echo "--- lldb bt tail ---"
tail -n 140 "$BT" || true
exit 1
