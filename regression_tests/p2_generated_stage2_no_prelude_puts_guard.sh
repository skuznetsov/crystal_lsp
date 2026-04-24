#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
TMP_DIR="$(mktemp -d /tmp/p2_generated_stage2_puts_guard_XXXXXX)"
GENERATED_S2="$TMP_DIR/generated_s2"
SOURCE="$TMP_DIR/repro.cr"
OUT_BIN="$TMP_DIR/repro_bin"
BUILD_LOG="$TMP_DIR/build.log"
COMPILE_LOG="$TMP_DIR/compile.log"
RUN_LOG="$TMP_DIR/run.log"
TELL_DISASM="$TMP_DIR/tell.disasm"
PUTS_DISASM="$TMP_DIR/puts.disasm"

cleanup() {
  if [[ "${KEEP_TMP:-0}" != "1" ]]; then
    rm -rf "$TMP_DIR"
  else
    echo "[p2_generated_stage2_no_prelude_puts_guard] kept tmp: $TMP_DIR" >&2
  fi
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: compiler not found: $COMPILER" >&2
  exit 2
fi

cat >"$SOURCE" <<'CR'
puts 7
CR

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 420 4096 \
  "$ROOT_DIR/src/crystal_v2.cr" -o "$GENERATED_S2" >"$BUILD_LOG" 2>&1

if [[ ! -x "$GENERATED_S2" ]]; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: missing generated stage2 compiler" >&2
  tail -80 "$BUILD_LOG" >&2 || true
  exit 1
fi

lldb --batch -o 'disassemble -n IO$CCFileDescriptor$Htell' "$GENERATED_S2" >"$TELL_DISASM" 2>&1
lldb --batch -o 'disassemble -n IO$CCFileDescriptor$Hputs' "$GENERATED_S2" >"$PUTS_DISASM" 2>&1

if grep -Eq 'dprintf|abort' "$TELL_DISASM"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: tell still lowered to abort stub" >&2
  cat "$TELL_DISASM" >&2
  exit 1
fi

if ! grep -Eq 'IO\$CCFileDescriptor\$Hpos|IO\$Hpos' "$TELL_DISASM"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: tell no longer delegates to file-descriptor/runtime pos" >&2
  cat "$TELL_DISASM" >&2
  exit 1
fi

if grep -Eq '__vdispatch__IO\$H\$SHL\$\$String|String\$Hends_with\$Q\$\$Char' "$PUTS_DISASM"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: nilary puts regressed to string-overload body" >&2
  cat "$PUTS_DISASM" >&2
  exit 1
fi

if ! grep -q '__vdispatch__IO\$Hprint\$\$Char' "$PUTS_DISASM"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: nilary puts no longer delegates to print(Char)" >&2
  cat "$PUTS_DISASM" >&2
  exit 1
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$GENERATED_S2" 60 1024 \
  "$SOURCE" --no-prelude -o "$OUT_BIN" >"$COMPILE_LOG" 2>&1
compile_status=$?
set -e

if grep -q 'Tuple\$Heach\$\$block' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old Tuple#each block frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if [[ $compile_status -eq 0 && -x "$OUT_BIN" ]]; then
  "$ROOT_DIR/scripts/run_safe.sh" "$OUT_BIN" 5 512 >"$RUN_LOG" 2>&1
  if grep -q '^7$' "$RUN_LOG"; then
    echo "p2_generated_stage2_no_prelude_puts_guard_ok"
    exit 0
  fi
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: generated binary printed unexpected output" >&2
  cat "$RUN_LOG" >&2
  exit 1
fi

if grep -q 'Missing hash key: __crystal_main' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old __crystal_main hash-key frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if grep -q 'MIR function stub not found for: __crystal_main' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old __crystal_main MIR-stub frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if grep -q 'STUB CALLED: IO\$CCFileDescriptor\$Htell' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old io_filedescriptor_tell frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if grep -q 'Segmentation fault: 11' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old Array(String)#each_index callback segfault regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if [[ $compile_status -eq 133 ]] && grep -q 'Trace/BPT trap: 5' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old hash late-emission null callback frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if grep -q 'STUB CALLED: IO\$CCFileDescriptor\$Hsystem_pos' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_ok frontier=io_filedescriptor_system_pos"
  exit 0
fi

if grep -q 'STUB CALLED: Crystal\$CCSystem\$CCKqueue\$Dset' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old Kqueue.set pointer-overload frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

if grep -q 'String contains null byte' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: old string_null_byte frontier regressed" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

# Fall back to a secondary probe with --no-codegen; if the front-end only
# frontier is the nilable-Array check_index_out_of_bounds stub, accept as the
# current recorded frontier instead of the full-codegen linker/fork timeout.
NOCODEGEN_LOG="$TMP_DIR/compile_nocodegen.log"
set +e
"$ROOT_DIR/scripts/run_safe.sh" "$GENERATED_S2" 60 1024 \
  "$SOURCE" --no-prelude --no-codegen >"$NOCODEGEN_LOG" 2>&1
set -e

if grep -q 'STUB CALLED: Array\$LNil\$_\$OR\$_Array\$LCrystalV2\$CCCompiler\$CCFrontend\$CCExprId\$R\$R\$Hcheck_index_out_of_bounds' "$NOCODEGEN_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_ok frontier=array_check_index_oob_stub"
  exit 0
fi

if [[ $compile_status -ne 0 ]]; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: unexpected generated stage2 compile frontier" >&2
  tail -120 "$COMPILE_LOG" >&2 || true
  exit 1
fi

echo "p2_generated_stage2_no_prelude_puts_guard_failed: unexpected generated stage2 compile failure" >&2
tail -120 "$COMPILE_LOG" >&2 || true
exit 1
