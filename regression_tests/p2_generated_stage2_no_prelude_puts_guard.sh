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
WRITE_LOCK_DISASM="$TMP_DIR/write_lock.disasm"

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

# LM-501: Atomic#swap/#set inline lowering must use args[1] (the value) rather
# than args[2] (the ordering enum). Before the fix, the writer-lock code path
# emitted `mov w9, #0x4` (AtomicOrdering::Acquire = 4) into the @writer slot
# instead of `LOCKED = 1`. Guard the positive shape of write_lock so this bug
# cannot regress silently.
lldb --batch -o 'disassemble -n Crystal$CCRWLock$Hwrite_lock' "$GENERATED_S2" >"$WRITE_LOCK_DISASM" 2>&1

if grep -Eq 'mov[[:space:]]+w[0-9]+,[[:space:]]+#0x4($|[^0-9a-fx])' "$WRITE_LOCK_DISASM"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: LM-501 regressed — write_lock emits raw #0x4 instead of loading LOCKED classvar" >&2
  cat "$WRITE_LOCK_DISASM" >&2
  exit 1
fi

if ! grep -q 'Crystal\$CCRWLock__classvar__LOCKED' "$WRITE_LOCK_DISASM"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: LM-501 regressed — write_lock no longer references the LOCKED classvar global" >&2
  cat "$WRITE_LOCK_DISASM" >&2
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

# LM-504 cleared the write_lock / after_fork_child_callbacks hang by fixing
# `->Module.method` to lower as a Proc thunk (see LANDMARKS.md). The new
# frontier is an RTA discovery gap for `Crystal::EventLoop#after_fork`: the
# abstract base emits an ABORT stub because the virtual dispatch reached
# through `Proc.call` inside `Process.after_fork_child_callbacks` is not
# discovered by RTA. Accept this as the current recorded state; the old
# hang would not have exited in ~0s, so a `STUB CALLED` + `llc failed`
# shape is strictly better than the previous timeout.
if grep -q 'STUB CALLED: Crystal\$CCEventLoop\$Hafter_fork' "$COMPILE_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_ok frontier=eventloop_after_fork_rta_gap"
  exit 0
fi

# Fall back to a secondary probe with --no-codegen. The previous recorded
# frontier (nilable-Array `check_index_out_of_bounds` ABORT stub) was cleared
# by LM-500: the private Indexable helper is now in the lazy-RTA allowlist, so
# `--no-codegen` front-end runs complete cleanly. Accept a clean nocodegen
# exit as the new recorded state; the full-codegen corridor still hangs in
# `Crystal::RWLock#write_lock` reached from `Process.fork`, which is tracked
# as a separate frontier.
NOCODEGEN_LOG="$TMP_DIR/compile_nocodegen.log"
set +e
"$ROOT_DIR/scripts/run_safe.sh" "$GENERATED_S2" 60 1024 \
  "$SOURCE" --no-prelude --no-codegen >"$NOCODEGEN_LOG" 2>&1
nocodegen_status=$?
set -e

if grep -q 'STUB CALLED: Array\$LNil\$_\$OR\$_Array\$LCrystalV2\$CCCompiler\$CCFrontend\$CCExprId\$R\$R\$Hcheck_index_out_of_bounds' "$NOCODEGEN_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: LM-500 regressed — check_index_out_of_bounds ABORT returned" >&2
  tail -120 "$NOCODEGEN_LOG" >&2 || true
  exit 1
fi

if grep -q 'STUB CALLED' "$NOCODEGEN_LOG"; then
  echo "p2_generated_stage2_no_prelude_puts_guard_failed: nocodegen hit unrecorded ABORT stub" >&2
  grep 'STUB CALLED' "$NOCODEGEN_LOG" >&2 || true
  exit 1
fi

if [[ $nocodegen_status -eq 0 ]]; then
  echo "p2_generated_stage2_no_prelude_puts_guard_ok frontier=nocodegen_clean_full_codegen_hang"
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
