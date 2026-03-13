#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

BIN="$1"
if [[ ! -x "$BIN" ]]; then
  echo "Compiler is not executable: $BIN" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_ast_to_hir_ctor_probe.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/probe.cr"
OUT_BIN="$TMP_DIR/probe_bin"
COMPILE_LOG="$TMP_DIR/compile.log"
RUN_LOG="$TMP_DIR/run.log"

cat >"$SRC" <<CR
require "${ROOT_DIR}/src/compiler/hir/ast_to_hir"

arena = CrystalV2::Compiler::Frontend::AstArena.new
key = arena.object_id.to_u64
sources = {key => ""} of UInt64 => String
paths = {key => "/tmp/direct"} of UInt64 => String
main_arenas = [arena.as(CrystalV2::Compiler::Frontend::ArenaLike)]
hir_mod = Crystal::HIR::Module.new("main")
hir_mod.bootstrap_reinitialize_runtime_state
conv = Crystal::HIR::AstToHir.new(
  arena,
  "main",
  sources,
  paths,
  main_arenas,
  hir_module: hir_mod,
  link_libraries: [] of String,
)
puts "const_lit=#{conv.constant_literal_values.size}"
puts "const_types=#{conv.constant_types.size}"
CR

set +e
"$BIN" "$SRC" -o "$OUT_BIN" >"$COMPILE_LOG" 2>&1
compile_rc=$?
set -e

if [[ $compile_rc -eq 0 ]]; then
  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$OUT_BIN" 5 1024 >"$RUN_LOG" 2>&1
  run_rc=$?
  set -e

  if [[ $run_rc -eq 0 ]] &&
     rg -Fq 'const_lit=0' "$RUN_LOG" &&
     rg -Fq 'const_types=0' "$RUN_LOG"; then
    echo "not reproduced (direct AstToHir constructor probe stayed clean)"
    exit 0
  fi

  echo "inconclusive: direct AstToHir constructor probe built but runtime output drifted" >&2
  cat "$RUN_LOG" >&2 || true
  exit 2
fi

if rg -Fq '/src/compiler/hir/ast_to_hir.cr' "$COMPILE_LOG" &&
   rg -Fq '[DEBUG_STAGE1] hir_converter=' "$COMPILE_LOG"; then
  echo "reproduced: compiler crashed while compiling the direct AstToHir constructor probe"
  exit 1
fi

echo "inconclusive: compiler exited $compile_rc before reaching the direct AstToHir probe frontier" >&2
tail -n 60 "$COMPILE_LOG" >&2 || true
exit 2
