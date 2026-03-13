#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
if [[ ! -x "$COMPILER" ]]; then
  echo "Compiler is not executable: $COMPILER" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_astarena_libnode.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/libnode_probe.cr"
BIN="$TMP_DIR/libnode_probe.bin"
COMPILE_LOG="$TMP_DIR/compile.log"
RUN_LOG="$TMP_DIR/run.log"

cat >"$SRC" <<CR
require "${ROOT_DIR}/src/compiler/bootstrap_shims"
require "${ROOT_DIR}/src/compiler/frontend/ast"

span = CrystalV2::Compiler::Frontend::Span.zero
arena = CrystalV2::Compiler::Frontend::AstArena.new
body = [CrystalV2::Compiler::Frontend::ExprId.new(7)]
id = arena.add_typed(
  CrystalV2::Compiler::Frontend::LibNode.new(
    span,
    "__MacroContext__".to_slice,
    body
  )
)
base = arena[id]
puts base.node_kind
puts CrystalV2::Compiler::Frontend.node_kind(base)
puts base.is_a?(CrystalV2::Compiler::Frontend::LibNode)
case base
when CrystalV2::Compiler::Frontend::LibNode
  puts true
  puts String.new(base.name)
  puts base.body.try(&.size) || 0
else
  puts false
end
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_LOG" 2>&1
compile_rc=$?
set -e

if [[ $compile_rc -eq 0 ]]; then
  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$BIN" 5 512 >"$RUN_LOG" 2>&1
  run_rc=$?
  set -e

  stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_LOG" | tr -d '\r')"
  expected=$'Lib\nLib\ntrue\ntrue\n__MacroContext__\n1'

  if [[ $run_rc -eq 0 && "$stdout_text" == "$expected" ]]; then
    echo "not reproduced (AstArena stored and read back LibNode structurally)"
    exit 0
  fi

  echo "reproduced: compiled AstArena LibNode probe drifted at runtime" >&2
  cat "$RUN_LOG" >&2 || true
  exit 1
fi

if rg -Fq '/src/compiler/frontend/ast.cr' "$COMPILE_LOG" &&
   rg -Fq '/libnode_probe.cr' "$COMPILE_LOG"; then
  echo "reproduced: compiler crashed while compiling the direct AstArena LibNode probe"
  exit 1
fi

echo "inconclusive: compiler exited $compile_rc before the AstArena LibNode frontier" >&2
tail -n 60 "$COMPILE_LOG" >&2 || true
exit 2
