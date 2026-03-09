#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/frontend-node-kind.XXXXXX")"
COMPILE_STDOUT="$TMP_DIR/compile.stdout"
COMPILE_STDERR="$TMP_DIR/compile.stderr"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

write_probe() {
  local path="$1"
  local mode="$2"

  cat >"$path" <<CR
require "${ROOT_DIR}/src/compiler/bootstrap_shims"
require "${ROOT_DIR}/src/compiler/frontend/ast"

span = CrystalV2::Compiler::Frontend::Span.zero
CR

  if [[ "$mode" == "direct" ]]; then
    cat >>"$path" <<'CR'
node = CrystalV2::Compiler::Frontend::MacroIfNode.new(
  span,
  CrystalV2::Compiler::Frontend::ExprId.new(0),
  CrystalV2::Compiler::Frontend::ExprId.new(1),
  nil
)
base = node.as(CrystalV2::Compiler::Frontend::Node)
CR
  else
    cat >>"$path" <<'CR'
arena = CrystalV2::Compiler::Frontend::AstArena.new
id = arena.add_typed(
  CrystalV2::Compiler::Frontend::MacroIfNode.new(
    span,
    CrystalV2::Compiler::Frontend::ExprId.new(0),
    CrystalV2::Compiler::Frontend::ExprId.new(1),
    nil
  )
)
base = arena[id]
CR
  fi

  cat >>"$path" <<'CR'
puts base.node_kind
puts CrystalV2::Compiler::Frontend.node_kind(base)
puts base.is_a?(CrystalV2::Compiler::Frontend::MacroIfNode)
case base
when CrystalV2::Compiler::Frontend::MacroIfNode
  puts true
else
  puts false
end
CR
}

expected=$'MacroIf\nMacroIf\ntrue\ntrue'
all_ok=1

for mode in direct arena; do
  src="$TMP_DIR/${mode}_macroif.cr"
  bin="$TMP_DIR/${mode}_macroif.bin"
  run_log="$TMP_DIR/${mode}.run.log"

  write_probe "$src" "$mode"

  set +e
  "$COMPILER" "$src" -o "$bin" >"$COMPILE_STDOUT" 2>"$COMPILE_STDERR"
  compile_status=$?
  set -e

  echo "mode: $mode"
  echo "compile_status: $compile_status"

  if [[ $compile_status -ne 0 ]]; then
    echo "--- compile stdout ---"
    cat "$COMPILE_STDOUT"
    echo "--- compile stderr ---"
    cat "$COMPILE_STDERR"
    echo "reproduced: abstract Frontend::Node kind dispatch failed to compile"
    exit 0
  fi

  "$ROOT_DIR/scripts/run_safe.sh" "$bin" 5 256 >"$run_log"
  cat "$run_log"

  stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$run_log" | tr -d '\r')"
  if [[ "$stdout_text" != "$expected" ]]; then
    all_ok=0
  fi
done

if [[ $all_ok -eq 1 ]]; then
  echo "not reproduced"
  exit 1
fi

echo "reproduced: abstract Frontend::Node kind dispatch chose the wrong overload path"
exit 0
