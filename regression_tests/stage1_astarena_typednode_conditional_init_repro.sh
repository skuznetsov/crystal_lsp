#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

COMPILER="$1"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKDIR="$(mktemp -d "$ROOT_DIR/.tmp_astarena_typednode_repro.XXXXXX")"
trap 'rm -rf "$WORKDIR"' EXIT

SRC="$WORKDIR/repro.cr"
OUT="$WORKDIR/repro_bin"

cat >"$SRC" <<'CR'
require "../src/compiler/frontend/ast"

arena = CrystalV2::Compiler::Frontend::AstArena.new
STDERR.puts "before-add"
node = CrystalV2::Compiler::Frontend::NilNode.new(CrystalV2::Compiler::Frontend::Span.zero)
id = arena.add_typed(node)
STDERR.puts "after-add #{id.index} size=#{arena.size}"
CR

if ! "$COMPILER" --release "$SRC" -o "$OUT"; then
  echo "reproduced: compiler failed to build AstArena typed-node init oracle" >&2
  exit 1
fi

set +e
RUN_OUTPUT="$("$ROOT_DIR/scripts/run_safe.sh" "$OUT" 5 1024 2>&1)"
RUN_RC=$?
set -e

printf '%s\n' "$RUN_OUTPUT"

if [[ $RUN_RC -eq 0 ]] && grep -q "after-add 0 size=1" <<<"$RUN_OUTPUT"; then
  echo "not reproduced: AstArena typed-node conditional-init add path is stable"
  exit 0
fi

echo "reproduced: AstArena typed-node conditional-init add path crashed or failed"
exit 1
