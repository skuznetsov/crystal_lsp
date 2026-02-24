#!/bin/bash
# Repro: stage1 can mis-lower macro-if OptionParser ast-cache flags into bare
# `_AST` / `_cache` calls (link failure).
# Usage: regression_tests/stage1_macro_if_ast_cache_link_repro.sh <compiler>

set -euo pipefail

BIN="${1:-}"
if [ -z "$BIN" ] || [ ! -x "$BIN" ]; then
  echo "usage: $0 <compiler>"
  exit 2
fi

SRC="$(mktemp /tmp/repro_macro_if_ast_cache.XXXXXX.cr)"
OUT="${SRC%.cr}.bin"
LOG="${SRC%.cr}.log"
trap 'rm -f "$SRC" "$OUT" "$OUT.o" "$OUT.ll" "$LOG"' EXIT

cat > "$SRC" <<'CR'
require "option_parser"

class Opts
  property ast_cache : Bool = false
end

opts = Opts.new
parser = OptionParser.new do |p|
  {% if flag?(:bootstrap_fast) %}
  p.on("--ast-cache", "Ignored in bootstrap_fast") { opts.ast_cache = false }
  p.on("--no-ast-cache", "Ignored in bootstrap_fast") { opts.ast_cache = false }
  {% else %}
  p.on("--ast-cache", "Enable AST cache (file-based)") { opts.ast_cache = true }
  p.on("--no-ast-cache", "Disable AST cache (file-based)") { opts.ast_cache = false }
  {% end %}
end
CR

set +e
"$BIN" --release "$SRC" -o "$OUT" >"$LOG" 2>&1
status=$?
set -e

if [ $status -ne 0 ] && rg -q "_AST|_cache" "$LOG"; then
  echo "reproduced: unresolved _AST/_cache from macro-if OptionParser ast-cache flags"
  exit 0
fi

echo "not reproduced"
cat "$LOG"
exit 1
