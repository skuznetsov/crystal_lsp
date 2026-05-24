#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/p2_nil_block_proc.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/repro"
LOG="$TMP_DIR/compile.log"
PROC_HIR="$TMP_DIR/block_proc.hir"

cat > "$SRC" <<'CR'
def consume(&block : String ->) : Nil
  block.call("x")
end

class Token
  def initialize
  end
end

consume do |owner|
  Token.new
end
CR

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 60 2048 \
  "$SRC" --no-prelude --emit=hir --no-link -o "$OUT" >"$LOG" 2>&1

HIR="$OUT.hir"
if [[ ! -f "$HIR" ]]; then
  echo "missing HIR output" >&2
  tail -80 "$LOG" >&2 || true
  exit 1
fi

token_id="$(awk '/type\.[0-9]+ = Class Token/ { sub("type\\.", "", $1); print $1; exit }' "$HIR")"
if [[ -z "$token_id" ]]; then
  echo "missing Token type in HIR" >&2
  rg -n 'Class Token|__crystal_block_proc|Proc' "$HIR" >&2 || true
  exit 1
fi

awk '
  /^func @__crystal_block_proc_/ { inside = 1 }
  inside { print }
  inside && /^}/ { exit }
' "$HIR" > "$PROC_HIR"

if [[ ! -s "$PROC_HIR" ]]; then
  echo "missing raw block proc HIR" >&2
  rg -n '__crystal_block_proc|Proc' "$HIR" >&2 || true
  exit 1
fi

if grep -Eq "type\\.[0-9]+ = Proc Proc\\(15, ${token_id}\\)" "$HIR"; then
  echo "raw block proc type kept the incidental block body return type" >&2
  rg -n "Proc Proc\\(15, ${token_id}\\)|__crystal_block_proc" "$HIR" >&2 || true
  exit 1
fi

if grep -Eq "^func @__crystal_block_proc_.* -> ${token_id} \\{" "$PROC_HIR"; then
  echo "raw block proc function returns Token instead of Nil" >&2
  cat "$PROC_HIR" >&2
  exit 1
fi

if ! grep -Eq '^func @__crystal_block_proc_.* -> 16 \{' "$PROC_HIR"; then
  echo "raw block proc function is not Nil-returning" >&2
  cat "$PROC_HIR" >&2
  exit 1
fi

if ! grep -Eq 'return %[0-9]+' "$PROC_HIR" || ! grep -q 'literal nil : Nil' "$PROC_HIR"; then
  echo "raw block proc does not terminate with an explicit nil value" >&2
  cat "$PROC_HIR" >&2
  exit 1
fi

echo "p2_nil_return_block_proc_no_prelude_ok"
