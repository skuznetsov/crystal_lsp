#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/p2_prior_nil_guard_infer.XXXXXX")"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$TMP_DIR/prior_nil_guard_infer.cr" <<'CR'
class ExprId
end

class Box
  def maybe : ExprId | Nil
    ExprId.new
  end
end

def consume(id : ExprId) : Nil
end

def helper(box : Box)
  value = box.maybe
  return ExprId.new unless value
  expr_id = value
  expr_id
end

consume(helper(Box.new))
CR

CRYSTAL_V2_STOP_AFTER_HIR=1 "$COMPILER" "$TMP_DIR/prior_nil_guard_infer.cr" \
  --no-prelude --emit hir -o "$TMP_DIR/out" > "$TMP_DIR/compile.log" 2>&1

expr_id_type="$(awk '/= Class ExprId$/ { sub(/^type[.]/, "", $1); print $1; exit }' "$TMP_DIR/out.hir")"
box_type="$(awk '/= Class Box$/ { sub(/^type[.]/, "", $1); print $1; exit }' "$TMP_DIR/out.hir")"
if [[ -z "$expr_id_type" || -z "$box_type" ]]; then
  echo "p2 prior nil guard inference regression: could not find ExprId type id" >&2
  sed -n '1,80p' "$TMP_DIR/out.hir" >&2
  exit 1
fi

if ! grep -Fq "func @helper"'$'"Box(%0: ${box_type}) -> ${expr_id_type}" "$TMP_DIR/out.hir"; then
  echo "p2 prior nil guard inference regression: helper did not infer concrete ExprId return" >&2
  grep -En 'func @helper|call consume' "$TMP_DIR/out.hir" >&2 || true
  exit 1
fi

if ! grep -Fq 'call consume$ExprId' "$TMP_DIR/out.hir"; then
  echo "p2 prior nil guard inference regression: consume call kept a nilable ExprId argument" >&2
  grep -En 'call consume' "$TMP_DIR/out.hir" >&2 || true
  exit 1
fi

if grep -Eq 'call consume[$]Nil [|] ExprId|call consume[$]ExprId [|] Nil' "$TMP_DIR/out.hir"; then
  echo "p2 prior nil guard inference regression: nilable consume target leaked into HIR" >&2
  grep -En 'call consume' "$TMP_DIR/out.hir" >&2 || true
  exit 1
fi

echo "p2_prior_nil_guard_infer_no_prelude_ok"
