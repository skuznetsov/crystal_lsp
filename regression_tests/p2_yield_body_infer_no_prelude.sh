#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/p2_yield_body_infer.XXXXXX")"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$TMP_DIR/yield_body_infer.cr" <<'CR'
struct SpinLike
  def lock
  end

  def unlock
  end

  def sync(&)
    lock
    begin
      yield
    ensure
      unlock
    end
  end
end

def use_spin
  spin = SpinLike.new
  spin.sync { 7 }
end

use_spin
CR

CRYSTAL_V2_STOP_AFTER_HIR=1 "$COMPILER" "$TMP_DIR/yield_body_infer.cr" \
  --no-prelude --emit hir -o "$TMP_DIR/out" > "$TMP_DIR/compile.log" 2>&1

spin_type="$(awk '/= Struct SpinLike$/ { sub(/^type[.]/, "", $1); print $1; exit }' "$TMP_DIR/out.hir")"
if [[ -z "$spin_type" ]]; then
  echo "p2 yield body inference regression: could not find SpinLike type id" >&2
  sed -n '1,100p' "$TMP_DIR/out.hir" >&2
  exit 1
fi

if ! grep -Fq "func @use_spin() ->" "$TMP_DIR/out.hir" ||
   ! grep -Fq "call %3.SpinLike#lock()" "$TMP_DIR/out.hir" ||
   ! grep -Fq "call %3.SpinLike#unlock()" "$TMP_DIR/out.hir" ||
   ! grep -Fq "literal 7 : Int32" "$TMP_DIR/out.hir"; then
  echo "p2 yield body inference regression: sync(&) did not lower its yield/ensure body as expected" >&2
  grep -En 'func @use_spin|SpinLike#lock|SpinLike#unlock|literal 7|SpinLike#sync' "$TMP_DIR/out.hir" >&2 || true
  exit 1
fi

echo "p2_yield_body_infer_no_prelude_ok"
