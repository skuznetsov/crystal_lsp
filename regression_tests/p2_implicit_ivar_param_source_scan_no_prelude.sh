#!/usr/bin/env bash
# No-prelude guard for registration-time implicit ivar discovery from
# `def initialize(@field : T)`. The scan must use source-backed `@` detection
# before trusting Parameter flags, because generated stage2 can expose corrupt
# Parameter fields for ordinary untyped params during full-prelude registration.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_implicit_ivar_param_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/implicit_ivar_param.cr"
OUT="$TMP_DIR/implicit_ivar_param"
LOG="$TMP_DIR/implicit_ivar_param.log"

cat >"$SRC" <<'CR'
class Object
end

class Box
  def initialize(@value : Int32)
  end

  def passthrough(x, y)
    x
  end
end

Box.new(1)
CR

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 20 1024 \
  "$SRC" --no-prelude --emit hir --no-link -o "$OUT" >"$LOG" 2>&1

if ! grep -Fq 'func @Box#initialize$Int32' "$OUT.hir"; then
  echo "p2 implicit ivar param regression: missing typed initialize" >&2
  cat "$LOG" >&2
  cat "$OUT.hir" >&2
  exit 1
fi

if ! grep -Fq 'field_set %0.@@value' "$OUT.hir"; then
  echo "p2 implicit ivar param regression: missing source-backed ivar field_set" >&2
  cat "$LOG" >&2
  cat "$OUT.hir" >&2
  exit 1
fi

echo "p2_implicit_ivar_param_source_scan_no_prelude_ok"
