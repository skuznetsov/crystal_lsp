#!/usr/bin/env bash
# No-prelude guard for nested module registration. Registration must avoid
# eager body inference, but demanded lowering still has to infer the concrete
# return type for a nested class method.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_nested_module_registration_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/nested_module_registration.cr"
OUT="$TMP_DIR/nested_module_registration"
LOG="$TMP_DIR/nested_module_registration.log"

cat >"$SRC" <<'CR'
class Object
end

module M
  module N
    def self.value
      7
    end
  end
end

M::N.value
CR

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 20 1024 \
  "$SRC" --no-prelude --emit hir --no-link -o "$OUT" >"$LOG" 2>&1

if ! grep -q 'func @M::N.value() -> 4' "$OUT.hir"; then
  echo "p2 nested module regression: demanded lowering did not infer Int32 return" >&2
  cat "$LOG" >&2
  cat "$OUT.hir" >&2
  exit 1
fi

echo "p2_nested_module_registration_no_prelude_ok"
