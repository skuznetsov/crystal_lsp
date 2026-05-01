#!/usr/bin/env bash
# No-prelude guard for enum class setters whose implicit return is a typed
# parameter. The full-prelude stage2 frontier hit this shape at Errno.value=:
# registration-time return inference must not require unsafe DefNode name slices.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_enum_class_setter_return_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/enum_class_setter_return.cr"
OUT="$TMP_DIR/enum_class_setter_return"
LOG="$TMP_DIR/enum_class_setter_return.log"

cat >"$SRC" <<'CR'
class Object
end

enum ErrnoTest
  E1 = 1

  def self.value=(errno : ErrnoTest)
    errno
  end

  def self.read : self
    new(1)
  end
end

ErrnoTest.value = ErrnoTest.read
CR

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 20 1024 \
  "$SRC" --no-prelude --emit hir --no-link -o "$OUT" >"$LOG" 2>&1

if ! grep -q 'func @ErrnoTest.value=.*-> 32' "$OUT.hir"; then
  echo "p2 enum setter regression: missing concrete ErrnoTest.value= return" >&2
  cat "$LOG" >&2
  cat "$OUT.hir" >&2
  exit 1
fi

echo "p2_enum_class_setter_return_infer_no_prelude_ok"
