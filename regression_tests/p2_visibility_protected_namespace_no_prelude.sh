#!/usr/bin/env bash
# No-prelude guard: protected access follows Crystal's namespace rule. A nested
# type such as Box::Iter may call protected Box methods on a Box receiver, while
# an unrelated namespace must still be rejected.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_visibility_protected_namespace_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

PASS_SRC="$TMP_DIR/protected_namespace_pass.cr"
FAIL_SRC="$TMP_DIR/protected_namespace_fail.cr"
PASS_OUT="$TMP_DIR/protected_namespace_pass"
FAIL_OUT="$TMP_DIR/protected_namespace_fail"
PASS_LOG="$TMP_DIR/pass.log"
FAIL_LOG="$TMP_DIR/fail.log"

cat >"$PASS_SRC" <<'CR'
class Object
end

class Box(T)
  protected def entries_size : Int32
    1
  end

  class Iter(T)
    def initialize(@box : Box(T))
    end

    def probe : Int32
      @box.entries_size
    end
  end
end

Box::Iter(Int32).new(Box(Int32).new).probe
CR

"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 20 1024 \
  "$PASS_SRC" --no-prelude --emit hir --no-link -o "$PASS_OUT" >"$PASS_LOG" 2>&1

cat >"$FAIL_SRC" <<'CR'
class Object
end

class Box(T)
  protected def entries_size : Int32
    1
  end
end

class Stranger(T)
  def probe(box : Box(T)) : Int32
    box.entries_size
  end
end

Stranger(Int32).new.probe(Box(Int32).new)
CR

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 20 1024 \
  "$FAIL_SRC" --no-prelude --emit hir --no-link -o "$FAIL_OUT" >"$FAIL_LOG" 2>&1
STATUS=$?
set -e

if [[ "$STATUS" -eq 0 ]]; then
  echo "p2 visibility regression: unrelated protected call compiled" >&2
  cat "$FAIL_LOG" >&2
  exit 1
fi

if ! grep -Eq "protected method 'entries_size' called for Box\\(Int32\\)" "$FAIL_LOG"; then
  echo "p2 visibility regression: expected protected diagnostic missing" >&2
  cat "$FAIL_LOG" >&2
  exit 1
fi

echo "p2_visibility_protected_namespace_no_prelude_ok"
