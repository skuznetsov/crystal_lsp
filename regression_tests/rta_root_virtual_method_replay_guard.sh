#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"
TMP_DIR="$(mktemp -d /tmp/cv2_rta_root_virtual.XXXXXX)"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$TMP_DIR/repro.cr" <<'CR'
class IO
  def <<(value : String)
    self
  end
end

class Object
end

class Exception < Object
  def inspect_with_backtrace(io : IO)
    io << "base"
  end
end

class MyError < Exception
  def inspect_with_backtrace(io : IO)
    io << "override"
  end
end

class Unrelated < Object
  def touch
  end
end

def print_exception(exception : Object)
  exception.inspect_with_backtrace(IO.new)
end

Unrelated.new.touch
print_exception(MyError.new)
CR

CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 30 512 \
    "$TMP_DIR/repro.cr" --no-prelude --emit hir --no-link -o "$TMP_DIR/out" \
    > "$TMP_DIR/compile.log" 2>&1

HIR="$TMP_DIR/out.hir"
if [[ ! -s "$HIR" ]]; then
  echo "rta_root_virtual_method_replay_failed: missing HIR" >&2
  tail -80 "$TMP_DIR/compile.log" >&2 || true
  exit 1
fi

if ! grep -q 'MyError#inspect_with_backtrace\$IO' "$HIR"; then
  echo "rta_root_virtual_method_replay_failed: missing concrete override target" >&2
  grep -n 'inspect_with_backtrace' "$HIR" >&2 || true
  exit 1
fi

if grep -q 'Unrelated#inspect_with_backtrace' "$HIR"; then
  echo "rta_root_virtual_method_replay_failed: replayed root virtual method to unrelated live owner" >&2
  grep -n 'inspect_with_backtrace' "$HIR" >&2 || true
  exit 1
fi

echo "rta_root_virtual_method_replay_ok"
