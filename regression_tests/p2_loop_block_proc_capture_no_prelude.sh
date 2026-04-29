#!/usr/bin/env bash
# Regression: standalone block-proc materialization must preserve captures and
# untyped block parameter runtime shape when the block body is wrapped in loop.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_loop_block_proc_capture_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/repro.cr"
OUT="$TMP_DIR/out"
LOG="$TMP_DIR/compile.log"
HIR="$OUT.hir"

cat >"$SRC" <<'CR'
class Object
end

class Reference < Object
end

class Buffer < Reference
end

class Reader < Reference
  def read(buffer : Buffer) : Int32
    0
  end
end

def with_reader(&block)
  block.call(Reader.new)
end

def drive : Int32
  hash = 1
  buffer = Buffer.new
  with_reader do |file|
    loop do
      bytes_read = file.read(buffer)
      break if bytes_read <= 0
      hash = hash + bytes_read
    end
  end
  hash
end

drive
CR

CRYSTAL_V2_STOP_AFTER_HIR=1 DEBUG_BLOCK_PROC_CAPTURES=1 \
  "$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 60 1024 \
  "$SRC" --no-prelude --emit hir -o "$OUT" >"$LOG" 2>&1

if [[ ! -s "$HIR" ]]; then
  echo "p2_loop_block_proc_capture_failed: missing HIR artifact" >&2
  cat "$LOG" >&2
  exit 1
fi

if ! grep -Eq '\[BLOCK_PROC_CAPTURE\].*refs=.*buffer.*captures=.*buffer.*hash' "$LOG"; then
  echo "p2_loop_block_proc_capture_failed: loop body identifiers were not captured" >&2
  grep '\[BLOCK_PROC_CAPTURE\]' "$LOG" >&2 || true
  exit 1
fi

if grep -Eq 'func @__crystal_block_proc_[0-9]+\(%0: 0\)' "$HIR"; then
  echo "p2_loop_block_proc_capture_failed: block proc kept a VOID first parameter" >&2
  grep -n 'func @__crystal_block_proc' "$HIR" >&2 || true
  exit 1
fi

if ! grep -q 'Reader#read$Buffer' "$HIR"; then
  echo "p2_loop_block_proc_capture_failed: block proc did not preserve Reader#read(Buffer)" >&2
  exit 1
fi

echo "p2_loop_block_proc_capture_no_prelude_ok"
