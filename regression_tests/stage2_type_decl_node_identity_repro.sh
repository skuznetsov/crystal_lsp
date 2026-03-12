#!/usr/bin/env bash
set -euo pipefail

BIN="${1:-/tmp/stage2_release_cached}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2-type-decl-node.XXXXXX")"
SRC_FILE="$WORKDIR/lib_struct_minimal.cr"
LOG_FILE="$WORKDIR/repro.log"
OUT_BIN="$WORKDIR/repro.bin"

trap 'rm -rf "$WORKDIR"' EXIT

if [[ ! -x "$BIN" ]]; then
  echo "error: compiler binary not found/executable: $BIN" >&2
  exit 2
fi

cat >"$SRC_FILE" <<'EOF'
lib LibX
  struct Foo
    a : Int
    b : Long
  end
end
EOF

cd "$ROOT"

set +e
CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$BIN" --no-prelude "$SRC_FILE" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if [[ "$RC" -ne 0 ]]; then
  echo "reproduced: stage2 crashed on the tiny LibX::Foo no-prelude HIR oracle"
  exit 0
fi

echo "not reproduced"
exit 1
