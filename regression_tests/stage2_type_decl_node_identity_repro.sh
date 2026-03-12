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
DEBUG_CLASS_ARENA=LibX::Foo \
  "$BIN" --no-prelude "$SRC_FILE" -o "$OUT_BIN" >"$LOG_FILE" 2>&1
RC=$?
set -e

if grep -E -q '\[CLASS_ARENA\] .*class=LibX::Foo' "$LOG_FILE" &&
   grep -E -q 'first=CrystalV2::Compiler::Frontend::Node:last=CrystalV2::Compiler::Frontend::Node' "$LOG_FILE"; then
  echo "reproduced: stage2 downgraded LibX::Foo field nodes to generic Frontend::Node"
  exit 0
fi

if [[ "$RC" -ne 0 ]]; then
  echo "not reproduced (compiler exited $RC without the generic-node signature)"
  exit 1
fi

echo "not reproduced"
exit 1
