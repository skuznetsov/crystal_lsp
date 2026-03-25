#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
SRC="$ROOT_DIR/regression_tests/stage1_default_arg_padding_repro.cr"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage1_default_arg_padding.XXXXXX")"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "error: compiler binary not found/executable: $COMPILER" >&2
  exit 2
fi

WRAPPER="$WORKDIR/default_arg_padding.sh"
OUT_BASE="$WORKDIR/default_arg_padding"
LOG="$WORKDIR/default_arg_padding.log"
ARTIFACT="${OUT_BASE}.ll"

cat >"$WRAPPER" <<EOF
#!/usr/bin/env bash
set -euo pipefail
exec "$COMPILER" --release --no-prelude --no-ast-cache --emit llvm-ir --no-link "$SRC" -o "$OUT_BASE"
EOF
chmod +x "$WRAPPER"

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$WRAPPER" 30 2048 >"$LOG" 2>&1
STATUS=$?
set -e

if [[ $STATUS -ne 0 ]]; then
  echo "reproduced: compiler failed before LLVM IR on omitted default-arg padding"
  tail -n 80 "$LOG"
  exit 1
fi

if [[ ! -f "$ARTIFACT" ]]; then
  echo "inconclusive: expected LLVM IR artifact missing: $ARTIFACT" >&2
  tail -n 80 "$LOG" >&2 || true
  exit 2
fi

if ! grep -Fq "call i32 @foo(i32 1)" "$ARTIFACT"; then
  echo "reproduced: omitted default argument was not materialized as literal 1 in LLVM IR"
  rg -n -C 2 "call .*@foo\\(i32 [01]\\)|define i32 @foo" "$ARTIFACT" || sed -n '1,120p' "$ARTIFACT"
  exit 1
fi

echo "not reproduced: omitted default argument is preserved as literal 1 in LLVM IR"
