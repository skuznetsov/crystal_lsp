#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
SRC="$ROOT_DIR/regression_tests/stage1_hash_lookup_string_eq_repro.cr"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage1_hash_lookup_string_eq.XXXXXX")"
WRAPPER="$WORKDIR/run_compile.sh"
COMPILE_LOG="$WORKDIR/compile.log"
RUN_LOG="$WORKDIR/run.log"
OUT="$WORKDIR/hash_lookup_string_eq"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "error: compiler binary not found/executable: $COMPILER" >&2
  exit 2
fi

cat > "$WRAPPER" <<EOF
#!/usr/bin/env bash
set -euo pipefail
exec "$COMPILER" "$SRC" --release -o "$OUT"
EOF
chmod +x "$WRAPPER"

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$WRAPPER" 30 2048 >"$COMPILE_LOG" 2>&1
COMPILE_STATUS=$?
set -e

if [[ $COMPILE_STATUS -ne 0 ]]; then
  echo "reproduced: compiler failed before building Hash lookup/equality witness"
  tail -n 120 "$COMPILE_LOG"
  exit 1
fi

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$OUT" 5 512 >"$RUN_LOG" 2>&1
RUN_STATUS=$?
set -e

if [[ $RUN_STATUS -eq 0 ]]; then
  echo "not reproduced: Hash lookup/equality preserves semantics"
  exit 0
fi

if rg -q '\[EXIT: 1\]' "$RUN_LOG"; then
  echo "reproduced: Hash lookup/equality returned the wrong result"
  tail -n 80 "$RUN_LOG"
  exit 1
fi

echo "reproduced: Hash lookup/equality crashed"
tail -n 80 "$RUN_LOG"
exit 1
