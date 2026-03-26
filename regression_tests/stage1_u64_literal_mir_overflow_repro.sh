#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/regression_tests/stage1_u64_literal_mir_overflow_repro.cr"
COMPILER="$1"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage1_u64_literal_mir_overflow.XXXXXX")"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "error: compiler binary not found/executable: $COMPILER" >&2
  exit 2
fi

WRAPPER="$WORKDIR/run.sh"
LOG="$WORKDIR/run.log"
OUT_BASE="$WORKDIR/out"
ARTIFACT="${OUT_BASE}.mir"

{
  echo "#!/usr/bin/env bash"
  echo "set -euo pipefail"
  echo "export CRYSTAL_V2_STOP_AFTER_MIR=1"
  printf 'exec %q ' "$COMPILER"
  printf '%q ' --release --no-prelude --no-ast-cache --emit mir --no-link
  printf '%q ' "$SRC" -o "$OUT_BASE"
  echo
} >"$WRAPPER"
chmod +x "$WRAPPER"

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$WRAPPER" 60 2048 >"$LOG" 2>&1
rc=$?
set -e

if [[ $rc -ne 0 ]]; then
  echo "reproduced: compiler overflowed before MIR artifact on the UInt64 literal carrier"
  tail -n 80 "$LOG"
  exit 1
fi

if [[ ! -f "$ARTIFACT" ]]; then
  echo "inconclusive: expected MIR artifact missing: $ARTIFACT" >&2
  tail -n 80 "$LOG" >&2 || true
  exit 2
fi

echo "not reproduced: compiler survived the UInt64 literal MIR overflow carrier"
