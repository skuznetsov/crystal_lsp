#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <stage1-compiler> <stage2-compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/regression_tests/stage2_numeric_literal_mir_oracle.cr"
STAGE1="$1"
STAGE2="$2"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_numeric_literal_mir.XXXXXX")"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

for compiler in "$STAGE1" "$STAGE2"; do
  if [[ ! -x "$compiler" ]]; then
    echo "error: compiler binary not found/executable: $compiler" >&2
    exit 2
  fi
done

run_phase() {
  local label="$1"
  local compiler="$2"
  local out_base="$WORKDIR/${label}"
  local wrapper="$WORKDIR/${label}.sh"
  local log="$WORKDIR/${label}.log"
  local artifact="${out_base}.mir"

  {
    echo "#!/usr/bin/env bash"
    echo "set -euo pipefail"
    echo "export CRYSTAL_V2_STOP_AFTER_MIR=1"
    printf 'exec %q ' "$compiler"
    printf '%q ' --release --no-prelude --no-ast-cache --emit mir --no-link
    printf '%q ' "$SRC" -o "$out_base"
    echo
  } >"$wrapper"
  chmod +x "$wrapper"

  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$wrapper" 40 2048 >"$log" 2>&1
  local rc=$?
  set -e

  if [[ $rc -ne 0 ]]; then
    if [[ $label == stage1 ]]; then
      echo "inconclusive: stage1 failed on numeric MIR oracle" >&2
      tail -n 80 "$log" >&2 || true
      exit 2
    fi
    echo "reproduced: self-hosted stage2 failed before MIR artifact on numeric literal oracle"
    tail -n 80 "$log"
    exit 1
  fi

  if [[ ! -f "$artifact" ]]; then
    echo "inconclusive: expected artifact missing: $artifact" >&2
    tail -n 80 "$log" >&2 || true
    exit 2
  fi
}

run_phase stage1 "$STAGE1"
run_phase stage2 "$STAGE2"

if ! diff -u "$WORKDIR/stage1.mir" "$WORKDIR/stage2.mir" >"$WORKDIR/mir.diff"; then
  echo "reproduced: stage2 diverges from stage1 in MIR for numeric literals"
  sed -n '1,120p' "$WORKDIR/mir.diff"
  exit 1
fi

echo "not reproduced: stage2 matches stage1 MIR on numeric literals"
