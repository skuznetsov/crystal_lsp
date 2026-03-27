#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <stage1-compiler> <stage2-compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/regression_tests/stage2_array_literal_of_type_hir_oracle.cr"
STAGE1="$1"
STAGE2="$2"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_array_literal_of_type_hir_oracle.XXXXXX")"

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

run_hir() {
  local label="$1"
  local compiler="$2"
  local out_base="$WORKDIR/$label"
  local wrapper="$WORKDIR/$label.sh"
  local log="$WORKDIR/$label.log"
  local artifact="${out_base}.hir"

  {
    echo "#!/usr/bin/env bash"
    echo "set -euo pipefail"
    echo "export CRYSTAL_V2_STOP_AFTER_HIR=1"
    echo "export CRYSTAL_V2_TRUST_SLICE_ADDR=1"
    printf 'exec %q ' "$compiler"
    printf '%q ' --release --no-prelude --no-ast-cache --emit hir
    printf '%q ' "$SRC" -o "$out_base"
    echo
  } >"$wrapper"
  chmod +x "$wrapper"

  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$wrapper" 15 1024 >"$log" 2>&1
  local status=$?
  set -e

  if [[ $label == "stage1" && $status -ne 0 ]]; then
    echo "inconclusive: stage1 failed during array-literal of_type HIR oracle" >&2
    tail -n 80 "$log" >&2 || true
    exit 2
  fi

  if [[ $label == "stage2" && $status -ne 0 ]]; then
    echo "reproduced: stage2 failed before completing array-literal of_type HIR emission"
    tail -n 80 "$log"
    exit 1
  fi

  if [[ ! -f "$artifact" ]]; then
    echo "inconclusive: expected artifact missing after $label HIR emission: $artifact" >&2
    tail -n 80 "$log" >&2 || true
    exit 2
  fi
}

normalize_focus() {
  local input="$1"
  local output="$2"

  sed -E 's/^([[:space:]]*)type\.[0-9]+/\1type.N/' "$input" \
    | grep -E 'CallNode|NamedArgument|Array\(CrystalV2::Compiler::Frontend::NamedArgument\)' \
    >"$output"
}

run_hir "stage1" "$STAGE1"
run_hir "stage2" "$STAGE2"

normalize_focus "$WORKDIR/stage1.hir" "$WORKDIR/stage1.focus"
normalize_focus "$WORKDIR/stage2.hir" "$WORKDIR/stage2.focus"

if ! diff -u "$WORKDIR/stage1.focus" "$WORKDIR/stage2.focus" >"$WORKDIR/focus.diff"; then
  echo "reproduced: stage2 diverges from stage1 on the array-literal of_type HIR oracle"
  sed -n '1,120p' "$WORKDIR/focus.diff"
  exit 1
fi

echo "not reproduced: stage2 matches stage1 on the array-literal of_type HIR oracle"
