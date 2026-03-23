#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <stage1-compiler> <stage2-compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/regression_tests/stage2_macro_method_char_arg_oracle.cr"
STAGE1="$1"
STAGE2="$2"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_macro_method_char_arg.XXXXXX")"

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
  local compiler_label="$1"
  local compiler="$2"
  local phase="$3"
  local stop_env="$4"
  local output_ext="$5"
  shift 5
  local extra_args=("$@")

  local out_base="$WORKDIR/${compiler_label}_${phase}"
  local wrapper="$WORKDIR/${compiler_label}_${phase}.sh"
  local log="$WORKDIR/${compiler_label}_${phase}.log"
  local artifact="${out_base}.${output_ext}"

  {
    echo "#!/usr/bin/env bash"
    echo "set -euo pipefail"
    if [[ -n "$stop_env" ]]; then
      echo "export ${stop_env}=1"
    fi
    printf 'exec %q ' "$compiler"
    printf '%q ' --release --no-prelude --no-ast-cache
    printf '%q ' "${extra_args[@]}"
    printf '%q ' "$SRC" -o "$out_base"
    echo
  } >"$wrapper"
  chmod +x "$wrapper"

  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$wrapper" 40 2048 >"$log" 2>&1
  local status=$?
  set -e

  if [[ $compiler_label == "stage1" && $status -ne 0 ]]; then
    echo "inconclusive: stage1 failed during ${phase}" >&2
    tail -n 80 "$log" >&2 || true
    exit 2
  fi

  if [[ $compiler_label == "stage2" && $status -ne 0 ]]; then
    echo "reproduced: stage2 failed during ${phase} on the macro method.ends_with?('=') oracle"
    tail -n 80 "$log"
    exit 1
  fi

  if [[ ! -f "$artifact" ]]; then
    echo "inconclusive: expected artifact missing after ${compiler_label} ${phase}: $artifact" >&2
    tail -n 80 "$log" >&2 || true
    exit 2
  fi
}

compare_phase() {
  local phase="$1"
  local output_ext="$2"
  local stage1_artifact="$WORKDIR/stage1_${phase}.${output_ext}"
  local stage2_artifact="$WORKDIR/stage2_${phase}.${output_ext}"
  local diff_file="$WORKDIR/${phase}.diff"

  if ! diff -u "$stage1_artifact" "$stage2_artifact" >"$diff_file"; then
    echo "reproduced: stage2 diverges from stage1 in ${phase} for the macro method.ends_with?('=') oracle"
    sed -n '1,120p' "$diff_file"
    exit 1
  fi
}

run_phase "stage1" "$STAGE1" "hir" "CRYSTAL_V2_STOP_AFTER_HIR" "hir" --emit hir
run_phase "stage2" "$STAGE2" "hir" "CRYSTAL_V2_STOP_AFTER_HIR" "hir" --emit hir
compare_phase "hir" "hir"

run_phase "stage1" "$STAGE1" "mir" "CRYSTAL_V2_STOP_AFTER_MIR" "mir" --emit mir --no-link
run_phase "stage2" "$STAGE2" "mir" "CRYSTAL_V2_STOP_AFTER_MIR" "mir" --emit mir --no-link
compare_phase "mir" "mir"

run_phase "stage1" "$STAGE1" "ll" "" "ll" --emit llvm-ir --no-link
run_phase "stage2" "$STAGE2" "ll" "" "ll" --emit llvm-ir --no-link
compare_phase "ll" "ll"

echo "not reproduced: stage2 matches stage1 on hir/mir/ll for the macro method.ends_with?('=') oracle"
