#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <stage1-compiler> <stage2-compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/regression_tests/stage2_abstract_macro_char_literal_parse_repro.cr"
STAGE1="$1"
STAGE2="$2"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_main_block_copy_ll_oracle.XXXXXX")"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

extract_main() {
  local input_ll="$1"
  local output_txt="$2"
  python3 - "$input_ll" "$output_txt" <<'PY'
from pathlib import Path
import sys

input_path = Path(sys.argv[1])
output_path = Path(sys.argv[2])
text = input_path.read_text()
start = text.index("define void @__crystal_main")
end = text.index("; Program entry point", start)
output_path.write_text(text[start:end])
PY
}

run_ll() {
  local compiler_label="$1"
  local compiler="$2"
  local out_base="$WORKDIR/${compiler_label}"
  local artifact="${out_base}.ll"
  local wrapper="$WORKDIR/${compiler_label}.sh"
  local log="$WORKDIR/${compiler_label}.log"

  {
    echo "#!/usr/bin/env bash"
    echo "set -euo pipefail"
    printf 'exec %q ' "$compiler"
    printf '%q ' --release --no-prelude --no-ast-cache --emit llvm-ir --no-link
    printf '%q ' "$SRC" -o "$out_base"
    echo
  } >"$wrapper"
  chmod +x "$wrapper"

  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$wrapper" 40 2048 >"$log" 2>&1
  local status=$?
  set -e

  if [[ $status -ne 0 ]]; then
    echo "inconclusive: ${compiler_label} failed to emit llvm-ir" >&2
    tail -n 80 "$log" >&2 || true
    exit 2
  fi

  if [[ ! -f "$artifact" ]]; then
    echo "inconclusive: missing ${compiler_label} artifact $artifact" >&2
    tail -n 80 "$log" >&2 || true
    exit 2
  fi

  extract_main "$artifact" "$WORKDIR/${compiler_label}.main.ll"
}

run_ll "stage1" "$STAGE1"
run_ll "stage2" "$STAGE2"

if ! diff -u "$WORKDIR/stage1.main.ll" "$WORKDIR/stage2.main.ll"; then
  echo "reproduced: stage2 __crystal_main llvm block transport diverges from stage1" >&2
  exit 1
fi

echo "not reproduced: stage2 matches stage1 for __crystal_main llvm block transport"
