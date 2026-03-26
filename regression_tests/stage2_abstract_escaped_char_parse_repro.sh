#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 2 ]; then
  echo "Usage: $0 <stage1-compiler> <stage2-compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STAGE1="$1"
STAGE2="$2"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_abstract_escaped_char.XXXXXX")"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

if [[ ! -x "$STAGE1" ]]; then
  echo "error: stage1 compiler not found/executable: $STAGE1" >&2
  exit 2
fi

if [[ ! -x "$STAGE2" ]]; then
  echo "error: stage2 compiler not found/executable: $STAGE2" >&2
  exit 2
fi

ABSTRACT_SRC="$ROOT_DIR/regression_tests/stage2_abstract_escaped_char_parse_repro.cr"
CONCRETE_SRC="$WORKDIR/concrete_control.cr"

sed '1s/^abstract //' "$ABSTRACT_SRC" >"$CONCRETE_SRC"

run_parse_only() {
  local compiler="$1"
  local src="$2"
  local label="$3"
  local out="$WORKDIR/${label}.out"
  local wrapper="$WORKDIR/${label}.sh"

  cat >"$wrapper" <<EOF
#!/usr/bin/env bash
set -euo pipefail
export CRYSTAL_V2_STOP_AFTER_PARSE=1
exec "$compiler" --release --no-prelude --no-ast-cache "$src" -o "$out"
EOF
  chmod +x "$wrapper"

  set +e
  local output
  output="$("$ROOT_DIR/scripts/run_safe.sh" "$wrapper" 20 1024 2>&1)"
  local status=$?
  set -e

  printf '%s' "$output" >"$WORKDIR/${label}.log"
  return $status
}

if ! run_parse_only "$STAGE1" "$ABSTRACT_SRC" "stage1_abstract"; then
  echo "error: stage1 control unexpectedly failed on abstract escaped-char reducer" >&2
  tail -n 40 "$WORKDIR/stage1_abstract.log" >&2
  exit 2
fi

if ! run_parse_only "$STAGE2" "$CONCRETE_SRC" "stage2_concrete"; then
  echo "error: stage2 concrete control unexpectedly failed on escaped-char reducer" >&2
  tail -n 40 "$WORKDIR/stage2_concrete.log" >&2
  exit 2
fi

if ! run_parse_only "$STAGE2" "$ABSTRACT_SRC" "stage2_abstract"; then
  echo "reproduced: stage2 still fails on abstract escaped-char reducer while stage1 and concrete control pass"
  tail -n 40 "$WORKDIR/stage2_abstract.log"
  exit 1
fi

echo "not reproduced: abstract escaped-char reducer parses correctly on stage2"
