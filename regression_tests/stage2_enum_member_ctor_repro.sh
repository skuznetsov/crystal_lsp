#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <stage1-compiler> <stage2-compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT_DIR/regression_tests/stage2_enum_member_ctor_repro.cr"
STAGE1="$1"
STAGE2="$2"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_enum_member_ctor.XXXXXX")"

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
  local wrapper="$WORKDIR/${label}.sh"
  local log="$WORKDIR/${label}.log"

  {
    echo "#!/usr/bin/env bash"
    echo "set -euo pipefail"
    echo "export DEBUG_ENUM_ARENA=Kind"
    echo "export DEBUG_ENUM_SOURCE=1"
    echo "export CRYSTAL_V2_STOP_AFTER_HIR=1"
    printf 'exec %q ' "$compiler"
    printf '%q ' "$SRC" --release --no-prelude --no-ast-cache --emit hir -o "$WORKDIR/${label}_out"
    echo
  } >"$wrapper"
  chmod +x "$wrapper"

  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$wrapper" 40 2048 >"$log" 2>&1
  local status=$?
  set -e

  echo "$status"
}

assert_expected_log_shape() {
  local label="$1"
  local log="$WORKDIR/${label}.log"

  if rg -q '\[ENUM_MEMBER\] enum=Kind member=' "$log"; then
    echo "reproduced: ${label} still treats implicit enum members as explicit-valued" >&2
    tail -n 120 "$log" >&2 || true
    exit 1
  fi

  if ! rg -q '\[ENUM_MEMBER\] enum=Kind members_done=1 count=2' "$log"; then
    echo "inconclusive: ${label} did not reach enum member completion" >&2
    tail -n 120 "$log" >&2 || true
    exit 2
  fi

  if ! rg -q '\[ENUM_MEMBER\] enum=Kind body_done=1 count=2' "$log"; then
    echo "inconclusive: ${label} did not reach enum body completion" >&2
    tail -n 120 "$log" >&2 || true
    exit 2
  fi
}

assert_expected_status() {
  local label="$1"
  local status="$2"
  local log="$WORKDIR/${label}.log"

  case "$status" in
    0|134)
      ;;
    138|139)
      echo "reproduced: ${label} still hits the old enum crash corridor" >&2
      tail -n 120 "$log" >&2 || true
      exit 1
      ;;
    *)
      echo "inconclusive: unexpected ${label} exit status ${status}" >&2
      tail -n 120 "$log" >&2 || true
      exit 2
      ;;
  esac
}

stage1_status="$(run_hir stage1 "$STAGE1")"
assert_expected_status stage1 "$stage1_status"
assert_expected_log_shape stage1

stage2_status="$(run_hir stage2 "$STAGE2")"
assert_expected_status stage2 "$stage2_status"
assert_expected_log_shape stage2

echo "not reproduced: stage2 no longer corrupts implicit enum members on the HIR enum oracle"
