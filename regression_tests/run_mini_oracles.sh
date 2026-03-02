#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  regression_tests/run_mini_oracles.sh [compiler] [test1.cr test2.cr ...]

Defaults:
  compiler: bin/crystal_v2
  tests:
    regression_tests/file_join_splat.cr
    regression_tests/forall_nil_union_return.cr
    regression_tests/test_byteformat_decode_u32.cr
    regression_tests/test_nested_macro_record.cr
    regression_tests/test_select_map_stress.cr

Env:
  MINI_ORACLE_TIMEOUT  (default: 10 seconds)
  MINI_ORACLE_MAX_MEM  (default: 512 MB)
USAGE
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

COMPILER="${1:-bin/crystal_v2}"
if [[ $# -gt 0 ]]; then
  shift
fi

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: Compiler not found at $COMPILER" >&2
  echo "Build with: crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace" >&2
  exit 2
fi

TIMEOUT_SECS="${MINI_ORACLE_TIMEOUT:-10}"
MAX_MEM_MB="${MINI_ORACLE_MAX_MEM:-512}"
TMP_ROOT="${TMPDIR:-/tmp}/crystal_v2_mini_oracles"
mkdir -p "$TMP_ROOT"

DEFAULT_TESTS=(
  "regression_tests/file_join_splat.cr"
  "regression_tests/forall_nil_union_return.cr"
  "regression_tests/test_byteformat_decode_u32.cr"
  "regression_tests/test_nested_macro_record.cr"
  "regression_tests/test_select_map_stress.cr"
)

if [[ $# -gt 0 ]]; then
  TESTS=("$@")
else
  TESTS=("${DEFAULT_TESTS[@]}")
fi

PASS=0
FAIL=0

for src in "${TESTS[@]}"; do
  if [[ ! -f "$src" ]]; then
    echo "FAIL (missing): $src"
    FAIL=$((FAIL + 1))
    continue
  fi

  name="$(basename "$src" .cr)"
  bin_path="$TMP_ROOT/${name}.bin"
  compile_log="$TMP_ROOT/${name}.compile.log"

  expect="$(grep -m1 '^# EXPECT:' "$src" | sed 's/^# EXPECT: *//' || true)"

  set +e
  "$COMPILER" "$src" -o "$bin_path" >"$compile_log" 2>&1
  compile_status=$?
  set -e

  if [[ $compile_status -ne 0 ]]; then
    echo "FAIL (compile): $name"
    tail -n 8 "$compile_log"
    FAIL=$((FAIL + 1))
    rm -f "$bin_path"
    continue
  fi

  set +e
  output="$(scripts/run_safe.sh "$bin_path" "$TIMEOUT_SECS" "$MAX_MEM_MB" 2>&1)"
  run_status=$?
  set -e

  if [[ -n "$expect" ]]; then
    if [[ $run_status -eq 0 ]] && printf '%s' "$output" | grep -qF "$expect"; then
      echo "PASS: $name"
      PASS=$((PASS + 1))
    elif [[ $run_status -ne 0 ]]; then
      echo "FAIL (crash/timeout): $name"
      echo "  Output: $(printf '%s' "$output" | tail -3)"
      FAIL=$((FAIL + 1))
    else
      echo "FAIL (output): $name — expected '$expect'"
      echo "  Output: $(printf '%s' "$output" | tail -5)"
      FAIL=$((FAIL + 1))
    fi
  else
    if [[ $run_status -eq 0 ]]; then
      echo "PASS: $name"
      PASS=$((PASS + 1))
    else
      echo "FAIL (crash/timeout): $name"
      echo "  Output: $(printf '%s' "$output" | tail -3)"
      FAIL=$((FAIL + 1))
    fi
  fi

  rm -f "$bin_path"
done

echo
echo "Mini-oracles: $PASS passed, $FAIL failed out of $((PASS + FAIL)) tests"
[[ $FAIL -eq 0 ]]
