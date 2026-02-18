#!/bin/bash
# Run complex regressions that target known bootstrap/codegen failure patterns.
# Usage: ./regression_tests/run_complex.sh [path-to-compiler] [quick|full|test_name|path]

set -u

COMPILER="${1:-bin/crystal_v2}"
MODE="${2:-quick}"
TIMEOUT="${TIMEOUT:-12}"
MAX_MEM="${MAX_MEM:-768}"
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-45}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SAFE_RUN="${SCRIPT_DIR}/../scripts/run_safe.sh"
BIN_DIR="${SCRIPT_DIR}/bin"

quick_cases=(
  "complex/test_find_nil_and_value.cr"
  "complex/test_find_nil_only.cr"
  "complex/test_yield_three_args.cr"
  "complex/test_while_yield_method.cr"
  "complex/test_string_to_u64_like.cr"
)

full_cases=(
  "${quick_cases[@]}"
  "complex/test_channel_receive_state.cr"
  "complex/test_option_parser_to_s.cr"
  "complex/test_generic_nil_return.cr"
)

if [ ! -x "$SAFE_RUN" ]; then
  echo "ERROR: Safe runner not found: $SAFE_RUN"
  exit 1
fi

if [ ! -x "$COMPILER" ]; then
  if command -v "$COMPILER" >/dev/null 2>&1; then
    COMPILER="$(command -v "$COMPILER")"
  else
    echo "ERROR: Compiler not found at $COMPILER"
    echo "Build with: crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace"
    exit 1
  fi
fi

cases=()
case "$MODE" in
  quick)
    cases=("${quick_cases[@]}")
    ;;
  full)
    cases=("${full_cases[@]}")
    ;;
  *)
    if [ -f "${SCRIPT_DIR}/${MODE}" ]; then
      cases=("${MODE}")
    elif [ -f "${SCRIPT_DIR}/complex/${MODE}.cr" ]; then
      cases=("complex/${MODE}.cr")
    else
      echo "ERROR: Unknown mode or test: $MODE"
      echo "Usage: $0 [compiler] [quick|full|test_name|path]"
      exit 1
    fi
    ;;
esac

mkdir -p "$BIN_DIR"

pass=0
fail=0

run_compile_with_timeout() {
  local timeout_secs="$1"
  local output_file="$2"
  shift 2

  "$@" >"$output_file" 2>&1 &
  local cmd_pid=$!
  local elapsed=0

  while kill -0 "$cmd_pid" 2>/dev/null; do
    if [ "$elapsed" -ge "$timeout_secs" ]; then
      kill -9 "$cmd_pid" 2>/dev/null
      wait "$cmd_pid" 2>/dev/null
      return 124
    fi
    sleep 1
    elapsed=$((elapsed + 1))
  done

  wait "$cmd_pid"
  return $?
}

for rel_path in "${cases[@]}"; do
  src_path="${SCRIPT_DIR}/${rel_path}"
  if [ ! -f "$src_path" ]; then
    echo "FAIL (missing): $rel_path"
    fail=$((fail + 1))
    continue
  fi

  name=$(basename "$src_path" .cr)
  bin_path="${BIN_DIR}/complex_${name}"

  expect=$(grep -m1 '^# EXPECT:' "$src_path" | sed 's/^# EXPECT: *//')
  expect_exit=$(grep -m1 '^# EXPECT-EXIT:' "$src_path" | sed 's/^# EXPECT-EXIT: *//')
  if [ -z "${expect_exit:-}" ]; then
    expect_exit=0
  fi

  compile_log=$(mktemp /tmp/run_complex_compile.XXXXXX)
  run_compile_with_timeout "$COMPILE_TIMEOUT" "$compile_log" "$COMPILER" "$src_path" -o "$bin_path"
  compile_exit=$?
  compile_output=$(cat "$compile_log")
  rm -f "$compile_log"

  if [ "$compile_exit" -eq 124 ]; then
    echo "FAIL (compile-timeout): ${name} — exceeded ${COMPILE_TIMEOUT}s"
    fail=$((fail + 1))
    rm -f "$bin_path"
    continue
  fi

  if [ "$compile_exit" -ne 0 ]; then
    echo "FAIL (compile): ${name}"
    echo "  ${compile_output}" | head -10
    fail=$((fail + 1))
    rm -f "$bin_path"
    continue
  fi

  output=$("$SAFE_RUN" "$bin_path" "$TIMEOUT" "$MAX_MEM" 2>/dev/null)
  exit_code=$?

  if [ "$exit_code" -ne "$expect_exit" ]; then
    echo "FAIL (exit): ${name} — expected $expect_exit, got $exit_code"
    echo "  Output: $(echo "$output" | tail -5)"
    fail=$((fail + 1))
    rm -f "$bin_path"
    continue
  fi

  if [ -n "${expect:-}" ] && ! echo "$output" | grep -qF "$expect"; then
    echo "FAIL (output): ${name} — expected '$expect'"
    echo "  Output: $(echo "$output" | tail -5)"
    fail=$((fail + 1))
    rm -f "$bin_path"
    continue
  fi

  echo "PASS: ${name}"
  pass=$((pass + 1))
  rm -f "$bin_path"
done

echo ""
echo "Complex regressions: $pass passed, $fail failed out of $((pass + fail))"
[ "$fail" -eq 0 ] && exit 0 || exit 1
