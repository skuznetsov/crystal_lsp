#!/bin/bash
# Run all regression tests
# Usage: ./regression_tests/run_all.sh [path-to-compiler]
#
# Each .cr file can have "# EXPECT: <marker>" on any line.
# If present, the runner checks that marker appears in output.
# Otherwise, just checks for clean exit (code 0).

COMPILER="${1:-bin/crystal_v2}"
TIMEOUT=10
MAX_MEM=512
BIN_DIR="regression_tests/bin"
PASS=0
FAIL=0

if [ ! -x "$COMPILER" ]; then
  echo "ERROR: Compiler not found at $COMPILER"
  echo "Build with: crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace"
  exit 1
fi

mkdir -p "$BIN_DIR"

for src in regression_tests/*.cr; do
  name=$(basename "$src" .cr)
  bin_path="${BIN_DIR}/${name}"

  # Extract expected marker from "# EXPECT: <marker>" comment
  expect=$(grep -m1 '^# EXPECT:' "$src" | sed 's/^# EXPECT: *//')

  # Compile into regression_tests/bin/
  compile_output=$("$COMPILER" "$src" 2>&1)
  if [ $? -ne 0 ]; then
    echo "FAIL (compile): $name"
    echo "  ${compile_output}" | head -3
    FAIL=$((FAIL + 1))
    # compiled binary lands next to source by default — move if exists
    rm -f "regression_tests/${name}"
    continue
  fi

  # The compiler outputs binary next to the source file; move it
  if [ -f "regression_tests/${name}" ]; then
    mv "regression_tests/${name}" "$bin_path"
  fi

  if [ ! -f "$bin_path" ]; then
    echo "FAIL (no binary): $name"
    FAIL=$((FAIL + 1))
    continue
  fi

  # Run with timeout
  output=$(scripts/run_safe.sh "$bin_path" $TIMEOUT $MAX_MEM 2>/dev/null)
  exit_code=$?

  # Check result
  if [ -n "$expect" ]; then
    if echo "$output" | grep -qF "$expect"; then
      echo "PASS: $name"
      PASS=$((PASS + 1))
    elif [ $exit_code -ne 0 ]; then
      echo "FAIL (crash/timeout): $name"
      echo "  Output: $(echo "$output" | tail -3)"
      FAIL=$((FAIL + 1))
    else
      echo "FAIL (output): $name — expected '$expect'"
      echo "  Output: $(echo "$output" | tail -5)"
      FAIL=$((FAIL + 1))
    fi
  else
    if [ $exit_code -eq 0 ]; then
      echo "PASS: $name"
      PASS=$((PASS + 1))
    else
      echo "FAIL (crash/timeout): $name"
      echo "  Output: $(echo "$output" | tail -3)"
      FAIL=$((FAIL + 1))
    fi
  fi

  # Cleanup binary
  rm -f "$bin_path"
done

echo ""
echo "Results: $PASS passed, $FAIL failed out of $((PASS + FAIL)) tests"
[ $FAIL -eq 0 ] && exit 0 || exit 1
