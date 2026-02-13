#!/bin/bash
# Run all regression tests
# Usage: ./regression_tests/run_all.sh [path-to-compiler]

COMPILER="${1:-bin/crystal_v2}"
TIMEOUT=10
MAX_MEM=512
PASS=0
FAIL=0

if [ ! -x "$COMPILER" ]; then
  echo "ERROR: Compiler not found at $COMPILER"
  echo "Build with: crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace"
  exit 1
fi

for src in regression_tests/*.cr; do
  name=$(basename "$src" .cr)
  bin_path="regression_tests/${name}"
  
  # Compile
  if ! "$COMPILER" "$src" 2>/dev/null; then
    echo "FAIL (compile): $name"
    FAIL=$((FAIL + 1))
    continue
  fi
  
  # Run with timeout
  output=$(scripts/run_safe.sh "$bin_path" $TIMEOUT $MAX_MEM 2>/dev/null)
  exit_code=$?
  
  # Check for _ok marker in output
  if echo "$output" | grep -q "${name}_ok"; then
    echo "PASS: $name"
    PASS=$((PASS + 1))
  elif [ $exit_code -ne 0 ]; then
    echo "FAIL (crash/timeout): $name"
    echo "  Output: $(echo "$output" | head -5)"
    FAIL=$((FAIL + 1))
  else
    echo "FAIL (output): $name"
    echo "  Output: $(echo "$output" | head -5)"
    FAIL=$((FAIL + 1))
  fi
  
  # Cleanup binary
  rm -f "$bin_path"
done

echo ""
echo "Results: $PASS passed, $FAIL failed"
[ $FAIL -eq 0 ] && exit 0 || exit 1
