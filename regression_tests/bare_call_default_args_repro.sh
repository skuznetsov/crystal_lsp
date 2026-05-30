#!/usr/bin/env bash
# Regression test: a bare identifier (no parens) that resolves to a method with
# all-default parameters must apply the defaults, not pass 0/null.
#
# Bug: lower_identifier's top-level-function fallback built the Call with empty
# args and mangled the zero-arg name, skipping default-argument filling that the
# parenthesized `name()` path performs via apply_default_args. So:
#   def f(x : Int32 = 7); x; end
#   puts f        # printed 0   (expected 7)
#   puts f.to_s   # printed 0   (expected 7)
#   make_array.join(",")  # `make_array` resolved to null -> .join segfault (139)
# `f()` (with parens) was unaffected.
#
# Fix: lower_identifier fills defaults via apply_default_args and re-mangles with
# the resulting arg types before emitting the Call.
set -euo pipefail

COMPILER="${1:-./bin/adamas}"
TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/bare_call_default.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

fail=0
run_case() {
  local name="$1" src="$2" expect="$3"
  local cr="$TMP_DIR/$name.cr" bin="$TMP_DIR/$name.bin" out="$TMP_DIR/$name.out"
  printf '%s' "$src" >"$cr"
  if ! "$COMPILER" "$cr" -o "$bin" >"$TMP_DIR/$name.compile" 2>&1; then
    echo "FAIL[$name]: compile error"; cat "$TMP_DIR/$name.compile"; fail=1; return
  fi
  "$bin" >"$out" 2>/dev/null || true
  local got; got="$(cat "$out")"
  if [[ "$got" == "$expect" ]]; then
    echo "PASS[$name]: [$got]"
  else
    echo "FAIL[$name]: expected [$expect] got [$got]"; fail=1
  fi
}

# bare all-default call returns the default value (was 0).
run_case bare_default 'def f(x : Int32 = 7) : Int32
  x
end
puts f' "7"

# bare all-default call used as a receiver (was 0 -> sometimes a null deref).
run_case bare_receiver 'def f(x : Int32 = 7) : Int32
  x
end
puts f.to_s' "7"

# multi default, named-arg overloads present alongside the bare call.
run_case multi_default 'def cp(x : Int32 = 0, y : Int32 = 0) : String
  "(#{x}, #{y})"
end
puts cp
puts cp(x: 5)' "(0, 0)
(5, 0)"

# bare all-default receiver feeding a method (was null -> segfault).
run_case bare_receiver_chain 'def make_array(size : Int32 = 3, fill : Int32 = 0) : Array(Int32)
  result = [] of Int32
  size.times { result << fill }
  result
end
puts make_array.join(",")' "0,0,0"

# zero-param bare call must still work (control).
run_case zero_param 'def g : Int32
  9
end
puts g' "9"

if [[ $fail -eq 0 ]]; then
  echo "reproduced: bare default-arg calls apply defaults"
  exit 0
else
  echo "NOT FIXED: bare default-arg call regression"
  exit 1
fi
