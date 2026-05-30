#!/bin/bash
# Compiler regression: reaching Fiber.yield -> EventLoop#sleep(0) pulls in
# Crystal::EventLoop::Polling::Event + Timers(Crystal::EventLoop::Polling::Event).
# The unqualified generic owner `Timers(...)` must resolve to the registered
# template `Crystal::EventLoop::Timers` so Timers#add/#delete/#next_ready? are
# lowered to real bodies (no LLVM abort stub).
#
# Baseline (broken): stderr contained `STUB CALLED: ...Timers$...$Hadd...`, exit 134.
#
# This is a COMPILE-TIME invariant check (the documented DoD), verified via
# `--emit llvm-ir`. It was previously filed as a runtime `.cr` whose
# `EXPECT: before_yield` marker (a) is printed before Fiber.yield regardless of
# whether Timers is lowered or stubbed, so it never distinguished the two, and
# (b) is lost to stdout buffering because the V2 runtime fiber/thread path still
# crashes inside Fiber.yield (GC thread registration / Thread::LinkedList(Fiber);
# tracked separately as the fiber-runtime cascade). The IR check is the strong,
# non-flaky guard for what this regression actually protects.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/adamas}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/bench_fiber_yield_timers_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/fiber_yield_sleep.cr"
IR="$TMP_DIR/out.ll"
cat >"$SRC" <<'CR'
puts "before_yield"
Fiber.yield
puts "after_yield"
CR

"$COMPILER" "$SRC" --emit llvm-ir -o "$TMP_DIR/out" >"$TMP_DIR/compile.log" 2>&1

if [[ ! -f "$IR" ]]; then
  echo "FAIL: no IR emitted at $IR" >&2
  tail -20 "$TMP_DIR/compile.log" >&2
  exit 1
fi

# Timers#add / #delete / #next_ready? must be lowered to real bodies.
for m in 'Hadd' 'Hdelete' 'Hnext_ready'; do
  if ! grep -Eq "define .*@Timers\\\$L.*\\\$${m}" "$IR"; then
    echo "FAIL: Timers#${m} was not lowered (expected a 'define ... @Timers\$L...\$${m}')" >&2
    grep -nE '@Timers' "$IR" | head >&2 || true
    exit 1
  fi
done

# No abort/STUB stub for any Timers method.
if grep -nE '@Timers' "$IR" | grep -iqE 'stub|abort'; then
  echo "FAIL: an abort/STUB stub was emitted for a Timers method" >&2
  grep -nE '@Timers' "$IR" | grep -iE 'stub|abort' >&2
  exit 1
fi

echo "PASS: bench_fiber_yield_sleep_path (Timers add/delete/next_ready? lowered, no stub)"
