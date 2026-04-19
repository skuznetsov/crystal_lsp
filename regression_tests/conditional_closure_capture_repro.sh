#!/bin/bash
# Falsifier reducer for P1 I14-monotonic dominance (basic shape).
#
# Motivation: under I14-monotonic (see ast_to_hir.cr BoxedLocal docs),
# `.locals` truncation at branch/case merge sites is safe ONLY IF the
# box allocation dominates every subsequent read/write of the local.
# `hoist_box_for_local` must enforce this by emitting the allocation
# into the function entry block, regardless of the branch context at
# hoist time.
#
# Shape: a mutable parent-scope local (`counter`) is captured by a
# proc literal. The proc is re-assigned in BOTH branches of an `if`
# whose condition is runtime-unknown (`ARGV.size == 0`). Both
# branches construct a proc that mutates `counter`, then the outer
# scope calls the proc and reads `counter` through the parent scope.
#
# This exercises:
#   1. Proc literal lowered inside a branch. Correct P1 must have
#      predeclared/seeded the `counter` box before this branch; invoking
#      hoist_box_for_local from the branch is a compiler bug.
#   2. Re-hoisting: second branch's proc literal triggers another
#      hoist call for the same name — MUST be idempotent and return
#      the same box_ptr.
#   3. Non-zero initial value (`counter = 5`) catches the unsafe
#      "entry allocation + zero-init is enough" shortcut. The box must
#      be seeded from the local's current value before branch lowering.
#   4. Outer read after the if: `puts counter` reads through the box.
#      The box alloc+seed must dominate this read.
#
# Proc|Nil union and Array(Proc) are avoided: both have separate
# pre-P1 codegen issues that would conflate unrelated failures. The
# pre-declaration `p = ...` fixes p's type as Proc(Int32, Int32) so
# the branch reassignments do not introduce a union.
#
# Exit semantics:
#   exit 1 — CORRECT: output is "12\n12" (proc mutated counter from
#            5 to 12, outer read sees 12).
#   exit 0 — REPRODUCED: dominance bug (wrong output, verifier fail,
#            runtime crash on the compiled binary).
#   exit 2 — INCONCLUSIVE: compile failure, runner error. Does NOT
#            claim bug reproduction.
#
# Status: currently PASSES (exit 1) because pre-P1 globals-based
# capture path shares `counter`. This is a FORWARD guard: it must
# continue to exit 1 after the P1 atomic flip.

set -u

COMPILER="${1:-}"
if [[ -z "$COMPILER" ]]; then
  echo "Usage: $0 <compiler>" >&2
  exit 2
fi

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

SRC="$TMPDIR/conditional_closure_capture.cr"
BIN="$TMPDIR/conditional_closure_capture"

cat > "$SRC" <<'EOF'
counter = 5
p = ->(x : Int32) { counter += x; counter }

if ARGV.size == 0
  p = ->(x : Int32) { counter += x; counter }
else
  p = ->(x : Int32) { counter = x; counter }
end

puts p.call(7)
puts counter
EOF

if ! "$COMPILER" "$SRC" -o "$BIN" >"$TMPDIR/compile.log" 2>&1; then
  echo "inconclusive: compile_failed"
  sed 's/^/  /' "$TMPDIR/compile.log" | tail -20
  exit 2
fi

OUT=$("$BIN" 2>&1)
RC=$?

if [[ $RC -ne 0 ]]; then
  echo "reproduced: runtime failure (rc=$RC): $OUT"
  exit 0
fi

if [[ "$OUT" == "12"$'\n'"12" ]]; then
  echo "correct: proc.call(7) -> result=12, counter=12"
  exit 1
fi

echo "reproduced: expected '12\\n12', got: $(printf '%q' "$OUT")"
exit 0
