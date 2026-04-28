#!/usr/bin/env bash
# Regression test for Path::[] arity2_splat null deref on empty splat.
# Before fix: `Path.[](name : String | Path, *parts)` body lowered to
# `Path.new(name, *parts)`. With empty parts (Tuple()), the splat was padded
# at the call site, producing a recursive `Path.new$Path | String_Tuple()`
# that called itself instead of dispatching to the per-variant
# `Path.new$Path` / `Path.new$String` overloads. At runtime the body
# indexed `parts[0]` against a null Tuple → segfault.
#
# Root cause: try_emit_union_arg_dispatch (HIR) was gated on receiver_id
# being non-nil, so it only fired for instance methods. Class method
# calls (like `Path.new`) have nil receiver, so the union arg never got
# variant-branched dispatch.
#
# Fix: relax the receiver_id check at the call site of
# try_emit_union_arg_dispatch and accept ValueId? in its signature.
# After the fix: Path.[]$Path | String$arity2_splat emits union_is +
# branch + UnionUnwrap + per-variant Path.new$Path_Tuple() /
# Path.new$String_Tuple() and joins with empty Tuple correctly.
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/path_idx_empty_splat.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_OUT="$TMP_DIR/compile.out"
COMPILE_ERR="$TMP_DIR/compile.err"
RUN_OUT="$TMP_DIR/run.out"

cleanup() {
  if [[ "$KEEP_TMP" != "1" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
# Path::[] with no extra parts dispatches Path.new(name) where
# name : Path | String. Each variant has its own explicit overload.
puts Path["/tmp"].to_s
puts Path[Path.new("/tmp")].to_s
CR

set +e
"$COMPILER" "$SRC" -o "$BIN" >"$COMPILE_OUT" 2>"$COMPILE_ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "compile failed"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- stderr ---"
  cat "$COMPILE_ERR"
  echo "--- stdout ---"
  cat "$COMPILE_OUT"
  exit 2
fi

./scripts/run_safe.sh "$BIN" 5 256 >"$RUN_OUT"
stdout_text="$(awk '/^=== STDOUT ===/{flag=1;next}/^=== STDERR ===/{flag=0}flag' "$RUN_OUT" | tr -d '\r')"

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "stdout:"
printf '%s\n' "$stdout_text"

expected=$'/tmp\n/tmp'

if [[ "$stdout_text" == "$expected" ]]; then
  echo "fixed: Path::[] empty-splat dispatches via union_is branch correctly"
  exit 0
fi

echo "unexpected output (expected: /tmp twice)"
cat "$RUN_OUT"
exit 1
