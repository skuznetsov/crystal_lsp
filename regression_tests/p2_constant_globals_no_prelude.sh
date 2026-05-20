#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_constant_globals.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

cat >"$tmpdir/repro.cr" <<'CR'
private UNLOCKED = 0
private LOCKED = 1
CR

log="$tmpdir/repro.log"
out="$tmpdir/repro"

"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 30 1024 \
  "$tmpdir/repro.cr" --no-prelude --emit llvm-ir --no-link -o "$out" \
  >"$log" 2>&1

if [[ ! -s "$out.ll" ]]; then
  echo "constant global guard did not emit LLVM IR" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

if grep -Eq 'Segmentation fault|Bus error|EXC_BAD_ACCESS|\[CRASH\]' "$log"; then
  echo "constant global guard crashed during LLVM emission" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

if grep -Eq '@(__classvar__|[^[:space:]]+__classvar__)([[:space:]=]|$)' "$out.ll"; then
  echo "constant global guard emitted malformed class-var global name" >&2
  grep -En '@(__classvar__|[^[:space:]]+__classvar__)([[:space:]=]|$)' "$out.ll" >&2 || true
  exit 1
fi

grep -Eq '@Object__classvar__UNLOCKED[[:space:]]*=[[:space:]]*global i32' "$out.ll"
grep -Eq '@Object__classvar__LOCKED[[:space:]]*=[[:space:]]*global i32' "$out.ll"
grep -Eq 'store i32 1, ptr @Object__classvar__LOCKED' "$out.ll"

echo "p2_constant_globals_no_prelude_ok"
