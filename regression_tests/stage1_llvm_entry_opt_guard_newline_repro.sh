#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-}"
if [[ -z "$COMPILER" || ! -x "$COMPILER" ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

if ! command -v opt >/dev/null 2>&1; then
  echo "opt is required but not found in PATH" >&2
  exit 2
fi

if ! command -v llvm-dis >/dev/null 2>&1; then
  echo "llvm-dis is required but not found in PATH" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/stage1_llvm_entry_guard.XXXXXX")"
SRC="$TMP_DIR/hello.cr"
OUT="$TMP_DIR/hello"
LL="$OUT.ll"
OPT_BC="$LL.opt.bc"
OPT_LL="$LL.opt.ll"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

cat > "$SRC" <<'CR'
puts 1
CR

CRYSTAL_V2_PIPELINE_CACHE=0 "$COMPILER" "$SRC" --release --no-link -o "$OUT" >/dev/null 2>"$TMP_DIR/build.err"

if ! perl -0ne 'exit(index($_, "\n") >= 0 ? 0 : 1)' "$LL"; then
  echo "reproduced (entry opt guard flattened LLVM IR into a single line)"
  file "$LL"
  exit 1
fi

if ! rg -q 'noinline optnone' "$LL"; then
  echo "reproduced (entry opt guard did not patch entry functions)"
  exit 1
fi

opt -O0 "$LL" -o "$OPT_BC" >/dev/null 2>"$TMP_DIR/opt.err"
llvm-dis "$OPT_BC" -o "$OPT_LL"

if ! rg -q '^define .*@main\(' "$OPT_LL"; then
  echo "reproduced (opt output lost main after entry guard rewrite)"
  exit 1
fi

if ! rg -q '^define .*@__crystal_main\(' "$OPT_LL"; then
  echo "reproduced (opt output lost __crystal_main after entry guard rewrite)"
  exit 1
fi

echo "not reproduced"
