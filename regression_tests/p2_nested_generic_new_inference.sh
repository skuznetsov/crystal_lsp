#!/usr/bin/env bash
# Guard constructor inference for nested generic classes declared under generic
# namespaces, e.g. Indexable(T)::IndexIterator(A).new(self).
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="${1:-$ROOT_DIR/bin/crystal_v2}"

if [[ ! -x "$COMPILER" ]]; then
  echo "ERROR: compiler not found: $COMPILER" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/p2_nested_generic_new_XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/indexable_iterators.cr"
OUT="$TMP_DIR/out"
LOG="$TMP_DIR/run.log"

cat >"$SRC" <<'CR'
a = ["x"]
items = a.each
items.next
again = items.each
again.next
indices = a.each_index
indices.next
CR

CRYSTAL_V2_STOP_AFTER_HIR=1 \
CRYSTAL_V2_PHASE_STATS=1 \
DEBUG_MISSING_SUMMARY=1 \
DEBUG_MISSING_SAMPLES=1 \
DEBUG_MISSING_TOP=40 \
  "$ROOT_DIR/scripts/run_safe.sh" "$COMPILER" 60 1024 \
    "$SRC" --emit hir --no-link -o "$OUT" >"$LOG" 2>&1

HIR="$OUT.hir"
if [[ ! -s "$HIR" ]]; then
  echo "p2 nested generic new regression: missing HIR artifact" >&2
  cat "$LOG" >&2
  exit 1
fi

if grep -q 'Indexable(T)::IndexIterator.new\$Array(String)' "$HIR" "$LOG"; then
  echo "p2 nested generic new regression: unspecialized IndexIterator.new remained" >&2
  grep -n 'Indexable(T)::IndexIterator.new\$Array(String)' "$HIR" "$LOG" >&2 || true
  exit 1
fi

if grep -q 'Indexable(T)::ItemIterator(Indexable(T)::ItemIterator(Array(String), String), String).new' "$HIR" "$LOG"; then
  echo "p2 nested generic new regression: ItemIterator#each used Indexable#each body" >&2
  grep -n 'Indexable(T)::ItemIterator(Indexable(T)::ItemIterator(Array(String), String), String).new' "$HIR" "$LOG" >&2 || true
  exit 1
fi

for required in \
  'func @Indexable(T)::ItemIterator(Array(String), String).new\$Array(String)' \
  'func @Indexable(T)::ItemIterator(Array(String), String)#each' \
  'func @Indexable(T)::ItemIterator(Array(String), String)#next' \
  'func @Indexable(T)::IndexIterator(Array(String)).new\$Array(String)' \
  'func @Indexable(T)::IndexIterator(Array(String))#next'
do
  if ! grep -q "$required" "$HIR"; then
    echo "p2 nested generic new regression: missing $required" >&2
    grep -n 'Indexable(T)::.*Iterator' "$HIR" >&2 || true
    cat "$LOG" >&2
    exit 1
  fi
done

echo "p2_nested_generic_new_inference_ok"
