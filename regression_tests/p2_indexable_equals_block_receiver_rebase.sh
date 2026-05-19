#!/usr/bin/env bash
set -euo pipefail

compiler="${1:-bin/crystal_v2}"
tmpdir="$(mktemp -d /tmp/cv2_indexable_equals_block.XXXXXX)"
trap 'rm -rf "$tmpdir"' EXIT

cat > "$tmpdir/repro.cr" <<'CR'
a = [1]
b = [1]
puts(a == b)
CR

log="$tmpdir/repro.log"
scripts/run_safe.sh "$compiler" 120 4096 \
  "$tmpdir/repro.cr" --emit llvm-ir --no-link -o "$tmpdir/repro" \
  >"$log" 2>&1

if grep -q 'Indexable\$LT\$R\$Hequals\$Q\$\$Indexable_block' "$log"; then
  echo "unexpected generic Indexable(T)#equals? block target" >&2
  grep -n 'Indexable\$LT\$R\$Hequals\$Q\$\$Indexable_block' "$log" >&2
  exit 1
fi

if grep -q 'STUB CALLED: Indexable\$LT\$R\$Hequals' "$log"; then
  echo "unexpected Indexable(T)#equals? abort stub" >&2
  grep -n 'STUB CALLED: Indexable\$LT\$R\$Hequals' "$log" >&2
  exit 1
fi

grep -q 'call i1 @Array\$LInt32\$R\$Hequals\$Q\$\$Array\$LInt32\$R_block' "$log"

echo "p2_indexable_equals_block_receiver_rebase_ok"
