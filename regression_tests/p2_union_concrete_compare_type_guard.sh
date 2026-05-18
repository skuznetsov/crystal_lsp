#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_union_concrete_compare.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

cat >"$tmpdir/repro.cr" <<'CR'
def payload(mangled : String)
  key = mangled.split("_", 2)[1]?
  if key == "i32" || key == "u32"
    1
  else
    0
  end
end

payload("x_i32")
CR

log="$tmpdir/repro.log"
out="$tmpdir/repro"

"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 60 2048 \
  "$tmpdir/repro.cr" --emit llvm-ir --no-link -o "$out" \
  >"$log" 2>&1

if [[ ! -s "$out.ll" ]]; then
  echo "union concrete compare guard did not emit LLVM IR" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

if grep -qE 'left_payload = load float|left_as_int = add i64 .*left_payload' "$out.ll"; then
  echo "union concrete compare guard found first-variant payload integerization" >&2
  grep -nE 'left_payload = load float|left_as_int = add i64 .*left_payload' "$out.ll" >&2 || true
  exit 1
fi

grep -q 'type_match = icmp eq i32' "$out.ll"
grep -q 'payload_cmp = icmp eq i64' "$out.ll"

echo "p2_union_concrete_compare_type_guard_ok"
