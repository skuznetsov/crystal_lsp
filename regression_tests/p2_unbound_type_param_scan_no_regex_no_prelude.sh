#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_unbound_type_params.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

cat >"$tmpdir/repro.cr" <<'CR'
module N
  def foo(x : Array(T)) : Nil
  end
end

class C
  include N
end
CR

log="$tmpdir/repro.log"
out="$tmpdir/repro"

"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 30 1024 \
  "$tmpdir/repro.cr" --no-prelude --emit llvm-ir --no-link -o "$out" \
  >"$log" 2>&1

if [[ ! -s "$out.ll" ]]; then
  echo "unbound type-param scan guard did not emit LLVM IR" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

if grep -Eq 'Regex\$CCMatchData|unbound_type_params_from_type_name|Segmentation fault|Bus error|EXC_BAD_ACCESS|\\[CRASH\\]' "$log"; then
  echo "unbound type-param scan guard saw the Regex/MatchData crash family" >&2
  tail -120 "$log" >&2 || true
  exit 1
fi

echo "p2_unbound_type_param_scan_no_regex_no_prelude_ok"
