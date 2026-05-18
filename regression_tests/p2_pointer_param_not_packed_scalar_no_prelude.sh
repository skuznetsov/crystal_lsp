#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_pointer_param_not_packed.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

cat >"$tmpdir/repro.cr" <<'CR'
lib LibC
  fun exit(status : Int32) : NoReturn
end

x = 4.0_f64
p = pointerof(x)
y = p.value / 2.0_f64

if y == 2.0_f64
  LibC.exit(3)
else
  LibC.exit(7)
end
CR

log="$tmpdir/repro.log"
out="$tmpdir/repro"

"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 60 2048 \
  "$tmpdir/repro.cr" --no-prelude -o "$out" \
  >"$log" 2>&1

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$out" 5 256 >"$tmpdir/run.log" 2>&1
run_rc=$?
set -e

if [[ $run_rc -ne 3 ]]; then
  echo "pointer parameter guard expected exit code 3, got $run_rc" >&2
  cat "$tmpdir/run.log" >&2
  exit 1
fi

echo "p2_pointer_param_not_packed_scalar_no_prelude_ok"
