#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
workdir="$(mktemp -d /tmp/crystalv2-do-block-hash.XXXXXX)"
trap 'rm -rf "$workdir"' EXIT

cat > "$workdir/repro.cr" <<'CR'
foo do |x|
  x
end
CR

set +e
"$compiler" "$workdir/repro.cr" --no-prelude --no-codegen >"$workdir/out.txt" 2>"$workdir/err.txt"
status=$?
set -e

if [ $status -eq 139 ]; then
  echo "reproduced: compiler crashed while hashing Bytes key during top-level do-block compile"
  exit 0
fi

if [ $status -eq 1 ] && {
  grep -q "undefined local variable or method 'foo'" "$workdir/err.txt" ||
  grep -q "compilation failed due to name resolution errors" "$workdir/err.txt"
}; then
  echo "not reproduced: compiler reached expected name-resolution error"
  exit 1
fi

cat "$workdir/err.txt" >&2
printf 'unexpected status: %s\n' "$status" >&2
exit 3
