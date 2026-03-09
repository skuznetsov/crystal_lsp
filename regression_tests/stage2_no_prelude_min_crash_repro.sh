#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
workdir="$(mktemp -d "${TMPDIR:-/tmp}/crystalv2-no-prelude-min.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT

src="$workdir/repro.cr"
stdout_file="$workdir/out.txt"
stderr_file="$workdir/err.txt"

cat >"$src" <<'CR'
X = 1
CR

set +e
"$compiler" build "$src" --no-prelude --no-codegen >"$stdout_file" 2>"$stderr_file"
status=$?
set -e

if [[ $status -eq 139 ]]; then
  echo "reproduced: compiler crashed on minimal no-prelude compile"
  exit 0
fi

if [[ $status -eq 0 ]]; then
  echo "not reproduced"
  exit 1
fi

echo "--- stdout ---" >&2
cat "$stdout_file" >&2
echo "--- stderr ---" >&2
cat "$stderr_file" >&2
printf 'unexpected status: %s\n' "$status" >&2
exit 3
