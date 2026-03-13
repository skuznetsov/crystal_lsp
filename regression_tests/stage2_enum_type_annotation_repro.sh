#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
workdir="$(mktemp -d "${TMPDIR:-/tmp}/crystalv2-stage2-enum-type.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT

src="$workdir/repro.cr"
stdout_file="$workdir/out.txt"
stderr_file="$workdir/err.txt"

cat >"$src" <<'CR'
enum Sig : Int32
  A
  def trap : Nil
  end
end
CR

set +e
"$compiler" build "$src" --no-prelude --no-codegen >"$stdout_file" 2>"$stderr_file"
status=$?
set -e

if [[ $status -eq 139 ]]; then
  echo "reproduced: compiler crashed on enum type annotation parse"
  exit 0
fi

if [[ $status -eq 0 ]]; then
  if grep -q "Parsed 1 top-level expressions" "$stdout_file"; then
    echo "not reproduced"
    exit 1
  fi
  echo "--- stdout ---" >&2
  cat "$stdout_file" >&2
  echo "--- stderr ---" >&2
  cat "$stderr_file" >&2
  echo "unexpected success output" >&2
  exit 3
fi

echo "--- stdout ---" >&2
cat "$stdout_file" >&2
echo "--- stderr ---" >&2
cat "$stderr_file" >&2
printf 'unexpected status: %s\n' "$status" >&2
exit 3
