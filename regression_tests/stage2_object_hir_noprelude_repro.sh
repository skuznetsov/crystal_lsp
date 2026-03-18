#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/compiler" >&2
  exit 2
fi

compiler=$1
repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
log_dir=$(mktemp -d "${TMPDIR:-/tmp}/stage2_object_hir_noprelude_repro.XXXXXX")

set +e
/usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 \
  "$repo_root/scripts/timeout_sample_lldb.sh" \
  -t 120 -m 8192 -s 5 -l 10 -n 8 --no-series \
  -o "$log_dir" \
  -- "$compiler" --release --no-prelude "$repo_root/src/stdlib/object.cr" -o "$log_dir/out"
status=$?
set -e

case "$status" in
  0)
    echo "not reproduced"
    exit 0
    ;;
  125|138|139)
    echo "reproduced: object HIR no-prelude compile failed with status=$status"
    echo "log: $log_dir"
    exit 1
    ;;
  *)
    echo "inconclusive: unexpected status=$status"
    echo "log: $log_dir"
    exit 2
    ;;
esac
