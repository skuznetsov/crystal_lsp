#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/compiler" >&2
  exit 2
fi

compiler=$1
repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
source_file="$repo_root/regression_tests/stage2_reparsed_module_wrapper_repro.cr"
log_dir=$(mktemp -d "${TMPDIR:-/tmp}/stage2_reparsed_module_wrapper_repro.XXXXXX")

set +e
"$repo_root/scripts/timeout_sample_lldb.sh" \
  -t 20 -m 2048 -s 5 -l 10 -n 6 --no-series \
  -o "$log_dir" \
  -- "$compiler" --release --no-prelude "$source_file" \
  -o "$log_dir/out"
status=$?
set -e

cmd_log="$log_dir/command.log"

if [[ $status -eq 0 ]]; then
  echo "not reproduced: compiler finished the path-wrapper HIR repro"
  echo "log: $log_dir"
  exit 0
fi

if [[ -f "$cmd_log" ]] && grep -qF "[STAGE2_TRACE] lower_main: exprs=0" "$cmd_log"; then
  echo "not reproduced: compiler reached lower_main exprs=0 on the path-wrapper repro"
  echo "log: $log_dir"
  exit 0
fi

case "$status" in
  125|138|139)
    echo "reproduced: compiler failed before lower_main on the path-wrapper module repro"
    echo "log: $log_dir"
    exit 1
    ;;
  *)
    echo "inconclusive: unexpected status=$status"
    echo "log: $log_dir"
    exit 2
    ;;
esac
