#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/compiler" >&2
  exit 2
fi

compiler=$1
repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
workdir=$(mktemp -d "${TMPDIR:-/tmp}/stage1_join_io_tuple_block_runtime_oracle.XXXXXX")
compile_wrap="$workdir/compile.sh"
run_wrap="$workdir/run.sh"
bin="$workdir/probe"
log="$workdir/run.log"

cleanup() {
  rm -rf "$workdir"
}
trap cleanup EXIT

cat >"$compile_wrap" <<EOF
#!/usr/bin/env bash
set -euo pipefail
export CRYSTAL_V2_LLVM_WORKERS=1
export CRYSTAL_V2_LLVM_REUSE_BLOCK_BUFFER=1
exec "$compiler" "$repo_root/regression_tests/stage1_join_io_tuple_block_runtime_oracle.cr" -o "$bin"
EOF
chmod +x "$compile_wrap"

"$repo_root/scripts/run_safe.sh" "$compile_wrap" 420 3072 >/dev/null

cat >"$run_wrap" <<EOF
#!/usr/bin/env bash
set -euo pipefail
exec "$bin"
EOF
chmod +x "$run_wrap"

set +e
"$repo_root/scripts/run_safe.sh" "$run_wrap" 10 512 >"$log" 2>&1
run_status=$?
set -e

if [[ $run_status -ne 0 ]]; then
  cat "$log" >&2
  echo "reproduced: join(io) tuple-block runtime failed"
  exit 1
fi

if ! grep -Fq "1:a, 2:b, 3:c" "$log"; then
  cat "$log" >&2
  echo "reproduced: unexpected join(io) tuple-block output"
  exit 1
fi

echo "not reproduced: join(io) tuple-block runtime exited cleanly"
