#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
bin="${root}/bin/crystal_v2"

if [[ ! -x "$bin" ]]; then
  echo "error: ${bin} not found or not executable" >&2
  exit 1
fi

files=("${root}"/examples/bootstrap_*.cr)
if [[ ${#files[@]} -eq 0 ]]; then
  echo "error: no bootstrap_*.cr files found in ${root}/examples" >&2
  exit 1
fi

echo "mode,file,mir_opt_ms,total_ms"

run_one() {
  local mode="$1"
  local file="$2"
  local args=("--stats" "--no-link" "--no-llvm-opt")

  if [[ "$mode" == "no-ltp" ]]; then
    args+=("--no-ltp")
  fi

  local timing
  timing="$("$bin" "${args[@]}" "$file" 2>/dev/null | rg -m1 '^Timing ')"

  local mir_opt total
  mir_opt="$(printf '%s\n' "$timing" | sed -n 's/.*mir_opt=\([^ ]*\).*/\1/p')"
  total="$(printf '%s\n' "$timing" | sed -n 's/.*total=\([^ ]*\).*/\1/p')"

  echo "${mode},$(basename "$file"),${mir_opt:-NA},${total:-NA}"
}

for file in "${files[@]}"; do
  run_one ltp "$file"
  run_one no-ltp "$file"
done
