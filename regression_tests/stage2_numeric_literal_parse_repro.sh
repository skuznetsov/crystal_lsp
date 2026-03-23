#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/compiler" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER="$1"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/stage2_numeric_literal_parse.XXXXXX")"
LITERALS=(
  "1"
  "1_2"
  "1.5"
  "1e2"
  "1_f32"
  "1i64"
  "1u8"
)

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

if [[ ! -x "$COMPILER" ]]; then
  echo "error: compiler binary not found/executable: $COMPILER" >&2
  exit 2
fi

for literal in "${LITERALS[@]}"; do
  safe_name="$(printf '%s' "$literal" | tr -c 'A-Za-z0-9_' '_')"
  src="$WORKDIR/${safe_name}.cr"
  out="$WORKDIR/${safe_name}.out"
  wrapper="$WORKDIR/${safe_name}.sh"
  log="$WORKDIR/${safe_name}.log"

  printf '%s\n' "$literal" >"$src"

  {
    echo "#!/usr/bin/env bash"
    echo "set -euo pipefail"
    echo "export CRYSTAL_V2_STOP_AFTER_PARSE=1"
    printf 'exec %q ' "$COMPILER"
    printf '%q ' --release --no-prelude
    printf '%q ' "$src" -o "$out"
    echo
  } >"$wrapper"
  chmod +x "$wrapper"

  set +e
  "$ROOT_DIR/scripts/run_safe.sh" "$wrapper" 15 1024 >"$log" 2>&1
  rc=$?
  set -e

  if [[ $rc -ne 0 ]]; then
    echo "reproduced: self-hosted stage2 failed parse-only on numeric literal '$literal'"
    tail -n 80 "$log"
    exit 1
  fi
done

echo "not reproduced: stage2 survives parse-only on plain, underscored, float, exponent, and suffix numeric literals"
