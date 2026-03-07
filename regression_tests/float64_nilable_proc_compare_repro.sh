#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUN_SAFE="$REPO_ROOT/scripts/run_safe.sh"

if ! command -v opt >/dev/null 2>&1; then
  echo "opt is required but not found in PATH"
  exit 2
fi

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/float64_nilable_proc_compare.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
WRAPPER="$TMP_DIR/compile.sh"
OUT_BASE="$TMP_DIR/repro"
LL_FILE="$OUT_BASE.ll"
SAFE_OUT="$TMP_DIR/compile.safe.out"
SAFE_ERR="$TMP_DIR/compile.safe.err"
OPT_ERR="$TMP_DIR/opt.err"

cleanup() {
  if [[ "$KEEP_TMP" != "1" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
def probe
  progress_match = true
  slow_ms = nil
  if progress_match
    slow_ms = 50.0
  end

  body_proc = -> {
    expr_start = slow_ms ? Time.instant : nil
    if slow_ms && expr_start
      elapsed = (Time.instant - expr_start).total_milliseconds
      if elapsed >= slow_ms
        STDERR.puts elapsed.round(1)
      end
    end
  }

  body_proc.call
end

probe
CR

cat >"$WRAPPER" <<EOF
#!/usr/bin/env bash
exec "$COMPILER" "$SRC" --emit llvm-ir --no-link -o "$OUT_BASE"
EOF
chmod +x "$WRAPPER"

set +e
"$RUN_SAFE" "$WRAPPER" 120 2048 >"$SAFE_OUT" 2>"$SAFE_ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 || ! -f "$LL_FILE" ]]; then
  echo "compile failed"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- safe stdout ---"
  cat "$SAFE_OUT"
  echo "--- safe stderr ---"
  cat "$SAFE_ERR"
  exit 2
fi

set +e
opt -O0 "$LL_FILE" -o "$OUT_BASE.opt.bc" >"$TMP_DIR/opt.out" 2>"$OPT_ERR"
opt_status=$?
set -e

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "compile_status: $compile_status"
echo "opt_status: $opt_status"

if grep -Eq 'icmp ne i32 .*double|icmp eq i32 .*double|icmp s[lg]e i32 .*double|icmp u[lg]e i32 .*double' "$LL_FILE"; then
  echo "reproduced: invalid int-vs-double compare leaked into nilable Float64 proc comparison"
  exit 0
fi

if [[ $opt_status -ne 0 ]]; then
  echo "reproduced: opt rejected generated LLVM IR"
  echo "--- opt stderr ---"
  cat "$OPT_ERR"
  exit 0
fi

if grep -Fq 'payload_cmp = fcmp oge double' "$LL_FILE"; then
  echo "not reproduced"
  exit 1
fi

echo "unexpected IR pattern"
echo "--- opt stderr ---"
  cat "$OPT_ERR"
echo "--- IR snippet ---"
rg -n 'payload_cmp|fcmp|icmp|Nil\$_\$OR\$_Float64\.union' "$LL_FILE" -n -C 2 | sed -n '1,80p'
exit 2
