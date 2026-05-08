#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
compiler="$1"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/cv2_cli_output_tail.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

cat >"$tmpdir/repro.cr" <<'CR'
class Exception
  class CallStack
    def self.skip(path : String) : Nil
    end
  end
end

Exception::CallStack.skip("x")
CR

emit_log="$tmpdir/emit.log"
emit_base="$tmpdir/repro_emit"
ir="$tmpdir/repro_emit.ll"

set +e
"$ROOT_DIR/scripts/run_safe.sh" "$compiler" 60 4096 \
  "$tmpdir/repro.cr" --no-prelude --emit llvm-ir --no-link -o "$emit_base" \
  >"$emit_log" 2>&1
emit_rc=$?
set -e

if [[ $emit_rc -ne 0 ]]; then
  echo "CLI output tail guard failed before binary mode: emit-only LLVM mode failed" >&2
  tail -n 120 "$emit_log" >&2 || true
  exit 1
fi

if [[ ! -f "$ir" ]]; then
  awk '
    /^=== STDOUT ===$/ {capture=1; next}
    /^=== STDERR ===$/ {capture=0}
    capture {print}
  ' "$emit_log" >"$ir"
fi

if [[ ! -s "$ir" ]]; then
  echo "CLI output tail guard produced no LLVM IR in emit-only mode" >&2
  tail -n 120 "$emit_log" >&2 || true
  exit 1
fi

grep -Fq 'call void @Exception$CCCallStack$Dskip$$String(ptr @.str.0)' "$ir"
grep -Fq 'define void @Exception$CCCallStack$Dskip$$String' "$ir"

if command -v llc >/dev/null 2>&1; then
  llc -filetype=obj -o "$tmpdir/repro_emit.o" "$ir"
fi

bin_log="$tmpdir/bin.log"
bin_out="$tmpdir/repro_bin"

set +e
CRYSTAL_V2_TRACE_STDERR=1 "$ROOT_DIR/scripts/run_safe.sh" "$compiler" 60 4096 \
  "$tmpdir/repro.cr" --no-prelude -o "$bin_out" \
  >"$bin_log" 2>&1
bin_rc=$?
set -e

if [[ $bin_rc -ne 0 ]]; then
  echo "CLI output tail guard failed in normal binary-output mode" >&2
  tail -n 160 "$bin_log" >&2 || true
  exit 1
fi

if [[ ! -x "$bin_out" ]]; then
  echo "CLI output tail guard did not produce an executable binary" >&2
  tail -n 120 "$bin_log" >&2 || true
  exit 1
fi

grep -Fq '[STAGE2_TRACE] step5: generate done' "$bin_log"

"$ROOT_DIR/scripts/run_safe.sh" "$bin_out" 5 512 >/dev/null

echo "p2_stage2_cli_output_tail_no_prelude_ok"
