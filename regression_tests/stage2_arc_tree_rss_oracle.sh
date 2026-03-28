#!/bin/bash
set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <compiler> [max_rss_bytes]" >&2
  exit 1
fi

ROOT="/Users/sergey/Projects/Crystal/crystal_v2_repo"
COMPILER="$1"
MAX_RSS_BYTES="${2:-67108864}" # 64 MB default ceiling for the debug oracle
SRC="$ROOT/regression_tests/stage2_arc_tree_rss_oracle.cr"
WORKDIR="$(mktemp -d /tmp/stage2_arc_tree_rss.XXXXXX)"
BIN="$WORKDIR/tree_oracle"
COMPILE_WRAP="$WORKDIR/compile.sh"
RUN_WRAP="$WORKDIR/run.sh"
RUN_LOG="$WORKDIR/run.log"

cleanup() {
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

cat > "$COMPILE_WRAP" <<EOF
#!/bin/bash
cd "$ROOT" || exit 1
exec "$COMPILER" "$SRC" -o "$BIN"
EOF
chmod +x "$COMPILE_WRAP"
"$ROOT/scripts/run_safe.sh" "$COMPILE_WRAP" 40 2048 >/dev/null

cat > "$RUN_WRAP" <<EOF
#!/bin/bash
/usr/bin/time -l "$BIN"
EOF
chmod +x "$RUN_WRAP"

if ! "$ROOT/scripts/run_safe.sh" "$RUN_WRAP" 30 512 >"$RUN_LOG" 2>&1; then
  cat "$RUN_LOG" >&2
  echo "reproduced: runtime failed"
  exit 0
fi

RSS_BYTES="$(awk '/maximum resident set size/ {print $1; exit}' "$RUN_LOG")"
if [ -z "$RSS_BYTES" ]; then
  cat "$RUN_LOG" >&2
  echo "reproduced: missing rss sample"
  exit 0
fi

if [ "$RSS_BYTES" -le "$MAX_RSS_BYTES" ]; then
  echo "not reproduced: rss=${RSS_BYTES} <= ${MAX_RSS_BYTES}"
else
  echo "reproduced: rss=${RSS_BYTES} > ${MAX_RSS_BYTES}"
fi
