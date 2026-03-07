#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUN_SAFE="$REPO_ROOT/scripts/run_safe.sh"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/inline_ownerless_each_with_index.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
WRAPPER="$TMP_DIR/compile.sh"
OUT_BASE="$TMP_DIR/repro"
LL_FILE="$OUT_BASE.ll"
SAFE_OUT="$TMP_DIR/compile.safe.out"
SAFE_ERR="$TMP_DIR/compile.safe.err"

cleanup() {
  if [[ "$KEEP_TMP" != "1" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
module MiniEnumerable(T)
  abstract def each(& : T ->)

  def each_with_index(offset = 0, &)
    i = offset
    each do |elem|
      yield elem, i
      i += 1
    end
  end
end

class Foo
  include MiniEnumerable(Int32)

  def each(& : Int32 ->)
    yield 10
    yield 20
    yield 30
  end
end

def drive(foo : Foo) : Int32
  last = -1
  foo.each_with_index do |expr_id, idx|
    last = expr_id + idx
  end
  last
end

drive(Foo.new)
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

drive_body="$(awk '
  /^define i32 @drive\$\$Foo\(ptr %foo\) \{/ { in_fn=1 }
  in_fn { print }
  in_fn && /^}/ { exit }
' "$LL_FILE")"

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"

if [[ -z "$drive_body" ]]; then
  echo "unexpected: drive\$\$Foo not found"
  exit 2
fi

if grep -q 'ret i32 -1' <<<"$drive_body"; then
  echo "reproduced: owner-less top-level each_with_index block writeback still returns seed value"
  exit 0
fi

if grep -q 'ret i32 32' <<<"$drive_body"; then
  echo "not reproduced"
  exit 1
fi

echo "unexpected IR pattern"
echo "--- drive\$\$Foo ---"
printf '%s\n' "$drive_body"
exit 2
