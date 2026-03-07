#!/usr/bin/env bash
set -euo pipefail

COMPILER="${1:-./bin/crystal_v2}"
KEEP_TMP="${KEEP_TMP:-0}"

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/contextual_builtin_generic_cache.XXXXXX")"
SRC="$TMP_DIR/repro.cr"
BIN="$TMP_DIR/repro.bin"
COMPILE_OUT="$TMP_DIR/compile.out"
COMPILE_ERR="$TMP_DIR/compile.err"

cleanup() {
  if [[ "$KEEP_TMP" != "1" ]]; then
    rm -rf "$TMP_DIR"
  fi
}
trap cleanup EXIT

cat >"$SRC" <<'CR'
module CrystalV2
  module Compiler
    module Frontend
      struct ExprId
        getter index : Int32

        def initialize(@index : Int32)
        end
      end

      class WhileNode
        getter body : Array(ExprId)

        def initialize(@body : Array(ExprId))
        end
      end

      class DefNode
        getter body : Array(ExprId)?

        def initialize(@body : Array(ExprId)?)
        end
      end
    end
  end
end

frontend = CrystalV2::Compiler::Frontend::DefNode.new([CrystalV2::Compiler::Frontend::ExprId.new(1)])
frontend_body = frontend.body

puts frontend_body.not_nil!.first.index
CR

set +e
DEBUG_MEMBER_CALL=body,index "$COMPILER" build "$SRC" --emit llvm-ir -o "$BIN" >"$COMPILE_OUT" 2>"$COMPILE_ERR"
compile_status=$?
set -e

if [[ $compile_status -ne 0 ]]; then
  echo "compile failed"
  echo "compiler: $COMPILER"
  echo "status: $compile_status"
  echo "tmp_dir: $TMP_DIR"
  echo "--- stderr ---"
  cat "$COMPILE_ERR"
  echo "--- stdout ---"
  cat "$COMPILE_OUT"
  exit 2
fi

echo "compiler: $COMPILER"
echo "tmp_dir: $TMP_DIR"
echo "member_call traces:"
member_lines="$(grep '\[MEMBER_CALL\]' "$COMPILE_ERR" | tr -d '\r' || true)"
printf '%s\n' "$member_lines"

if grep -Fq 'name=CrystalV2::Compiler::Frontend::DefNode#body' <<<"$member_lines" &&
   grep -Fq 'return=Nil | Array(CrystalV2::Compiler::Frontend::ExprId)' <<<"$member_lines"; then
  echo "not reproduced"
  exit 1
fi

if [[ -z "${member_lines//[$'\n\t ']}" ]]; then
  echo "reproduced: no member getter typing trace was emitted"
  exit 0
fi

echo "reproduced: contextual built-in generic cache returns wrong getter type"
exit 0
