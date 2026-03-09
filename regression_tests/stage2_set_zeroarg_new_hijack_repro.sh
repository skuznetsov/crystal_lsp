#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$1"
nm_bin="${NM_BIN:-$(command -v nm)}"
objdump_bin="${LLVM_OBJDUMP_BIN:-$(command -v llvm-objdump)}"

if [[ -z "${nm_bin}" || -z "${objdump_bin}" ]]; then
  echo "missing required tool: nm or llvm-objdump" >&2
  exit 2
fi

symbols="$("$nm_bin" -gU "$compiler" || true)"
if ! grep -q '_Set\$LString\$R\$Dnew$' <<<"$symbols"; then
  echo "missing symbol: _Set\$LString\$R\$Dnew" >&2
  exit 3
fi

if ! grep -q '_Set\$LString\$R\$Dnew\$\$Indexable\$LString\$R$' <<<"$symbols"; then
  echo "missing symbol: _Set\$LString\$R\$Dnew\$\$Indexable\$LString\$R" >&2
  exit 3
fi

disasm="$("$objdump_bin" -d --disassemble-symbols='_Set$LString$R$Dnew' "$compiler" || true)"
if [[ -z "$disasm" ]]; then
  echo "failed to disassemble _Set\$LString\$R\$Dnew" >&2
  exit 3
fi

if grep -q '__vdispatch__Indexable\$Hsize' <<<"$disasm"; then
  echo "reproduced: bare Set(String).new was hijacked by Indexable overload lowering"
  exit 0
fi

echo "not reproduced"
exit 1
