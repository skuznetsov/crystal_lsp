#!/bin/bash
# Regression: record macro initialize params must preserve default values
# so named-arg calls like ParseOptionsT.new(format: :general) work.
# Root cause was MacroNodeValue#node_identifier_name for TypeDeclarationNode
# only returned the bare name, dropping type annotation and default value.
# This caused record's "def initialize(@format, @decimal_point)" to have no
# defaults → required=2 → 1-arg named call missed → initialize$Symbol STUB.

set -e
COMPILER="${1:-bin/crystal_v2}"
TMPDIR_LOCAL="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_LOCAL"' EXIT

cat > "$TMPDIR_LOCAL/test.cr" << 'CRYSTAL'
module Float
  module FastFloat
    @[Flags]
    enum CharsFormat
      Scientific = 1 << 0
      Fixed      = 1 << 2
      General = Fixed | Scientific
    end

    record ParseOptionsT(UC),
      format : CharsFormat = CharsFormat::General,
      decimal_point : UC = '.'.ord.to_u8
  end
end

# zero-arg uses defaults
opts0 = Float::FastFloat::ParseOptionsT(UInt8).new
# named-arg: must resolve initialize, not generate STUB
opts1 = Float::FastFloat::ParseOptionsT(UInt8).new(format: Float::FastFloat::CharsFormat::General)
puts "ok"
CRYSTAL

"$COMPILER" "$TMPDIR_LOCAL/test.cr" -o "$TMPDIR_LOCAL/test_bin" 2>&1
"$TMPDIR_LOCAL/test_bin"
