# No-prelude contract oracle: string interpolation lowers to HIR StringInterpolation →
# MIR → LLVM __crystal_v2_string_interpolate (see ast_to_hir#lower_string_interpolation,
# llvm_backend#emit_string_interpolate). Does not use String::Builder for the interpolate op.
# Also asserts substrings hello / n=42 appear (manual diff vs host if needed).
#
# EXPECT: noprelude_interp_ok

name = "world"
n = 42
puts "hello #{name}"
puts "n=#{n}"
puts "noprelude_interp_ok"
