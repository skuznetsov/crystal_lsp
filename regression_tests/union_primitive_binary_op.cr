# Regression: primitive-union receiver binary ops (lower_binary intercept).
#
# Root cause (fixed): when a binary op `left op right` had a union left type
# whose variants were all numeric primitives (e.g. `Int32 | UInt32`), the old
# lowering path in ast_to_hir.cr#lower_binary bypassed the primitive inline
# path (because `is_integer_type` checks only Int8..Int128) and fell through
# either to the non-integer `<<` method branch or to `emit_binary_call`.
# Both paths built a method name like `Int32 | UInt32#%$Int32` and emitted a
# Call with virtual=false. hir_to_mir couldn't resolve the method, routed to
# extern_call, and crashed at runtime with:
#   STUB CALLED: Int32$_$OR$_UInt32$H$MOD$$Int32
#   STUB CALLED: Int32$_$OR$_UInt32$H$SHL$$Int32
#
# Fix: try_lower_binary_primitive_union dispatches per variant inline via
# UnionIs / UnionUnwrap / BinaryOperation / UnionWrap / Phi, wrapping
# arithmetic results back into the source union so downstream types match
# Crystal semantics.
#
# This test exercises %, //, <<, >>, +, -, *, and comparison on a mixed
# Int32 | UInt32 receiver so any regression in the inline dispatch trips it.
#
# EXPECT: union_prim_binop_ok

d : Int32 | UInt32 = 236_i32
d = 236_u32 if 1 == 1

puts d % 10                 # 6  (UInt32 % Int32)
puts d // 10                # 23 (UInt32 // Int32)
puts d << 1                 # 472
puts d >> 2                 # 59
puts d + 4                  # 240
puts d - 6                  # 230
puts d * 2                  # 472

# Comparison returns Bool on both variants.
puts (d > 100)              # true
puts (d == 236)             # true

# Negative signed path (flooring semantics for // and %).
s : Int32 | UInt32 = -7_i32
puts s % 3                  # 2  (flooring, not truncating)
puts s // 3                 # -3

puts "union_prim_binop_ok"
