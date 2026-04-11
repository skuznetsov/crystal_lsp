# Regression: sprintf("%.Nf", Float64) with explicit precision.
#
# Prior bugs (both fixed in this change):
#   1. `Char - Char` (e.g. `'2' - '0'`) was inferred as Char instead of Int32
#      in lower_binary_primitive, so consume_number read the type-id of the
#      Char|Int32 union (15) as the precision value. Result: always 15 digits.
#   2. `===` was not recognized by is_comparison_op?, so BinaryOperation(Eq)
#      for `===` was typed as the left operand type instead of Bool. This
#      broke `if cond` downstream of `cond = x === y`, causing Ryu's rounding
#      block to unconditionally take the wrong branch on precision=0.
#
# EXPECT: sprintf_float_precision_ok

raise "case1 got #{sprintf("%.3f", 236.15_f64).inspect}" unless sprintf("%.3f", 236.15_f64) == "236.150"
raise "case2 got #{sprintf("%.1f", 230119292.0_f64).inspect}" unless sprintf("%.1f", 230119292.0_f64) == "230119292.0"
raise "case3 got #{sprintf("%.3f", 1.2345_f64).inspect}" unless sprintf("%.3f", 1.2345_f64) == "1.234"
raise "case4 got #{sprintf("%.0f", 2.5_f64).inspect}" unless sprintf("%.0f", 2.5_f64) == "2"

# Additional precision variants
raise "case5" unless sprintf("%.2f", 1.005_f64) == "1.00"
raise "case6" unless sprintf("%.5f", 0.1_f64) == "0.10000"
raise "case7" unless sprintf("%.0f", 3.5_f64) == "4"

# Multi-digit precision (e.g. "%.10f") is still broken: consume_number
# parses digits via a `case/when` inside a `while` loop, and V2 currently
# loses local mutations across while iterations when the body is a case
# expression. Re-enable once that is fixed.
# raise "case8" unless sprintf("%.10f", 1.0_f64) == "1.0000000000"

# Direct exercise of === operator (regression for is_comparison_op? fix)
c = 50_u8
cond = c === '-'
raise "=== should be false for '2' === '-'" if cond
cond2 = c === '2'
raise "=== should be true for '2' === '2'" unless cond2

puts "sprintf_float_precision_ok"
