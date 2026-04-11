# Regression test: module-scoped constants initialized with String#to_unsafe
# must be typed as Pointer(UInt8), not Pointer(Char).
#
# Bug: `element_type_for_type_name("String")` returned "Char" (iteration element),
# which was used by the `to_unsafe` fallback in `infer_type_from_expr`, so the
# constant type became Pointer(Char). Downstream codegen then used stride 4
# (Char is Int32) instead of stride 1 (UInt8), producing garbage when indexing.
# EXPECT: module_const_to_unsafe_ok

module Foo
  TABLE_PTR = "hello".to_unsafe
end

ptr = Foo::TABLE_PTR
b0 = ptr[0].to_i
b1 = ptr[1].to_i
b4 = ptr[4].to_i

# 'h'=104 'e'=101 'o'=111
if b0 == 104 && b1 == 101 && b4 == 111
  puts "module_const_to_unsafe_ok"
else
  puts "FAIL: b0=#{b0} b1=#{b1} b4=#{b4}"
end
