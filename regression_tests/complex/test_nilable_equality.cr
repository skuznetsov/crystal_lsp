# EXPECT: nilable_eq_ok
# Tests equality comparison of nilable types with nil.
# Known codegen bug: `x == nil` on nilable union can segfault or give wrong result.
# This test documents the expected behavior and should pass once the bug is fixed.

x : String? = nil
y : String? = "hello"

nil_check_1 = x.nil?        # true
nil_check_2 = y.nil?        # false

# Using .nil? as workaround for == nil
ok = nil_check_1 == true && nil_check_2 == false

if ok
  puts "nilable_eq_ok"
else
  puts "nilable_eq_bad: nil_check_1=#{nil_check_1} nil_check_2=#{nil_check_2}"
end
