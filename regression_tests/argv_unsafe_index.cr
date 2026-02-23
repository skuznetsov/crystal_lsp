# Regression test: ARGV_UNSAFE[n] must use instance Pointer#[], not static Pointer.[]
# Bug: uppercase variable names were routed through static bracket call resolution,
# causing ARGV_UNSAFE[n] to call a wrong stub function returning null.
# EXPECT: ptr_index_ok
#
# We test ARGV_UNSAFE[0] (the program name), which is always available.

# Direct ARGV_UNSAFE indexing (was broken — static bracket call returned null)
p0_direct = ARGV_UNSAFE[0]
s_direct = String.new(p0_direct)

# Local copy indexing (always worked)
argv_local = ARGV_UNSAFE
p0_local = argv_local[0]
s_local = String.new(p0_local)

if s_direct.bytesize > 0 && s_direct == s_local
  puts "ptr_index_ok"
else
  puts "FAIL: direct.bs=#{s_direct.bytesize.to_s}, local.bs=#{s_local.bytesize.to_s}"
end
