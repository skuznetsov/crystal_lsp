# Classification: stdlib-surface oracle (manual String::Builder API under the current compiler).
# Exercises synthesized String$CCBuilder$Dnew$$String + related stubs — not the interpolation bedrock path.
#
# String::Builder.new(String) — no-prelude synthesized ctor + ABI
# EXPECT: abc
io = String::Builder.new("abc")
puts io.to_s
