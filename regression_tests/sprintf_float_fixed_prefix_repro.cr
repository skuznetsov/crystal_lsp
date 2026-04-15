formatted = sprintf("%.3f", 1.25_f64)
raise "expected 1.250, got #{formatted}" unless formatted == "1.250"

buf = uninitialized UInt8[1076]
size = Float::Printer::RyuPrintf.d2fixed_buffered_n(1.25_f64, 3_u32, buf.to_unsafe)
direct = String.new(buf.to_unsafe, size)
raise "expected direct Ryu 1.250, got #{direct}" unless direct == "1.250"

puts "sprintf_float_fixed_prefix_ok"
