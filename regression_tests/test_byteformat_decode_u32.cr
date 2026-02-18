# EXPECT: byteformat_u32_ok
io = IO::Memory.new
orig = 0x12345678_u32
io.write_bytes(orig, IO::ByteFormat::LittleEndian)
io.rewind
value = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
puts(value == orig ? "byteformat_u32_ok" : "byteformat_u32_bad")
