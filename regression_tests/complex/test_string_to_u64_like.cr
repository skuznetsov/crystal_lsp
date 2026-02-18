# EXPECT: string_to_u64_like_ok

signed = "123".to_i
u8 = "123".to_u8
u16 = "123".to_u16
u32 = "123".to_u32
u64 = "123".to_u64

ok = signed == 123 &&
     u8 == 123_u8 &&
     u16 == 123_u16 &&
     u32 == 123_u32 &&
     u64 == 123_u64

puts(ok ? "string_to_u64_like_ok" : "string_to_u64_like_bad")
