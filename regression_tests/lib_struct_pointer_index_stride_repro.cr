# Reducer: Pointer(LibStruct)[i] must stride by sizeof(struct), not pointer width.
# Wrong stride (e.g. 8) makes ptr[1] overlap ptr[0].b for a 16-byte lib struct.
# Second check: whole lib-struct value through ptr[i] = (indexed memcpy).
# Requires typed Pointer(T) from malloc (HIR interns Pointer(T) when T resolves).
# EXPECT: indexed_lib_struct_stride_ok

lib LibX
  struct S
    a : UInt64
    b : UInt64
  end
end

ptr = Pointer(LibX::S).malloc(2)
ptr[0].a = 111_u64
ptr[0].b = 333_u64
ptr[1].a = 222_u64
stride_ok = ptr[0].a == 111_u64 && ptr[0].b == 333_u64 && ptr[1].a == 222_u64

w = LibX::S.new
w.a = 501_u64
w.b = 502_u64
ptr[1] = w
whole_ok = ptr[1].a == 501_u64 && ptr[1].b == 502_u64

if stride_ok && whole_ok
  puts "indexed_lib_struct_stride_ok"
end
