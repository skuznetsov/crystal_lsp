# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __fixdfti(a : Float64) : Int128
  # Break a into sign, exponent, significand parts.
  a_rep = a.unsafe_as(UInt64)
  a_abs = a_rep & UInt64::MAX.unsafe_shr(1)
  sign = a_rep & (UInt64.new!(1).unsafe_shl(63)) != 0 ? -1 : 1
  significand_bits = Float64::MANT_DIGITS &- 1
  exponent = a_abs.unsafe_shr(significand_bits).to_i! &- (Float64::MAX_EXP &- 1)
  implicit_bit = UInt64.new!(1).unsafe_shl(significand_bits)
  significand = (a_abs & (implicit_bit &- 1)) | implicit_bit

  # If exponent is negative, the result is zero.
  if exponent < 0
    return Int128.new!(0)
  end

  # If the value is too large for the integer type, saturate.
  if exponent >= 128
    return sign == 1 ? Int128::MAX : Int128::MIN
  end

  # If 0 <= exponent < significandBits, right shift to get the result.
  # Otherwise, shift left. (`#<<` handles this)
  Int128.new!(sign) * (Int128.new!(significand) << (exponent &- significand_bits))
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __fixsfti(a : Float32) : Int128
  # Break a into sign, exponent, significand parts.
  a_rep = a.unsafe_as(UInt32)
  a_abs = a_rep & UInt32::MAX.unsafe_shr(1)
  sign = a_rep & (UInt32.new!(1).unsafe_shl(31)) != 0 ? -1 : 1
  significand_bits = Float32::MANT_DIGITS &- 1
  exponent = a_abs.unsafe_shr(significand_bits).to_i! &- (Float32::MAX_EXP &- 1)
  implicit_bit = UInt32.new!(1).unsafe_shl(significand_bits)
  significand = (a_abs & (implicit_bit &- 1)) | implicit_bit

  # If exponent is negative, the result is zero.
  if exponent < 0
    return Int128.new!(0)
  end

  # If the value is too large for the integer type, saturate.
  if exponent >= 128
    return sign == 1 ? Int128::MAX : Int128::MIN
  end

  # If 0 <= exponent < significandBits, right shift to get the result.
  # Otherwise, shift left. (`#<<` handles this)
  Int128.new!(sign) * (Int128.new!(significand) << (exponent &- significand_bits))
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __fixunsdfti(a : Float64) : UInt128
  # Break a into sign, exponent, significand parts.
  a_rep = a.unsafe_as(UInt64)
  a_abs = a_rep & UInt64::MAX.unsafe_shr(1)
  sign = a_rep & (UInt64.new!(1).unsafe_shl(63)) != 0 ? -1 : 1
  significand_bits = Float64::MANT_DIGITS &- 1
  exponent = a_abs.unsafe_shr(significand_bits).to_i! &- (Float64::MAX_EXP &- 1)
  implicit_bit = UInt64.new!(1).unsafe_shl(significand_bits)
  significand = (a_abs & (implicit_bit &- 1)) | implicit_bit

  # If either the value or the exponent is negative, the result is zero.
  if sign == -1 || exponent < 0
    return UInt128.new!(0)
  end

  # If the value is too large for the integer type, saturate.
  if exponent >= 128
    return UInt128::MAX
  end

  # If 0 <= exponent < significandBits, right shift to get the result.
  # Otherwise, shift left. (`#<<` handles this)
  UInt128.new!(significand) << (exponent &- significand_bits)
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __fixunssfti(a : Float32) : UInt128
  # Break a into sign, exponent, significand parts.
  a_rep = a.unsafe_as(UInt32)
  a_abs = a_rep & UInt32::MAX.unsafe_shr(1)
  sign = a_rep & (UInt32.new!(1).unsafe_shl(31)) != 0 ? -1 : 1
  significand_bits = Float32::MANT_DIGITS &- 1
  exponent = a_abs.unsafe_shr(significand_bits).to_i! &- (Float32::MAX_EXP &- 1)
  implicit_bit = UInt32.new!(1).unsafe_shl(significand_bits)
  significand = (a_abs & (implicit_bit &- 1)) | implicit_bit

  # If either the value or the exponent is negative, the result is zero.
  if sign == -1 || exponent < 0
    return UInt128.new!(0)
  end

  # If the value is too large for the integer type, saturate.
  if exponent >= 128
    return UInt128::MAX
  end

  # If 0 <= exponent < significandBits, right shift to get the result.
  # Otherwise, shift left. (`#<<` handles this)
  UInt128.new!(significand) << (exponent &- significand_bits)
end
