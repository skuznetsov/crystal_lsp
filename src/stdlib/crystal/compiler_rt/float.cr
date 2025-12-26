# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __floattidf(a : Int128) : Float64
  if a == 0
    return Float64.new!(0)
  end
  s = a.unsafe_shr(127)
  a = (a ^ s) &- s
  sd = UInt64.new!(128) &- a.leading_zeros_count # number of significant digits
  e = sd &- 1                                     # exponent
  if sd > Float64::MANT_DIGITS
    #  start: 0000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQxxxxxxxxxxxxxxxxxx
    # finish: 000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQR
    #                                               12345678901234567890123456
    # 1 = msb 1 bit
    # P = bit MANT_DIGITS-1 bits to the right of 1
    # Q = bit MANT_DIGITS bits to the right of 1
    # R = "or" of all bits to the right of Q
    if sd == Float64::MANT_DIGITS &+ 1
      a = a.unsafe_shl(1)
    elsif sd == Float64::MANT_DIGITS &+ 2
      # do nothing
    else
      a2 = (a & UInt128::MAX.unsafe_shr((128 &+ Float64::MANT_DIGITS &+ 2) &- sd)) != 0
      a = Int128.new!(UInt128.new!(a).unsafe_shr(sd &- (Float64::MANT_DIGITS &+ 2)))
      a |= 1 if a2
    end
    # finish:
    a |= 1 if (a & 4) != 0 # Or P into R
    a &+= 1                # round - this step may add a significant bit
    a = a.unsafe_shr(2)    # dump Q and R
    # a is now rounded to MANT_DIGITS or MANT_DIGITS+1 bits
    if a & Int128.new!(1).unsafe_shl(Float64::MANT_DIGITS) != 0
      a = a.unsafe_shr(1)
      e &+= 1
    end
    # a is now rounded to MANT_DIGITS bits
  else
    a = a.unsafe_shl(Float64::MANT_DIGITS &- sd)
    # a is now rounded to MANT_DIGITS bits
  end
  fb = (UInt64.new!(1).unsafe_shl(63) & s) |
       (e &+ Float64::MAX_EXP &- 1).unsafe_shl(Float64::MANT_DIGITS &- 1) |
       (a & ~(UInt64::MAX.unsafe_shl(Float64::MANT_DIGITS &- 1)))
  fb.unsafe_as(Float64)
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __floattisf(a : Int128) : Float32
  if a == 0
    return Float32.new!(0)
  end
  s = a.unsafe_shr(127)
  a = (a ^ s) &- s
  sd = UInt32.new!(128) &- a.leading_zeros_count # number of significant digits
  e = sd &- 1                                     # exponent
  if sd > Float32::MANT_DIGITS
    #  start: 0000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQxxxxxxxxxxxxxxxxxx
    # finish: 000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQR
    #                                               12345678901234567890123456
    # 1 = msb 1 bit
    # P = bit MANT_DIGITS-1 bits to the right of 1
    # Q = bit MANT_DIGITS bits to the right of 1
    # R = "or" of all bits to the right of Q
    if sd == Float32::MANT_DIGITS &+ 1
      a = a.unsafe_shl(1)
    elsif sd == Float32::MANT_DIGITS &+ 2
      # do nothing
    else
      a2 = (a & UInt128::MAX.unsafe_shr((128 &+ Float32::MANT_DIGITS &+ 2) &- sd)) != 0
      a = Int128.new!(UInt128.new!(a).unsafe_shr(sd &- (Float32::MANT_DIGITS &+ 2)))
      a |= 1 if a2
    end
    # finish:
    a |= 1 if (a & 4) != 0 # Or P into R
    a &+= 1                # round - this step may add a significant bit
    a = a.unsafe_shr(2)    # dump Q and R
    # a is now rounded to MANT_DIGITS or MANT_DIGITS+1 bits
    if a & Int128.new!(1).unsafe_shl(Float32::MANT_DIGITS) != 0
      a = a.unsafe_shr(1)
      e &+= 1
    end
    # a is now rounded to MANT_DIGITS bits
  else
    a = a.unsafe_shl(Float32::MANT_DIGITS &- sd)
    # a is now rounded to MANT_DIGITS bits
  end
  fb = (UInt32.new!(1).unsafe_shl(31) & s) |
       (e &+ Float32::MAX_EXP &- 1).unsafe_shl(Float32::MANT_DIGITS &- 1) |
       (a & ~(UInt32::MAX.unsafe_shl(Float32::MANT_DIGITS &- 1)))
  fb.unsafe_as(Float32)
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __floatuntidf(a : UInt128) : Float64
  if a == 0
    return Float64.new!(0)
  end
  sd = UInt64.new!(128) &- a.leading_zeros_count # number of significant digits
  e = sd &- 1                                     # exponent
  if sd > Float64::MANT_DIGITS
    #  start: 0000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQxxxxxxxxxxxxxxxxxx
    # finish: 000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQR
    #                                               12345678901234567890123456
    # 1 = msb 1 bit
    # P = bit MANT_DIGITS-1 bits to the right of 1
    # Q = bit MANT_DIGITS bits to the right of 1
    # R = "or" of all bits to the right of Q
    if sd == Float64::MANT_DIGITS &+ 1
      a = a.unsafe_shl(1)
    elsif sd == Float64::MANT_DIGITS &+ 2
      # do nothing
    else
      a2 = (a & UInt128::MAX.unsafe_shr((128 &+ Float64::MANT_DIGITS &+ 2) &- sd)) != 0
      a = UInt128.new!(a.unsafe_shr(sd &- (Float64::MANT_DIGITS &+ 2)))
      a |= 1 if a2
    end
    # finish:
    a |= 1 if (a & 4) != 0 # Or P into R
    a &+= 1                # round - this step may add a significant bit
    a = a.unsafe_shr(2)    # dump Q and R
    # a is now rounded to MANT_DIGITS or MANT_DIGITS+1 bits
    if a & UInt128.new!(1).unsafe_shl(Float64::MANT_DIGITS) != 0
      a = a.unsafe_shr(1)
      e &+= 1
    end
    # a is now rounded to MANT_DIGITS bits
  else
    a = a.unsafe_shl(Float64::MANT_DIGITS &- sd)
    # a is now rounded to MANT_DIGITS bits
  end
  fb = (e &+ Float64::MAX_EXP &- 1).unsafe_shl(Float64::MANT_DIGITS &- 1) |
       (a & ~(UInt64::MAX.unsafe_shl(Float64::MANT_DIGITS &- 1)))
  fb.unsafe_as(Float64)
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/tree/82b74363a943b570c4ee7799d5f3ee4b3e7163a5/compiler-rt/lib/builtins
fun __floatuntisf(a : UInt128) : Float32
  if a == 0
    return Float32.new!(0)
  end
  sd = UInt32.new!(128) &- a.leading_zeros_count # number of significant digits
  e = sd &- 1                                     # exponent
  if sd > Float32::MANT_DIGITS
    #  start: 0000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQxxxxxxxxxxxxxxxxxx
    # finish: 000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxPQR
    #                                               12345678901234567890123456
    # 1 = msb 1 bit
    # P = bit MANT_DIGITS-1 bits to the right of 1
    # Q = bit MANT_DIGITS bits to the right of 1
    # R = "or" of all bits to the right of Q
    if sd == Float32::MANT_DIGITS &+ 1
      a = a.unsafe_shl(1)
    elsif sd == Float32::MANT_DIGITS &+ 2
      # do nothing
    else
      a2 = (a & UInt128::MAX.unsafe_shr((128 &+ Float32::MANT_DIGITS &+ 2) &- sd)) != 0
      a = UInt128.new!(a.unsafe_shr(sd &- (Float32::MANT_DIGITS &+ 2)))
      a |= 1 if a2
    end
    # finish:
    a |= 1 if (a & 4) != 0 # Or P into R
    a &+= 1                # round - this step may add a significant bit
    a = a.unsafe_shr(2)    # dump Q and R
    # a is now rounded to MANT_DIGITS or MANT_DIGITS+1 bits
    if a & UInt128.new!(1).unsafe_shl(Float32::MANT_DIGITS) != 0
      a = a.unsafe_shr(1)
      e &+= 1
    end
    # a is now rounded to MANT_DIGITS bits
  else
    a = a.unsafe_shl(Float32::MANT_DIGITS &- sd)
    # a is now rounded to MANT_DIGITS bits
  end
  fb = (e &+ Float32::MAX_EXP &- 1).unsafe_shl(Float32::MANT_DIGITS &- 1) |
       (a & ~(UInt32::MAX.unsafe_shl(Float32::MANT_DIGITS &- 1)))
  fb.unsafe_as(Float32)
end
