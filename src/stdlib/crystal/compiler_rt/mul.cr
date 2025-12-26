# :nodoc:
# Ported from https://github.com/llvm/llvm-project/blob/ce59ccd04023cab3a837da14079ca2dcbfebb70c/compiler-rt/lib/builtins/int_mulo_impl.inc
fun __mulosi4(a : Int32, b : Int32, overflow : Int32*) : Int32
  overflow.value = 0
  result = a &* b
  if a == Int32::MIN
    if b != 0 && b != 1
      overflow.value = 1
    end
    return result
  end
  if b == Int32::MIN
    if a != 0 && a != 1
      overflow.value = 1
    end
    return result
  end
  sa = a >> 31
  abs_a = (a ^ sa) &- sa
  sb = b >> 31
  abs_b = (b ^ sb) &- sb
  if abs_a < 2 || abs_b < 2
    return result
  end
  if sa == sb
    if abs_a > (Int32::MAX // abs_b)
      overflow.value = 1
    end
  else
    if abs_a > (Int32::MIN // (Int32.new(0) &- abs_b))
      overflow.value = 1
    end
  end
  return result
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/blob/ce59ccd04023cab3a837da14079ca2dcbfebb70c/compiler-rt/lib/builtins/int_mulo_impl.inc
fun __mulodi4(a : Int64, b : Int64, overflow : Int32*) : Int64
  overflow.value = 0
  result = a &* b
  if a == Int64::MIN
    if b != 0 && b != 1
      overflow.value = 1
    end
    return result
  end
  if b == Int64::MIN
    if a != 0 && a != 1
      overflow.value = 1
    end
    return result
  end
  sa = a >> 63
  abs_a = (a ^ sa) &- sa
  sb = b >> 63
  abs_b = (b ^ sb) &- sb
  if abs_a < 2 || abs_b < 2
    return result
  end
  if sa == sb
    if abs_a > (Int64::MAX // abs_b)
      overflow.value = 1
    end
  else
    if abs_a > (Int64::MIN // (Int64.new(0) &- abs_b))
      overflow.value = 1
    end
  end
  return result
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/blob/ce59ccd04023cab3a837da14079ca2dcbfebb70c/compiler-rt/lib/builtins/int_mulo_impl.inc
fun __muloti4(a : Int128, b : Int128, overflow : Int32*) : Int128
  overflow.value = 0
  result = a &* b
  if a == Int128::MIN
    if b != 0 && b != 1
      overflow.value = 1
    end
    return result
  end
  if b == Int128::MIN
    if a != 0 && a != 1
      overflow.value = 1
    end
    return result
  end
  sa = a >> 127
  abs_a = (a ^ sa) &- sa
  sb = b >> 127
  abs_b = (b ^ sb) &- sb
  if abs_a < 2 || abs_b < 2
    return result
  end
  if sa == sb
    if abs_a > (Int128::MAX // abs_b)
      overflow.value = 1
    end
  else
    if abs_a > (Int128::MIN // (Int128.new(0) &- abs_b))
      overflow.value = 1
    end
  end
  return result
end
