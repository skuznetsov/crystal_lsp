# :nodoc:
# Ported from https://github.com/llvm/llvm-project/blob/2e9df860468425645dcd1b241c5dbf76c072e314/compiler-rt/lib/builtins
fun __powisf2(a : Float32, b : Int32) : Float32
  recip = b < 0
  r = 1f32

  loop do
    r *= a if b & 1 != 0
    b = b.unsafe_div 2
    break if b == 0
    a *= a
  end

  recip ? 1 / r : r
end

# :nodoc:
# Ported from https://github.com/llvm/llvm-project/blob/2e9df860468425645dcd1b241c5dbf76c072e314/compiler-rt/lib/builtins
fun __powidf2(a : Float64, b : Int32) : Float64
  recip = b < 0
  r = 1f64

  loop do
    r *= a if b & 1 != 0
    b = b.unsafe_div 2
    break if b == 0
    a *= a
  end

  recip ? 1 / r : r
end
