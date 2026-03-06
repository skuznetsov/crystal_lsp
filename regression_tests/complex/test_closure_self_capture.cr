# EXPECT: closure_self_ok
# Tests closures/lambdas that capture self's instance variables.
# V2 has a known bug where closure self-capture generates literal offsets
# instead of loads. This test verifies the pattern works correctly.

class Matcher
  @data : Array(UInt8)
  @offset : Int32

  def initialize(@data, @offset)
  end

  def data; @data; end
  def offset; @offset; end
  def offset=(v); @offset = v; end

  def match_byte(expected : UInt8) : Bool
    return false if @offset >= @data.size
    if @data[@offset] == expected
      @offset += 1
      true
    else
      false
    end
  end

  def match_bytes(expected : Array(UInt8)) : Bool
    return false if @offset + expected.size > @data.size
    i = 0
    while i < expected.size
      return false unless @data[@offset + i] == expected[i]
      i += 1
    end
    @offset += expected.size
    true
  end
end

# Test basic matching without closures first
m = Matcher.new([104u8, 101u8, 108u8, 108u8, 111u8], 0)

ok = true

if !m.match_byte(104u8)
  puts "FAIL: match_byte h"
  ok = false
end
if m.offset != 1
  puts "FAIL: offset after match: #{m.offset}"
  ok = false
end

m2 = Matcher.new([65u8, 66u8, 67u8], 0)
if !m2.match_bytes([65u8, 66u8])
  puts "FAIL: match_bytes AB"
  ok = false
end
if m2.offset != 2
  puts "FAIL: offset after match_bytes: #{m2.offset}"
  ok = false
end

# Test with lambda stored in variable
class Scanner
  @bytes : Array(UInt8)
  @pos : Int32

  def initialize(@bytes)
    @pos = 0
  end

  def pos; @pos; end

  def scan_while(&block : UInt8 -> Bool) : Int32
    start = @pos
    while @pos < @bytes.size
      break unless yield @bytes[@pos]
      @pos += 1
    end
    @pos - start
  end
end

s = Scanner.new([65u8, 66u8, 67u8, 48u8, 49u8])
# scan uppercase letters (65-90)
count = s.scan_while { |b| b >= 65u8 && b <= 90u8 }
if count != 3
  puts "FAIL: scan count=#{count}"
  ok = false
end
if s.pos != 3
  puts "FAIL: scan pos=#{s.pos}"
  ok = false
end

puts ok ? "closure_self_ok" : "closure_self_FAIL"
