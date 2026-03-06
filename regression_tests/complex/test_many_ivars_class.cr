# EXPECT: many_ivars_ok
# Tests a class with 20+ instance variables of mixed types.
# Stresses field offset calculation in the codegen.
# The real Parser class has 20+ fields — wrong offsets cause NULL reads.

class BigClass
  @f01 : Int32
  @f02 : String
  @f03 : Bool
  @f04 : Int32
  @f05 : Array(Int32)
  @f06 : String
  @f07 : Int32
  @f08 : Bool
  @f09 : Int32
  @f10 : String
  @f11 : Int32
  @f12 : Array(String)
  @f13 : Bool
  @f14 : Int32
  @f15 : String
  @f16 : Int32
  @f17 : Bool
  @f18 : Int32
  @f19 : String
  @f20 : Int32
  @f21 : Bool
  @f22 : Int32
  @f23 : String
  @f24 : Int32

  def initialize(
    @f01, @f02, @f03, @f04, @f05, @f06,
    @f07, @f08, @f09, @f10, @f11, @f12,
    @f13, @f14, @f15, @f16, @f17, @f18,
    @f19, @f20, @f21, @f22, @f23, @f24
  )
  end

  def f01; @f01; end
  def f02; @f02; end
  def f03; @f03; end
  def f04; @f04; end
  def f05; @f05; end
  def f06; @f06; end
  def f07; @f07; end
  def f08; @f08; end
  def f09; @f09; end
  def f10; @f10; end
  def f11; @f11; end
  def f12; @f12; end
  def f13; @f13; end
  def f14; @f14; end
  def f15; @f15; end
  def f16; @f16; end
  def f17; @f17; end
  def f18; @f18; end
  def f19; @f19; end
  def f20; @f20; end
  def f21; @f21; end
  def f22; @f22; end
  def f23; @f23; end
  def f24; @f24; end
end

obj = BigClass.new(
  1, "two", true, 4, [5, 6], "seven",
  8, false, 10, "eleven", 12, ["thirteen", "fourteen"],
  true, 16, "seventeen", 18, false, 20,
  "twenty-one", 22, true, 24, "twenty-five", 26
)

ok = true

if obj.f01 != 1
  puts "FAIL f01: #{obj.f01}"
  ok = false
end
if obj.f02 != "two"
  puts "FAIL f02: #{obj.f02}"
  ok = false
end
if obj.f03 != true
  puts "FAIL f03: #{obj.f03}"
  ok = false
end
if obj.f07 != 8
  puts "FAIL f07: #{obj.f07}"
  ok = false
end
if obj.f10 != "eleven"
  puts "FAIL f10: #{obj.f10}"
  ok = false
end
if obj.f12.size != 2
  puts "FAIL f12 size: #{obj.f12.size}"
  ok = false
end
if obj.f16 != 18
  puts "FAIL f16: #{obj.f16}"
  ok = false
end
if obj.f19 != "twenty-one"
  puts "FAIL f19: #{obj.f19}"
  ok = false
end
if obj.f24 != 26
  puts "FAIL f24: #{obj.f24}"
  ok = false
end

# Also test that methods work on the object
if obj.f05[0] != 5 || obj.f05[1] != 6
  puts "FAIL f05 array"
  ok = false
end
if obj.f12[0] != "thirteen"
  puts "FAIL f12 array"
  ok = false
end

puts ok ? "many_ivars_ok" : "many_ivars_FAIL"
