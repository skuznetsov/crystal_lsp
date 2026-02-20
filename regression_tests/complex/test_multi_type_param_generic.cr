# EXPECT: multi_param_ok
# Tests generic class with two type parameters.
# NOTE: Type inference from constructor args not yet implemented;
# using explicit type args as workaround.

class Pair(A, B)
  getter first : A
  getter second : B

  def initialize(@first : A, @second : B)
  end
end

p = Pair(Int32, Int32).new(10, 20)
if p.first == 10 && p.second == 20
  puts "multi_param_ok"
else
  puts "multi_param_bad: first=#{p.first} second=#{p.second}"
end
