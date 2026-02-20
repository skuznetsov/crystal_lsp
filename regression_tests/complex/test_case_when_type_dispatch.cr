# EXPECT: case_dispatch_ok
# Tests case/when with type matching on union variable.
# Known bug: case/when Type dispatches to wrong class's method.

class A
  def name : String
    "A"
  end
end

class B
  def name : String
    "B"
  end
end

def identify(x : A | B) : String
  case x
  when A then x.name
  when B then x.name
  else "?"
  end
end

r1 = identify(A.new)
r2 = identify(B.new)

if r1 == "A" && r2 == "B"
  puts "case_dispatch_ok"
else
  puts "case_dispatch_bad: r1=#{r1} r2=#{r2}"
end
