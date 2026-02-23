# Regression test: named-only params in initialize must execute body
# Bug: adding _named suffix to mangled function name caused name mismatch
# between def registration and call site resolution, resulting in dead-code stub.
# EXPECT: named_init_ok

class Foo
  @items : Array(String)

  def initialize(*, @active : Bool = false)
    @items = [] of String
  end

  def add(s : String)
    @items << s
  end

  def count
    @items.size
  end
end

f = Foo.new
f.add("hello")
f.add("world")
if f.count == 2
  puts "named_init_ok"
else
  puts "FAIL: count=#{f.count.to_s}"
end
