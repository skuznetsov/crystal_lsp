# EXPECT: while_yield_method_ok

class CounterWrapper
  def run(&)
    index = 1
    while index > 0
      index -= 1
    end
    yield index
  end
end

ok = false
CounterWrapper.new.run do |value|
  ok = value == 0
end

puts(ok ? "while_yield_method_ok" : "while_yield_method_bad")
