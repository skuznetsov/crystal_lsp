# EXPECT: block_ivar_ok
# Tests proc stored in variable and called later.
# Simplified version to avoid spawn path link issue.

class Transformer
  @transform : Proc(String, String)

  def initialize(block : Proc(String, String))
    @transform = block
  end

  def apply(input : String) : String
    @transform.call(input)
  end
end

fn = ->(s : String) { s.upcase }
t = Transformer.new(fn)
result = t.apply("hello")
if result == "HELLO"
  puts "block_ivar_ok"
else
  puts "block_ivar_bad: #{result}"
end
