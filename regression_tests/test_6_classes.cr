abstract class N
# EXPECT: done
  abstract def to_str : String
end
class NumN < N
  getter v : Int32
  def initialize(@v : Int32); end
  def to_str : String; @v.to_s; end
end
class IdN < N
  getter s : String
  def initialize(@s : String); end
  def to_str : String; @s; end
end
class BinN < N
  getter op : String
  getter left : N
  getter right : N
  def initialize(@op : String, @left : N, @right : N); end
  def to_str : String; "(#{@left.to_str} #{@op} #{@right.to_str})"; end
end
class CallN < N
  getter name : String
  getter args : Array(N)
  def initialize(@name : String, @args : Array(N)); end
  def to_str : String
    strs = @args.map { |a| a.to_str }
    "#{@name}(#{strs.join(", ")})"
  end
end
class AssN < N
  getter vn : String
  getter val : N
  def initialize(@vn : String, @val : N); end
  def to_str : String; "#{@vn} = #{@val.to_str}"; end
end
class DefN < N
  getter fn : String
  getter ps : Array(String)
  getter bd : Array(N)
  def initialize(@fn : String, @ps : Array(String), @bd : Array(N)); end
  def to_str : String
    bs = @bd.map { |s| s.to_str }
    "def #{@fn}(#{@ps.join(", ")})\n  #{bs.join("\n  ")}\nend"
  end
end

# Build all 6 types
a = IdN.new("a")
b = IdN.new("b")
plus = BinN.new("+", a, b)
defn = DefN.new("add", ["a", "b"], [plus] of N)
n3 = NumN.new(3)
n4 = NumN.new(4)
call = CallN.new("add", [n3, n4] of N)
assign1 = AssN.new("x", call)
x_ref = IdN.new("x")
mul = BinN.new("*", x_ref, NumN.new(2))
assign2 = AssN.new("y", BinN.new("+", mul, NumN.new(1)))

stmts = [defn, assign1, assign2] of N
stmts.each { |s| puts s.to_str }
puts "done"
