# EXPECT: struct_hier_ok
# Tests struct field at different offsets in class hierarchy subclasses.
# When abstract class has concrete subclasses with different field layouts,
# the struct field (@span) is at different byte offsets in each subclass.
# Virtual dispatch must load from the CORRECT offset for each concrete type.

struct Span
  @a : Int32
  @b : Int32
  @c : Int32

  def initialize(@a, @b, @c)
  end

  def a; @a; end
  def b; @b; end
  def c; @c; end
end

abstract class Expr
  abstract def span : Span
  abstract def eval : Int32
end

# Subclass with span FIRST (offset 4 after type_id)
class NumExpr < Expr
  @span : Span
  @value : Int32

  def initialize(@span, @value)
  end

  def span; @span; end
  def eval; @value; end
end

# Subclass with span AFTER a String field (different offset)
class NameExpr < Expr
  @name : String
  @span : Span

  def initialize(@name, @span)
  end

  def span; @span; end
  def eval; @name.size; end
end

# Subclass with span AFTER multiple fields (yet another offset)
class BinExpr < Expr
  @left : Expr
  @right : Expr
  @op : Int32
  @span : Span

  def initialize(@left, @right, @op, @span)
  end

  def span; @span; end
  def eval
    case @op
    when 0 then @left.eval + @right.eval
    when 1 then @left.eval - @right.eval
    else        @left.eval * @right.eval
    end
  end
end

# Subclass with span in the MIDDLE
class CallExpr < Expr
  @name : String
  @span : Span
  @arg : Expr

  def initialize(@name, @span, @arg)
  end

  def span; @span; end
  def eval; @arg.eval * 2; end
end

ok = true

# Create nodes with span at different offsets
n1 = NumExpr.new(Span.new(0, 5, 1), 10)
n2 = NameExpr.new("hello", Span.new(6, 11, 1))
n3 = NumExpr.new(Span.new(12, 15, 1), 20)
b1 = BinExpr.new(n1, n3, 0, Span.new(0, 15, 1))
c1 = CallExpr.new("double", Span.new(16, 30, 1), n1)

# Store in abstract array
exprs = Array(Expr).new
exprs << n1
exprs << n2
exprs << n3
exprs << b1
exprs << c1

# Virtual dispatch: span access must use correct offset for each type
expected_a = [0, 6, 12, 0, 16]
expected_b = [5, 11, 15, 15, 30]

exprs.each_with_index do |e, i|
  s = e.span
  if s.a != expected_a[i]
    puts "FAIL span.a[#{i}]: #{s.a} != #{expected_a[i]}"
    ok = false
  end
  if s.b != expected_b[i]
    puts "FAIL span.b[#{i}]: #{s.b} != #{expected_b[i]}"
    ok = false
  end
end

# Eval through virtual dispatch
if b1.eval != 30
  puts "FAIL binexpr eval: #{b1.eval}"
  ok = false
end
if c1.eval != 20
  puts "FAIL callexpr eval: #{c1.eval}"
  ok = false
end

puts ok ? "struct_hier_ok" : "struct_hier_FAIL"
