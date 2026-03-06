# EXPECT: many_sub_ok
# Tests 12+ concrete subclasses of an abstract class, each with a struct field.
# Virtual dispatch to access the struct field through the abstract interface.
# Stresses type_id-based vtable at scale (like the parser with 50+ node types).

struct Info
  @line : Int32
  @col : Int32
  @len : Int32

  def initialize(@line, @col, @len)
  end

  def line; @line; end
  def col; @col; end
  def len; @len; end

  def merge(other : Info) : Info
    l = line <= other.line ? self : other
    r = (col + len) >= (other.col + other.len) ? self : other
    Info.new(l.line, l.col, r.len)
  end
end

abstract class Token
  abstract def info : Info
  abstract def kind : Int32
end

class TokInt < Token
  @info : Info; @value : Int32
  def initialize(@info, @value); end
  def info; @info; end
  def kind; 1; end
end

class TokStr < Token
  @info : Info; @value : String
  def initialize(@info, @value); end
  def info; @info; end
  def kind; 2; end
end

class TokIdent < Token
  @info : Info; @name : String
  def initialize(@info, @name); end
  def info; @info; end
  def kind; 3; end
end

class TokPlus < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 4; end
end

class TokMinus < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 5; end
end

class TokStar < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 6; end
end

class TokSlash < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 7; end
end

class TokLParen < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 8; end
end

class TokRParen < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 9; end
end

class TokComma < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 10; end
end

class TokDot < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 11; end
end

class TokEOF < Token
  @info : Info
  def initialize(@info); end
  def info; @info; end
  def kind; 12; end
end

# Create tokens of various types
tokens = Array(Token).new
tokens << TokInt.new(Info.new(1, 0, 3), 42)
tokens << TokPlus.new(Info.new(1, 4, 1))
tokens << TokStr.new(Info.new(1, 6, 5), "hello")
tokens << TokStar.new(Info.new(1, 12, 1))
tokens << TokIdent.new(Info.new(1, 14, 3), "foo")
tokens << TokLParen.new(Info.new(1, 17, 1))
tokens << TokInt.new(Info.new(1, 18, 1), 1)
tokens << TokComma.new(Info.new(1, 19, 1))
tokens << TokInt.new(Info.new(1, 21, 1), 2)
tokens << TokRParen.new(Info.new(1, 22, 1))
tokens << TokMinus.new(Info.new(2, 0, 1))
tokens << TokSlash.new(Info.new(2, 2, 1))
tokens << TokDot.new(Info.new(2, 4, 1))
tokens << TokEOF.new(Info.new(2, 5, 0))

ok = true

# Verify all kinds via virtual dispatch
expected_kinds = [1, 4, 2, 6, 3, 8, 1, 10, 1, 9, 5, 7, 11, 12]
tokens.each_with_index do |tok, i|
  if tok.kind != expected_kinds[i]
    puts "FAIL kind[#{i}]: #{tok.kind} != #{expected_kinds[i]}"
    ok = false
  end
end

# Verify struct field access via virtual dispatch
tokens.each_with_index do |tok, i|
  inf = tok.info
  if inf.line < 1 || inf.line > 2
    puts "FAIL info[#{i}].line: #{inf.line}"
    ok = false
  end
end

# Merge pattern (like Span#cover)
acc = tokens[0].info
tokens.each do |tok|
  acc = acc.merge(tok.info)
end
if acc.line != 1
  puts "FAIL merge line: #{acc.line}"
  ok = false
end

# Access by index with guard (arena pattern)
def safe_info(tokens : Array(Token), idx : Int32) : Info
  return Info.new(0, 0, 0) if idx < 0 || idx >= tokens.size
  tokens[idx].info
end

i = safe_info(tokens, 4)
if i.line != 1 || i.col != 14
  puts "FAIL safe_info(4): #{i.line},#{i.col}"
  ok = false
end

i2 = safe_info(tokens, -1)
if i2.line != 0
  puts "FAIL safe_info(-1): #{i2.line}"
  ok = false
end

puts ok ? "many_sub_ok" : "many_sub_FAIL"
