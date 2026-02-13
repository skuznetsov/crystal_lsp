# Test: Simulated compiler pipeline (lex → parse → emit)
# EXPECT: pipeline done
# Write a mini program to a file, then lex and process it

# Write test source
File.write("/tmp/mini_src.cr", "x = 10\ny = 20\nputs x + y\n")

# Read it back
source = File.read("/tmp/mini_src.cr")
puts "Source bytes: #{source.bytesize}"

# Simple tokenizer (inline, no abstract class dispatch)
class Token
  getter kind : Int32
  getter text : String
  getter line : Int32

  def initialize(@kind : Int32, @text : String, @line : Int32)
  end
end

# Token kinds
IDENT = 0
NUM = 1
OP = 2
EQ = 3
NL = 4
EOF_TOK = 5

tokens = [] of Token
pos = 0
line = 1

while pos < source.bytesize
  c = source.byte_at(pos).to_i32
  if c == 10
    tokens << Token.new(NL, "\\n", line)
    line += 1
    pos += 1
  elsif c == 32 || c == 9
    pos += 1
  elsif c >= 48 && c <= 57
    start = pos
    while pos < source.bytesize
      b = source.byte_at(pos).to_i32
      break unless b >= 48 && b <= 57
      pos += 1
    end
    tokens << Token.new(NUM, source.byte_slice(start, pos - start), line)
  elsif (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || c == 95
    start = pos
    while pos < source.bytesize
      b = source.byte_at(pos).to_i32
      break unless (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || b == 95
      pos += 1
    end
    tokens << Token.new(IDENT, source.byte_slice(start, pos - start), line)
  elsif c == 61
    tokens << Token.new(EQ, "=", line)
    pos += 1
  elsif c == 43 || c == 45 || c == 42 || c == 47
    tokens << Token.new(OP, source.byte_slice(pos, 1), line)
    pos += 1
  else
    pos += 1
  end
end
tokens << Token.new(EOF_TOK, "", line)

puts "Tokens: #{tokens.size}"

# Simple evaluator
vars = {} of String => Int32

i = 0
while i < tokens.size
  tok = tokens[i]
  if tok.kind == IDENT && i + 2 < tokens.size
    next_tok = tokens[i + 1]
    if next_tok.kind == EQ
      val_tok = tokens[i + 2]
      if val_tok.kind == NUM
        # Simple assignment: x = 10
        vars[tok.text] = val_tok.text.to_i
        i += 3
        next
      end
    end
    if tok.text == "puts" && i + 3 < tokens.size
      # puts x + y
      a_tok = tokens[i + 1]
      op_tok = tokens[i + 2]
      b_tok = tokens[i + 3]
      if op_tok.kind == OP && op_tok.text == "+"
        a_val = a_tok.kind == NUM ? a_tok.text.to_i : (vars[a_tok.text]? || 0)
        b_val = b_tok.kind == NUM ? b_tok.text.to_i : (vars[b_tok.text]? || 0)
        puts a_val + b_val
        i += 4
        next
      end
    end
  end
  i += 1
end

File.delete("/tmp/mini_src.cr")
puts "pipeline done"
