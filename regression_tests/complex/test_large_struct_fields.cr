# EXPECT: large_struct_ok
# Tests struct with 6 fields (exactly like Span in the parser).
# Verifies all fields are accessible and struct can be passed/returned correctly.

struct Span
  @start_offset : Int32
  @end_offset : Int32
  @start_line : Int32
  @start_column : Int32
  @end_line : Int32
  @end_column : Int32

  def initialize(@start_offset, @end_offset, @start_line, @start_column, @end_line, @end_column)
  end

  def start_offset; @start_offset; end
  def end_offset; @end_offset; end
  def start_line; @start_line; end
  def start_column; @start_column; end
  def end_line; @end_line; end
  def end_column; @end_column; end

  def cover(other : Span) : Span
    start_span = start_offset <= other.start_offset ? self : other
    end_span = end_offset >= other.end_offset ? self : other
    Span.new(
      start_span.start_offset,
      end_span.end_offset,
      start_span.start_line,
      start_span.start_column,
      end_span.end_line,
      end_span.end_column
    )
  end
end

ok = true

# Basic construction and field access
s = Span.new(10, 20, 1, 5, 1, 15)
if s.start_offset != 10
  puts "FAIL start_offset: #{s.start_offset}"
  ok = false
end
if s.end_offset != 20
  puts "FAIL end_offset: #{s.end_offset}"
  ok = false
end
if s.start_line != 1
  puts "FAIL start_line: #{s.start_line}"
  ok = false
end
if s.start_column != 5
  puts "FAIL start_column: #{s.start_column}"
  ok = false
end
if s.end_line != 1
  puts "FAIL end_line: #{s.end_line}"
  ok = false
end
if s.end_column != 15
  puts "FAIL end_column: #{s.end_column}"
  ok = false
end

# Cover
s2 = Span.new(5, 25, 1, 0, 2, 10)
c = s.cover(s2)
if c.start_offset != 5 || c.end_offset != 25
  puts "FAIL cover offsets: #{c.start_offset},#{c.end_offset}"
  ok = false
end
if c.start_line != 1 || c.start_column != 0
  puts "FAIL cover start: #{c.start_line},#{c.start_column}"
  ok = false
end
if c.end_line != 2 || c.end_column != 10
  puts "FAIL cover end: #{c.end_line},#{c.end_column}"
  ok = false
end

# Store in array
spans = Array(Span).new
10.times do |i|
  spans << Span.new(i * 10, i * 10 + 5, i + 1, 0, i + 1, 5)
end

# Loop cover
acc = spans[0]
spans.each do |sp|
  acc = acc.cover(sp)
end
if acc.start_offset != 0 || acc.end_offset != 95
  puts "FAIL loop cover: #{acc.start_offset},#{acc.end_offset}"
  ok = false
end

# Return from function
def make_span(line : Int32) : Span
  Span.new(line * 100, line * 100 + 50, line, 0, line, 50)
end

ms = make_span(5)
if ms.start_offset != 500 || ms.start_line != 5
  puts "FAIL make_span: #{ms.start_offset},#{ms.start_line}"
  ok = false
end

puts ok ? "large_struct_ok" : "large_struct_FAIL"
