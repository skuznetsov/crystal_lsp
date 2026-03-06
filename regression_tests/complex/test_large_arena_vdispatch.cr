# EXPECT: arena_vd_ok
# Tests large arena (200+ nodes) with virtual dispatch and struct field access.
# Stresses the codegen at scale — similar to the parser's AstArena with 50+ node types.

struct Loc
  @line : Int32
  @col : Int32

  def initialize(@line, @col)
  end

  def line; @line; end
  def col; @col; end
end

abstract class Item
  abstract def loc : Loc
  abstract def tag : Int32
end

class ItemA < Item
  @loc : Loc; @val : Int32
  def initialize(@loc, @val); end
  def loc; @loc; end
  def tag; 1; end
end

class ItemB < Item
  @loc : Loc; @name : String
  def initialize(@loc, @name); end
  def loc; @loc; end
  def tag; 2; end
end

class ItemC < Item
  @loc : Loc; @x : Int32; @y : Int32
  def initialize(@loc, @x, @y); end
  def loc; @loc; end
  def tag; 3; end
end

class ItemD < Item
  @loc : Loc; @data : Array(Int32)
  def initialize(@loc, @data); end
  def loc; @loc; end
  def tag; 4; end
end

class ItemE < Item
  @loc : Loc; @flag : Bool; @count : Int32
  def initialize(@loc, @flag, @count); end
  def loc; @loc; end
  def tag; 5; end
end

class ItemF < Item
  @loc : Loc
  def initialize(@loc); end
  def loc; @loc; end
  def tag; 6; end
end

class ItemG < Item
  @loc : Loc; @left : Int32; @right : Int32; @op : String
  def initialize(@loc, @left, @right, @op); end
  def loc; @loc; end
  def tag; 7; end
end

class ItemH < Item
  @name : String; @loc : Loc  # Note: loc is NOT first field
  def initialize(@name, @loc); end
  def loc; @loc; end
  def tag; 8; end
end

# Arena
class Arena
  @items : Array(Item)

  def initialize
    @items = Array(Item).new
  end

  def add(item : Item) : Int32
    @items << item
    @items.size - 1
  end

  @[AlwaysInline]
  def get_loc(id : Int32) : Loc
    return Loc.new(0, 0) if id < 0
    @items[id].loc
  end

  def size
    @items.size
  end
end

arena = Arena.new
ok = true

# Add 200 items of 8 different types
200.times do |i|
  loc = Loc.new(i + 1, (i * 3) % 80)
  case i % 8
  when 0 then arena.add(ItemA.new(loc, i))
  when 1 then arena.add(ItemB.new(loc, "n#{i}"))
  when 2 then arena.add(ItemC.new(loc, i, i * 2))
  when 3 then arena.add(ItemD.new(loc, [i]))
  when 4 then arena.add(ItemE.new(loc, i % 2 == 0, i))
  when 5 then arena.add(ItemF.new(loc))
  when 6 then arena.add(ItemG.new(loc, i - 1, i + 1, "+"))
  when 7 then arena.add(ItemH.new("h#{i}", loc))
  end
end

if arena.size != 200
  puts "FAIL: size=#{arena.size}"
  ok = false
end

# Verify all locs via virtual dispatch
200.times do |i|
  loc = arena.get_loc(i)
  expected_line = i + 1
  expected_col = (i * 3) % 80
  if loc.line != expected_line
    puts "FAIL: loc[#{i}].line=#{loc.line} expected=#{expected_line}"
    ok = false
    break
  end
  if loc.col != expected_col
    puts "FAIL: loc[#{i}].col=#{loc.col} expected=#{expected_col}"
    ok = false
    break
  end
end

# Guard for invalid id
z = arena.get_loc(-1)
if z.line != 0 || z.col != 0
  puts "FAIL: get_loc(-1)=#{z.line},#{z.col}"
  ok = false
end

puts ok ? "arena_vd_ok" : "arena_vd_FAIL"
