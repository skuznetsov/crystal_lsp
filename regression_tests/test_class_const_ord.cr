# Class constants using .ord.to_u8 must be initialized at runtime
# EXPECT: 35
# Tests: deferred constant init for MemberAccessNode expressions
# Bug: constants like HASH = '#'.ord.to_u8 were initialized to 0
#       because MemberAccessNode wasn't recognized as needing deferred init

class Foo
  HASH = '#'.ord.to_u8
  SPACE = 0x20_u8
  AT = '@'.ord.to_u8

  def check
    puts HASH    # 35
    puts SPACE   # 32
    puts AT      # 64
  end
end

Foo.new.check
puts "class_const_ord_ok"
