# EXPECT: generic_nil_return_ok

class Box(T)
  def fetch(default : T?) : T?
    default
  end

  def maybe : T?
    fetch(nil)
  end
end

box = Box(Int32).new
value = box.maybe

puts(value.nil? ? "generic_nil_return_ok" : "generic_nil_return_bad")
