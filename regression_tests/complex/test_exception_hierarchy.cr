# EXPECT: exception_ok
# Tests basic exception raising and rescue.
# Exercises raise codegen and rescue variable binding.
# NOTE: simplified from full hierarchy to avoid memory explosion.

def try_operation(op : Int32) : String
  begin
    if op == 1
      raise "not found"
    elsif op == 2
      raise "validation error"
    else
      return "success"
    end
  rescue ex
    "caught:#{ex.message}"
  end
end

r1 = try_operation(1)
r2 = try_operation(2)
r3 = try_operation(3)

if r1 == "caught:not found" &&
   r2 == "caught:validation error" &&
   r3 == "success"
  puts "exception_ok"
else
  puts "exception_bad: #{r1}, #{r2}, #{r3}"
end
