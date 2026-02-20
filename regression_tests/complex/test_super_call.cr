# Test: super calls should not create infinite recursion.
# Verifies parent class super dispatch bypasses child overrides.

class Animal
  def speak : String
    "..."
  end
end

class Dog < Animal
  def speak : String
    "woof_" + super
  end
end

class Puppy < Dog
  def speak : String
    "yip_" + super
  end
end

d = Dog.new
p = Puppy.new

if d.speak == "woof_..." && p.speak == "yip_woof_..."
  puts "super_call_ok"
else
  puts "super_call_bad: d=#{d.speak} p=#{p.speak}"
end
