# EXPECT: 0
class FK
end

class FE
end

h = Hash(FK, FE).new(initial_capacity: 8)
puts h.size
