# EXPECT: nilable_chain_ok
# Tests nilable method chaining with if-let patterns.
# Exercises nilable union narrowing and control flow.
# NOTE: uses .nil? instead of == nil / != nil (known codegen bug with nilable ==)

class Config
  getter host : String?
  getter port : Int32?

  def initialize(@host : String?, @port : Int32?)
  end

  def address : String?
    if h = @host
      if p = @port
        "#{h}:#{p}"
      else
        h
      end
    else
      nil
    end
  end
end

c1 = Config.new("localhost", 8080)
c2 = Config.new("example.com", nil)
c3 = Config.new(nil, 3000)

r1 = c1.address
r2 = c2.address
r3 = c3.address

ok = true
ok = false unless r1 == "localhost:8080"
ok = false unless r2 == "example.com"
ok = false unless r3.nil?

if ok
  puts "nilable_chain_ok"
else
  puts "nilable_chain_bad"
end
