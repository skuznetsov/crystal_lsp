# Globals / static members smoke sample for LLDB (Demo::VALUE vs @@counter).
# Used by scripts/check_lldb_locals.sh.

class Demo
  VALUE = 42
  @@counter = 1

  def self.counter
    @@counter
  end
end

puts Demo::VALUE + Demo.counter
