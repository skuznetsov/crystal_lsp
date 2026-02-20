# EXPECT: closure_multi_ok
# Tests closure captures with multiple variables across nested blocks.
# Exercises capture detection and closure cell generation in lower_block_to_proc.

class Collector
  @procs = [] of Proc(Int32)

  def add(&block : -> Int32)
    @procs << block
  end

  def run_all : Array(Int32)
    results = [] of Int32
    @procs.each do |p|
      results << p.call
    end
    results
  end
end

x = 10
y = 20
z = 30

collector = Collector.new
collector.add { x + y }
collector.add { y + z }
collector.add { x + z }

results = collector.run_all
if results.size == 3 && results[0] == 30 && results[1] == 50 && results[2] == 40
  puts "closure_multi_ok"
else
  puts "closure_multi_bad"
end
