# EXPECT: stored_proc_ok
# Tests block-to-proc conversion stored in instance variable and invoked later.
# This is the EXACT pattern that crashes Stage 2 bootstrap (OptionParser callbacks).
# Pattern: method(&block) -> @callback = block -> @callback.call(arg)

class Handler
  getter callback : Proc(String, Nil)

  def initialize(@callback : Proc(String, Nil))
  end
end

class Registry
  def initialize
    @handlers = {} of String => Handler
  end

  def on(name : String, &block : String ->)
    @handlers[name] = Handler.new(block)
  end

  def trigger(name : String, value : String)
    if handler = @handlers[name]?
      handler.callback.call(value)
    end
  end
end

result = ""
reg = Registry.new
reg.on("name") { |v| result = v }
reg.trigger("name", "hello")

if result == "hello"
  puts "stored_proc_ok"
else
  puts "stored_proc_bad: result=#{result}"
end
