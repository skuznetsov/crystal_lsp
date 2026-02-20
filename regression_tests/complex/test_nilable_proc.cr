# EXPECT: nilable_proc_ok
# Tests nilable proc type (Proc | Nil).
# Pattern: optional callback stored in instance variable.

class EventEmitter
  @on_event : Proc(String, Nil)?

  def initialize
    @on_event = nil
  end

  def on_event(&block : String ->)
    @on_event = block
  end

  def emit(msg : String)
    if cb = @on_event
      cb.call(msg)
    end
  end
end

result = ""
emitter = EventEmitter.new
emitter.emit("before")  # should do nothing (nil callback)

emitter.on_event { |msg| result = msg }
emitter.emit("after")

if result == "after"
  puts "nilable_proc_ok"
else
  puts "nilable_proc_bad: result=#{result}"
end
