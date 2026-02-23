# EXPECT: ok-named-defaults
class C
  def run(*, out_io : IO = STDOUT, err_io : IO = STDERR) : Int32
    out_io.puts "ok-named-defaults"
    0
  end
end

exit C.new.run
