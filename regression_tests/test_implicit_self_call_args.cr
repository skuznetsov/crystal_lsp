# EXPECT: implicit-self-ok

class CliLike
  def run(mode : String)
    if valid_mm_mode?(mode)
      puts "implicit-self-ok"
    else
      puts "implicit-self-bad"
    end
  end

  private def valid_mm_mode?(mode : String) : Bool
    mode == "balanced"
  end
end

CliLike.new.run("balanced")
