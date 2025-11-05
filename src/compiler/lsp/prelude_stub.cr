module Kernel
  def puts(*args)
    nil
  end

  def self.puts(*args)
    nil
  end
end

module Dir
  def self.glob(pattern : String, *, match_hidden : Bool = false)
    [] of String
  end

  def self.each_child(path : String, &block : String ->)
    nil
  end

  def self.mkdir(path : String)
    nil
  end
end

module File
  def self.read(path : String) : String
    ""
  end

  def self.write(path : String, content : String)
    nil
  end
end

def puts(*args)
  nil
end
