struct Time
  getter unix : Int64

  def initialize(@unix : Int64 = 0_i64)
  end

  def self.utc
    Time.new
  end
end

alias Bytes = Slice(UInt8)

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

  def self.expand_path(path : String, base : String? = nil) : String
    if base && !path.starts_with?("/")
      "#{base}/#{path}"
    else
      path
    end
  end

  def self.exists?(path : String) : Bool
    false
  end

  def self.file?(path : String) : Bool
    false
  end

  def self.directory?(path : String) : Bool
    false
  end

  def self.join(*parts : String) : String
    parts.join("/")
  end

  def self.dirname(path : String) : String
    path
  end

  def self.basename(path : String, suffix : String? = nil) : String
    suffix && path.ends_with?(suffix) ? path[0, path.bytesize - suffix.bytesize] : path
  end

  def self.info(path : String) : Info
    Info.new(Time.utc)
  end

  def self.each_line(path : String, &block : String ->)
    # no-op stub
  end

  struct Info
    getter modification_time : Time

    def initialize(@modification_time : Time)
    end
  end
end

def puts(*args)
  nil
end

module ENV
  def self.[](key : String) : String?
    nil
  end

  def self.[]=(key : String, value : String) : String
    value
  end

  def self.fetch(key : String, default_value : String? = nil) : String
    default_value || ""
  end
end
