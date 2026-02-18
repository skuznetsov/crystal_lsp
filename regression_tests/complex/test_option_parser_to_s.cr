# EXPECT: option_parser_ok

require "option_parser"

parser = OptionParser.new
parser.banner = "Usage: complex [options]"
parser.on("-h", "--help", "Show help") { }
parser.on("-v", "--version", "Show version") { }

output = String.build do |io|
  parser.to_s(io)
end

puts(output.includes?("Usage: complex [options]") ? "option_parser_ok" : "option_parser_bad")
