#!/usr/bin/env crystal
# Parse a set of files with both the upstream Crystal parser and the v2 Pratt parser.
# Reports per-file success and basic diagnostics count (v2 only).

require "option_parser"
require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/watchdog"

project_root = File.expand_path("..", __DIR__)

# Compile-time resolution of upstream paths so the Crystal compiler accepts the require.
{% if env("CRYSTAL_UPSTREAM_ROOT") %}
  {% upstream_root = env("CRYSTAL_UPSTREAM_ROOT") %}
  require {{ "#{upstream_root}/src/compiler/crystal/syntax/virtual_file" }}
  require {{ "#{upstream_root}/src/compiler/crystal/syntax/exception" }}
  require {{ "#{upstream_root}/src/compiler/crystal/syntax/parser" }}
{% else %}
  # Fallback to sibling checkout: ../crystal relative to crystal_v2_repo/tools
  require "../../crystal/src/compiler/crystal/syntax/virtual_file"
  require "../../crystal/src/compiler/crystal/syntax/exception"
  require "../../crystal/src/compiler/crystal/syntax/parser"
{% end %}

module CompareParsers
  alias Watchdog = CrystalV2::Compiler::Frontend::Watchdog

  struct FileResult
    getter path : String
    getter v2_ok : Bool
    getter v2_diagnostics : Int32
    getter upstream_ok : Bool
    getter upstream_error : String?

    def initialize(@path, @v2_ok, @v2_diagnostics, @upstream_ok, @upstream_error)
    end
  end

  MAX_TIME = (ENV["COMPARE_TIMEOUT"]? || "10").to_f.seconds

  def self.parse_v2(path : String) : {Bool, Int32}
    source = File.read(path)
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)

    Watchdog.enable!("parse_v2 #{path}", MAX_TIME)
    begin
      parser.parse_program
      {true, parser.diagnostics.size}
    rescue Watchdog::TimeoutError
      {false, parser.diagnostics.size}
    ensure
      Watchdog.disable!
    end
  end

  def self.parse_upstream(path : String) : {Bool, String?}
    source = File.read(path)
    parser = Crystal::Parser.new(source)
    begin
      parser.parse
      {true, nil}
    rescue ex
      {false, ex.message}
    end
  end

  def self.run(files : Array(String)) : Array(FileResult)
    files.map do |path|
      v2_ok, v2_diags = parse_v2(path)
      upstream_ok, upstream_error = parse_upstream(path)
      FileResult.new(path, v2_ok, v2_diags, upstream_ok, upstream_error)
    end
  end
end

files = [] of String
limit = nil

OptionParser.parse do |p|
  p.banner = "Usage: compare_parser_outputs [options] [files...]"
  p.on("-n NUM", "--limit=NUM", "Limit number of files") { |n| limit = n.to_i }
end

if ARGV.empty?
  files = Dir.glob(File.join(project_root, "src/**/*.cr")).sort
else
  files = ARGV.dup
end

if limit
  files = files.first(limit.not_nil!)
end

results = CompareParsers.run(files)

total = results.size
upstream_fail = results.count { |r| !r.upstream_ok }
v2_fail = results.count { |r| !r.v2_ok }
v2_diag_sum = results.sum(&.v2_diagnostics)

puts "Files tested: #{total}"
puts "Upstream failures: #{upstream_fail}"
puts "V2 failures: #{v2_fail}"
puts "V2 diagnostics (total): #{v2_diag_sum}"
puts

(results.select { |r| !r.upstream_ok || !r.v2_ok || r.v2_diagnostics > 0 }).each do |r|
  puts "#{r.path}"
  puts "  upstream: #{r.upstream_ok ? "OK" : "FAIL: #{r.upstream_error}"}"
  puts "  v2      : #{r.v2_ok ? "OK" : "FAIL"} (diagnostics=#{r.v2_diagnostics})"
end
