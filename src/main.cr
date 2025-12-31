require "./runtime"
require "./compiler"

module CrystalV2
  VERSION = "0.1.0"

  # Entry point: for now just print roadmap info.
  def self.run(args : Array(String))
    Runtime.bootstrap
    Compiler::CLI.new(args).run
  end
end

# Don't run CLI when loaded as a library (e.g., by specs)
macro run_main_if_not_spec
  {% unless @top_level.has_constant?("Spec") %}
    CrystalV2.run(ARGV)
  {% end %}
end

run_main_if_not_spec
