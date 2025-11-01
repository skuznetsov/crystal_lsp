#!/usr/bin/env crystal

require "./compiler/cli"

# Entry point
exit_code = CrystalV2::Compiler::CLI.new(ARGV).run
exit exit_code
