require "spec"
require "../src/compiler/cli"

describe CrystalV2::Compiler::CLI do
  it "reports semantic errors when --no-codegen is used" do
    file_path = File.join(__DIR__, "semantic/test_data/missing_method.cr")
    out_io = IO::Memory.new
    err_io = IO::Memory.new

    cli = CrystalV2::Compiler::CLI.new([file_path, "--no-codegen"])
    cli.run(out_io: out_io, err_io: err_io)

    err_io.rewind
    diag = err_io.gets_to_end
    diag.should contain("undefined local variable or method 'say_hello'")
  end

  it "dumps nested scopes when --dump-symbols is used" do
    file_path = File.join(__DIR__, "semantic/test_data/nested_symbols.cr")
    out_io = IO::Memory.new
    err_io = IO::Memory.new

    cli = CrystalV2::Compiler::CLI.new([file_path, "--dump-symbols", "--no-codegen"])
    cli.run(out_io: out_io, err_io: err_io)

    err_io.rewind
    err_io.gets_to_end.should be_empty

    out_io.rewind
    output = out_io.gets_to_end
    output.should contain("class Greeter")
    output.should contain("  method greet (params: name)")
    output.should contain("    variable name")
  end

  it "emits semantic diagnostics for incompatible redefinitions with --no-codegen" do
    file_path = File.join(__DIR__, "semantic/test_data/incompatible_redefinition.cr")
    out_io = IO::Memory.new
    err_io = IO::Memory.new

    cli = CrystalV2::Compiler::CLI.new([file_path, "--no-codegen"])
    cli.run(out_io: out_io, err_io: err_io)

    err_io.rewind
    diagnostics = err_io.gets_to_end
    diagnostics.should contain("error[E2001]")
    diagnostics.should contain("cannot redefine class 'Thing' as method")
    diagnostics.should contain("previous class defined here")
  end

  it "emits E2003 error for class reopening with different superclass" do
    file_path = File.join(__DIR__, "semantic/test_data/superclass_mismatch.cr")
    out_io = IO::Memory.new
    err_io = IO::Memory.new

    cli = CrystalV2::Compiler::CLI.new([file_path, "--no-codegen"])
    cli.run(out_io: out_io, err_io: err_io)

    err_io.rewind
    diagnostics = err_io.gets_to_end
    diagnostics.should contain("error[E2003]")
    diagnostics.should contain("class 'Foo' already defined with superclass 'Bar'")
    diagnostics.should contain("previous superclass declared here")
  end
end
