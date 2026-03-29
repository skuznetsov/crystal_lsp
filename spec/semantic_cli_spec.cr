require "spec"
require "../src/compiler/cli"

private def with_temp_shadow_project(files : Hash(String, String), &)
  dir = File.join(Dir.tempdir, "semantic_shadow_cli_#{Random::Secure.hex(6)}")
  Dir.mkdir_p(dir)
  files.each do |name, source|
    File.write(File.join(dir, name), source)
  end

  begin
    yield dir
  ensure
    FileUtils.rm_rf(dir) if Dir.exists?(dir)
  end
end

private def with_semantic_shadow_env(&)
  previous = ENV["CRYSTAL_V2_SEMANTIC_SHADOW"]?
  ENV["CRYSTAL_V2_SEMANTIC_SHADOW"] = "1"

  begin
    yield
  ensure
    if previous
      ENV["CRYSTAL_V2_SEMANTIC_SHADOW"] = previous
    else
      ENV.delete("CRYSTAL_V2_SEMANTIC_SHADOW")
    end
  end
end

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

  it "reports generated resolution diagnostics separately in semantic shadow summaries" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bad(name)
          def {{name.id}}
            missing + 1
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bad(:alpha)
        alpha()
      CR
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      output = out_io.to_s
      output.should contain("Semantic shadow:")
      output.should contain("generated_resolution_diags=1")
      output.should contain("generated_type_diags=1")
      output.should contain("Semantic shadow unit: path=#{main_path}")
    end
  end

  it "reports generated type diagnostics separately in semantic shadow summaries" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bad(name)
          def {{name.id}}
            1 + "x"
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bad(:alpha)
        alpha()
      CR
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      output = out_io.to_s
      output.should contain("Semantic shadow:")
      output.should contain("generated_resolution_diags=0")
      output.should contain("generated_type_diags=1")
      output.should contain("Semantic shadow unit: path=#{main_path}")
    end
  end

  it "reports semantic declaration provenance for macro-expanded methods" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        def direct_greet
        end

        macro define_alpha
          def alpha
          end
        end

        define_alpha
      CR
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      output = out_io.to_s
      output.should contain("Semantic shadow declarations: methods provenance")
      output.should contain("semantic_direct_total=1")
      output.should contain("semantic_macro_expanded_total=1")
      output.should contain("generated_symbols=1")
    end
  end

  it "counts generated overload families in shadow summaries" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        def greet
        end

        macro define_greet(name)
          def greet(value : {{name.id}})
          end
        end

        define_greet(:Int32)
      CR
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      output = out_io.to_s
      output.should contain("semantic_direct_total=1")
      output.should contain("semantic_macro_expanded_total=1")
      output.should contain("generated_symbols=1")
    end
  end

  it "prints macro definition note for cross-file generated diagnostics" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bad(name)
          def {{name.id}}
            missing + 1
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bad(:alpha)
        alpha()
      CR
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      diagnostics = err_io.to_s
      diagnostics.should contain("note: expanded from macro call here")
      diagnostics.should contain("note: macro defined here")
      diagnostics.should contain(File.join(dir, "lib.cr"))
    end
  end

  it "does not print redundant macro definition note for same-file generated diagnostics" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        macro define_bad(name)
          def {{name.id}}
            missing + 1
          end
        end

        define_bad(:alpha)
        alpha()
      CR
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      diagnostics = err_io.to_s
      diagnostics.should contain("note: expanded from macro call here")
      diagnostics.should_not contain("note: macro defined here")
    end
  end
end
