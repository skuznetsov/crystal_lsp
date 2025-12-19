require "../spec_helper"

# Integration tests for yield-function inlining across required files.
# These exercises the AST-to-HIR yield substitution path (inline-only functions).

module YieldInliningTestHelper
  COMPILER_PATH = File.expand_path("../../bin/crystal_v2", __DIR__)

  def self.compile_and_run(files : Hash(String, String), main_file : String) : String
    dir = "/tmp/crystal_v2_yield_inline_#{Random.rand(100000)}"
    exe = "/tmp/crystal_v2_yield_inline_#{Random.rand(100000)}"

    Dir.mkdir(dir)
    files.each do |name, content|
      File.write(File.join(dir, name), content)
    end

    main_path = File.join(dir, main_file)

    begin
      result = `#{COMPILER_PATH} build #{main_path} -o #{exe} --no-prelude 2>&1`
      raise "Compilation failed: #{result}" unless $?.success?

      `#{exe} 2>&1`
    ensure
      files.keys.each do |name|
        path = File.join(dir, name)
        File.delete(path) if File.exists?(path)
      end
      Dir.delete(dir) if Dir.exists?(dir)

      File.delete(exe) if File.exists?(exe)
      File.delete("#{exe}.ll") if File.exists?("#{exe}.ll")
      File.delete("#{exe}.ll.opt.ll") if File.exists?("#{exe}.ll.opt.ll")
    end
  end
end

describe "Yield inlining integration" do
  it "inlines yield methods across required files" do
    files = {
      "lib.cr" => <<-CR,
        struct X
          def initialize(@v : Int32)
          end

          def yield_v
            yield @v
          end
        end
        CR
      "main.cr" => <<-CR,
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        require "./lib"

        x = X.new(7)
        x.yield_v do |n|
          LibC.printf("%d\\n", n)
        end
        CR
    }

    output = YieldInliningTestHelper.compile_and_run(files, "main.cr")
    output.strip.should eq("7")
  end
end

