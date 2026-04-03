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

private def with_semantic_compile_env(&)
  previous = ENV["CRYSTAL_V2_SEMANTIC_COMPILE"]?
  ENV["CRYSTAL_V2_SEMANTIC_COMPILE"] = "1"

  begin
    yield
  ensure
    if previous
      ENV["CRYSTAL_V2_SEMANTIC_COMPILE"] = previous
    else
      ENV.delete("CRYSTAL_V2_SEMANTIC_COMPILE")
    end
  end
end

private def with_semantic_shadow_strict_env(&)
  previous_shadow = ENV["CRYSTAL_V2_SEMANTIC_SHADOW"]?
  previous_strict = ENV["CRYSTAL_V2_SEMANTIC_SHADOW_STRICT"]?
  ENV["CRYSTAL_V2_SEMANTIC_SHADOW"] = "1"
  ENV["CRYSTAL_V2_SEMANTIC_SHADOW_STRICT"] = "1"

  begin
    yield
  ensure
    if previous_shadow
      ENV["CRYSTAL_V2_SEMANTIC_SHADOW"] = previous_shadow
    else
      ENV.delete("CRYSTAL_V2_SEMANTIC_SHADOW")
    end

    if previous_strict
      ENV["CRYSTAL_V2_SEMANTIC_SHADOW_STRICT"] = previous_strict
    else
      ENV.delete("CRYSTAL_V2_SEMANTIC_SHADOW_STRICT")
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

  it "runs semantic compile prepass on the real compile path before lowering" do
    with_temp_shadow_project({
      "main.cr" => "1\n",
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("Semantic compile prepass:")
      out_io.to_s.should contain("compile_parse_diags=0")
      out_io.to_s.should contain("shadow_parse_diags=0")
      out_io.to_s.should contain("parse_diag_gaps=0")
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "fails compile early on semantic compile prepass resolution errors" do
    with_temp_shadow_project({
      "main.cr" => "missing + 1\n",
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 0

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(1)
      out_io.to_s.should contain("Semantic compile prepass:")
      out_io.to_s.should contain("compile_parse_diags=0")
      out_io.to_s.should contain("shadow_parse_diags=0")
      out_io.to_s.should contain("parse_diag_gaps=0")
      out_io.to_s.should contain("resolution_diags=1")
      diagnostics = err_io.to_s
      diagnostics.should contain("undefined local variable or method 'missing'")
      diagnostics.should contain("error: compilation failed due to semantic compile prepass errors")
    end
  end

  it "fails compile early on semantic compile prepass type errors" do
    with_temp_shadow_project({
      "main.cr" => "1 + \"x\"\n",
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 0

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(1)
      out_io.to_s.should contain("Semantic compile prepass:")
      out_io.to_s.should contain("compile_parse_diags=0")
      out_io.to_s.should contain("shadow_parse_diags=0")
      out_io.to_s.should contain("parse_diag_gaps=0")
      out_io.to_s.should contain("type_diags=1")
      diagnostics = err_io.to_s
      diagnostics.should contain("error[E3001]")
      diagnostics.should contain("Operator '+' not defined for Int32 and String")
      diagnostics.should contain("error: compilation failed due to semantic compile prepass errors")
    end
  end

  it "keeps semantic compile prepass green when compile and aggregate parse diagnostics match" do
    with_temp_shadow_project({
      "main.cr" => ")\n",
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      output = out_io.to_s
      diagnostics = err_io.to_s
      output.should contain("Semantic compile prepass:")
      output.should contain("compile_parse_diags=1")
      output.should contain("shadow_parse_diags=1")
      output.should contain("parse_diag_gaps=0")
      diagnostics.should_not contain("semantic shadow strict parse diagnostic mismatch")
      diagnostics.should_not contain("error: compilation failed due to semantic compile aggregate parser errors")
    end
  end

  it "keeps semantic compile prepass green for variadic macro params" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        macro build(*properties)
          {% for property in properties %}
            class {{property.id}}
            end
          {% end %}
        end

        build Foo, Bar
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps compile-path macro reflection green for skip_file and has_constant? branches" do
    with_temp_shadow_project({
      "event_loop.cr" => <<-'CRYSTAL',
        abstract class Crystal::EventLoop
        end

        {% if flag?(:wasi) %}
          class Crystal::EventLoop::Wasi < Crystal::EventLoop
          end
        {% elsif flag?(:unix) %}
          {% if flag?("evloop=libevent") %}
            class Crystal::EventLoop::LibEvent < Crystal::EventLoop
            end
          {% elsif flag?("evloop=epoll") || flag?(:android) || flag?(:linux) %}
            abstract class Crystal::EventLoop::Polling < Crystal::EventLoop
            end
          {% elsif flag?("evloop=kqueue") || flag?(:darwin) || flag?(:freebsd) %}
            abstract class Crystal::EventLoop::Polling < Crystal::EventLoop
            end
          {% else %}
            class Crystal::EventLoop::LibEvent < Crystal::EventLoop
            end
          {% end %}
        {% elsif flag?(:win32) %}
          class Crystal::EventLoop::IOCP < Crystal::EventLoop
          end
        {% end %}
      CRYSTAL
      "evented.cr" => <<-'CRYSTAL',
        require "./event_loop"

        {% skip_file unless flag?(:wasi) || Crystal::EventLoop.has_constant?(:LibEvent) %}

        module IO::Evented
          VALUE = 1
        end
      CRYSTAL
      "fd.cr" => <<-'CRYSTAL',
        class IO
        end

        require "./evented"

        module Crystal::System::FileDescriptor
          {% if IO.has_constant?(:Evented) %}
            VALUE = IO::Evented::VALUE
          {% else %}
            VALUE = 0
          {% end %}
        end
      CRYSTAL
      "socket.cr" => <<-'CRYSTAL',
        require "./fd"

        module Crystal::System::Socket
          {% if Crystal::EventLoop.has_constant?(:Polling) %}
            VALUE = Crystal::EventLoop::Polling
          {% else %}
            VALUE = 0
          {% end %}
        end
      CRYSTAL
      "main.cr" => <<-'CRYSTAL',
        require "./socket"

        Crystal::System::FileDescriptor::VALUE
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for hash-backed macro iteration and mutation" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        {% begin %}
          {% properties = {} of Nil => Nil %}
          {% properties["Foo".id] = {key: "Bar".id.stringify} %}
          {% for name, value in properties %}
            class {{name.id}}
            end

            class {{value[:key].id}}
            end
          {% end %}
        {% end %}
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for top-level begin macros with nested branch text and later macro defs" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        {% begin %}
        def spawn(same_thread = false)
          {% if flag?(:execution_context) %}
            1
          {% else %}
            value = 1
            {% if flag?(:preview_mt) %} value = 2 if same_thread {% end %}
            value
          {% end %}
        end
        {% end %}

        macro spawn(call)
          {{call}}
        end

        spawn(spawn(false))
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for top-level macro if without begin wrapper and later macro defs" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        {% if flag?(:darwin) %}
        def choose
          1
        end
        {% else %}
        def choose
          2
        end
        {% end %}

        macro wrap(call)
          {{call}}
        end

        wrap(choose)
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for nested top-level macro control literals without expander-only features" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        {% if flag?(:unix) %}
          {% if flag?(:bsd) %}
            def host_impl
              1
            end
          {% else %}
            def host_impl
              2
            end
            {% if flag?(:linux) %}
              def linux_impl
                3
              end
            {% end %}
          {% end %}
        {% elsif flag?(:win32) %}
          def host_impl
            4
          end
        {% else %}
          def host_impl
            5
          end
        {% end %}

        host_impl
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for splat call traversal" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        def consume(*values)
        end

        values = [1]
        consume(*values)
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for T.class annotation dispatch" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        class Int32
          def self.multiplicative_identity
            1
          end
        end

        module Reflect(T)
          def self.type : T.class
            T
          end
        end

        module Enumerable(T)
          abstract def each(& : T ->)

          def self.element_type(value) : T
            uninitialized T
          end

          def productish(& : T -> _)
            Reflect(typeof(yield Enumerable.element_type(self))).type.multiplicative_identity
          end
        end

        class Box
          include Enumerable(Int32)

          def each(& : Int32 ->)
            yield 1
          end
        end

        Box.new.productish { |x| x + 1 }
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for untyped reduce block propagation" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        module Enumerable(T)
          abstract def each(& : T ->)

          def reduce(initial, &)
            memo = initial
            each do |e|
              memo = yield memo, e
            end
            memo
          end

          def sumish(initial : Number, & : T ->)
            reduce(initial) { |memo, e| memo + (yield e) }
          end
        end

        class Number
        end

        class Int32 < Number
        end

        class Box
          include Enumerable(Int32)

          def each(& : Int32 ->)
            yield 1
          end
        end

        Box.new.sumish(0) { |x| x + 1 }
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for untyped each_with_object block propagation" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        class Hash(K, V)
          def initialize
          end

          def []=(key : K, value : V) : V
            value
          end
        end

        module Enumerable(T)
          abstract def each(& : T ->)

          def self.element_type(value) : T
            uninitialized T
          end

          def each_with_object(obj, &)
            each do |elem|
              yield elem, obj
            end
            obj
          end

          def to_hish
            each_with_object(Hash(typeof(Enumerable.element_type(self)[0]), typeof(Enumerable.element_type(self)[1])).new) do |item, hash|
              hash[item[0]] = item[1]
            end
          end
        end

        class Pairs
          include Enumerable(Tuple(Int32, String))

          def each(& : Tuple(Int32, String) ->)
            yield {1, "x"}
          end
        end

        Pairs.new.to_hish
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for transitive included-module reduce propagation" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        class String
          def +(other : String) : String
            self
          end
        end

        module Enumerable(T)
          abstract def each(& : T ->)

          def reduce(initial, &)
            memo = initial
            each do |elem|
              memo = yield memo, elem
            end
            memo
          end

          def sum(initial)
            sum initial, &.itself
          end

          def sum(initial, & : T ->)
            reduce(initial) { |memo, e| memo + (yield e) }
          end
        end

        module Indexable(T)
          include Enumerable(T)
        end

        class Names
          include Indexable(String)

          def each(& : String ->)
            yield "x"
          end
        end

        Names.new.sum("")
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for transitive included-module each_with_object propagation" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        class Hash(K, V)
          def initialize
          end

          def fetch(key : K)
            yield
          end

          def [](key : K) : V
            uninitialized V
          end

          def []=(key : K, value : V) : V
            value
          end
        end

        class Int32
          def +(other : Int32) : Int32
            self
          end

          def self.zero : Int32
            0
          end
        end

        class String
          def downcase : String
            self
          end
        end

        module Enumerable(T)
          abstract def each(& : T ->)

          def each_with_object(obj, &)
            each do |elem|
              yield elem, obj
            end
            obj
          end

          def tally_by(hash, &)
            each_with_object(hash) do |item, hash|
              value = yield item
              count = hash.fetch(value) { typeof(hash[value]).zero }
              hash[value] = count + 1
            end
          end
        end

        module Indexable(T)
          include Enumerable(T)
        end

        class Names
          include Indexable(String)

          def each(& : String ->)
            yield "x"
          end
        end

        Names.new.tally_by(Hash(String, Int32).new, &.downcase)
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for record macros inside class-owned module reopens" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        macro record(__name name, *properties, **kwargs)
          struct {{name.id}}
          end
        end

        struct Float
        end

        module Float::FastFloat
          record Point, x : Int32
        end

        Float::FastFloat::Point.new
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("Semantic compile prepass:")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for stdlib record macros with user-defined generic type applications" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        require "/Users/sergey/Projects/Crystal/crystal_v2_repo/src/stdlib/object/properties"
        require "/Users/sergey/Projects/Crystal/crystal_v2_repo/src/stdlib/macros"

        struct Object
        end

        struct Value
        end

        struct Number < Value
        end

        struct Int32 < Number
        end

        struct UInt8 < Number
        end

        struct Float
        end

        module Float::FastFloat
          @[Flags]
          enum CharsFormat
            General = 0
          end

          record ParseOptionsT(UC), format : CharsFormat = :general, decimal_point : UC = 0x2E
          alias ParseOptions = ParseOptionsT(UInt8)
        end

        Float::FastFloat::ParseOptions.new
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("semantic_diags=0")
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for camel-cased enum flag predicate methods" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        @[Flags]
        enum CharsFormat
          JsonFmt = 1 << 0
          Fixed = 1 << 1
        end

        struct ParseOptionsT(UC)
          def initialize(@format : CharsFormat = :json_fmt)
          end

          def format : CharsFormat
            @format
          end
        end

        def probe(options : ParseOptionsT(UInt8))
          options.format.json_fmt?
        end

        probe(ParseOptionsT(UInt8).new)
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "keeps semantic compile prepass green for brace tuple type identifiers in generic arguments" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        class Array(T)
          def self.new(size : Int32)
            self
          end
        end

        Array({UInt64, UInt64, UInt64}).new(1)
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end

  it "reports compile and shadow parse diagnostics separately in semantic shadow summaries" do
    with_temp_shadow_project({
      "main.cr" => ")\n",
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
      output.should contain("compile_parse_diags=1")
      output.should contain("shadow_parse_diags=1")
      output.should contain("parse_diag_gaps=0")
      output.should contain("Semantic shadow parse diagnostics: compile_total=1 compile_unique=1 shadow_total=1 shadow_unique=1 gaps=0")
      output.should contain("Semantic shadow unit: path=#{main_path}")
    end
  end

  it "keeps strict semantic shadow green when parse parity matches" do
    with_temp_shadow_project({
      "main.cr" => ")\n",
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_strict_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      output = out_io.to_s
      diagnostics = err_io.to_s
      output.should contain("parse_diag_gaps=0")
      output.should contain("Semantic shadow parse diagnostics: compile_total=1 compile_unique=1 shadow_total=1 shadow_unique=1 gaps=0")
      diagnostics.should_not contain("warning: semantic shadow failed:")
    end
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

  it "reports generated diagnostics inside macro-expanded class bodies" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bad_class
          class BadBox
            def self.call
              missing + 1
            end
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bad_class
        BadBox.call
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
      diagnostics = err_io.to_s
      output.should contain("generated_resolution_diags=1")
      output.should contain("generated_type_diags=1")
      output.should contain("Semantic shadow unit: path=#{main_path}")
      diagnostics.should contain("BadBox")
      diagnostics.should contain("missing + 1")
      diagnostics.should contain("note: expanded from macro call here")
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

  it "keeps non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        macro define_bundle
          class Alpha
          end

          module Beta
          end

          enum Mode
            One
          end

          FLAG = 1
        end

        define_bundle
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps cross-file non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bundle
          class Alpha
          end

          module Beta
          end

          enum Mode
            One
          end

          FLAG = 1
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bundle
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps argful non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        macro define_bundle(class_name, module_name, enum_name, const_name)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end

        define_bundle(:Alpha, :Beta, :Mode, :FLAG)
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps cross-file argful non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bundle(class_name, module_name, enum_name, const_name)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bundle(:Alpha, :Beta, :Mode, :FLAG)
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps named-arg non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        macro define_bundle(class_name, module_name, enum_name, const_name)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end

        define_bundle(class_name: :Alpha, module_name: :Beta, enum_name: :Mode, const_name: :FLAG)
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps cross-file named-arg non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bundle(class_name, module_name, enum_name, const_name)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bundle(class_name: :Alpha, module_name: :Beta, enum_name: :Mode, const_name: :FLAG)
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps default-arg cross-file non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bundle(class_name = :Alpha, module_name = :Beta, enum_name = :Mode, const_name = :FLAG)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bundle
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps same-file default-arg non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        macro define_bundle(class_name = :Alpha, module_name = :Beta, enum_name = :Mode, const_name = :FLAG)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end

        define_bundle
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps block-yield non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        macro define_bundle
          {{yield}}
        end

        define_bundle do
          class Alpha
          end

          module Beta
          end

          enum Mode
            One
          end

          FLAG = 1
        end
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps cross-file block-yield non-method macro-call declaration parity green" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bundle
          {{yield}}
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bundle do
          class Alpha
          end

          module Beta
          end

          enum Mode
            One
          end

          FLAG = 1
        end
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
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: classes provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: classes provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: modules provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: enums provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance collector_direct_total=0 collector_direct_unique=0 collector_macro_expanded_total=1 collector_macro_expanded_unique=1")
      output.should contain("Semantic shadow declarations: constants provenance semantic_direct_total=0 semantic_direct_unique=0 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1")
    end
  end

  it "keeps strict semantic shadow green when declaration parity matches" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bundle(class_name, module_name, enum_name, const_name)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bundle(:Alpha, :Beta, :Mode, :FLAG)
      CR
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_strict_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      output = out_io.to_s
      diagnostics = err_io.to_s
      output.should contain("declaration_gaps=0")
      output.should contain("Semantic shadow declarations: classes collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: modules collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: enums collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      output.should contain("Semantic shadow declarations: constants collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0")
      diagnostics.should_not contain("warning: semantic shadow failed:")
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

  it "attributes generated overload families to the generated contributor unit" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        def greet
        end

        macro define_greet(name)
          def greet(value : {{name.id}})
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_greet(:Int32)
      CR
    }) do |dir|
      lib_path = File.join(dir, "lib.cr")
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new

      with_semantic_shadow_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        cli.run(out_io: out_io, err_io: err_io)
      end

      output = out_io.to_s
      lib_line = output.lines.find { |line| line.includes?("Semantic shadow unit: path=#{lib_path}") }
      main_line = output.lines.find { |line| line.includes?("Semantic shadow unit: path=#{main_path}") }

      lib_line.should_not be_nil
      main_line.should_not be_nil
      lib_line.not_nil!.should contain("generated_symbols=0")
      main_line.not_nil!.should contain("generated_symbols=1")
    end
  end

  it "reports non-method macro-expanded declaration provenance in shadow summaries" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_bundle
          class Alpha
          end

          module Beta
          end

          enum Delta
            One
          end

          GAMMA = 1
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_bundle
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
      output.should contain("generated_symbols=4")
      output.should contain("Semantic shadow declarations: classes provenance")
      output.should contain("Semantic shadow declarations: modules provenance")
      output.should contain("Semantic shadow declarations: enums provenance")
      output.should contain("Semantic shadow declarations: constants provenance")
      output.should contain("semantic_macro_expanded_total=1")
    end
  end

  it "reports macro-expanded macro declaration provenance in shadow summaries" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_trace
          macro generated_trace
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_trace
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
      output.should contain("generated_symbols=1")
      output.should contain("Semantic shadow declarations: macros provenance")
      output.should contain("semantic_direct_total=1")
      output.should contain("semantic_macro_expanded_total=1")
    end
  end

  it "preserves both direct and generated class provenance across reopenings" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_alpha
          class Alpha
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_alpha

        class Alpha
          def self.extra
          end
        end
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
      output.should contain("Semantic shadow declarations: classes provenance")
      output.should contain("semantic_direct_total=1")
      output.should contain("semantic_macro_expanded_total=1")
      output.should contain("generated_symbols=1")
    end
  end

  it "preserves both direct and generated module provenance across reopenings" do
    with_temp_shadow_project({
      "lib.cr"  => <<-CR,
        macro define_alpha
          module Alpha
          end
        end
      CR
      "main.cr" => <<-CR,
        require "./lib"
        define_alpha

        module Alpha
          VALUE = 1
        end
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
      output.should contain("Semantic shadow declarations: modules provenance")
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

  it "prints macro definition note for cross-file generated type diagnostics" do
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

      diagnostics = err_io.to_s
      diagnostics.should contain("error[E3001]")
      diagnostics.should contain("[generated]")
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

  it "does not print redundant macro definition note for same-file generated type diagnostics" do
    with_temp_shadow_project({
      "main.cr" => <<-CR,
        macro define_bad(name)
          def {{name.id}}
            1 + "x"
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
      diagnostics.should contain("error[E3001]")
      diagnostics.should contain("[generated]")
      diagnostics.should contain("note: expanded from macro call here")
      diagnostics.should_not contain("note: macro defined here")
    end
  end

  it "keeps semantic compile prepass green for stdlib class_property macros" do
    with_temp_shadow_project({
      "main.cr" => <<-'CRYSTAL',
        require "/Users/sergey/Projects/Crystal/crystal_v2_repo/src/stdlib/object/properties"

        class Reference < Object
        end

        struct Time
        end

        class Time::Location < Reference
          class_property(local : Location) { self }
        end

        Time::Location.local
      CRYSTAL
    }) do |dir|
      main_path = File.join(dir, "main.cr")
      output_path = File.join(dir, "main")
      out_io = IO::Memory.new
      err_io = IO::Memory.new
      status = 1

      with_semantic_compile_env do
        cli = CrystalV2::Compiler::CLI.new([main_path, "--no-prelude", "--stats", "--verbose", "--no-link", "-o", output_path])
        status = cli.run(out_io: out_io, err_io: err_io)
      end

      status.should eq(0)
      out_io.to_s.should contain("resolution_diags=0")
      out_io.to_s.should contain("type_diags=0")
      err_io.to_s.should be_empty
    end
  end
end
