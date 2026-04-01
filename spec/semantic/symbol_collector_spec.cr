require "spec"
require "./ast_fixtures"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/symbol"

module SymbolCollectorSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include SymbolCollectorSpecAliases

describe Semantic::SymbolCollector do
  it "collects macro definitions into the global symbol table" do
    arena = Frontend::AstArena.new
    macro_id = AstFixtures.make_macro(arena, "greet")

    program = Frontend::Program.new(arena, [macro_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup_macro("greet").should_not be_nil
    symbol.should be_a(Semantic::MacroSymbol)

    macro_def = arena[macro_id].as(Frontend::MacroDefNode)
    symbol.as(Semantic::MacroSymbol).body.should eq(macro_def.body)
  end

  it "redefines macros when names repeat" do
    arena = Frontend::AstArena.new
    first_macro = AstFixtures.make_macro(arena, "repeat")
    second_macro = AstFixtures.make_macro(arena, "repeat")

    program = Frontend::Program.new(arena, [first_macro, second_macro])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup_macro("repeat").should_not be_nil
    symbol.as(Semantic::MacroSymbol).node_id.should eq(second_macro)
  end

  it "collects method definitions with parameters" do
    arena = Frontend::AstArena.new
    method_id = AstFixtures.make_def(arena, "greet", params: ["name"])

    program = Frontend::Program.new(arena, [method_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup("greet").should_not be_nil
    method_symbol = symbol.as(Semantic::MethodSymbol)
    method_symbol.params.map { |p| String.new(p.name.not_nil!) }.should eq(["name"])
    method_symbol.scope.lookup("name").should be_a(Semantic::VariableSymbol)
  end

  it "collects method return type annotation" do
    source = <<-CR
      def get_number : Int32
        42
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup("get_number").should_not be_nil
    method_symbol = symbol.as(Semantic::MethodSymbol)
    method_symbol.return_annotation.should eq("Int32")
  end

  it "does not recurse on nested union annotations while detecting type parameters" do
    source = <<-CR
      def materialize(
        value : Hash(String, Int32 | Nil),
        & : String -> Int32 | Nil
      )
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    symbol = context.symbol_table.lookup("materialize").should_not be_nil
    method_symbol = symbol.as(Semantic::MethodSymbol)
    method_symbol.type_parameters.should be_nil
  end

  it "does not treat brace tuple and named tuple annotations as generic method params" do
    source = <<-CR
      def second(tuple : {Int32, String, Bool})
        tuple[1]
      end

      def pick(pair : {left: Int32, right: String})
        pair[:right]
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    second_symbol = context.symbol_table.lookup("second").should_not be_nil
    pick_symbol = context.symbol_table.lookup("pick").should_not be_nil

    second_symbol.as(Semantic::MethodSymbol).type_parameters.should be_nil
    pick_symbol.as(Semantic::MethodSymbol).type_parameters.should be_nil
  end

  it "expands macros inherited through the class hierarchy" do
    source = <<-CR
      class Object
        macro def_hash(*fields)
          def hash(hasher)
            {% for field in fields %}
              hasher = {{field.id}}.hash(hasher)
            {% end %}
            hasher
          end
        end
      end

      class Reference < Object
      end

      class Box < Reference
        def initialize(@x : Int32)
        end

        def_hash @x
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    box_symbol = context.symbol_table.lookup("Box").should_not be_nil
    box_class = box_symbol.as(Semantic::ClassSymbol)
    box_class.scope.lookup("hash").should be_a(Semantic::MethodSymbol)
  end

  it "expands inherited getter? macros with symbol arguments" do
    source = <<-CR
      class Object
        macro getter?(*names, &block)
          {% for name in names %}
            {% if name.is_a?(TypeDeclaration) %}
              {% var_name = name.var.id %}
              {% type = name.type %}
              {% if block %}
                @{{var_name}} : {{type}}? {% if name.value %} = {{name.value}} {% end %}
              {% else %}
                @{{name}}
              {% end %}
            {% elsif name.is_a?(Assign) %}
              {% var_name = name.target %}
              {% type = nil %}
              @{{name}}
            {% else %}
              {% var_name = name.id %}
              {% type = nil %}
            {% end %}

            def {{var_name}}? {% if type %} : {{type}} {% end %}
              {% if block %}
                if (%value = @{{var_name}}).nil?
                  @{{var_name}} = {{yield}}
                else
                  %value
                end
              {% else %}
                @{{var_name}}
              {% end %}
            end
          {% end %}
        end
      end

      class Reference < Object
      end

      class Group < Reference
        getter? :break
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    group_symbol = context.symbol_table.lookup("Group").should_not be_nil
    group_class = group_symbol.as(Semantic::ClassSymbol)
    group_class.scope.lookup("break?").should be_a(Semantic::MethodSymbol)
  end

  it "collects class-body macro-generated methods into the class scope" do
    source = <<-CR
      class Flags
        {% for flag in [:undf, :abs] %}
          def {{flag.id}}?
            true
          end
        {% end %}

        def sample
          undf? && abs?
        end
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    flags_symbol = context.symbol_table.lookup("Flags").should_not be_nil
    flags_class = flags_symbol.as(Semantic::ClassSymbol)
    flags_class.scope.lookup("undf?").should be_a(Semantic::MethodSymbol)
    flags_class.scope.lookup("abs?").should be_a(Semantic::MethodSymbol)
  end

  it "reparses class-body macro fragments in class context" do
    source = <<-CR
      class Thread
        {% if true %}
          getter scheduler : String { "ready" }
        {% end %}
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    thread_symbol = context.symbol_table.lookup("Thread").should_not be_nil
    thread_class = thread_symbol.as(Semantic::ClassSymbol)
    thread_class.scope.lookup("scheduler").should be_a(Semantic::MethodSymbol)
  end

  it "expands sorted @type.instance_vars macro loops in class bodies" do
    source = <<-CR
      class Box
        @z : Int32
        @a : Int32

        {% for ivar, i in @type.instance_vars.map(&.name).sort %}
          def value_{{i.id}}
            @{{ivar.id}}
          end
        {% end %}
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    box_symbol = context.symbol_table.lookup("Box").should_not be_nil
    box_class = box_symbol.as(Semantic::ClassSymbol)
    box_class.scope.lookup("value_0").should be_a(Semantic::MethodSymbol)
    box_class.scope.lookup("value_1").should be_a(Semantic::MethodSymbol)
  end

  it "expands string-interpolated ids in class-body macro expressions" do
    source = <<-CR
      class Secure
        {% for type in [Int8, Int16] %}
          private def rand_type(type : {{type}}.class, needed_bytes = sizeof({{type}})) : {{type}}
            result = rand_type({{"U\#{type}".id}}, needed_bytes)
            {{type}}.new!(result)
          end
        {% end %}
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    secure_symbol = context.symbol_table.lookup("Secure").should_not be_nil
    secure_class = secure_symbol.as(Semantic::ClassSymbol)
    secure_class.scope.lookup("rand_type").should_not be_nil
  end

  it "expands identifier.constants macro loops in class bodies" do
    source = <<-CR
      enum DayOfWeek
        Monday
        Tuesday
      end

      class Time
        {% for name in DayOfWeek.constants %}
          def {{name.id.downcase}}? : Bool
            true
          end
        {% end %}
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    time_symbol = context.symbol_table.lookup("Time").should_not be_nil
    time_class = time_symbol.as(Semantic::ClassSymbol)
    time_class.scope.lookup("monday?").should be_a(Semantic::MethodSymbol)
    time_class.scope.lookup("tuesday?").should be_a(Semantic::MethodSymbol)
  end

  it "collects class definitions and nested methods" do
    arena = Frontend::AstArena.new
    method_id = AstFixtures.make_def(arena, "greet")
    class_id = AstFixtures.make_class(arena, "Greeter", body: [method_id])

    program = Frontend::Program.new(arena, [class_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    class_symbol = context.symbol_table.lookup("Greeter").should_not be_nil
    class_symbol.should be_a(Semantic::ClassSymbol)
    class_scope = class_symbol.as(Semantic::ClassSymbol).scope
    class_scope.lookup("greet").should be_a(Semantic::MethodSymbol)
  end

  it "emits diagnostic for incompatible redefinition" do
    arena = Frontend::AstArena.new
    class_id = AstFixtures.make_class(arena, "Thing")
    def_id = AstFixtures.make_def(arena, "Thing")

    program = Frontend::Program.new(arena, [class_id, def_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    diags = collector.diagnostics
    diags.size.should eq(1)
    diag = diags.first
    diag.code.should eq("E2001")
    diag.message.should contain("cannot redefine class 'Thing' as method")
  end

  it "warns when method parameter shadows outer variable" do
    arena = Frontend::AstArena.new
    inner_def = AstFixtures.make_def(arena, "inner", params: ["name"])
    outer_def = AstFixtures.make_def(arena, "outer", params: ["name"], body: [inner_def])

    program = Frontend::Program.new(arena, [outer_def])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.any? { |diag| diag.code == "W2001" }.should be_true
  end

  it "emits error when parameters duplicate within a scope" do
    arena = Frontend::AstArena.new
    def_id = AstFixtures.make_def(arena, "echo", params: ["value", "value"])

    program = Frontend::Program.new(arena, [def_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.any? { |diag| diag.code == "E2002" }.should be_true
  end

  it "collects lib scopes and aliases" do
    source = <<-CR
      lib LibC
        alias Int = Int32
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    lib_symbol = context.symbol_table.lookup("LibC").should_not be_nil
    lib_symbol.should be_a(Semantic::ModuleSymbol)
    lib_scope = lib_symbol.as(Semantic::ModuleSymbol).scope
    alias_symbol = lib_scope.lookup("Int").should_not be_nil
    alias_symbol.should be_a(Semantic::AliasSymbol)
    alias_symbol.as(Semantic::AliasSymbol).target.should eq("Int32")
  end

  it "resolves nested include targets relative to the current lexical scope" do
    source = <<-CR
      module Outer
        module Inner
          macro ping
          end
        end

        class Host
          include Inner
        end
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    outer = context.symbol_table.lookup("Outer").should be_a(Semantic::ModuleSymbol)
    host = outer.as(Semantic::ModuleSymbol).scope.lookup("Host").should be_a(Semantic::ClassSymbol)
    host_scope = host.as(Semantic::ClassSymbol).scope

    host_scope.included_modules.map(&.name).should contain("Inner")
    host_scope.lookup_macro("ping").should be_a(Semantic::MacroSymbol)
  end

  it "resolves generic include targets from their callee symbol" do
    source = <<-CR
      module Iterator(T)
        def stop
        end

        class Host
          include Iterator(T)
        end
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    iterator = context.symbol_table.lookup("Iterator").should be_a(Semantic::ModuleSymbol)
    host = iterator.as(Semantic::ModuleSymbol).scope.lookup("Host").should be_a(Semantic::ClassSymbol)
    host_scope = host.as(Semantic::ClassSymbol).scope

    host_scope.included_modules.map(&.name).should contain("Iterator")
    host_scope.lookup("stop").should be_a(Semantic::MethodSymbol)
  end

  it "tracks module includers for later implicit self lookup" do
    source = <<-CR
      module NeedsFd
      end

      class Host
        include NeedsFd

        def fd
        end
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    needs_fd = context.symbol_table.lookup("NeedsFd").should be_a(Semantic::ModuleSymbol)
    includers = needs_fd.as(Semantic::ModuleSymbol).instance_includers
    includers.size.should eq(1)
    includers.first.should be_a(Semantic::ClassSymbol)
    includers.first.name.should eq("Host")
  end

  it "expands included hooks into the including class scope" do
    source = <<-CR
      module SystemError
        macro included
          extend ::SystemError::ClassMethods

          macro from_errno(message, **opts)
            ::{{ @type }}.from_os_error({{ message }}, 1, {{ opts.double_splat }})
          end
        end

        module ClassMethods
          def from_os_error(message : String?, os_error, **opts)
            self.new(message)
          end
        end
      end

      class Exception
        def initialize(@message : String?)
        end
      end

      class MyError < Exception
        include SystemError
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    my_error = context.symbol_table.lookup("MyError").should be_a(Semantic::ClassSymbol)
    my_error_class = my_error.as(Semantic::ClassSymbol)

    my_error_class.scope.lookup_macro("from_errno").should be_a(Semantic::MacroSymbol)
    my_error_class.class_scope.included_modules.map(&.name).should contain("ClassMethods")
    my_error_class.class_scope.lookup("from_os_error").should be_a(Semantic::MethodSymbol)
  end

  it "allows reopening classes when only one declaration has an explicit superclass" do
    source = <<-CR
      class File
      end

      class File < IO::FileDescriptor
      end

      class File
      end

      class Allocate
      end

      class Allocate < Value
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.none? { |diag| diag.code == "E2003" }.should be_true
  end

  it "collects uppercase assignment constants in class scopes" do
    source = <<-CR
      module Crystal::HIR
        class TaintAnalyzer
          FFI_METHODS = 1

          def ffi_methods
            FFI_METHODS
          end
        end
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    crystal = context.symbol_table.lookup("Crystal").should be_a(Semantic::ModuleSymbol)
    hir = crystal.as(Semantic::ModuleSymbol).scope.lookup("HIR").should be_a(Semantic::ModuleSymbol)
    taint = hir.as(Semantic::ModuleSymbol).scope.lookup("TaintAnalyzer").should be_a(Semantic::ClassSymbol)
    taint.as(Semantic::ClassSymbol).scope.lookup("FFI_METHODS").should be_a(Semantic::ConstantSymbol)
  end

  it "collects module-body macro-generated constants" do
    source = <<-CR
      module Crystal::MIR
        {% if true %}
          TARGET_POINTER_BYTES_U64 = 8_u64
        {% else %}
          TARGET_POINTER_BYTES_U64 = 4_u64
        {% end %}
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    crystal = context.symbol_table.lookup("Crystal").should be_a(Semantic::ModuleSymbol)
    mir = crystal.as(Semantic::ModuleSymbol).scope.lookup("MIR").should be_a(Semantic::ModuleSymbol)
    mir.as(Semantic::ModuleSymbol).scope.lookup("TARGET_POINTER_BYTES_U64").should be_a(Semantic::ConstantSymbol)
  end

  it "collects module-body macro-generated constants inside outer class namespaces" do
    source = <<-CR
      class Float
        module FastFloat
          module Powers
            {% if true %}
              POWER_OF_FIVE_128 = [1_u64, 2_u64]
            {% end %}
          end
        end
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    float = context.symbol_table.lookup("Float").should be_a(Semantic::ClassSymbol)
    fast_float = float.as(Semantic::ClassSymbol).scope.lookup("FastFloat").should be_a(Semantic::ModuleSymbol)
    powers = fast_float.as(Semantic::ModuleSymbol).scope.lookup("Powers").should be_a(Semantic::ModuleSymbol)
    powers.as(Semantic::ModuleSymbol).scope.lookup("POWER_OF_FIVE_128").should be_a(Semantic::ConstantSymbol)
  end

  it "collects module-body macro-generated constants for path-style nested modules" do
    source = <<-CR
      class Float
      end

      module Float::FastFloat
        module Powers
          {% if true %}
            POWER_OF_FIVE_128 = [1_u64, 2_u64]
          {% end %}
        end
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    float = context.symbol_table.lookup("Float").should be_a(Semantic::ClassSymbol)
    fast_float = float.as(Semantic::ClassSymbol).scope.lookup("FastFloat").should be_a(Semantic::ModuleSymbol)
    powers = fast_float.as(Semantic::ModuleSymbol).scope.lookup("Powers").should be_a(Semantic::ModuleSymbol)
    powers.as(Semantic::ModuleSymbol).scope.lookup("POWER_OF_FIVE_128").should be_a(Semantic::ConstantSymbol)
  end

  it "collects module-body macro-generated class vars with nilable path types" do
    source = <<-CR
      class Thread
      end

      lib LibC
        struct MachTimebaseInfo
        end
      end

      module Crystal::System::Thread
        {% if true %}
          @@current_thread : ::Thread?
          @@mach_timebase_info : LibC::MachTimebaseInfo?
        {% end %}
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    crystal = context.symbol_table.lookup("Crystal").should be_a(Semantic::ModuleSymbol)
    system = crystal.as(Semantic::ModuleSymbol).scope.lookup("System").should be_a(Semantic::ModuleSymbol)
    thread_mod = system.as(Semantic::ModuleSymbol).scope.lookup("Thread").should be_a(Semantic::ModuleSymbol)

    current_thread = thread_mod.as(Semantic::ModuleSymbol).scope.lookup("@@current_thread").should be_a(Semantic::ClassVarSymbol)
    current_thread.as(Semantic::ClassVarSymbol).declared_type.should eq("::Thread?")

    mach_timebase = thread_mod.as(Semantic::ModuleSymbol).scope.lookup("@@mach_timebase_info").should be_a(Semantic::ClassVarSymbol)
    mach_timebase.as(Semantic::ClassVarSymbol).declared_type.should eq("LibC::MachTimebaseInfo?")
  end

  it "preserves class-body defaults for class variable assignments" do
    source = <<-CR
      module PendingStore
        @@pending = {} of Int32 => Int32
      end
    CR

    parser = Frontend::Parser.new(Frontend::Lexer.new(source))
    program = parser.parse_program
    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.should be_empty

    pending_store = context.symbol_table.lookup("PendingStore").should be_a(Semantic::ModuleSymbol)
    pending = pending_store.as(Semantic::ModuleSymbol).scope.lookup("@@pending").should be_a(Semantic::ClassVarSymbol)
    pending = pending.as(Semantic::ClassVarSymbol)

    pending.has_default?.should be_true
    pending.default_value.should_not be_nil
  end
end
