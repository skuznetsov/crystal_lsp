require "../spec_helper"

# Integration tests that compile Crystal code to LLVM IR and verify correctness
# NOTE: Do NOT require driver.cr as it has main entry point that executes immediately
# These tests exercise the full compilation pipeline: Parse → HIR → MIR → LLVM IR

module CodegenTestHelper
  COMPILER_PATH = File.expand_path("../../bin/crystal_v2", __DIR__)

  # Helper to compile and execute, returning stdout
  def self.compile_and_run(source : String) : String
    tmp_cr = "/tmp/crystal_v2_test_#{Random.rand(100000)}.cr"
    tmp_exe = "/tmp/crystal_v2_test_#{Random.rand(100000)}"

    File.write(tmp_cr, source)

    begin
      # Compile
      result = `#{COMPILER_PATH} build #{tmp_cr} -o #{tmp_exe} --no-prelude 2>&1`
      raise "Compilation failed: #{result}" unless $?.success?

      # Run
      output = `#{tmp_exe} 2>&1`
      output
    ensure
      File.delete(tmp_cr) if File.exists?(tmp_cr)
      File.delete(tmp_exe) if File.exists?(tmp_exe)
      File.delete("#{tmp_exe}.ll") if File.exists?("#{tmp_exe}.ll")
      File.delete("#{tmp_exe}.ll.opt.ll") if File.exists?("#{tmp_exe}.ll.opt.ll")
    end
  end
end

describe "Codegen Integration" do
  # ═══════════════════════════════════════════════════════════════════════════
  # PRIMITIVES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "primitive types" do
    it "handles Int32 arithmetic" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        a = 10
        b = 3
        LibC.printf("%d %d %d %d %d\\n", a + b, a - b, a * b, a / b, a % b)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("13 7 30 3 1")
    end

    it "handles Float64 arithmetic" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        a = 10.0
        b = 3.0
        LibC.printf("%.1f\\n", a / b)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("3.3")
    end

    it "handles boolean operations" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        t = true
        f = false
        LibC.printf("%d %d %d %d\\n", t ? 1 : 0, f ? 1 : 0, (t && f) ? 1 : 0, (t || f) ? 1 : 0)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("1 0 0 1")
    end

    it "handles integer boundaries" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        max = 2147483647
        min = -2147483648
        LibC.printf("%d %d\\n", max, min)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("2147483647 -2147483648")
    end

    it "handles bitwise operations" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        LibC.printf("%d %d %d %d %d\\n", 0b1010 & 0b1100, 0b1010 | 0b1100, 0b1010 ^ 0b1100, 1 << 4, 16 >> 2)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("8 14 6 16 4")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONTROL FLOW
  # ═══════════════════════════════════════════════════════════════════════════

  describe "control flow" do
    it "handles if/else" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        x = 10
        if x > 5
          LibC.printf("big\\n")
        else
          LibC.printf("small\\n")
        end
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("big")
    end

    it "handles nested if" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        x = 10
        if x > 5
          if x > 8
            LibC.printf("very big\\n")
          else
            LibC.printf("medium\\n")
          end
        end
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("very big")
    end

    it "handles while loop" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        i = 0
        sum = 0
        while i < 5
          sum = sum + i
          i = i + 1
        end
        LibC.printf("%d %d\\n", i, sum)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("5 10")
    end

    it "handles early return" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        def check(n : Int32) : Int32
          if n < 0
            return -1
          end
          if n == 0
            return 0
          end
          n * 2
        end

        LibC.printf("%d %d %d\\n", check(-5), check(0), check(5))
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("-1 0 10")
    end

    it "handles ternary operator" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        x = 5
        r1 = x > 3 ? 1 : 0
        r2 = x > 10 ? 1 : 0
        LibC.printf("%d %d\\n", r1, r2)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("1 0")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # STRUCTS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "structs" do
    it "handles struct with Int32 fields" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        struct Point
          @x : Int32
          @y : Int32

          def initialize(@x : Int32, @y : Int32)
          end

          def x : Int32
            @x
          end

          def y : Int32
            @y
          end

          def sum : Int32
            @x + @y
          end
        end

        p = Point.new(3, 4)
        LibC.printf("%d %d %d\\n", p.x, p.y, p.sum)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("3 4 7")
    end

    it "handles struct with Float64 fields" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        struct Vec2
          @x : Float64
          @y : Float64

          def initialize(@x : Float64, @y : Float64)
          end

          def x : Float64
            @x
          end

          def y : Float64
            @y
          end

          def length_sq : Float64
            @x * @x + @y * @y
          end
        end

        v = Vec2.new(3.0, 4.0)
        LibC.printf("%.1f\\n", v.length_sq)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("25.0")
    end

    it "handles struct with mixed types" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        struct Mixed
          @int_val : Int32
          @float_val : Float64
          @bool_val : Bool

          def initialize(@int_val : Int32, @float_val : Float64, @bool_val : Bool)
          end

          def int_val : Int32
            @int_val
          end

          def float_val : Float64
            @float_val
          end

          def bool_val : Bool
            @bool_val
          end
        end

        m = Mixed.new(42, 3.14, true)
        LibC.printf("%d %.2f %d\\n", m.int_val, m.float_val, m.bool_val ? 1 : 0)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("42 3.14 1")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CLASSES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "classes" do
    it "handles class with mutable state" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        class Counter
          @value : Int32

          def initialize(@value : Int32)
          end

          def value : Int32
            @value
          end

          def increment
            @value = @value + 1
          end
        end

        c = Counter.new(10)
        LibC.printf("%d\\n", c.value)
        c.increment
        LibC.printf("%d\\n", c.value)
        c.increment
        LibC.printf("%d\\n", c.value)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("10\n11\n12")
    end

    it "handles multiple class instances" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        class Box
          @value : Int32

          def initialize(@value : Int32)
          end

          def value : Int32
            @value
          end

          def set(v : Int32)
            @value = v
          end
        end

        b1 = Box.new(1)
        b2 = Box.new(2)
        b3 = Box.new(3)
        LibC.printf("%d %d %d\\n", b1.value, b2.value, b3.value)
        b1.set(100)
        LibC.printf("%d %d %d\\n", b1.value, b2.value, b3.value)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("1 2 3\n100 2 3")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # UNION TYPES (NILABLE)
  # ═══════════════════════════════════════════════════════════════════════════

  describe "union types" do
    it "handles nilable Int32" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        def maybe(give : Bool) : Int32 | Nil
          if give
            42
          else
            nil
          end
        end

        r1 = maybe(true)
        r2 = maybe(false)

        if r1.nil?
          LibC.printf("nil\\n")
        else
          LibC.printf("%d\\n", r1.not_nil!)
        end

        if r2.nil?
          LibC.printf("nil\\n")
        else
          LibC.printf("%d\\n", r2.not_nil!)
        end
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("42\nnil")
    end

    it "handles nilable class reference" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        class Node
          @value : Int32
          @next : Node | Nil

          def initialize(@value : Int32)
            @next = nil
          end

          def value : Int32
            @value
          end

          def next_node : Node | Nil
            @next
          end

          def set_next(n : Node | Nil)
            @next = n
          end

          def has_next : Bool
            !@next.nil?
          end
        end

        n1 = Node.new(1)
        n2 = Node.new(2)
        n1.set_next(n2)

        LibC.printf("%d %d\\n", n1.has_next ? 1 : 0, n2.has_next ? 1 : 0)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("1 0")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # RECURSION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "recursion" do
    it "handles factorial" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        def factorial(n : Int32) : Int32
          if n <= 1
            1
          else
            n * factorial(n - 1)
          end
        end

        LibC.printf("%d %d %d %d\\n", factorial(0), factorial(1), factorial(5), factorial(10))
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("1 1 120 3628800")
    end

    it "handles fibonacci" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        def fib(n : Int32) : Int32
          if n <= 1
            n
          else
            fib(n - 1) + fib(n - 2)
          end
        end

        LibC.printf("%d %d %d %d %d\\n", fib(0), fib(1), fib(2), fib(10), fib(20))
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("0 1 1 55 6765")
    end

    it "handles mutual recursion" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        def is_even(n : Int32) : Bool
          if n == 0
            true
          else
            is_odd(n - 1)
          end
        end

        def is_odd(n : Int32) : Bool
          if n == 0
            false
          else
            is_even(n - 1)
          end
        end

        LibC.printf("%d %d %d %d\\n", is_even(0) ? 1 : 0, is_even(10) ? 1 : 0, is_odd(7) ? 1 : 0, is_odd(8) ? 1 : 0)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("1 1 1 0")
    end

    it "handles GCD" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        def gcd(a : Int32, b : Int32) : Int32
          if b == 0
            a
          else
            gcd(b, a % b)
          end
        end

        LibC.printf("%d %d %d\\n", gcd(48, 18), gcd(100, 35), gcd(17, 13))
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("6 5 1")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # EDGE CASES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "edge cases" do
    it "handles zero values" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        zero_int = 0
        zero_float = 0.0
        LibC.printf("%d %.1f\\n", zero_int, zero_float)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("0 0.0")
    end

    it "handles negative numbers" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        neg_int = -42
        neg_float = -3.14
        LibC.printf("%d %.2f\\n", neg_int, neg_float)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("-42 -3.14")
    end

    it "handles struct with zero values" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        struct ZeroStruct
          @a : Int32
          @b : Float64

          def initialize(@a : Int32, @b : Float64)
          end

          def a : Int32
            @a
          end

          def b : Float64
            @b
          end
        end

        z = ZeroStruct.new(0, 0.0)
        LibC.printf("%d %.1f\\n", z.a, z.b)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("0 0.0")
    end

    it "handles deeply nested expressions" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        result = ((1 + 2) * (3 + 4)) - ((5 - 6) * (7 - 8))
        LibC.printf("%d\\n", result)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      # (3 * 7) - ((-1) * (-1)) = 21 - 1 = 20
      output.strip.should eq("20")
    end

    it "handles method chaining pattern" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        def double(n : Int32) : Int32
          n * 2
        end

        def add_one(n : Int32) : Int32
          n + 1
        end

        result = add_one(double(add_one(double(1))))
        LibC.printf("%d\\n", result)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      # double(1)=2, add_one(2)=3, double(3)=6, add_one(6)=7
      output.strip.should eq("7")
    end
  end

  describe "typeof in generic type refs" do
    it "monomorphizes generic instantiations with typeof(self) and typeof(arg)" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        struct Box(I, X)
          def initialize
          end
        end

        class Foo
          def initialize
          end

          def make(x)
            Box(typeof(self), typeof(x)).new
          end
        end

        Foo.new.make(123)
        LibC.printf("ok\\n")
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("ok")
    end
  end

  describe "nested type resolution" do
    it "resolves nested structs inside a class (prefers local scope)" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        struct Inner
          def ping : Int32
            1
          end
        end

        class Outer
          struct Inner
            def initialize(@x : Int32)
            end

            def ping : Int32
              @x
            end
          end

          def make : Int32
            Inner.new(7).ping
          end
        end

        o = Outer.new
        LibC.printf("%d %d\\n", o.make, Inner.new.ping)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("7 1")
    end
  end

  describe "stdlib-style combinators" do
    it "monomorphizes module-nested iterator types (with_object)" do
      source = <<-CR
        lib LibC
          fun printf(format : UInt8*, ...) : Int32
        end

        module Iterator(T)
          def with_object(obj : Int32)
            WithObjectIterator(typeof(self), T, typeof(obj)).new(self, obj)
          end

          struct WithObjectIterator(I, T, O)
            def initialize(@iter : I, @value : O)
            end

            def value : O
              @value
            end
          end
        end

        class Dir
          struct EntryIterator
            include Iterator(String)

            def initialize
            end
          end

          def each : EntryIterator
            EntryIterator.new
          end

          def run
            each.with_object(123).value
          end
        end

        v = Dir.new.run
        LibC.printf("%d\\n", v)
        CR

      output = CodegenTestHelper.compile_and_run(source)
      output.strip.should eq("123")
    end
  end
end
